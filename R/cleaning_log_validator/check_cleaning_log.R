check_cleaning_log <- function(
    survey_df,
    choices_df,
    cleaning_log_df,
    cl_cols = list(
      uuid = "uuid",
      question = "question",
      old_value = "old_value",
      change_type = "change_type",
      new_value = "new_value"
    ),
    required_change_type = NULL,
    sheet_names = list(
      survey = "survey",
      choices = "choices",
      cleaning_log = "cleaning_log"
    ),
    header_rows_cleaning_log = 1L,
    token_split_pattern = "[\\s,]+"
) {
  
  # ================================
  # 1. INPUT VALIDATION
  # ================================
  required_cols <- list(
    survey = c("type", "name"),
    choices = c("list_name", "name"),
    cleaning_log = unlist(cl_cols)
  )
  
  check_missing <- function(df, req) {
    miss <- setdiff(req, names(df))
    if (length(miss) > 0) {
      stop(sprintf("Missing columns: %s", paste(miss, collapse = ", ")))
    }
  }
  
  check_missing(survey_df, required_cols$survey)
  check_missing(choices_df, required_cols$choices)
  check_missing(cleaning_log_df, required_cols$cleaning_log)
  
  # ================================
  # 2. NORMALIZATION
  # ================================
  normalize <- function(df) {
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
    df
  }
  
  survey_df       <- normalize(survey_df)
  choices_df      <- normalize(choices_df)
  cleaning_log_df <- normalize(cleaning_log_df)
  
  # Remove non-question rows
  cleaning_log_df <- cleaning_log_df |>
    dplyr::mutate(
      row = dplyr::row_number(),
      cl_row = row + header_rows_cleaning_log
    ) |>
    dplyr::filter(!.data[[cl_cols$question]] %in% c(
      "percentage_missing",
      "duration_audit_sum_all_minutes"
    ))
  
  # ================================
  # 3. LOGGING INFRASTRUCTURE
  # ================================
  log_entries <- list()
  
  append_log <- function(cl_row, uuid, question, message, rule_id) {
    log_entries[[length(log_entries) + 1]] <<- data.frame(
      cl_row   = cl_row,
      uuid     = uuid,
      question = question,
      message  = message,
      rule_id  = rule_id,
      stringsAsFactors = FALSE
    )
  }
  
  # ================================
  # 4. SURVEY / CHOICES LOOKUPS
  # ================================
  parse_type <- function(x) {
    if (is.na(x) || x == "") return(c(NA, NA))
    p <- strsplit(gsub("\\s+", " ", trimws(x)), " ")[[1]]
    c(
      base = p[1],
      list = if (p[1] %in% c("select_one", "select_multiple") && length(p) > 1) p[2] else NA
    )
  }
  
  type_mat <- t(vapply(survey_df$type, parse_type, character(2)))
  name_to_base_type <- setNames(tolower(type_mat[, 1]), survey_df$name)
  name_to_list_name <- setNames(type_mat[, 2], survey_df$name)
  
  choices_set <- unique(paste(choices_df$list_name, choices_df$name, sep = "|"))
  
  allowed_change_types <- c(
    "change_response",
    "blank_response",
    "remove_survey",
    "no_action"
  )
  
  # ================================
  # 5. ROW-BY-ROW VALIDATION
  # ================================
  for (i in seq_len(nrow(cleaning_log_df))) {
    
    ct   <- cleaning_log_df[[cl_cols$change_type]][i]
    nv   <- cleaning_log_df[[cl_cols$new_value]][i]
    q    <- cleaning_log_df[[cl_cols$question]][i]
    uuid <- cleaning_log_df[[cl_cols$uuid]][i]
    cl_r <- cleaning_log_df$cl_row[i]
    
    # ---- change_type validity ----
    if (is.na(ct) || !ct %in% allowed_change_types) {
      append_log(
        cl_r, uuid, q,
        sprintf("Invalid change_type '%s'. Allowed: %s",
                ct, paste(allowed_change_types, collapse = ", ")),
        "CL_INVALID_CHANGE_TYPE"
      )
      next
    }
    
    # ---- new_value allowed only for change_response ----
    if (ct != "change_response" && !is.na(nv) && trimws(nv) != "") {
      append_log(
        cl_r, uuid, q,
        sprintf("new_value must be blank when change_type is '%s'.", ct),
        "CL_NEW_VALUE_NOT_ALLOWED"
      )
    }
    
    # ---- slash-style select_multiple ----
    if (!is.na(q) && grepl("/", q)) {
      
      parts <- strsplit(q, "/", fixed = TRUE)[[1]]
      base_q <- parts[1]
      choice <- parts[2]
      
      if (!base_q %in% survey_df$name) {
        append_log(cl_r, uuid, q,
                   sprintf("Base question '%s' not found in survey.", base_q),
                   "CL_QUESTION_NOT_IN_SURVEY")
        next
      }
      
      if (name_to_base_type[base_q] != "select_multiple") {
        append_log(cl_r, uuid, q,
                   "Slash notation only valid for select_multiple questions.",
                   "CL_INVALID_SLASH_USAGE")
        next
      }
      
      ln <- name_to_list_name[base_q]
      if (!paste(ln, choice, sep = "|") %in% choices_set) {
        append_log(
          cl_r, uuid, q,
          sprintf("Choice '%s' not in list '%s'.", choice, ln),
          "CL_SELECT_MULTIPLE_BAD_CHOICE"
        )
      }
    }
  }
  
  # ================================
  # 6. DUPLICATE & CONFLICT CHECKS
  # ================================
  dup_q <- cleaning_log_df |>
    dplyr::filter(.data[[cl_cols$change_type]] %in% c("change_response", "blank_response")) |>
    dplyr::count(
      .data[[cl_cols$uuid]],
      .data[[cl_cols$question]],
      name = "n"
    ) |>
    dplyr::filter(n > 1)
  
  for (i in seq_len(nrow(dup_q))) {
    append_log(
      cl_row = NA,
      uuid = dup_q[[cl_cols$uuid]][i],
      question = dup_q[[cl_cols$question]][i],
      message = "Multiple change_response / blank_response actions for the same uuid and question.",
      rule_id = "CL_DUPLICATE_ACTION"
    )
  }
  
  dup_remove <- cleaning_log_df |>
    dplyr::filter(.data[[cl_cols$change_type]] %in% c(
      "change_response", "blank_response", "remove_survey"
    )) |>
    dplyr::group_by(.data[[cl_cols$uuid]]) |>
    dplyr::summarise(
      has_remove = any(.data[[cl_cols$change_type]] == "remove_survey"),
      n_actions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(has_remove & n_actions > 1)
  
  for (i in seq_len(nrow(dup_remove))) {
    append_log(
      cl_row = NA,
      uuid = dup_remove[[cl_cols$uuid]][i],
      question = NA,
      message = "remove_survey conflicts with other actions for the same uuid.",
      rule_id = "CL_REMOVE_SURVEY_CONFLICT"
    )
  }
  
  # ================================
  # 7. TYPE-SPECIFIC new_value CHECKS
  # ================================
  non_slash <- cleaning_log_df |>
    dplyr::filter(!grepl("/", .data[[cl_cols$question]]))
  
  for (i in seq_len(nrow(non_slash))) {
    
    if (non_slash[[cl_cols$change_type]][i] != "change_response") next
    
    q    <- non_slash[[cl_cols$question]][i]
    nv   <- non_slash[[cl_cols$new_value]][i]
    uuid <- non_slash[[cl_cols$uuid]][i]
    cl_r <- non_slash$cl_row[i]
    
    bt <- name_to_base_type[q]
    ln <- name_to_list_name[q]
    
    if (is.na(bt)) {
      append_log(cl_r, uuid, q,
                 sprintf("Question '%s' not found in survey.", q),
                 "CL_QUESTION_MISSING")
      next
    }
    
    if (bt == "select_one" &&
        !paste(ln, nv, sep = "|") %in% choices_set) {
      append_log(cl_r, uuid, q,
                 sprintf("Value '%s' not in choices list '%s'.", nv, ln),
                 "CL_SELECT_ONE_BAD_CHOICE")
    }

    if (bt == "select_multiple") {
      tokens <- unlist(strsplit(nv, " +"))

      for (t in tokens) {
        if (!paste(ln, trimws(t), sep = "|") %in% choices_set) {
          append_log(cl_r, uuid, q, sprintf("Token '%s' not in choices list '%s'.", t, ln), "CL_SELECT_MULTIPLE_BAD_CHOICE")
        }
      }
    }
    
    if (bt %in% c("integer", "int", "decimal") &&
        suppressWarnings(is.na(as.numeric(nv)))) {
      append_log(cl_r, uuid, q,
                 sprintf("Non-numeric new_value '%s'.", nv),
                 "CL_NUMERIC_NOT_NUMBER")
    }
  }
  
  # ================================
  # 8. RETURN
  # ================================
  log_df <- if (length(log_entries) == 0) {
    data.frame(
      cl_row = integer(),
      uuid = character(),
      question = character(),
      message = character(),
      rule_id = character()
    )
  } else {
    do.call(rbind, log_entries) |>
      dplyr::arrange(cl_row)
  }
  
  list(
    valid = nrow(log_df) == 0,
    log = log_df
  )
}
