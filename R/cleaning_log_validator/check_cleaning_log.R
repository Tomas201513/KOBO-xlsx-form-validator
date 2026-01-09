
# ================================
# VALIDATION FUNCTION (UNCHANGED)
# ================================
check_cleaning_log<- function(
    survey_df,
    choices_df,
    cleaning_log_df,
    cl_cols = list(uuid = "uuid",
                   question = "question",
                   old_value = "old_value",
                   change_type = "change_type",
                   new_value = "new_value"),
    required_change_type = NULL,
    sheet_names = list(survey = "survey", choices = "choices", cleaning_log = "cleaning_log"),
    header_rows_cleaning_log = 1L,
    token_split_pattern = "[\\s,]+"
) {
  
  # ---- Input Validation ----
  required_cols <- list(
    survey = c("type", "name"),
    choices = c("list_name", "name"),
    cleaning_log = unlist(cl_cols)
  )
  
  check_missing <- function(df, required_names) {
    missing_cols <- setdiff(required_names, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
    }
  }
  
  check_missing(survey_df, required_cols$survey)
  check_missing(choices_df, required_cols$choices)
  check_missing(cleaning_log_df, required_cols$cleaning_log)
  
  # ---- Normalize Column Types ----
  normalize_factors <- function(df) {
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
    df
  }
  
  survey_df <- normalize_factors(survey_df)
  choices_df <- normalize_factors(choices_df)
  cleaning_log_df <- normalize_factors(cleaning_log_df)
  
  # ---- Logging ----
  log_entries <- list()
  
  append_log <- function(row_idx, uuid, question, message, rule_id) {
    log_entries[[length(log_entries) + 1]] <<- data.frame(
      cl_row = row_idx + header_rows_cleaning_log,
      uuid = uuid,
      question = question,
      message = message,
      # rule_id = rule_id,
      stringsAsFactors = FALSE
    )
  }
  
  # ---- Allowed change types ----
  allowed_change_types <- c(
    "change_response",
    "blank_response",
    "remove_survey",
    "no_action"
  )
  
  # ---- Validate change_type & new_value rules ----
  for (i in seq_len(nrow(cleaning_log_df))) {
    
    ct <- cleaning_log_df[[cl_cols$change_type]][i]
    nv <- cleaning_log_df[[cl_cols$new_value]][i]
    uuid <- cleaning_log_df[[cl_cols$uuid]][i]
    q <- cleaning_log_df[[cl_cols$question]][i]
    
    # Invalid or NA change_type
    if (is.na(ct) || !ct %in% allowed_change_types) {
      append_log(
        i, uuid, q,
        sprintf("Invalid change_type '%s'. Allowed values: %s.",
                ct, paste(allowed_change_types, collapse = ", ")),
        "CL_INVALID_CHANGE_TYPE"
      )
      next
    }
    
    # new_value must be NA if not change_response
    if (ct != "change_response" &&
        !is.na(nv) &&
        trimws(nv) != "") {
      append_log(
        i, uuid, q,
        sprintf("new_value must be left blank when change_type is '%s'.", ct),
        "CL_NEW_VALUE_NOT_ALLOWED"
      )
    }
    
    # Question must exist in survey$name
    if (!is.na(q) && trimws(q) != "" && !q %in% survey_df$name) {
      append_log(
        i,
        uuid,
        q,
        sprintf(
          "Question '%s' does not exist in KOBO name column.",
          q
        ),
        "CL_QUESTION_NOT_IN_SURVEY"
      )
      next
    }
    
  }
  
  # ---- Duplicate action detection ----
  action_types <- c("change_response", "blank_response", "remove_survey")
  
  dup_actions <- cleaning_log_df |>
    dplyr::filter(.data[[cl_cols$change_type]] %in% action_types) |>
    dplyr::count(
      .data[[cl_cols$uuid]],
      .data[[cl_cols$question]],
      name = "n"
    ) |>
    dplyr::filter(n > 1)
  
  if (nrow(dup_actions) > 0) {
    for (i in seq_len(nrow(dup_actions))) {
      append_log(
        row_idx = 0,
        uuid = dup_actions[[cl_cols$uuid]][i],
        question = dup_actions[[cl_cols$question]][i],
        message = "Multiple conflicting actions (change_response / blank_response / remove_survey) for the same uuid and question.",
        rule_id = "CL_DUPLICATE_ACTION"
      )
    }
  }
  
  # ---- Continue with existing validations (type-based) ----
  parse_types <- function(type_str) {
    if (is.na(type_str) || type_str == "") return(list(
      base_type = NA_character_,
      list_name = NA_character_
    ))
    
    type_clean <- gsub("\\s+", " ", trimws(type_str))
    parts <- strsplit(type_clean, " ")[[1]]
    
    base_type <- parts[1]
    list_name <- if (base_type %in% c("select_one", "select_multiple") && length(parts) > 1) {
      parts[2]
    } else {
      NA_character_
    }
    
    list(
      base_type = base_type,
      list_name = list_name
    )
  }
  
  
  type_info <- lapply(survey_df$type, parse_types)
  base_type_vec <- sapply(type_info, `[[`, "base_type")
  list_name_vec <- sapply(type_info, `[[`, "list_name")
  
  name_to_base_type <- setNames(base_type_vec, survey_df$name)
  name_to_list_name <- setNames(list_name_vec, survey_df$name)
  
  choices_set <- unique(paste(choices_df$list_name, choices_df$name, sep = "|"))
  
  for (i in seq_len(nrow(cleaning_log_df))) {
    
    if (cleaning_log_df[[cl_cols$change_type]][i] != "change_response") next
    
    q <- cleaning_log_df[[cl_cols$question]][i]
    nv <- cleaning_log_df[[cl_cols$new_value]][i]
    uuid <- cleaning_log_df[[cl_cols$uuid]][i]
    
    if (is.na(name_to_base_type[q])) {
      append_log(i, uuid, q, sprintf("Question '%s' not found in survey.", q), "CL_QUESTION_MISSING")
      next
    }
    
    bt <- tolower(name_to_base_type[q])
    ln <- name_to_list_name[q]
    
    if (bt == "select_one") {
      if (!paste(ln, nv, sep = "|") %in% choices_set) {
        append_log(i, uuid, q, sprintf("Value '%s' not in choices list '%s'.", nv, ln), "CL_SELECT_ONE_BAD_CHOICE")
      }
    }
    
    if (bt == "select_multiple") {
      tokens <- unlist(strsplit(nv, " +"))
      # token_split_pattern <- " +"
      
      for (t in tokens) {
        if (!paste(ln, trimws(t), sep = "|") %in% choices_set) {
          append_log(i, uuid, q, sprintf("Token '%s' not in choices list '%s'.", t, ln), "CL_SELECT_MULTIPLE_BAD_CHOICE")
        }
      }
    }
    
    if (bt %in% c("integer", "int", "decimal") && !is.numeric(as.numeric(nv))) {
      append_log(i, uuid, q, sprintf("Non-numeric new_value '%s'.", nv), "CL_NUMERIC_NOT_NUMBER")
    }
  }
  
  # ---- Return ----
  log_df <- if (length(log_entries) == 0) {
    data.frame(
      cl_row = integer(),uuid = character(), question = character(),
               message = character(), 
               # rule_id = character(),
               )
  } else {
    do.call(rbind, log_entries)
  }
  
  list(
    valid = nrow(log_df) == 0L,
    log = log_df
  )
}