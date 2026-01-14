# Custom Rule: No Spaces Inside Constructs
# Enforces: no spaces inside ${...}, "..." or '...' across selected columns.
# Produces structured log with detailed location info.

# =============================================================================
# check_no_spaces_inside()
# - Enforces: no spaces inside ${...}, "..." or '...' across selected columns.
# - Produces:
#     * legacy `report` (aggregated per cell)
#     * structured `log` with detailed location info:
#         id, source, level, sheet, row, field, message, rule_id
# - Logs include offending substring, inner content, and character positions.
# - Excel row numbers = data frame row index + header_rows
# =============================================================================
check_no_spaces_inside <- function(df,
                                   cols = c("name", "relevant", "calculation", "constraint"),
                                   auto_fix = FALSE,
                                   verbose = TRUE,
                                   strict_whitespace = c("space", "any"),
                                   sheet_name = "survey",
                                   header_rows = 1L,
                                   space_violation_level = c("warning", "error", "info"),
                                   context_radius = 20L) {
  strict_whitespace <- match.arg(strict_whitespace)
  space_violation_level <- match.arg(space_violation_level)
  
  # ---- Validation ----
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in data frame: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Coerce target columns to character
  df[cols] <- lapply(df[cols], function(x) if (is.factor(x)) as.character(x) else as.character(x))
  
  # ---- Patterns ----
  pat_braced <- "\\$\\{[^}]*\\}"
  pat_double <- "\"[^\"]*\""
  pat_single <- "'[^']*'"
  
  # Extract all matches incl. start positions & lengths
  # Returns list of data.frames: start, length, match, inner
  extract_matches_df <- function(s, pattern, kind = c("braces","double","single")) {
    kind <- match.arg(kind)
    if (is.na(s) || s == "") return(data.frame(start=integer(0), length=integer(0), match=character(0), inner=character(0)))
    m <- gregexpr(pattern, s, perl = TRUE)
    starts <- as.integer(m[[1]])
    lens <- attr(m[[1]], "match.length")
    if (length(starts) == 1L && starts[1] == -1L) {
      return(data.frame(start=integer(0), length=integer(0), match=character(0), inner=character(0)))
    }
    matches <- regmatches(s, m)[[1]]
    inner <- switch(
      kind,
      braces = sub("\\}$", "", sub("^\\$\\{", "", matches)),
      double = sub("\"$", "", sub("^\"", "", matches)),
      single = sub("'$", "", sub("^'", "", matches))
    )
    data.frame(start = starts, length = lens, match = matches, inner = inner, stringsAsFactors = FALSE)
  }
  
  # Space policy
  has_prohibited_ws <- function(inner) {
    if (strict_whitespace == "space") {
      grepl(" ", inner, fixed = TRUE)
    } else {
      grepl("\\s", inner, perl = TRUE) # any whitespace
    }
  }
  
  # Build a context preview string and an underline to show exact span
  make_context <- function(s, start, end, radius = context_radius) {
    n <- nchar(s)
    left <- max(1L, start - radius)
    right <- min(n, end + radius)
    ctx <- substr(s, left, right)
    # underline using spaces + ^^^^ markers
    underline <- paste0(
      paste0(rep(" ", start - left), collapse = ""),
      paste0(rep("^", end - start + 1L), collapse = "")
    )
    list(context = ctx, pointer = underline, left_index = left, right_index = right)
  }
  
  # ---- Auto-fix helpers ----
  strip_ws_inner <- function(text) {
    if (strict_whitespace == "space") {
      gsub(" +", "", text, perl = TRUE)
    } else {
      gsub("\\s+", "", text, perl = TRUE) # any whitespace
    }
  }
  
  remove_spaces_inside_braces <- function(s) {
    if (is.na(s) || s == "") return(s)
    m <- gregexpr(pat_braced, s, perl = TRUE)
    hits <- regmatches(s, m)
    if (length(hits) && length(hits[[1]]) > 0) {
      new_hits <- vapply(hits[[1]], function(h) {
        inner <- sub("^\\$\\{", "", h)
        inner <- sub("\\}$", "", inner)
        inner_nospace <- strip_ws_inner(inner)
        paste0("${", inner_nospace, "}")
      }, character(1))
      regmatches(s, m) <- list(new_hits)
    }
    s
  }
  
  remove_spaces_inside_double_quotes <- function(s) {
    if (is.na(s) || s == "") return(s)
    m <- gregexpr(pat_double, s, perl = TRUE)
    hits <- regmatches(s, m)
    if (length(hits) && length(hits[[1]]) > 0) {
      new_hits <- vapply(hits[[1]], function(h) {
        inner <- sub("^\"", "", h)
        inner <- sub("\"$", "", inner)
        inner_nospace <- strip_ws_inner(inner)
        paste0("\"", inner_nospace, "\"")
      }, character(1))
      regmatches(s, m) <- list(new_hits)
    }
    s
  }
  
  remove_spaces_inside_single_quotes <- function(s) {
    if (is.na(s) || s == "") return(s)
    m <- gregexpr(pat_single, s, perl = TRUE)
    hits <- regmatches(s, m)
    if (length(hits) && length(hits[[1]]) > 0) {
      new_hits <- vapply(hits[[1]], function(h) {
        inner <- sub("^'", "", h)
        inner <- sub("'$", "", inner)
        inner_nospace <- strip_ws_inner(inner)
        paste0("'", inner_nospace, "'")
      }, character(1))
      regmatches(s, m) <- list(new_hits)
    }
    s
  }
  
  fix_inside_constructs <- function(s) {
    if (is.na(s) || s == "") return(s)
    s <- remove_spaces_inside_braces(s)
    s <- remove_spaces_inside_double_quotes(s)
    s <- remove_spaces_inside_single_quotes(s)
    s
  }
  
  # ---- Legacy report + precise log ----
  report_list <- list()
  idx <- 1L
  
  log_entries <- list()
  log_id <- 1L
  append_log <- function(level, row_idx, field, message, rule_id) {
    log_entries[[log_id]] <<- data.frame(
      id      = log_id,
      source  = "custom",
      level   = level,
      sheet   = sheet_name,
      row     = as.integer(row_idx + header_rows),
      field   = field,
      message = message,
      rule_id = rule_id,
      stringsAsFactors = FALSE
    )
    log_id <<- log_id + 1L
  }
  
  # ---- Scan rows and optionally fix ----
  for (col in cols) {
    values <- df[[col]]
    for (r in seq_along(values)) {
      s <- values[r]
      if (is.na(s) || s == "") next
      
      # Collect detailed violations with positions:
      detailed <- list()
      
      # ${...}
      braces_df <- extract_matches_df(s, pat_braced, kind = "braces")
      if (nrow(braces_df) > 0) {
        for (i in seq_len(nrow(braces_df))) {
          inner <- braces_df$inner[i]
          if (has_prohibited_ws(inner)) {
            start <- braces_df$start[i]
            end <- start + braces_df$length[i] - 1L
            ctx <- make_context(s, start, end)
            detailed[[length(detailed) + 1L]] <- list(
              rule_id = "R_NO_SPACES_IN_BRACES",
              match = braces_df$match[i],
              inner = inner,
              start = start, end = end,
              context = ctx$context, pointer = ctx$pointer,
              context_left_index = ctx$left_index, context_right_index = ctx$right_index
            )
          }
        }
      }
      
      # "..."
      doubles_df <- extract_matches_df(s, pat_double, kind = "double")
      if (nrow(doubles_df) > 0) {
        for (i in seq_len(nrow(doubles_df))) {
          inner <- doubles_df$inner[i]
          if (has_prohibited_ws(inner)) {
            start <- doubles_df$start[i]
            end <- start + doubles_df$length[i] - 1L
            ctx <- make_context(s, start, end)
            detailed[[length(detailed) + 1L]] <- list(
              rule_id = "R_NO_SPACES_IN_DOUBLE_QUOTES",
              match = doubles_df$match[i],
              inner = inner,
              start = start, end = end,
              context = ctx$context, pointer = ctx$pointer,
              context_left_index = ctx$left_index, context_right_index = ctx$right_index
            )
          }
        }
      }
      
      # '...'
      singles_df <- extract_matches_df(s, pat_single, kind = "single")
      if (nrow(singles_df) > 0) {
        for (i in seq_len(nrow(singles_df))) {
          inner <- singles_df$inner[i]
          if (has_prohibited_ws(inner)) {
            start <- singles_df$start[i]
            end <- start + singles_df$length[i] - 1L
            ctx <- make_context(s, start, end)
            detailed[[length(detailed) + 1L]] <- list(
              rule_id = "R_NO_SPACES_IN_SINGLE_QUOTES",
              match = singles_df$match[i],
              inner = inner,
              start = start, end = end,
              context = ctx$context, pointer = ctx$pointer,
              context_left_index = ctx$left_index, context_right_index = ctx$right_index
            )
          }
        }
      }
      
      # Aggregate legacy report row if any violation exists
      if (length(detailed) > 0) {
        report_list[[idx]] <- data.frame(
          row    = r,
          column = col,
          value  = s,
          issues = paste(vapply(detailed, function(x) x$rule_id, character(1)), collapse = "|"),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
      
      # Log each violation with precise message
      if (length(detailed) > 0) {
        for (dv in detailed) {
          # Human-friendly message with positions and snippet
          msg <- sprintf(
            paste0(
              "Violation in %s at char %d-%d: %s\n",
              "Inner content with spaces: \"%s\"\n",
              "Context (chars %d-%d in field):\n%s\n%s\n",
              "%s"
            ),
            dv$rule_id, dv$start, dv$end, dv$match,
            dv$inner,
            dv$context_left_index, dv$context_right_index,
            dv$context,
            dv$pointer,
            if (auto_fix) "Auto-fix applied: spaces inside construct have been removed." else "No auto-fix."
          )
          
          append_log(
            level   = space_violation_level,
            row_idx = r,
            field   = col,
            message = msg,
            rule_id = dv$rule_id
          )
        }
        
        # Apply fix once per cell after logging
        if (auto_fix) {
          df[[col]][r] <- fix_inside_constructs(s)
        }
      }
    }
  }
  
  # finalize report + log
  report <- if (length(report_list) == 0) {
    data.frame(row = integer(0), column = character(0), value = character(0), issues = character(0))
  } else {
    do.call(rbind, report_list)
  }
  
  log_df <- if (length(log_entries) == 0) {
    data.frame(id=integer(0), source=character(0), level=character(0),
               sheet=character(0), row=integer(0), field=character(0),
               message=character(0), rule_id=character(0))
  } else {
    do.call(rbind, log_entries)
  }
  
  all_ok <- nrow(report) == 0L
  
  if (verbose) {
    if (all_ok) {
      message(sprintf("All checks passed (%s whitespace policy).",
                      if (strict_whitespace == "space") "ASCII space only" else "ANY whitespace"))
    } else if (!auto_fix) {
      message(sprintf("Found %d violation row(s). Set auto_fix=TRUE to automatically remove spaces inside constructs.",
                      nrow(report)))
    } else {
      message(sprintf("Auto-fix applied. %d violation row(s) were detected and logged.",
                      nrow(report)))
    }
  }
  
  list(
    valid  = all_ok,
    data   = df,
    report = report,
    log    = log_df
  )
}


# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Check for spaces inside constructs (wrapper for XLS-Validator)
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
check_no_spaces_inside_rule <- function(xlsform_data) {
  # Check if survey sheet exists
  if (!"survey" %in% names(xlsform_data) || is.null(xlsform_data$survey)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  survey_df <- xlsform_data$survey
  
  # Determine which columns to check (only those that exist)
  target_cols <- c("name", "relevant", "calculation", "constraint", "choice_filter")
  cols_to_check <- intersect(target_cols, names(survey_df))
  
  if (length(cols_to_check) == 0) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Call the implementation
  result <- check_no_spaces_inside(
    df = survey_df,
    cols = cols_to_check,
    auto_fix = FALSE,
    verbose = FALSE,
    strict_whitespace = "space",
    sheet_name = "survey",
    header_rows = 1L,
    space_violation_level = "warning"
  )
  
  log_df <- result$log
  
  # Add status column if missing
  if (nrow(log_df) > 0 && !"status" %in% names(log_df)) {
    log_df$status <- "open"
  }
  
  # Return empty with correct schema if no issues
  if (nrow(log_df) == 0) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  log_df
}

# Register rule
register_rule(
  rule_id = "no_spaces_inside",
  rule_fn = check_no_spaces_inside_rule,
  description = "Check for spaces inside ${...}, '...' and \"...\" constructs",
  level = "warning",
  enabled = TRUE
)
