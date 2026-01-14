# Custom Rule: Brackets and Connectors Validation
# Validates balanced parentheses, ${...} braces, and spacing around 'and'/'or' connectors.

# =============================================================================
# check_brackets_and_connectors()
# - Columns checked: relevant, calculation, constraint (configurable)
# - Validates:
#     * Balanced parentheses: '(' ... ')'
#     * Balanced ${...} braces (each '${' must close with '}')
#     * Spacing rule: only single spaces immediately before/after 'and' or 'or'
#       (outside quotes). All other ASCII spaces flagged.
# - Produces structured log:
#     (id, source, level, sheet, row, field, message, rule_id)
# - Does not auto-fix (validation only).
# =============================================================================
check_brackets_and_connectors <- function(df,
                                          cols = c("relevant","calculation","constraint"),
                                          sheet_name = "survey",
                                          header_rows = 1L,
                                          level_bracket = c("error","warning","info"),
                                          level_space   = c("warning","error","info"),
                                          context_radius = 20L) {
  level_bracket <- match.arg(level_bracket)
  level_space   <- match.arg(level_space)
  
  # --- Validation ---
  missing_cols <- setdiff(c("name","type", cols), names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in df: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Normalize to character
  df$type <- if (is.factor(df$type)) as.character(df$type) else df$type
  df$name <- if (is.factor(df$name)) as.character(df$name) else df$name
  for (cl in cols) {
    df[[cl]] <- if (is.factor(df[[cl]])) as.character(df[[cl]]) else df[[cl]]
  }
  
  # --- Helpers ---
  make_context <- function(s, start, end, radius = context_radius) {
    n <- nchar(s)
    left <- max(1L, start - radius)
    right <- min(n, end + radius)
    ctx <- substr(s, left, right)
    underline <- paste0(
      paste0(rep(" ", start - left), collapse = ""),
      paste0(rep("^", end - start + 1L), collapse = "")
    )
    list(context = ctx, pointer = underline, left_index = left, right_index = right)
  }
  
  # Boolean mask of positions inside single/double quotes
  quote_mask <- function(s) {
    n <- nchar(s)
    if (n == 0) return(logical(0))
    mask <- logical(n)
    active <- NULL
    for (i in seq_len(n)) {
      ch <- substr(s, i, i)
      if (is.null(active)) {
        if (ch == "'") active <- "'"
        else if (ch == "\"") active <- "\""
        # Opening quote is considered "inside" for subsequent checks
        mask[i] <- !is.null(active)
      } else {
        mask[i] <- TRUE
        if (ch == active) active <- NULL
      }
    }
    mask
  }
  
  # Find occurrences of word connectors outside quotes
  find_connectors <- function(s, qmask) {
    out <- data.frame(connector=character(0), start=integer(0), end=integer(0))
    patterns <- list(and="\\band\\b", or="\\bor\\b")
    for (nm in names(patterns)) {
      m <- gregexpr(patterns[[nm]], s, perl = TRUE)
      starts <- as.integer(m[[1]])
      lens <- attr(m[[1]], "match.length")
      if (length(starts) == 1L && starts[1] == -1L) next
      for (k in seq_along(starts)) {
        st <- starts[k]; en <- st + lens[k] - 1L
        # ensure connector span is outside quotes entirely
        if (all(qmask[st:en] == FALSE)) {
          out <- rbind(out, data.frame(connector=nm, start=st, end=en))
        }
      }
    }
    out
  }
  
  # --- Logging ---
  log_entries <- list(); log_id <- 1L
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
  
  # --- Scan target columns ---
  for (col in cols) {
    vals <- df[[col]]
    for (r in seq_along(vals)) {
      s <- vals[r]
      if (is.na(s) || s == "") next
      
      qmask <- quote_mask(s)
      n <- nchar(s)
      
      # 1) Balanced parentheses (ignore inside quotes)
      stack <- integer(0)
      for (i in seq_len(n)) {
        if (qmask[i]) next
        ch <- substr(s, i, i)
        if (ch == "(") {
          stack <- c(stack, i)
        } else if (ch == ")") {
          if (length(stack) > 0) {
            stack <- stack[-length(stack)]
          } else {
            ctx <- make_context(s, i, i)
            msg <- sprintf(
              paste0("Unmatched closing parenthesis at char %d: \")\"\n",
                     "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
              i, ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level_bracket, r, col, msg, "R_PAREN_UNMATCHED_CLOSE")
          }
        }
      }
      if (length(stack) > 0) {
        for (op in stack) {
          ctx <- make_context(s, op, op)
          msg <- sprintf(
            paste0("Unmatched opening parenthesis at char %d: \"(\"\n",
                   "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
            op, ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log(level_bracket, r, col, msg, "R_PAREN_UNMATCHED_OPEN")
        }
      }
      
      # 2) Balanced ${...} braces (ignore inside quotes)
      # Locate all full ${...} matches
      m_full <- gregexpr("\\$\\{[^}]*\\}", s, perl = TRUE)
      starts_full <- as.integer(m_full[[1]])
      lens_full <- attr(m_full[[1]], "match.length")
      full_valid <- data.frame(start=integer(0), end=integer(0))
      if (!(length(starts_full) == 1L && starts_full[1] == -1L)) {
        for (k in seq_along(starts_full)) {
          st <- starts_full[k]; en <- st + lens_full[k] - 1L
          if (all(!qmask[st:en])) {
            full_valid <- rbind(full_valid, data.frame(start=st, end=en))
          }
        }
      }
      # All '${' occurrences
      m_op <- gregexpr("\\$\\{", s, perl = TRUE)
      starts_op <- as.integer(m_op[[1]])
      if (!(length(starts_op) == 1L && starts_op[1] == -1L)) {
        for (st in starts_op) {
          if (qmask[st]) next
          # If this '${' is not the start of a full match -> unmatched open
          if (!any(full_valid$start == st)) {
            ctx <- make_context(s, st, st+1L)
            msg <- sprintf(
              paste0("Unmatched '${' at char %d-%d.\n",
                     "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
              st, st+1L, ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level_bracket, r, col, msg, "R_BRACE_UNMATCHED_OPEN")
          }
        }
      }
      # All '}' occurrences
      m_cl <- gregexpr("\\}", s, perl = TRUE)
      starts_cl <- as.integer(m_cl[[1]])
      if (!(length(starts_cl) == 1L && starts_cl[1] == -1L)) {
        for (pos in starts_cl) {
          if (qmask[pos]) next
          # If this '}' is not the end of a full match -> unmatched close
          if (!any(full_valid$end == pos)) {
            ctx <- make_context(s, pos, pos)
            msg <- sprintf(
              paste0("Unmatched '}' at char %d.\n",
                     "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
              pos, ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level_bracket, r, col, msg, "R_BRACE_UNMATCHED_CLOSE")
          }
        }
      }
      
      # 3) Spacing rule around connectors (outside quotes only)
      conns <- find_connectors(s, qmask)
      
      # Check each connector has exactly one space before and after
      if (nrow(conns) > 0) {
        for (k in seq_len(nrow(conns))) {
          st <- conns$start[k]; en <- conns$end[k]; word <- conns$connector[k]
          before_ok <- (st > 1L) && (substr(s, st-1L, st-1L) == " ") && (!qmask[st-1L])
          after_ok  <- (en < n)   && (substr(s, en+1L, en+1L) == " ") && (!qmask[en+1L])
          if (!before_ok || !after_ok) {
            ctx <- make_context(s, st, en)
            msg <- sprintf(
              paste0("Connector '%s' must have exactly one space immediately before and after at char %d-%d: \"%s\"\n",
                     "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
              word, st, en, substr(s, st, en),
              ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level_space, r, col, msg, "R_CONNECTOR_SPACING")
          }
        }
      }
      
      # Flag any other ASCII spaces outside quotes that are NOT the single allowed spaces at connector edges
      # Build allowed space positions: st-1 and en+1 for each connector
      allowed_space_pos <- integer(0)
      if (nrow(conns) > 0) {
        for (k in seq_len(nrow(conns))) {
          if (conns$start[k] > 1L)  allowed_space_pos <- c(allowed_space_pos, conns$start[k]-1L)
          if (conns$end[k]   < n)   allowed_space_pos <- c(allowed_space_pos, conns$end[k]+1L)
        }
      }
      # All spaces outside quotes
      space_pos <- which(vapply(seq_len(n), function(i) substr(s, i, i) == " " && !qmask[i], logical(1)))
      viol_spaces <- setdiff(space_pos, allowed_space_pos)
      if (length(viol_spaces) > 0) {
        for (p in viol_spaces) {
          ctx <- make_context(s, p, p)
          msg <- sprintf(
            paste0("Space not permitted (except around 'and'/'or') at char %d: \" \"\n",
                   "Context (chars %d-%d):\n%s\n%s\nNo auto-fix."),
            p, ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log(level_space, r, col, msg, "R_SPACE_OUTSIDE_CONNECTOR")
        }
      }
    }
  }
  
  # --- Return log ---
  log_df <- if (length(log_entries) == 0) {
    data.frame(id=integer(0), source=character(0), level=character(0),
               sheet=character(0), row=integer(0), field=character(0),
               message=character(0), rule_id=character(0))
  } else {
    do.call(rbind, log_entries)
  }
  
  list(valid = nrow(log_df) == 0L, log = log_df)
}


# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Check brackets and connectors (wrapper for XLS-Validator)
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
check_brackets_connectors_rule <- function(xlsform_data) {
  # Check if survey sheet exists
  if (!"survey" %in% names(xlsform_data) || is.null(xlsform_data$survey)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  survey_df <- xlsform_data$survey
  
  # Check required columns
  if (!all(c("name", "type") %in% names(survey_df))) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Determine which columns to check (only those that exist)
  target_cols <- c("relevant", "calculation", "constraint")
  cols_to_check <- intersect(target_cols, names(survey_df))
  
  if (length(cols_to_check) == 0) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Call the implementation
  result <- check_brackets_and_connectors(
    df = survey_df,
    cols = cols_to_check,
    sheet_name = "survey",
    header_rows = 1L,
    level_bracket = "error",
    level_space = "warning",
    context_radius = 20L
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
  rule_id = "brackets_connectors",
  rule_fn = check_brackets_connectors_rule,
  description = "Validate balanced parentheses, ${...} braces, and spacing around 'and'/'or' connectors",
  level = "error",
  enabled = TRUE
)




