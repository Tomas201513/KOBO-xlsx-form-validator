# Validation Schema for XLS-Validator
# Defines the unified output format for all validation results

#' Create an empty validation results tibble
#' @return Empty tibble with validation schema columns
create_empty_results <- function() {
 tibble::tibble(
    id = integer(),
    source = character(),
    level = character(),
    sheet = character(),
    row = integer(),
    field = character(),
    message = character(),
    rule_id = character(),
    status = character()
  )
}

#' Create a single validation issue record
#' @param id Unique issue ID
#' @param source "odk" or "custom"
#' @param level "error", "warning", or "info"
#' @param sheet Sheet name (survey, choices, settings)
#' @param row Row number in the sheet (1-based, excluding header)
#' @param field Column/field name
#' @param message Human-readable issue description
#' @param rule_id Unique rule identifier
#' @param status "open", "fixed", or "ignored"
#' @return Single-row tibble
create_issue <- function(
  id,
  source,
  level,
  sheet = NA_character_,
  row = NA_integer_,
  field = NA_character_,
  message,
  rule_id,
  status = "open"
) {
  # Validate inputs
  stopifnot(source %in% c("odk", "custom"))
  stopifnot(level %in% c("error", "warning", "info"))
  stopifnot(status %in% c("open", "fixed", "ignored"))
  
  tibble::tibble(
    id = as.integer(id),
    source = as.character(source),
    level = as.character(level),
    sheet = as.character(sheet),
    row = as.integer(row),
    field = as.character(field),
    message = as.character(message),
    rule_id = as.character(rule_id),
    status = as.character(status)
  )
}

#' Combine multiple validation results into one tibble
#' @param ... Validation result tibbles to combine
#' @return Combined tibble with renumbered IDs
combine_results <- function(...) {
  results <- dplyr::bind_rows(...)
  
  if (nrow(results) == 0) {
    return(create_empty_results())
  }
  
  # Renumber IDs sequentially
  results$id <- seq_len(nrow(results))
  
  # Sort by priority: ODK errors first, then warnings, then custom
  level_order <- c("error" = 1, "warning" = 2, "info" = 3)
  source_order <- c("odk" = 1, "custom" = 2)
  
  results <- results |>
    dplyr::mutate(
      .level_order = level_order[level],
      .source_order = source_order[source]
    ) |>
    dplyr::arrange(.source_order, .level_order, sheet, row) |>
    dplyr::select(-c(.level_order, .source_order))
  
  # Renumber after sorting
  results$id <- seq_len(nrow(results))
  
  results
}

#' Validate that a results tibble conforms to the schema
#' @param results Tibble to validate
#' @return TRUE if valid, stops with error otherwise
validate_results_schema <- function(results) {
  required_cols <- c("id", "source", "level", "sheet", "row", "field", "message", "rule_id", "status")
  
  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check data types
  if (!is.integer(results$id) && !is.numeric(results$id)) {
    stop("Column 'id' must be integer")
  }
  
  valid_sources <- c("odk", "custom")
  invalid_sources <- setdiff(unique(results$source), valid_sources)
  if (length(invalid_sources) > 0) {
    stop("Invalid source values: ", paste(invalid_sources, collapse = ", "))
  }
  
  valid_levels <- c("error", "warning", "info")
  invalid_levels <- setdiff(unique(results$level), valid_levels)
  if (length(invalid_levels) > 0) {
    stop("Invalid level values: ", paste(invalid_levels, collapse = ", "))
  }
  
  valid_statuses <- c("open", "fixed", "ignored")
  invalid_statuses <- setdiff(unique(results$status), valid_statuses)
  if (length(invalid_statuses) > 0) {
    stop("Invalid status values: ", paste(invalid_statuses, collapse = ", "))
  }
  
  TRUE
}

#' Get summary statistics for validation results
#' @param results Validation results tibble
#' @return List with counts by source, level, and status
summarize_results <- function(results) {
  list(
    total = nrow(results),
    by_source = table(results$source),
    by_level = table(results$level),
    by_status = table(results$status),
    by_sheet = table(results$sheet),
    errors = sum(results$level == "error"),
    warnings = sum(results$level == "warning"),
    info = sum(results$level == "info"),
    open = sum(results$status == "open"),
    fixed = sum(results$status == "fixed")
  )
}



