# Custom Rule: Duplicate Field Names
# Template for detecting duplicate field names in XLSForm
#
# TODO: Implement your custom logic here
# This rule should check for duplicate 'name' values within and across sheets

#' Check for duplicate field names in XLSForm
#'
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
#'
check_duplicate_names <- function(xlsform_data) {
  # Template - implement your logic here
  # 
  # Example implementation outline:
  # 1. Get the 'name' column from survey sheet

# 2. Find duplicates using table() or duplicated()
  # 3. For each duplicate, create an issue with:
  #    - level = "error" (duplicates are serious)
  #    - sheet = "survey" or "choices"
  #    - row = the row number where duplicate appears
  #    - field = "name"
  #    - message = description of the duplicate
  #
  # Must return tibble with columns:
  # id, source, level, sheet, row, field, message, rule_id, status
  
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

# Register this rule
# Uncomment and modify once implemented:
# register_rule(
#   rule_id = "duplicate_names",
#   rule_fn = check_duplicate_names,
#   description = "Check for duplicate field names in survey and choices sheets",
#   level = "error",
#   enabled = TRUE
# )

