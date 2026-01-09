# Custom Rule: Mandatory Columns Check
# Template for checking required columns exist and have values
#
# TODO: Implement your custom logic here
# This rule should verify required columns (type, name, label) are present

#' Check for missing mandatory columns in XLSForm
#'
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
#'
check_mandatory_columns <- function(xlsform_data) {
  # Template - implement your logic here
  #
  # Required columns by sheet:
  # - survey: type, name (label recommended)
  # - choices: list_name, name (label recommended)
  # - settings: (form_title, form_id recommended)
  #
  # Check both:
  # 1. Column exists in the sheet
  # 2. Column has non-empty values in each row
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
#   rule_id = "mandatory_columns",
#   rule_fn = check_mandatory_columns,
#   description = "Check that required columns exist and have values",
#   level = "error",
#   enabled = TRUE
# )

