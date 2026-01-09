# Custom Rule: Spacing/Whitespace Issues
# Template for detecting leading/trailing whitespace
#
# TODO: Implement your custom logic here
# This rule should find cells with problematic whitespace

#' Check for whitespace issues in XLSForm
#'
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
#'
check_spacing_issues <- function(xlsform_data) {
  # Template - implement your logic here
  #
  # Common whitespace issues to check:
  # 1. Leading spaces: " field_name"
  # 2. Trailing spaces: "field_name "
  # 3. Multiple consecutive spaces in labels
  # 4. Tab characters in field names
  #
  # Focus on critical columns:
  # - name, type, list_name (should have NO whitespace issues)
  # - label, hint (minor - warning level)
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
#   rule_id = "spacing_issues",
#   rule_fn = check_spacing_issues,
#   description = "Check for leading/trailing whitespace in field names and values",
#   level = "warning",
#   enabled = TRUE
# )


