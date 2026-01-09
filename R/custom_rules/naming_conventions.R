# Custom Rule: Naming Conventions
# Template for enforcing naming patterns (e.g., snake_case)
#
# TODO: Implement your custom logic here
# This rule should enforce your organization's naming conventions

#' Check field naming conventions in XLSForm
#'
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
#'
check_naming_conventions <- function(xlsform_data) {
  # Template - implement your logic here
  #
  # Common naming convention checks:
  # 1. snake_case: field_name (lowercase, underscores)
  # 2. No special characters except underscore
  # 3. Must start with a letter
  # 4. Maximum length (e.g., 32 characters)
  # 5. No reserved words (e.g., 'name', 'type', 'label')
  #
  # ODK field name requirements:
  # - Must be valid XML element names
  # - Cannot start with numbers or special chars
  # - Cannot contain spaces
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
#   rule_id = "naming_conventions",
#   rule_fn = check_naming_conventions,
#   description = "Enforce field naming conventions (snake_case, valid characters)",
#   level = "warning",
#   enabled = TRUE
# )


