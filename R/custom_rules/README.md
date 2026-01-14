# Custom Validation Rules

Documentation for the custom XLSForm validation rules in XLS-Validator.

---

## Implemented Rules

### 1. No Spaces Inside Constructs (`no_spaces_inside`)

**File:** `no_spaces_inside.R`  
**Level:** warning  
**Columns checked:** `name`, `relevant`, `calculation`, `constraint`, `choice_filter`

Detects spaces inside:
- Variable references: `${...)}` (e.g., `${ my_var }`)
- Double-quoted strings: `"..."` (e.g., `"my choice "`)
- Single-quoted strings: `'...'` (e.g., `' yes'`)

**Rule IDs:**
- `R_NO_SPACES_IN_BRACES`
- `R_NO_SPACES_IN_DOUBLE_QUOTES`
- `R_NO_SPACES_IN_SINGLE_QUOTES`

---

### 2. Selected Function Validation (`selected_validation`)

**File:** `selected_validation.R`  
**Level:** error  
**Columns checked:** `name`, `relevant`, `calculation`, `constraint`, `choice_filter`

Validates `selected(${var}, 'choice')` expressions:
- Variable `${var}` must exist in survey
- Variable must be defined before it is referenced
- Variable must be `select_one` or `select_multiple` type
- Choice value must exist in choices sheet under the correct `list_name`

**Rule IDs:**
- `R_SELECTED_VAR_MISSING` - Referenced variable not found
- `R_SELECTED_REFERENCE_FUTURE` - Variable defined after reference
- `R_SELECTED_VAR_NOT_SELECT` - Variable is not a select type
- `R_SELECTED_CHOICE_MISSING` - Choice not found in choices sheet

---

### 3. Comparison Operators Validation (`comparisons`)

**File:** `comparisons.R`  
**Level:** error  
**Columns checked:** `relevant`, `calculation`, `constraint`

Validates comparison operators (`>`, `>=`, `<`, `<=`):
- Checks operand types (dot `.` and `${var}` references)
- Validates referenced variables exist
- Ensures operand types are comparable (integer, decimal, date, calculate)

**Rule IDs:**
- `R_COMPARISON_LEFT_VAR_MISSING` - Left operand variable not found
- `R_COMPARISON_RIGHT_VAR_MISSING` - Right operand variable not found
- `R_COMPARISON_LEFT_INVALID_TYPE` - Left operand has invalid type for comparison
- `R_COMPARISON_RIGHT_INVALID_TYPE` - Right operand has invalid type for comparison
- `R_COMPARISON_REFERENCE_FUTURE_LEFT` - Left operand references future question (optional)
- `R_COMPARISON_REFERENCE_FUTURE_RIGHT` - Right operand references future question (optional)

---

### 4. Brackets and Connectors Validation (`brackets_connectors`)

**File:** `brackets_connectors.R`  
**Level:** error (brackets), warning (spacing)  
**Columns checked:** `relevant`, `calculation`, `constraint`

Validates syntax structure:
- Balanced parentheses: `(` must have matching `)`
- Balanced braces: `${` must have matching `}`
- Connector spacing: `and`/`or` must have exactly one space before and after
- No extra spaces outside of connector contexts

**Rule IDs:**
- `R_PAREN_UNMATCHED_OPEN` - Opening `(` without closing `)`
- `R_PAREN_UNMATCHED_CLOSE` - Closing `)` without opening `(`
- `R_BRACE_UNMATCHED_OPEN` - Opening `${` without closing `}`
- `R_BRACE_UNMATCHED_CLOSE` - Closing `}` without opening `${`
- `R_CONNECTOR_SPACING` - Incorrect spacing around `and`/`or`
- `R_SPACE_OUTSIDE_CONNECTOR` - Space not permitted outside connector context

---

### 5. Cross-Sheet Reference Validation (`cross_sheet_refs`)

**File:** `cross_sheet_refs.R`  
**Level:** error (missing), info (orphaned)  
**Sheets checked:** `survey` (type column), `choices` (list_name column)

Validates list_name references between survey and choices:
- `select_one <list_name>` must reference an existing list in choices
- `select_multiple <list_name>` must reference an existing list in choices
- Reports orphaned choice lists that are never referenced (info level)

**Rule IDs:**
- `R_LIST_NAME_MISSING` - Referenced list_name not found in choices sheet
- `R_ORPHANED_CHOICE_LIST` - Choice list defined but never referenced in survey

---

## File Structure

```
R/custom_rules/
├── rule_registry.R          # Core registry system (do not modify)
├── README.md                 # This documentation
├── no_spaces_inside.R        # Rule 1: Space detection in constructs
├── selected_validation.R     # Rule 2: selected() function validation
├── comparisons.R             # Rule 3: Comparison operator validation
├── brackets_connectors.R     # Rule 4: Syntax structure validation
└── cross_sheet_refs.R        # Rule 5: Cross-sheet reference validation
```

---

## Adding New Rules

To add a new custom rule:

### 1. Create a new R file

```r
# R/custom_rules/my_rule.R

check_my_rule <- function(xlsform_data) {
  # Return empty data.frame with correct schema if no issues
  if (!"survey" %in% names(xlsform_data)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Your validation logic here...
  # Build log_df with issues found
  
  log_df
}

register_rule(
  rule_id = "my_rule",
  rule_fn = check_my_rule,
  description = "Description shown in settings",
  level = "warning",
  enabled = TRUE
)
```

### 2. Output Schema

Return a data.frame with these 9 columns:

| Column | Type | Description |
|--------|------|-------------|
| `id` | integer | Unique issue ID (1, 2, 3...) |
| `source` | character | Always `"custom"` |
| `level` | character | `"error"`, `"warning"`, or `"info"` |
| `sheet` | character | `"survey"`, `"choices"`, or `"settings"` |
| `row` | integer | Excel row number (data row index + 1) |
| `field` | character | Column name where issue was found |
| `message` | character | Human-readable description |
| `rule_id` | character | Unique rule identifier |
| `status` | character | Always `"open"` for new issues |

### 3. Row Number Convention

```
Excel Row 1 = Header row
Excel Row 2 = First data row (R index 1)
Excel Row 3 = Second data row (R index 2)

Formula: excel_row = r_index + 1
```

---

## Input Data Structure

Your function receives `xlsform_data`, a named list:

```r
xlsform_data <- list(
  survey = data.frame(...),    # Survey questions
  choices = data.frame(...),   # Choice lists
  settings = data.frame(...)   # Form settings
)
```

### Common Columns

| Sheet | Key Columns |
|-------|-------------|
| `survey` | `type`, `name`, `label`, `relevant`, `constraint`, `calculation` |
| `choices` | `list_name`, `name`, `label` |
| `settings` | `form_title`, `form_id`, `version` |

---

## Severity Levels

| Level | Use When |
|-------|----------|
| `error` | Form will fail validation or won't work correctly |
| `warning` | Potential problem, form may still work |
| `info` | Suggestion or style recommendation |

---

## Best Practices

1. **Check for required sheets/columns** before accessing them
2. **Handle NA values** with `is.na()` checks
3. **Return empty data.frame** with correct schema when no issues found
4. **Use descriptive messages** including the problematic value
5. **Set verbose = FALSE** when calling implementations from wrappers

---

## API Reference

### Registry Functions

| Function | Description |
|----------|-------------|
| `register_rule(rule_id, rule_fn, description, level, enabled)` | Register a rule |
| `list_rules()` | Get data frame of all registered rules |
| `set_rule_enabled(rule_id, enabled)` | Enable/disable a rule |
| `get_registered_rules(enabled_only)` | Get rule definitions |
| `run_custom_rules(xlsform_data)` | Execute all enabled rules |
