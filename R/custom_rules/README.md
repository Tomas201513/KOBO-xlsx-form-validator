# Custom Validation Rules Guide

A practical guide for writing custom XLSForm validation rules in XLS-Validator.

---

## Quick Start

```r
# 1. Create your rule file: R/custom_rules/my_rule.R

check_my_rule <- function(xlsform_data) {
  issues <- create_empty_results()
  
  # Your validation logic here...
  
  issues
}

register_rule(
  rule_id = "my_rule",
  rule_fn = check_my_rule,
  description = "What this rule checks",
  level = "warning",
  enabled = TRUE
)
```

---

## Input: What You Receive

Your function receives `xlsform_data`, a named list containing the XLSForm sheets as data frames:

```r
xlsform_data <- list(
  survey = data.frame(...),   # Survey questions
  choices = data.frame(...),  # Choice lists
  settings = data.frame(...)  # Form settings
)
```

### Common Columns

| Sheet | Key Columns |
|-------|-------------|
| `survey` | `type`, `name`, `label`, `hint`, `required`, `relevant`, `constraint`, `calculation` |
| `choices` | `list_name`, `name`, `label` |
| `settings` | `form_title`, `form_id`, `version` |

### Accessing Data

```r
# Get survey sheet
survey <- xlsform_data$survey

# Check if column exists
if ("name" %in% names(survey)) {
  names_col <- survey$name
}

# Iterate rows
for (i in seq_len(nrow(survey))) {
  value <- survey$name[i]
  # ...
}
```

---

## Output: Issue Schema

Return a tibble with exactly these 9 columns:

| Column | Type | Values | Description |
|--------|------|--------|-------------|
| `id` | integer | 1, 2, 3... | Unique issue ID |
| `source` | character | `"custom"` | Auto-set by registry |
| `level` | character | `"error"`, `"warning"`, `"info"` | Severity |
| `sheet` | character | `"survey"`, `"choices"`, `"settings"`, `NA` | Which sheet |
| `row` | integer | 2, 3, 4... or `NA` | Excel row number |
| `field` | character | Column name or `NA` | Which column |
| `message` | character | Any string | Human-readable description |
| `rule_id` | character | Your rule ID | Matches registration |
| `status` | character | `"open"` | Always "open" for new issues |

### Row Number Convention

```
Excel Row 1 = Header row
Excel Row 2 = First data row (R index 1)
Excel Row 3 = Second data row (R index 2)
...

Formula: excel_row = r_index + 1
```

---

## Helper Functions

### `create_empty_results()`
Returns an empty tibble with the correct schema. Always start with this.

```r
issues <- create_empty_results()
```

### `create_issue(...)`
Creates a single issue row with validation.

```r
new_issue <- create_issue(
  id = 1,
  source = "custom",
  level = "error",
  sheet = "survey",
  row = 5,
  field = "name",
  message = "Problem description",
  rule_id = "my_rule",
  status = "open"
)
```

### `register_rule(...)`
Registers your rule with the system.

```r
register_rule(
  rule_id = "my_rule",      # Unique identifier
  rule_fn = check_my_rule,  # Your function
  description = "...",      # Shown in settings
  level = "warning",        # Default severity
  enabled = TRUE            # Active by default
)
```

---

## Complete Example

```r
# R/custom_rules/empty_labels.R
# Checks for questions missing labels

check_empty_labels <- function(xlsform_data) {
  issues <- create_empty_results()
  issue_id <- 0
  
  # Skip if no survey sheet
  if (!"survey" %in% names(xlsform_data)) {
    return(issues)
  }
  
  survey <- xlsform_data$survey
  
  # Skip if no label column
  if (!"label" %in% names(survey)) {
    return(issues)
  }
  
  # Check each row
  for (i in seq_len(nrow(survey))) {
    # Skip notes and groups (they may not need labels)
    type <- survey$type[i]
    if (is.na(type)) next
    if (grepl("^(note|begin|end)", type)) next
    
    # Check if label is empty
    label <- survey$label[i]
    if (is.na(label) || trimws(label) == "") {
      issue_id <- issue_id + 1
      
      field_name <- if ("name" %in% names(survey)) survey$name[i] else NA
      
      issues <- dplyr::bind_rows(issues, create_issue(
        id = issue_id,
        source = "custom",
        level = "warning",
        sheet = "survey",
        row = i + 1,  # Convert to Excel row
        field = "label",
        message = sprintf("Question '%s' is missing a label", 
                          ifelse(is.na(field_name), paste("row", i), field_name)),
        rule_id = "empty_labels",
        status = "open"
      ))
    }
  }
  
  issues
}

register_rule(
  rule_id = "empty_labels",
  rule_fn = check_empty_labels,
  description = "Check for questions without labels",
  level = "warning",
  enabled = TRUE
)
```

---

## Severity Levels

| Level | Use When | UI Color |
|-------|----------|----------|
| `"error"` | Form will fail ODK validation or won't work | Red |
| `"warning"` | Potential problem, form may still work | Yellow |
| `"info"` | Suggestion or style recommendation | Blue |

---

## Common Patterns

### Check for duplicates
```r
name_counts <- table(survey$name[!is.na(survey$name)])
duplicates <- names(name_counts[name_counts > 1])
```

### Check for whitespace issues
```r
has_leading <- grepl("^\\s", value)
has_trailing <- grepl("\\s$", value)
```

### Validate naming conventions
```r
# Only alphanumeric and underscores
is_valid <- grepl("^[a-zA-Z][a-zA-Z0-9_]*$", name)
```

### Check referenced choice lists exist
```r
# Get all list_names from choices
choice_lists <- unique(xlsform_data$choices$list_name)

# Check select_one/select_multiple references
for (i in seq_len(nrow(survey))) {
  type <- survey$type[i]
  if (grepl("^select_(one|multiple)\\s+", type)) {
    list_ref <- sub("^select_(one|multiple)\\s+", "", type)
    if (!list_ref %in% choice_lists) {
      # Create issue for missing list
    }
  }
}
```

---

## Testing Your Rule

1. Place your `.R` file in `R/custom_rules/`
2. Restart the Shiny app (rules auto-load on startup)
3. Go to **Settings** tab to verify your rule appears
4. Upload a test form to see your issues in the log

---

## File Structure

```
R/custom_rules/
├── rule_registry.R      # Core registry (don't modify)
├── duplicate_names.R    # Template
├── mandatory_columns.R  # Template
├── naming_conventions.R # Template
├── spacing_rules.R      # Template
└── your_rule.R          # Your custom rules
```

---

## Tips

1. **Always return a tibble** - Even if empty, use `create_empty_results()`
2. **Handle missing columns** - Check `"column" %in% names(df)` before accessing
3. **Handle NA values** - Use `is.na()` checks before string operations
4. **Use descriptive messages** - Include the problematic value in the message
5. **Keep rules focused** - One rule = one type of check
6. **Test edge cases** - Empty sheets, missing columns, NA values

---

## API Reference

### Registry Functions

| Function | Description |
|----------|-------------|
| `register_rule(rule_id, rule_fn, description, level, enabled)` | Register a new rule |
| `list_rules()` | Get data frame of all registered rules |
| `set_rule_enabled(rule_id, enabled)` | Enable/disable a rule |
| `get_registered_rules(enabled_only)` | Get rule definitions |
| `run_custom_rules(xlsform_data)` | Execute all enabled rules |

### Schema Functions

| Function | Description |
|----------|-------------|
| `create_empty_results()` | Empty tibble with correct schema |
| `create_issue(...)` | Create single issue row |
| `combine_results(...)` | Merge multiple result tibbles |
| `summarize_results(results)` | Get counts by level/status |

