# XLS-Validator

An R-based validation platform for ODK XLSForms that wraps official ODK tools and adds custom validation rules.

## Features

- **Upload & Validate**: Upload XLSForm files (.xls, .xlsx) for instant validation
- **ODK Integration**: Uses official pyxform and ODK Validate tools (no reimplementation)
- **Custom Rules**: Extensible R-based custom validation rules
- **Interactive Editor**: Spreadsheet view with row navigation from issues
- **Corrections Workflow**: Edit cells, track changes, download corrected forms
- **Unified Reports**: Combined ODK + custom validation results in one view

## Quick Start

### Prerequisites

1. **R 4.3+**: https://cran.r-project.org/
2. **Python 3.9+**: https://www.python.org/ (with pip)
3. **Java 8+**: https://adoptium.net/

### Installation

1. Clone or download this repository

2. Install Python pyxform:
   ```bash
   pip install pyxform
   ```

3. Download ODK Validate:
   - Go to https://github.com/getodk/validate/releases
   - Download `ODK_Validate.zip`
   - Extract `ODK_Validate.jar` to the `tools/` folder

4. Install R packages:
   ```r
   install.packages(c(
     "shiny", "shinyjs", "bslib", "DT", "rhandsontable",
     "readxl", "writexl", "dplyr", "tibble", "stringr", "processx"
   ))
   ```

5. Run the app:
   ```r
   shiny::runApp()
   ```

### Docker Deployment

```bash
cd docker
docker-compose up --build
```

Access at: http://localhost:3838

## Project Structure

```
XLS-Validator/
├── app.R                    # Main Shiny application
├── R/
│   ├── odk/                 # ODK tool wrappers
│   │   ├── run_pyxform.R
│   │   ├── run_odk_validate.R
│   │   └── parse_odk_output.R
│   ├── custom_rules/        # Custom validation rules (templates)
│   │   ├── rule_registry.R
│   │   ├── duplicate_names.R
│   │   ├── mandatory_columns.R
│   │   ├── spacing_rules.R
│   │   └── naming_conventions.R
│   ├── schema/              # Validation result schema
│   ├── editor/              # XLSForm read/write/track
│   └── utils/               # Configuration and helpers
├── modules/                 # Shiny UI modules
├── www/                     # Static assets (CSS)
├── tools/                   # External tools (ODK_Validate.jar)
└── docker/                  # Docker configuration
```

## Usage Workflow

1. **Upload**: Click "Upload" and select your XLSForm (.xlsx)
2. **Review Issues**: Validation runs automatically, issues appear in the log
3. **Navigate**: Click any issue row to jump to that cell in the spreadsheet
4. **Edit**: Make corrections directly in the spreadsheet view
5. **Download**: Click "Download XLSX" to get the corrected form

## Adding Custom Rules

Custom rules are R functions in `R/custom_rules/`. To add a new rule:

1. Create a new R file (e.g., `my_rule.R`)

2. Implement the validation function:
   ```r
   check_my_rule <- function(xlsform_data) {
     issues <- create_empty_results()
     
     # Your validation logic here
     # xlsform_data$sheets contains: survey, choices, settings data frames
     
     # For each issue found:
     issues <- dplyr::bind_rows(issues, create_issue(
       id = 1,
       source = "custom",
       level = "warning",  # or "error", "info"
       sheet = "survey",
       row = 5,            # Excel row number
       field = "name",
       message = "Description of the issue",
       rule_id = "my_rule"
     ))
     
     issues
   }
   ```

3. Register the rule:
   ```r
   register_rule(
     rule_id = "my_rule",
     rule_fn = check_my_rule,
     description = "Check for my custom condition",
     level = "warning",
     enabled = TRUE
   )
   ```

## Validation Schema

All validation results follow this schema:

| Column | Type | Description |
|--------|------|-------------|
| id | integer | Unique issue ID |
| source | character | "odk" or "custom" |
| level | character | "error", "warning", "info" |
| sheet | character | "survey", "choices", "settings" |
| row | integer | Excel row number |
| field | character | Column name |
| message | character | Human-readable description |
| rule_id | character | Unique rule identifier |
| status | character | "open", "fixed", "ignored" |

## Configuration

Configuration is managed in `R/utils/config.R`. Key settings:

- `pyxform_cmd`: Path to pyxform executable
- `java_cmd`: Path to Java executable
- `odk_validate_jar`: Path to ODK_Validate.jar
- `max_file_size_mb`: Maximum upload size (default: 50MB)

## Troubleshooting

### pyxform not found
```bash
pip install pyxform
# or
python -m pip install pyxform
```

### Java not found
Install Java 8+ from https://adoptium.net/ and ensure it's in PATH.

### ODK_Validate.jar not found
Download from https://github.com/getodk/validate/releases and place in `tools/` folder.

## License

MIT License

## Credits

- [ODK](https://getodk.org/) - Open Data Kit tools
- [pyxform](https://github.com/XLSForm/pyxform) - XLSForm to XForm converter
- [R Shiny](https://shiny.rstudio.com/) - Web application framework



