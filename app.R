# XLS-Validator - ODK XLSForm Validation Platform
# Main Shiny Application Entry Point

# Load required libraries
library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(rhandsontable)
library(readxl)
library(writexl)
library(dplyr)
library(tibble)
library(stringr)
library(processx)
library(shinycssloaders)


# Source all R modules
source("R/utils/config.R")
source("R/utils/file_utils.R")
source("R/schema/validation_schema.R")
source("R/odk/run_pyxform.R")
source("R/odk/run_odk_validate.R")
source("R/odk/parse_odk_output.R")
source("R/editor/xlsform_reader.R")
source("R/editor/change_tracker.R")
source("R/editor/xlsform_writer.R")
source("R/custom_rules/rule_registry.R")
source("R/validate.R")
source("R/cleaning_log_validator/check_cleaning_log.R")

# Source Shiny modules
source("modules/mod_upload.R")
source("modules/mod_issues_log.R")
source("modules/mod_spreadsheet.R")
source("modules/mod_export.R")

# Application configuration
app_config <- get_config()

# UI Definition
ui <- bslib::page_navbar(

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    success = "#18bc9c",
    warning = "#f39c12",
    danger = "#e74c3c",
    info = "#3498db",
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("Fira Code")
  ),
  
  # Include custom CSS
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Enable shinyjs
  shinyjs::useShinyjs(),
  
  # About panel
  
  bslib::nav_panel(
    title = "Home",
    icon = shiny::icon("info-circle"),
    
    # --- Two-column responsive layout ---
    bslib::layout_columns(
      col_widths = c(6, 6),   # 50% / 50% on large screens; stacks on small screens
      
      # =========================
      # LEFT COLUMN (existing)
      # =========================
      bslib::card(
        bslib::card_header("About KOBO XLS form Validator"),
        bslib::card_body(
          shiny::tags$p(
            "XLS-Validator is an R-based validation platform for ODK XLSForms."
          ),
          shiny::tags$p(
            "It wraps official ODK validation tools (pyxform, ODK Validate) and ",
            "adds custom R validation rules to produce unified reports."
          ),
          shiny::tags$hr(),
          shiny::tags$h6("Features:"),
          shiny::tags$ul(
            shiny::tags$li("Upload and validate XLSForm files"),
            shiny::tags$li("View validation issues with row navigation"),
            shiny::tags$li("Edit cells directly in spreadsheet view"),
            shiny::tags$li("Apply corrections and download fixed forms")
          ),
          shiny::tags$hr(),
          shiny::tags$p(
            class = "text-muted",
            "Built with R Shiny, pyxform, and ODK Validate"
          )
        )
      ),
      
      # =========================
      # RIGHT COLUMN (rules)
      # =========================
      bslib::card(
        bslib::card_header("About Cleaning Log Review Rules"),
        bslib::card_body(
          # A DT output for the rules table
          DT::DTOutput("rules_tbl")
        )
      )
    )
  )
  ,
  
  
  
  # Main content panel
  bslib::nav_panel(
    title = "KOBO xlsx form validator",
    icon = shiny::icon("file-excel"),
    
    bslib::layout_columns(
      col_widths = 12,
      
      # Top row: Upload and summary
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          shiny::tags$span(
            shiny::icon("upload"),
            "Upload XLSForm"
          ),
          shiny::uiOutput("config_status")
        ),
        bslib::card_body(
          mod_upload_ui("upload")
        )
      ),
      
      # Middle row: Issues log
      bslib::card(
        class = "issues-card",
        bslib::card_body(
          mod_issues_log_ui("issues")
        )
      ),
      
      # Bottom row: Spreadsheet editor and export
      bslib::layout_columns(
        col_widths = c(8, 4),
        
        bslib::card(
          class = "spreadsheet-card",
          bslib::card_body(
            mod_spreadsheet_ui("spreadsheet")
          )
        ),
        
        bslib::card(
          class = "export-card",
          bslib::card_header(
            shiny::icon("download"),
            "Export & Download"
          ),
          bslib::card_body(
            mod_export_ui("export")
          )
        )
      )
    )
  ),
 

  
  # Cleaning Log Validator UI
  bslib::nav_panel(
    title = "Cleaning Log reviewer",
    icon = shiny::icon("check"),
    
    # =======================
    # TOP TOOLBAR (HORIZONTAL)
    # =======================
    bslib::card(
     min_height = "120px",
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          
          fileInput(
            "kobo_file",
            "Upload KOBO XLSForm",
            accept = ".xlsx"
          ),
          
          fileInput(
            "cl_file",
            "Upload Cleaning Log",
            accept = ".xlsx"
          ),
          div(style = "margin-top: 32px;",
          actionButton(
            "run_check",
            "Run Validation",
            icon = icon("play"),
            class = "btn-primary w-100"
          )),
        div(style = "margin-top: 32px;",
        
          downloadButton(
            "download_log",
            "Download Log",
            class = "btn-success w-100"
          ))
        )
      )
    ),
    
    # =======================
    # STATUS + TABLE (BOTTOM)
    # =======================
    bslib::card(
      bslib::card_header(
        shiny::icon("clipboard-check"),
        "Validation Results",
      ),
        uiOutput("status"),
      
      bslib::card_body(
        DTOutput("log_table") %>% withSpinner()
      )
    )
  )
  
  
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive config
  config <- shiny::reactive({
    get_config()
  })
  
  # Issue status tracking (for marking fixed/ignored)
  issue_status <- shiny::reactiveVal(list())
  
  # Upload module
  upload <- mod_upload_server("upload", config)
  
  # Issues log module
  issues <- mod_issues_log_server(
    "issues",
    validation_results = upload$validation_results,
    issue_status = issue_status
  )
  
  # Spreadsheet module
  spreadsheet <- mod_spreadsheet_server(
    "spreadsheet",
    xlsform_data = upload$xlsform_data,
    selected_issue = issues$selected_issue,
    validation_results = upload$validation_results
  )
  
  # Export module
  export <- mod_export_server(
    "export",
    xlsform_data = upload$xlsform_data,
    change_tracker = spreadsheet$change_tracker,
    config = config
  )
  
  # Handle re-validation request
  shiny::observeEvent(issues$revalidate_trigger(), {
    # Get current data with changes
    data <- upload$xlsform_data()
    tracker <- spreadsheet$change_tracker()
    
    if (is.null(data)) return()
    
    # If there are changes, apply them first
    if (!is.null(tracker) && count_changes(tracker) > 0) {
      data <- apply_changes(tracker, data)
    }
    
    # Create temp file and re-validate
    cfg <- config()
    download_info <- prepare_download(data, NULL, cfg)
    
    # Run validation (would need to update upload module state)
    shiny::showNotification(
      "Re-validation triggered. Please re-upload the file to see updated results.",
      type = "message",
      duration = 5
    )
  })
  
  # Config status indicator
  output$config_status <- shiny::renderUI({
    cfg <- config()
    validation <- validate_config(cfg)
    
    if (validation$valid) {
      shiny::tags$span(
        class = "badge bg-success",
        shiny::icon("check"),
        "Ready"
      )
    } else {
      shiny::tags$span(
        class = "badge bg-warning",
        title = paste(validation$messages, collapse = "\n"),
        shiny::icon("exclamation-triangle"),
        "Setup Required"
      )
    }
  })
  
  # Config info display
  output$config_info <- shiny::renderPrint({
    cfg <- config()
    validation <- validate_config(cfg)
    
    cat("=== XLS-Validator Configuration ===\n\n")
    
    cat("External Tools:\n")
    cat(sprintf("  pyxform: %s\n", 
                if (is.null(cfg$pyxform_cmd)) "NOT FOUND" else cfg$pyxform_cmd))
    cat(sprintf("  Java: %s\n", 
                if (is.null(cfg$java_cmd)) "NOT FOUND" else cfg$java_cmd))
    cat(sprintf("  ODK Validate JAR: %s\n", 
                if (is.null(cfg$odk_validate_jar)) "NOT FOUND" else cfg$odk_validate_jar))
    
    cat("\nSettings:\n")
    cat(sprintf("  Max file size: %d MB\n", cfg$max_file_size_mb))
    cat(sprintf("  Allowed extensions: %s\n", paste(cfg$allowed_extensions, collapse = ", ")))
    cat(sprintf("  Temp directory: %s\n", cfg$temp_dir))
    
    cat("\nStatus:\n")
    if (validation$valid) {
      cat("  All systems ready!\n")
    } else {
      cat("  Issues found:\n")
      for (msg in validation$messages) {
        cat(sprintf("    - %s\n", msg))
      }
    }
    
    cat("\nRegistered Custom Rules:\n")
    rules <- list_rules()
    if (nrow(rules) == 0) {
      cat("  No custom rules registered\n")
    } else {
      for (i in seq_len(nrow(rules))) {
        status <- if (rules$enabled[i]) "[enabled]" else "[disabled]"
        cat(sprintf("  - %s %s: %s\n", rules$id[i], status, rules$description[i]))
      }
    }
  })
  
  

  # Cleaning log validator ######################################################
  result <- reactiveVal(NULL)
  is_running <- reactiveVal(FALSE)
  
  observeEvent(input$run_check, {
    
    req(input$kobo_file, input$cl_file)
    
    is_running(TRUE)   # ⬅️ START loading
    result(NULL)       # clear previous results
    
    shiny::withProgress(
      message = "Running cleaning log validation...",
      value = 0, {
        
        tryCatch({
          
          incProgress(0.2, detail = "Reading XLSForm...")
          
          survey_df  <- read_xlsx(input$kobo_file$datapath, sheet = "survey")
          choices_df <- read_xlsx(input$kobo_file$datapath, sheet = "choices")
          
          incProgress(0.4, detail = "Reading cleaning log...")
          
          cleaning_log_df <- read_xlsx(input$cl_file$datapath, sheet = "cleaning_log")
          
          incProgress(0.7, detail = "Validating cleaning log...")
          
          res <- check_cleaning_log(
            survey_df = survey_df,
            choices_df = choices_df,
            cleaning_log_df = cleaning_log_df,
            cl_cols = list(
              uuid = "uuid",
              question = "question",
              old_value = "old_value",
              change_type = "change_type",
              new_value = "new_value"
            ),
            header_rows_cleaning_log = 1L,
            token_split_pattern = "[\\s,]+"
          )
          
          incProgress(1)
          
          result(res)
          
        }, error = function(e) {
          showNotification(e$message, type = "error", duration = NULL)
        })
        
      } # <-- closes withProgress body
    )
    
    is_running(FALSE)  # ⬅️ END loading
    
  }) # <-- closes observeEvent
  
  
  # Status text
  output$status <- renderUI({
    if (is_running()) {
      tagList(
        shiny::icon("spinner", class = "fa-spin"),
        " Running validation..."
      )
    } else {
      res <- result()
      if (is.null(res)) return("No validation run yet.")
      
      if (res$valid) {
        "✅ Cleaning log is VALID. No issues found."
      } else {
        paste0("❌ Cleaning log has ", nrow(res$log), " issue(s).")
      }
    }
  })
  
  
  # Log table
  output$log_table <- renderDT({
    if (is_running()) return(NULL)
    
    res <- result()
    if (is.null(res)) return(NULL)
    
    df <- res$log
    # Define colors per rule_id
    rule_colors <- c(
      CL_INVALID_CHANGE_TYPE = "#F3BEBD",
      CL_NEW_VALUE_NOT_ALLOWED = "#F1F1F1",
      CL_QUESTION_NOT_IN_SURVEY = "#F6E3E3",
      CL_DUPLICATE_ACTION = "#DAD9D9",
      CL_SELECT_ONE_BAD_CHOICE = "#ede7f6",
      CL_SELECT_MULTIPLE_BAD_CHOICE = "#F4F0E8",
      CL_NUMERIC_NOT_NUMBER = "#E6DDCA",
      CL_SELECT_MULTIPLE_BAD_CHOICE = "#E6DDCA"
      
    )
    
    datatable(
      df,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = which(names(df) == "rule_id") - 1,  # 0-based index
            visible = FALSE
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(df),          # apply to whole row
        valueColumns = "rule_id",     # still available for styling
        backgroundColor = styleEqual(
          names(rule_colors),
          unname(rule_colors)
        )
      )
    
  })
  
  
  # Download log
  output$download_log <- downloadHandler(
    filename = function() {
      paste0("cleaning_log_validation_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- result()
      if (is.null(res)) return(NULL)
      
      df <- res$log
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Validation Log")
      
      openxlsx::writeData(wb, "Validation Log", df)
      
      # Define colors per rule_id
      rule_colors <- c(
        CL_INVALID_CHANGE_TYPE = "#E6F2E0",
        CL_NEW_VALUE_NOT_ALLOWED = "#F3BEBD",
        CL_QUESTION_NOT_IN_SURVEY = "#F49695",
        CL_DUPLICATE_ACTION = "#FEEEED",
        CL_SELECT_ONE_BAD_CHOICE = "#FEEEED",
        CL_SELECT_MULTIPLE_BAD_CHOICE = "#F1F1F1",
        CL_NUMERIC_NOT_NUMBER = "#D1CAB8",
        CL_SELECT_MULTIPLE_BAD_CHOICE = "#E6DDCA"
      )
      
      # Apply row styles
      for (rule in names(rule_colors)) {
        rows <- which(df$rule_id == rule) + 1  # +1 for header row
        
        if (length(rows) > 0) {
          style <- openxlsx::createStyle(
            fgFill = rule_colors[[rule]]
          )
          
          openxlsx::addStyle(
            wb,
            sheet = "Validation Log",
            style = style,
            rows = rows,
            cols = 1:ncol(df),
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
      
      # Optional: make header bold
      header_style <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addStyle(
        wb,
        "Validation Log",
        style = header_style,
        rows = 1,
        cols = 1:ncol(df),
        gridExpand = TRUE
      )
      
      openxlsx::setColWidths(
        wb,
        "Validation Log",
        cols = 1:ncol(df),
        widths = "auto"
      )
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
  
  
  output$rules_tbl <- DT::renderDT({
    rules <- data.frame(
  
      Checks = c(
        "Flag if the value in 'change_type' is not one of: 'change_response', 'blank_response', 'remove_survey', or 'no_action'.",
        "Flag if 'new_value' is not empty when 'change_type' is 'blank_response' or 'no_action'.",
        "Flag if the 'question' in 'question/answer' type logs does not exist in the tool or dataset.",
        "Flag if the question in 'question/answer' type logs is not a select-multiple type when expected.",
        "Flag if the \"choice\" does not exist in the question's choice list for 'question/choice' type logs of multiple choice question.",
        "Flag if 'change_response' or 'blank_response' actions appear multiple times (twice or in combination) for the same UUID and question.",
        "Flag if 'remove_survey' occurs together with 'change_response' or 'blank_response' for the same UUID and question.",
        "Flag if the questions in 'question' column does not exist in the tool.",
        "Flag if the 'choice' in 'new_value' for select one type does not exist in the tool.",
        "Flag if numeric questions have non-numeric values in 'new_value'."
      ),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      rules,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tpi",                # table + pagination + info
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      ),
      escape = FALSE                # allow the single quotes to render nicely
    )
  })
  
  
}

# Run the application
shiny::shinyApp(ui = ui, server = server)

