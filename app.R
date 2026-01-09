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
  title = shiny::tags$span(
    shiny::icon("check-circle"),
    "XLS-Validator"
  ),
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
  
  # Main content panel
  bslib::nav_panel(
    title = "Validator",
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
 
  # Settings panel
  bslib::nav_panel(
    title = "Cleaning log Validator",
    # icon = shiny::icon
    
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
      )
  )),
  # Settings panel
  bslib::nav_panel(
    title = "Settings",
    icon = shiny::icon("cog"),
    
    bslib::card(
      bslib::card_header("System Configuration"),
      bslib::card_body(
        shiny::verbatimTextOutput("config_info")
      )
    )
  ),
  
  # About panel
  bslib::nav_panel(
    title = "About",
    icon = shiny::icon("info-circle"),
    
    bslib::card(
      bslib::card_header("About XLS-Validator"),
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
    )
  ),
  
  # Cleaning Log Validator UI
  bslib::nav_panel(
    title = "Cleaning Log Validator",
    icon = shiny::icon("check"),
    
    sidebarLayout(
      sidebarPanel(
          width = 3,
        fileInput(
          "kobo_file",
          "Upload KOBO XLSForm (survey & choices sheets)",
          accept = c(".xlsx"),
          placeholder = "No file selected"
        ),
        fileInput(
          "cl_file",
          "Upload Cleaning Log (cleaning_log sheet)",
          accept = c(".xlsx"),
          placeholder = "No file selected"
        ),
        tags$hr(),
        actionButton("run_check", "Run Validation", icon = icon("play"), class = "btn-primary"),
        br(), br(),
        downloadButton("download_log", "Download Validation Log", class = "btn-success")
      ),
      
      mainPanel(
        width = 9,
            h4("Validation Status"),
            verbatimTextOutput("status") %>% withSpinner(),  # Add loading state
            
            tags$hr(),
            
            h4("Validation Log"),
            DTOutput("log_table") %>% withSpinner()  # Add loading state
    
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
  
  observeEvent(input$run_check, {
    
    req(input$kobo_file, input$cl_file)
    
    tryCatch({
      
      # Read files
      survey_df <- read_xlsx(input$kobo_file$datapath, sheet = "survey")
      choices_df <- read_xlsx(input$kobo_file$datapath, sheet = "choices")
      cleaning_log_df <- read_xlsx(input$cl_file$datapath, sheet = "cleaning_log")
      
      # Run validation
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
      
      result(res)
      
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = NULL)
    })
  })
  
  # Status text
  output$status <- renderText({
    res <- result()
    if (is.null(res)) return("No validation run yet.")
    
    if (res$valid) {
      "✅ Cleaning log is VALID. No issues found."
    } else {
      paste0("❌ Cleaning log has ", nrow(res$log), " issue(s).")
    }
  })
  
  # Log table
  output$log_table <- renderDT({
    res <- result()
    if (is.null(res)) return(NULL)
    
    datatable(
      res$log,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
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
      writexl::write_xlsx(res$log, file)
    }
  )
  
  
}

# Run the application
shiny::shinyApp(ui = ui, server = server)

