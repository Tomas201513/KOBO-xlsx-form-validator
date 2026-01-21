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
library(jsonlite)

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
source("R/expression/expression_validator.R")
source("R/validate.R")
source("R/cleaning_log_validator/check_cleaning_log.R")

# Source Shiny modules
source("modules/mod_upload.R")
source("modules/mod_issues_log.R")
source("modules/mod_spreadsheet.R")
source("modules/mod_export.R")
source("modules/mod_rule_config.R")
source("modules/mod_cleaning_panel.R")


source("R/custom_rules/selected_validation.R")
source("R/custom_rules/brackets_connectors.R")
source("R/custom_rules/choice_list_validation.R")
source("R/custom_rules/comparisons.R")
source("R/custom_rules/cross_sheet_refs.R")
source("R/custom_rules/no_spaces_inside.R")
# source("R/custom_rules/question_validation.R")


# Application configuration
app_config <- get_config()

# UI Definition
ui <- bslib::page_navbar(
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    # Brand color palette - coral as primary accent for visibility
    primary = "#EE5859",         # Coral red - primary accent
    secondary = "#58585A",       # Dark gray
    success = "#58585A",         # Dark gray for success
    warning = "#F1797A",         # Lighter coral for warnings
    danger = "#EE5859",          # Coral red for errors
    info = "#58585A",            # Dark gray for info
    # Typography - Roboto family
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto Condensed"),
    code_font = bslib::font_google("Fira Code")
  ),
  
  # Include custom CSS and brand fonts
  shiny::tags$head(
    # Load Roboto Condensed for headings (bslib loads Roboto for body)
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400;700&display=swap"
    ),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Enable shinyjs
  shinyjs::useShinyjs(),
  
  # Hone panel
  
  bslib::nav_panel(
    title = "Home",
    icon = shiny::icon("home"),
    
    # --- Two-column responsive layout ---
    bslib::layout_columns(
      col_widths = c(6, 6),
      
      # =========================
      # LEFT COLUMN - About Validator
      # =========================
      bslib::card(
        class = "home-card",
        bslib::card_header(
          class = "home-card-header",
          "XLSForm Validator"
        ),
        bslib::card_body(
          class = "home-card-body",
          shiny::tags$p(
            class = "rules-intro-text",
            "The XLSForm Validator first runs ODK's ",
            shiny::tags$a(
              href = "https://getodk.org/xlsform/",
              target = "_blank",
              "XLSForm Online"
            ),
            " (pyxform + ODK Validate) checks plus ",
            shiny::tags$span(
              style = "color: #0d6efd;",  # Bootstrap blue
              "custom checks"
            ),
            " identified by the tool developers listed in the table below."
          )
          
          ,
          shiny::tags$p(
            shiny::tags$strong("Note: "),
            "ODK validation catches form structure and XML conversion errors. ",
            "Custom rules below provide additional XLSForm-specific checks."
          ),
          shiny::tags$div(
            class = "section-block",
            shiny::tags$p(class = "section-label", "What it does"),
            shiny::tags$ul(
              class = "feature-list",
              shiny::tags$li("Upload XLSForm files for instant validation"),
              shiny::tags$li("Navigate directly to issue rows in the spreadsheet"),
              shiny::tags$li("Edit cells in-browser and apply fixes"),
              shiny::tags$li("Export corrected forms ready for deployment")
            )
          ),
          
          
          
          shiny::tags$div(
            class = "rules-table-wrapper",
            DT::DTOutput("rules_tbl_validator")
          ),
          shiny::tags$div(
            class = "home-footer",
            shiny::tags$span("R Shiny"),
            shiny::tags$span(class = "separator", "/"),
            shiny::tags$span("pyxform"),
            shiny::tags$span(class = "separator", "/"),
            shiny::tags$span("ODK Validate"),
            shiny::tags$span(class = "separator", "\u00B7"),
            shiny::tags$a(
              href = "https://github.com/Tomas201513/KOBO-xlsx-form-validator",
              target = "_blank",
              "GitHub"
            )
          )
 
        )
      ),
      
      # =========================
      # RIGHT COLUMN - Cleaning Log Rules
      # =========================
      bslib::card(
        class = "home-card",
        bslib::card_header(
          class = "home-card-header",
          "Cleaning Log Reviewer"
        ),
        bslib::card_body(
          class = "home-card-body",
          shiny::tags$p(
            class = "rules-intro-text",
            "The tool is designed to help you validate cleaning logs against your KoboToolbox XLSForm structure. ",
            "It checks for common issues that are listed below.",
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$strong("Note: "),
            "This tool assumes that the cleaning log follows a specific format with required columns such as ",
            shiny::tags$code("uuid"),
            ", ",
            shiny::tags$code("question"),
            ", ",
            shiny::tags$code("change_type"),
            ", ",
            shiny::tags$code("old_value"),
            ", and ",
            shiny::tags$code("new_value"),
            "."
          )
          ,
          shiny::tags$div(
            class = "rules-table-wrapper",
            DT::DTOutput("rules_tbl")
          )
        )
      )
    )
  )
  ,
  
  ##########################################################################################################################################
  
  # Main content panel
  bslib::nav_panel(
    title = "Kobo XLSForm Validator",
    icon = shiny::icon("file-excel"),
    
    # Upload section - always visible at top
    bslib::card(
      class = "upload-card mb-3",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center py-2",
        shiny::tags$span(
          shiny::icon("upload"),
          "Upload XLSForm"
        ),
        shiny::uiOutput("config_status")
      ),
      bslib::card_body(
        class = "py-2",
        mod_upload_ui("upload")
      )
    ),
    
    # Main tabbed interface
    bslib::navset_card_tab(
      id = "main_tabs",
      
      # Issues Tab
      bslib::nav_panel(
        title = "Issues",
        icon = shiny::icon("list-check"),
        value = "issues_tab",
        shiny::div(
          class = "tab-content-full",
          mod_issues_log_ui("issues")
        )
      ),
      
      # # Spreadsheet Tab
      # bslib::nav_panel(
      #   title = "Spreadsheet",
      #   icon = shiny::icon("table"),
      #   value = "spreadsheet_tab",
      #   shiny::div(
      #     class = "tab-content-full",
      #     mod_spreadsheet_ui("spreadsheet")
      #   )
      # ),
      
      # Cleaning Tab
      bslib::nav_panel(
        title = "Cleaning",
        icon = shiny::icon("broom"),
        value = "cleaning_tab",
        shiny::div(
          class = "tab-content-full cleaning-tab-content",
          bslib::layout_columns(
            col_widths = c(8, 4),
            # Cleaning panel (main)
            shiny::div(
              class = "cleaning-main",
              mod_cleaning_panel_ui("cleaning")
            ),
            # Export panel (sidebar)
            bslib::card(
              class = "export-sidebar-card",
              bslib::card_header(
                class = "py-2",
                shiny::icon("download"),
                " Export & Download"
              ),
              bslib::card_body(
                class = "p-3",
                mod_export_ui("export")
              )
            )
          )
        )
      )
    )
  ),
  
  
  ##########################################################################################################################################
  # Cleaning Log Reviewr UI
  bslib::nav_panel(
    title = "Cleaning Log Reviewer",
    icon = shiny::icon("check"),
    
    # =======================
    # TOP TOOLBAR (HORIZONTAL)
    # =======================
    
    bslib::layout_columns(
      col_widths = c(4, 4, 2, 2),
      
      fileInput(
        "kobo_file",
        label = NULL,  # corrected spelling
        placeholder = "upload kobo xlsform...",
        accept = c(".xlsx", ".xls", ".xlsm")
      ),
      
      fileInput(
        "cl_file",
        label = NULL,  # corrected spelling
        placeholder = "upload cleaning log file...",
        accept = c(".xlsx", ".xls", ".xlsm")
      )
      ,
      actionButton(
        "run_check",
        "Run Validation",
        icon = icon("play"),
        class = "btn-primary w-100"
      ),
      
      downloadButton(
        "download_log",
        "Download Log",
        class = "btn-success w-100"
      )
      
    ),
    
    # =======================
    # STATUS + TABLE (BOTTOM)
    # =======================
    bslib::card(
      full_screen = TRUE,
      min_height = "400px",
      
      bslib::card_header(
        # shiny::icon("clipboard-check"),
        # "Validation Results",
        uiOutput("status")
      ),
      
      bslib::card_body(
        DTOutput("log_table") %>% shinycssloaders::withSpinner(
          type = 8,
          color = "#F6F4F0",
          size = 1
        )
      )
    )
  )
  ##########################################################################################################################################
#   
#   ,
#   # Main content panel
#   bslib::nav_panel(
#     title = "Geo Tools",
#     icon = shiny::icon("globe"),
# #   ),
#     #   bslib::card_body(
#     #     class = "py-2",
#     #     mod_upload_ui("upload")
#     #   )
#     # ),
#     
#     # Main tabbed interface
#     bslib::navset_card_tab(
#       id = "main_tabs",
#       
#       # Issues Tab
#       bslib::nav_panel(
#         title = "XLS to KML",
#         icon = shiny::icon("list-check"),
#         value = "issues_tab",
#         shiny::div(
#           class = "tab-content-full",
#           mod_issues_log_ui("issues")
#         )
#       )
#       # ,
#       
#       # # Cleaning Tab
#       # bslib::nav_panel(
#       #   title = "Cleaning",
#       #   icon = shiny::icon("broom"),
#       #   value = "cleaning_tab",
#       #   shiny::div(
#       #     class = "tab-content-full cleaning-tab-content",
#       #     bslib::layout_columns(
#       #       col_widths = c(8, 4),
#       #       # Cleaning panel (main)
#       #       shiny::div(
#       #         class = "cleaning-main",
#       #         mod_cleaning_panel_ui("cleaning")
#       #       ),
#       #       # Export panel (sidebar)
#       #       bslib::card(
#       #         class = "export-sidebar-card",
#       #         bslib::card_header(
#       #           class = "py-2",
#       #           shiny::icon("download"),
#       #           " Export & Download"
#       #         ),
#       #         bslib::card_body(
#       #           class = "p-3",
#       #           mod_export_ui("export")
#       #         )
#       #       )
#       #     )
#       #   )
#       # )
#     )
#   )    
    # # Upload section - always visible at top
    # bslib::card(
    #   class = "upload-card mb-3",
    #   bslib::card_header(
    #     class = "d-flex justify-content-between align-items-center py-2",
    #     shiny::tags$span(
    #       shiny::icon("upload"),
    #       "Upload XLSForm"
    #     ),
    #     shiny::uiOutput("config_status")
    
  
  
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive config
  config <- shiny::reactive({
    get_config()
  })
  
  # Issue status tracking (for marking fixed/ignored)
  issue_status <- shiny::reactiveVal(list())
  
  # Shared validation results - can be updated by upload or re-validation
  shared_validation_results <- shiny::reactiveVal(NULL)
  
  # Re-validation in progress flag
  revalidating <- shiny::reactiveVal(FALSE)
  
  # Working data - live edited version (separate from original upload)
  working_data <- shiny::reactiveVal(NULL)
  
  # Change tracker - shared across modules
  shared_change_tracker <- shiny::reactiveVal(create_change_tracker())
  
  # Upload module
  upload <- mod_upload_server("upload", config)
  
  # Initialize working_data when new file is uploaded
  shiny::observeEvent(upload$xlsform_data(), {
    data <- upload$xlsform_data()
    if (!is.null(data)) {
      working_data(data)
      # Reset change tracker for new file
      shared_change_tracker(create_change_tracker())
    }
  })
  
  # Sync upload results to shared reactive
  shiny::observeEvent(upload$validation_results(), {
    shared_validation_results(upload$validation_results())
    # Reset issue status when new file is uploaded
    issue_status(list())
  })
  
  # Issues log module - uses shared results
  issues <- mod_issues_log_server(
    "issues",
    validation_results = shared_validation_results,
    issue_status = issue_status,
    is_revalidating = revalidating
  )
  
  # Spreadsheet module - uses working_data for live edits
  spreadsheet <- mod_spreadsheet_server(
    "spreadsheet",
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    selected_issue = issues$selected_issue,
    validation_results = shared_validation_results,
    change_tracker = shared_change_tracker
  )
  
  # Sync spreadsheet changes back to shared change tracker
  shiny::observe({
    tracker <- spreadsheet$change_tracker()
    if (!is.null(tracker)) {
      shared_change_tracker(tracker)
    }
  })
  
  # Cleaning panel module - pass issues module for Skip button integration
  cleaning <- mod_cleaning_panel_server(
    "cleaning",
    selected_issue = issues$selected_issue,
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    change_tracker = shared_change_tracker,
    validation_results = shared_validation_results,
    issues_module = issues  # Pass issues module for Skip button integration
  )
  
  # Export module - uses shared results
  export <- mod_export_server(
    "export",
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    change_tracker = shared_change_tracker,
    config = config,
    validation_results = shared_validation_results
  )
  
  # Rule configuration module
  rule_config <- mod_rule_config_server("rule_config")
  
  # Handle re-validation request
  shiny::observeEvent(issues$revalidate_trigger(), {
    # Get working data (with live edits)
    data <- working_data()
    tracker <- shared_change_tracker()
    
    if (is.null(data)) {
      shiny::showNotification(
        "No form loaded. Please upload an XLSForm first.",
        type = "warning"
      )
      return()
    }
    
    # Show loading state
    revalidating(TRUE)
    
    tryCatch({
      # Apply any pending changes to working data
      updated_data <- data
      if (!is.null(tracker) && count_total_operations(tracker) > 0) {
        updated_data <- apply_changes(tracker, data)
        # Update working_data with applied changes
        working_data(updated_data)
      }
      
      # Run validation on updated data
      cfg <- config()
      new_results <- revalidate_xlsform(updated_data, tracker, cfg)
      
      # Update shared results
      shared_validation_results(new_results)
      
      # Reset issue statuses for fresh results
      issue_status(list())
      
      # Show result notification
      shiny::showNotification(
        sprintf("Re-validation complete: %d error(s), %d warning(s)",
                new_results$summary$errors, new_results$summary$warnings),
        type = if (new_results$summary$errors > 0) "warning" else "message",
        duration = 4
      )
    }, error = function(e) {
      shiny::showNotification(
        paste("Re-validation failed:", e$message),
        type = "error",
        duration = 6
      )
    }, finally = {
      revalidating(FALSE)
    })
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
      if (is.null(res)) return("Validation Result")
      
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
      
      CL_INVALID_CHANGE_TYPE        = "#F3BEBD", 
      CL_NEW_VALUE_NOT_ALLOWED      = "#F1F1F1", 
      CL_QUESTION_NOT_IN_SURVEY     = "#F6E3E3", 
      CL_INVALID_SLASH_USAGE        = "#E7F3F9",
      CL_SELECT_MULTIPLE_BAD_CHOICE = "#F4F0E8", 
      CL_DUPLICATE_ACTION           = "#DAD9D9", 
      CL_REMOVE_SURVEY_CONFLICT     = "#FFF0CC", 
      CL_QUESTION_MISSING           = "#EAF4EA", 
      CL_SELECT_ONE_BAD_CHOICE      = "#EDE7F6", 
      CL_NUMERIC_NOT_NUMBER         = "#E6DDCA"  
      
      
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
        CL_INVALID_CHANGE_TYPE        = "#F3BEBD", 
        CL_NEW_VALUE_NOT_ALLOWED      = "#F1F1F1", 
        CL_QUESTION_NOT_IN_SURVEY     = "#F6E3E3", 
        CL_INVALID_SLASH_USAGE        = "#E7F3F9",
        CL_SELECT_MULTIPLE_BAD_CHOICE = "#F4F0E8", 
        CL_DUPLICATE_ACTION           = "#DAD9D9", 
        CL_REMOVE_SURVEY_CONFLICT     = "#FFF0CC", 
        CL_QUESTION_MISSING           = "#EAF4EA", 
        CL_SELECT_ONE_BAD_CHOICE      = "#EDE7F6", 
        CL_NUMERIC_NOT_NUMBER         = "#E6DDCA"  
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
  
  
  
  output$rules_tbl_validator <- DT::renderDT({
    rules_df <- data.frame(
      `#` = 1:28,
      Checks = c(
        "Flag if a choice list referenced in a select_one or select_multiple question does not exist in the choices sheet.",
        "Flag if duplicate choice names exist within the same choice list.",
        "Flag if a choice list contains empty or missing choice names.",
        "Flag if a choice has an empty or missing label.",
        "Flag if a choice list is defined in the choices sheet but never used in the survey.",
        "Flag if a closing parenthesis ')' appears without a matching opening '(' outside quotes.",
        "Flag if an opening parenthesis '(' does not have a matching closing ')' outside quotes.",
        "Flag if a '${' brace does not have a corresponding closing '}' outside quotes.",
        "Flag if a '}' brace appears without a matching '${' outside quotes.",
        "Flag if the logical connectors 'and' or 'or' do not have exactly one space immediately before and after (outside quotes).",
        "Flag if any ASCII space appears outside quotes that is not directly adjacent to an 'and' or 'or' connector.",
        "Flag if the left operand variable referenced in a comparison (${var}) does not exist in the survey.",
        "Flag if the right operand variable referenced in a comparison (${var}) does not exist in the survey.",
        "Flag if the left operand variable in a comparison refers to a question defined after the current row.",
        "Flag if the right operand variable in a comparison refers to a question defined after the current row.",
        "Flag if the left operand in a comparison has an invalid type (not one of integer, decimal, date, or calculate).",
        "Flag if the right operand in a comparison has an invalid type (not one of integer, decimal, date, or calculate).",
        "Flag if a select_one or select_multiple question references a list_name that does not exist in the choices sheet.",
        "Flag if a choice list is defined in the choices sheet but is never referenced by any select_one or select_multiple question in the survey.",
        "Flag if there are spaces inside ${...} constructs in survey columns such as name, relevant, calculation, constraint, or choice_filter.",
        "Flag if there are spaces inside double-quoted `...` constructs in survey logic fields.",
        "Flag if there are spaces inside single-quoted '...' constructs in survey logic fields.",
        " Flag if a variable referenced in `selected(${var}, ...)` does not exist in the survey sheet.",
        " Flag if a variable is referenced before it is defined (must appear earlier in the survey).",
        " Flag if a variable used in `selected()` is not of type `select_one` or `select_multiple`.",
        " Flag if a choice referenced in `selected()` does not exist in the choices sheet under the corresponding `list_name`.",
        "Flag if an expression contains syntax errors (unbalanced brackets, quotes, empty field references, invalid operators, or unknown functions)",
        "Flag if an expression contains potential issues or non-critical problems (unknown fields in selected(), possible typos in function names, single '=' in comparisons, spaces inside ${}, wrong number of arguments in functions like if() or coalesce())"
        
      ),
      stringsAsFactors = FALSE
    )
    DT::datatable(
      rules_df,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 28,
        dom = "t",
        ordering = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "28px", className = "dt-center", targets = 0),
          list(className = "dt-left", targets = 1)
        )
      ),
      escape = FALSE
    )
  })
  
  output$rules_tbl <- DT::renderDT({
    rules <- data.frame(
      `#` = 1:10,
      Checks = c(
        "Flag if the value in 'change_type' is not one of: 'change_response', 'blank_response', 'remove_survey', or 'no_action'.",
        "Flag if 'new_value' is not empty when 'change_type' is 'blank_response' or 'no_action'.",
        "Flag if the 'question' in 'question/answer' type logs does not exist in the tool or dataset.",
        "Flag if the question in 'question/answer' type logs is not a select-multiple type when expected.",
        "Flag if the 'choice' does not exist in the question's choice list for 'question/choice' type logs of multiple choice question.",
        "Flag if 'change_response' or 'blank_response' actions appear multiple times (twice or in combination) for the same UUID and question.",
        "Flag if 'remove_survey' occurs together with 'change_response' or 'blank_response' for the same UUID and question.",
        "Flag if the questions in 'question' column does not exist in the tool.",
        "Flag if the 'choice' in 'new_value' for select one type does not exist in the tool.",
        "Flag if numeric questions have non-numeric values in 'new_value'."
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      rules,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 10,
        dom = "t",
        ordering = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "28px", className = "dt-center", targets = 0),
          list(className = "dt-left", targets = 1)
        )
      ),
      escape = FALSE
    )
  })
  
  
}

# Run the application
shiny::shinyApp(ui = ui, server = server)

