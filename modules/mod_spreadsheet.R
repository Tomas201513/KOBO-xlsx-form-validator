# Spreadsheet Editor Module for XLS-Validator
# Editable spreadsheet view with row highlighting and navigation

#' Spreadsheet Module UI
#' @param id Module namespace ID
mod_spreadsheet_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "spreadsheet-header",
      shiny::h4("Spreadsheet Editor"),
      shiny::div(
        class = "sheet-tabs",
        shiny::uiOutput(ns("sheet_tabs"))
      )
    ),
    shiny::div(
      class = "spreadsheet-container",
      rhandsontable::rHandsontableOutput(ns("spreadsheet"), height = "400px")
    ),
    shiny::div(
      class = "spreadsheet-footer",
      shiny::uiOutput(ns("edit_info"))
    )
  )
}

#' Spreadsheet Module Server
#' @param id Module namespace ID
#' @param xlsform_data Reactive XLSForm data
#' @param selected_issue Reactive selected issue from issues log
#' @param validation_results Reactive validation results
#' @return List with change_tracker reactive
mod_spreadsheet_server <- function(id, xlsform_data, selected_issue, validation_results) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Current sheet selection
    current_sheet <- shiny::reactiveVal("survey")
    
    # Change tracker
    change_tracker <- shiny::reactiveVal(create_change_tracker())
    
    # Local copy of data for editing
    local_data <- shiny::reactiveVal(NULL)
    
    # Update local data when xlsform_data changes
    shiny::observe({
      data <- xlsform_data()
      if (!is.null(data)) {
        local_data(data)
      }
    })
    
    # Render sheet tabs
    output$sheet_tabs <- shiny::renderUI({
      data <- local_data()
      if (is.null(data)) {
        return(NULL)
      }
      
      sheets <- get_available_sheets(data)
      active_sheet <- current_sheet()
      
      shiny::div(
        class = "btn-group",
        role = "group",
        lapply(sheets, function(sheet) {
          is_active <- sheet == active_sheet
          shiny::actionButton(
            ns(paste0("tab_", sheet)),
            label = toupper(sheet),
            class = paste("btn btn-sm", if (is_active) "btn-primary" else "btn-outline-secondary")
          )
        })
      )
    })
    
    # Handle sheet tab clicks
    shiny::observe({
      data <- local_data()
      if (is.null(data)) return()
      
      sheets <- get_available_sheets(data)
      
      lapply(sheets, function(sheet) {
        shiny::observeEvent(input[[paste0("tab_", sheet)]], {
          current_sheet(sheet)
        }, ignoreInit = TRUE)
      })
    })
    
    # Navigate to issue when selected
    shiny::observe({
      issue <- selected_issue()
      if (is.null(issue)) return()
      
      # Switch to the sheet
      if (!is.na(issue$sheet) && nchar(issue$sheet) > 0) {
        current_sheet(tolower(issue$sheet))
      }
    })
    
    # Get rows with issues for highlighting
    issue_rows <- shiny::reactive({
      results <- validation_results()
      sheet <- current_sheet()
      
      if (is.null(results) || nrow(results$issues) == 0) {
        return(integer())
      }
      
      issues <- results$issues[
        tolower(results$issues$sheet) == tolower(sheet) & !is.na(results$issues$row),
      ]
      
      unique(issues$row)
    })
    
    # Get changed rows for highlighting
    changed_rows <- shiny::reactive({
      tracker <- change_tracker()
      sheet <- current_sheet()
      get_changed_rows(tracker, sheet)
    })
    
    # Prepare data for display
    display_data <- shiny::reactive({
      data <- local_data()
      sheet <- current_sheet()
      
      if (is.null(data)) {
        return(data.frame())
      }
      
      df <- get_sheet_for_display(data, sheet, include_row_nums = TRUE)
      
      if (nrow(df) == 0) {
        return(data.frame(Message = "Sheet is empty"))
      }
      
      df
    })
    
    # Render spreadsheet
    output$spreadsheet <- rhandsontable::renderRHandsontable({
      df <- display_data()
      
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      
      issue_row_nums <- issue_rows()
      changed_row_nums <- changed_rows()
      
      # Create the handsontable
      hot <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        stretchH = "all",
        overflow = "visible",
        useTypes = FALSE,  # Treat all as text for consistent editing
        selectCallback = TRUE  # Enable selection callbacks
      )
      
      # Make Row column read-only (it's just for reference)
      if ("Row" %in% names(df)) {
        hot <- rhandsontable::hot_col(hot, "Row", readOnly = TRUE, type = "numeric")
      }
      
      # Configure table options
      hot <- rhandsontable::hot_table(
        hot,
        highlightCol = TRUE,
        highlightRow = TRUE,
        contextMenu = FALSE
      )
      
      hot
    })
    
    # Store original data snapshot for comparison (not affected by local_data updates)
    original_snapshot <- shiny::reactiveVal(NULL)
    
    # Take snapshot when data first loads
    shiny::observe({
      data <- xlsform_data()
      if (!is.null(data) && is.null(original_snapshot())) {
        original_snapshot(data)
      }
    })
    
    # Handle spreadsheet edits
    shiny::observeEvent(input$spreadsheet, {
      if (is.null(input$spreadsheet)) return()
      
      # Get the edited data from handsontable
      new_data <- rhandsontable::hot_to_r(input$spreadsheet)
      
      if (is.null(new_data) || nrow(new_data) == 0) return()
      
      # Get ORIGINAL data (not local_data which might be stale)
      orig_data <- original_snapshot()
      sheet <- current_sheet()
      
      if (is.null(orig_data)) return()
      
      original_df <- get_sheet_for_display(orig_data, sheet, include_row_nums = TRUE)
      
      if (nrow(original_df) != nrow(new_data)) return()
      
      # Get current tracker
      tracker <- change_tracker()
      
      # Find changes by comparing each cell
      for (i in seq_len(nrow(new_data))) {
        for (col in names(new_data)) {
          if (col == "Row") next  # Skip row number column
          if (!col %in% names(original_df)) next  # Skip if column doesn't exist in original
          
          old_val <- original_df[i, col]
          new_val <- new_data[i, col]
          
          # Convert to character and handle NA/NULL
          old_val <- if (is.null(old_val) || length(old_val) == 0 || is.na(old_val)) "" else as.character(old_val)
          new_val <- if (is.null(new_val) || length(new_val) == 0 || is.na(new_val)) "" else as.character(new_val)
          
          if (old_val != new_val) {
            # Get Excel row number from the Row column
            row_num <- new_data[i, "Row"]
            if (is.null(row_num) || length(row_num) == 0 || is.na(row_num)) {
              row_num <- i + 1  # Fallback: data row index + 1 for header
            }
            
            tracker <- add_change(
              tracker,
              sheet = sheet,
              row = as.integer(row_num),
              column = col,
              old_value = old_val,
              new_value = new_val
            )
          }
        }
      }
      
      change_tracker(tracker)
    }, ignoreInit = TRUE)
    
    # Render edit info
    output$edit_info <- shiny::renderUI({
      tracker <- change_tracker()
      count <- count_changes(tracker)
      
      issue <- selected_issue()
      
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::div(
          if (!is.null(issue) && !is.na(issue$row)) {
            shiny::span(
              class = "text-info",
              shiny::icon("map-marker-alt"),
              sprintf(" Viewing row %d", issue$row)
            )
          }
        ),
        shiny::div(
          if (count > 0) {
            shiny::span(
              class = "text-warning",
              shiny::icon("edit"),
              sprintf(" %d pending change(s)", count)
            )
          } else {
            shiny::span(class = "text-muted", "No changes")
          }
        )
      )
    })
    
    # Return change tracker
    return(list(
      change_tracker = change_tracker,
      current_sheet = current_sheet
    ))
  })
}

