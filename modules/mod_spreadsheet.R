# Spreadsheet Editor Module for XLS-Validator
# Enhanced editable spreadsheet view with row operations, cell highlighting, and context menu

#' Spreadsheet Module UI
#' @param id Module namespace ID
mod_spreadsheet_ui <- function(id) {

  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "spreadsheet-header",
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::h4("Spreadsheet Editor", class = "mb-0"),
        shiny::div(
          class = "sheet-tabs",
          shiny::uiOutput(ns("sheet_tabs"))
        )
      )
    ),
    
    # Toolbar for row operations
    shiny::div(
      class = "spreadsheet-toolbar mb-2",
      shiny::div(
        class = "btn-group btn-group-sm",
        role = "group",
        shiny::actionButton(
          ns("btn_add_row"),
          label = NULL,
          icon = shiny::icon("plus"),
          class = "btn-outline-success",
          title = "Add row at end"
        ),
        shiny::actionButton(
          ns("btn_insert_row"),
          label = NULL,
          icon = shiny::icon("arrow-down"),
          class = "btn-outline-primary",
          title = "Insert row below selected"
        ),
        shiny::actionButton(
          ns("btn_duplicate_row"),
          label = NULL,
          icon = shiny::icon("copy"),
          class = "btn-outline-info",
          title = "Duplicate selected row"
        ),
        shiny::actionButton(
          ns("btn_delete_row"),
          label = NULL,
          icon = shiny::icon("trash"),
          class = "btn-outline-danger",
          title = "Delete selected row"
        ),
        shiny::actionButton(
          ns("btn_move_up"),
          label = NULL,
          icon = shiny::icon("arrow-up"),
          class = "btn-outline-secondary",
          title = "Move row up"
        ),
        shiny::actionButton(
          ns("btn_move_down"),
          label = NULL,
          icon = shiny::icon("arrow-down"),
          class = "btn-outline-secondary",
          title = "Move row down"
        )
      ),
      shiny::span(
        class = "ms-3 text-muted small",
        shiny::uiOutput(ns("selection_info"), inline = TRUE)
      )
    ),
    
    # Spreadsheet container
    shiny::div(
      class = "spreadsheet-container",
      rhandsontable::rHandsontableOutput(ns("spreadsheet"), height = "450px")
    ),
    
    # Footer with edit info
    shiny::div(
      class = "spreadsheet-footer",
      shiny::uiOutput(ns("edit_info"))
    )
  )
}

#' Spreadsheet Module Server
#' @param id Module namespace ID
#' @param xlsform_data Reactive original XLSForm data
#' @param working_data Reactive working data (with live edits)
#' @param selected_issue Reactive selected issue from issues log
#' @param validation_results Reactive validation results
#' @param change_tracker Reactive change tracker
#' @return List with change_tracker reactive and current_sheet
mod_spreadsheet_server <- function(id, xlsform_data, working_data, selected_issue, 
                                    validation_results, change_tracker) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Current sheet selection
    current_sheet <- shiny::reactiveVal("survey")
    
    # Selected row in spreadsheet (1-based data row, not Excel row)
    selected_row <- shiny::reactiveVal(NULL)
    selected_col <- shiny::reactiveVal(NULL)
    
    # Local change tracker (syncs with parent)
    local_tracker <- shiny::reactiveVal(NULL)
    
    # Initialize local tracker from parent
    shiny::observe({
      tracker <- change_tracker()
      if (!is.null(tracker)) {
        local_tracker(tracker)
      }
    })
    
    # Get display data (from working_data or xlsform_data)
    display_source <- shiny::reactive({
      wd <- working_data()
      if (!is.null(wd)) return(wd)
      xlsform_data()
    })
    
    # Render sheet tabs
    output$sheet_tabs <- shiny::renderUI({
      data <- display_source()
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
      data <- display_source()
      if (is.null(data)) return()
      
      sheets <- get_available_sheets(data)
      
      lapply(sheets, function(sheet) {
        shiny::observeEvent(input[[paste0("tab_", sheet)]], {
          current_sheet(sheet)
          selected_row(NULL)
          selected_col(NULL)
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
      
      # Set selected row (convert Excel row to data row)
      if (!is.na(issue$row)) {
        selected_row(issue$row - 1)
      }
    })
    
    # Get rows with issues for highlighting
    issue_cells <- shiny::reactive({
      results <- validation_results()
      sheet <- current_sheet()
      
      if (is.null(results) || nrow(results$issues) == 0) {
        return(list(rows = integer(), cols = character(), levels = character()))
      }
      
      issues <- results$issues[
        tolower(results$issues$sheet) == tolower(sheet) & !is.na(results$issues$row),
      ]
      
      list(
        rows = issues$row,
        cols = issues$field,
        levels = issues$level
      )
    })
    
    # Get changed cells for highlighting
    changed_cells <- shiny::reactive({
      tracker <- local_tracker()
      sheet <- current_sheet()
      if (is.null(tracker)) return(list(rows = integer(), cols = character()))
      get_changed_cells(tracker, sheet)
    })
    
    # Prepare data for display
    display_data <- shiny::reactive({
      data <- display_source()
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
    
    # Custom cell renderer for highlighting
    # Build renderer based on issue and change cells
    build_renderer <- shiny::reactive({
      issues <- issue_cells()
      changes <- changed_cells()
      sheet <- current_sheet()
      
      # Build JavaScript renderer function
      # This highlights cells based on their state
      renderer_js <- "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        // Get Excel row number (row in data + 2: +1 for 0-index, +1 for header)
        var excelRow = row + 2;
        var colName = prop;
        
        // Issue highlighting
        var issueRows = %s;
        var issueCols = %s;
        var issueLevels = %s;
        
        for (var i = 0; i < issueRows.length; i++) {
          if (issueRows[i] === excelRow && (issueCols[i] === colName || issueCols[i] === null || issueCols[i] === 'NA')) {
            if (issueLevels[i] === 'error') {
              td.className += ' cell-error';
            } else if (issueLevels[i] === 'warning') {
              td.className += ' cell-warning';
            }
            break;
          }
        }
        
        // Change highlighting (takes precedence)
        var changedRows = %s;
        var changedCols = %s;
        
        for (var j = 0; j < changedRows.length; j++) {
          if (changedRows[j] === excelRow && changedCols[j] === colName) {
            td.className += ' cell-edited';
            break;
          }
        }
        
        return td;
      }
      "
      
      # Convert R vectors to JSON arrays for JS
      issue_rows_json <- jsonlite::toJSON(as.integer(issues$rows), auto_unbox = FALSE)
      issue_cols_json <- jsonlite::toJSON(as.character(issues$cols), auto_unbox = FALSE, na = "null")
      issue_levels_json <- jsonlite::toJSON(as.character(issues$levels), auto_unbox = FALSE)
      
      changed_rows_json <- jsonlite::toJSON(as.integer(changes$rows), auto_unbox = FALSE)
      changed_cols_json <- jsonlite::toJSON(as.character(changes$cols), auto_unbox = FALSE)
      
      sprintf(renderer_js, 
              issue_rows_json, issue_cols_json, issue_levels_json,
              changed_rows_json, changed_cols_json)
    })
    
    # Render spreadsheet
    output$spreadsheet <- rhandsontable::renderRHandsontable({
      df <- display_data()
      
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      
      renderer <- build_renderer()
      
      # Create the handsontable with enhanced features
      hot <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        stretchH = "all",
        overflow = "visible",
        useTypes = FALSE,
        selectCallback = TRUE,
        height = 450
      ) |>
        rhandsontable::hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = list(
            items = list(
              row_above = list(name = "Insert row above"),
              row_below = list(name = "Insert row below"),
              hsep1 = "---------",
              remove_row = list(name = "Delete row"),
              hsep2 = "---------",
              undo = list(name = "Undo"),
              redo = list(name = "Redo")
            )
          ),
          columnSorting = FALSE,
          manualColumnResize = TRUE,
          manualRowResize = TRUE
        )
      
      # Make Row column read-only
      if ("Row" %in% names(df)) {
        hot <- rhandsontable::hot_col(hot, "Row", readOnly = TRUE, renderer = renderer)
      }
      
      # Apply custom renderer to all columns
      for (col in names(df)) {
        if (col != "Row") {
          hot <- rhandsontable::hot_col(hot, col, renderer = renderer)
        }
      }
      
      hot
    })
    
    # Track selection from spreadsheet
    shiny::observeEvent(input$spreadsheet_select, {
      sel <- input$spreadsheet_select
      if (!is.null(sel)) {
        selected_row(sel$select$r + 1)
        selected_col(sel$select$c + 1)
      }
    })
    
    # Selection info display
    output$selection_info <- shiny::renderUI({
      row <- selected_row()
      df <- display_data()
      
      if (is.null(row) || is.null(df) || nrow(df) == 0) {
        return(shiny::span("No row selected"))
      }
      
      if (row > 0 && row <= nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"]
        shiny::span(sprintf("Selected: Row %d (Excel row %d)", row, excel_row))
      } else {
        shiny::span("No row selected")
      }
    })
    
    # Handle spreadsheet edits
    shiny::observeEvent(input$spreadsheet, {
      if (is.null(input$spreadsheet)) return()
      
      new_data <- rhandsontable::hot_to_r(input$spreadsheet)
      if (is.null(new_data) || nrow(new_data) == 0) return()
      
      # Get original data for comparison
      orig_data <- xlsform_data()
      sheet <- current_sheet()
      
      if (is.null(orig_data)) return()
      
      original_df <- get_sheet_for_display(orig_data, sheet, include_row_nums = TRUE)
      
      if (nrow(original_df) == 0) return()
      
      # Get current tracker
      tracker <- local_tracker()
      if (is.null(tracker)) {
        tracker <- create_change_tracker()
      }
      
      # Find changes by comparing each cell
      for (i in seq_len(min(nrow(new_data), nrow(original_df)))) {
        for (col in names(new_data)) {
          if (col == "Row") next
          if (!col %in% names(original_df)) next
          
          old_val <- original_df[i, col]
          new_val <- new_data[i, col]
          
          old_val <- if (is.null(old_val) || length(old_val) == 0 || is.na(old_val)) "" else as.character(old_val)
          new_val <- if (is.null(new_val) || length(new_val) == 0 || is.na(new_val)) "" else as.character(new_val)
          
          if (old_val != new_val) {
            row_num <- new_data[i, "Row"]
            if (is.null(row_num) || length(row_num) == 0 || is.na(row_num)) {
              row_num <- i + 1
            }
            
            tracker <- add_change(
              tracker,
              sheet = sheet,
              row = as.integer(row_num),
              column = col,
              old_value = old_val,
              new_value = new_val,
              comment = ""
            )
          }
        }
      }
      
      local_tracker(tracker)
    }, ignoreInit = TRUE)
    
    # Row operation: Add row at end
    shiny::observeEvent(input$btn_add_row, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      
      if (is.null(data) || is.null(tracker)) return()
      
      df <- get_sheet(data, sheet)
      new_row <- nrow(df) + 2
      
      tracker <- add_row_operation(tracker, sheet, "add", new_row)
      local_tracker(tracker)
      
      shiny::showNotification(
        sprintf("Row added at position %d", new_row),
        type = "message",
        duration = 2
      )
    })
    
    # Row operation: Insert row below selected
    shiny::observeEvent(input$btn_insert_row, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      row <- selected_row()
      
      if (is.null(data) || is.null(tracker)) return()
      
      if (is.null(row)) {
        shiny::showNotification("Please select a row first", type = "warning", duration = 2)
        return()
      }
      
      df <- display_data()
      if (row > 0 && row <= nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"] + 1
        tracker <- add_row_operation(tracker, sheet, "add", as.integer(excel_row))
        local_tracker(tracker)
        
        shiny::showNotification(
          sprintf("Row inserted at position %d", excel_row),
          type = "message",
          duration = 2
        )
      }
    })
    
    # Row operation: Duplicate selected row
    shiny::observeEvent(input$btn_duplicate_row, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      row <- selected_row()
      
      if (is.null(data) || is.null(tracker)) return()
      
      if (is.null(row)) {
        shiny::showNotification("Please select a row first", type = "warning", duration = 2)
        return()
      }
      
      df <- display_data()
      if (row > 0 && row <= nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"]
        tracker <- add_row_operation(tracker, sheet, "duplicate", as.integer(excel_row))
        local_tracker(tracker)
        
        shiny::showNotification(
          sprintf("Row %d duplicated", excel_row),
          type = "message",
          duration = 2
        )
      }
    })
    
    # Row operation: Delete selected row
    shiny::observeEvent(input$btn_delete_row, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      row <- selected_row()
      
      if (is.null(data) || is.null(tracker)) return()
      
      if (is.null(row)) {
        shiny::showNotification("Please select a row first", type = "warning", duration = 2)
        return()
      }
      
      df <- display_data()
      if (row > 0 && row <= nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"]
        tracker <- add_row_operation(tracker, sheet, "delete", as.integer(excel_row))
        local_tracker(tracker)
        
        shiny::showNotification(
          sprintf("Row %d marked for deletion", excel_row),
          type = "warning",
          duration = 2
        )
      }
    })
    
    # Row operation: Move up
    shiny::observeEvent(input$btn_move_up, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      row <- selected_row()
      
      if (is.null(data) || is.null(tracker)) return()
      
      if (is.null(row) || row <= 1) {
        shiny::showNotification("Cannot move this row up", type = "warning", duration = 2)
        return()
      }
      
      df <- display_data()
      if (row > 1 && row <= nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"]
        target_row <- df[row - 1, "Row"]
        tracker <- add_row_operation(tracker, sheet, "move", as.integer(excel_row), as.integer(target_row))
        local_tracker(tracker)
        selected_row(row - 1)
        
        shiny::showNotification(
          sprintf("Row %d moved up", excel_row),
          type = "message",
          duration = 2
        )
      }
    })
    
    # Row operation: Move down
    shiny::observeEvent(input$btn_move_down, {
      data <- display_source()
      sheet <- current_sheet()
      tracker <- local_tracker()
      row <- selected_row()
      
      if (is.null(data) || is.null(tracker)) return()
      
      df <- display_data()
      if (is.null(row) || row >= nrow(df)) {
        shiny::showNotification("Cannot move this row down", type = "warning", duration = 2)
        return()
      }
      
      if (row > 0 && row < nrow(df) && "Row" %in% names(df)) {
        excel_row <- df[row, "Row"]
        target_row <- df[row + 1, "Row"] + 1
        tracker <- add_row_operation(tracker, sheet, "move", as.integer(excel_row), as.integer(target_row))
        local_tracker(tracker)
        selected_row(row + 1)
        
        shiny::showNotification(
          sprintf("Row %d moved down", excel_row),
          type = "message",
          duration = 2
        )
      }
    })
    
    # Render edit info
    output$edit_info <- shiny::renderUI({
      tracker <- local_tracker()
      issue <- selected_issue()
      
      cell_count <- if (!is.null(tracker)) count_changes(tracker) else 0
      row_op_count <- if (!is.null(tracker)) count_row_operations(tracker) else 0
      total <- cell_count + row_op_count
      
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::div(
          if (!is.null(issue) && !is.na(issue$row)) {
            shiny::span(
              class = "text-info",
              shiny::icon("map-marker-alt"),
              sprintf(" Issue at row %d", issue$row)
            )
          }
        ),
        shiny::div(
          if (total > 0) {
            shiny::tagList(
              if (cell_count > 0) {
                shiny::span(
                  class = "badge bg-success me-1",
                  shiny::icon("edit"),
                  sprintf(" %d edit(s)", cell_count)
                )
              },
              if (row_op_count > 0) {
                shiny::span(
                  class = "badge bg-info",
                  shiny::icon("list"),
                  sprintf(" %d row op(s)", row_op_count)
                )
              }
            )
          } else {
            shiny::span(class = "text-muted", "No changes")
          }
        )
      )
    })
    
    # Return values
    return(list(
      change_tracker = local_tracker,
      current_sheet = current_sheet,
      selected_row = selected_row
    ))
  })
}
