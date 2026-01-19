# Issues Log Module for XLS-Validator
# Displays validation issues with clickable rows for navigation

#' Issues Log Module UI
#' @param id Module namespace ID
mod_issues_log_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Header with title and loading indicator
    shiny::div(
      class = "d-flex align-items-center mb-3",
      shiny::tags$h4("Validation Log", class = "mb-0 me-2"),
      shiny::uiOutput(ns("loading_indicator"))
    ),
    
    # Filters row using flexbox
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end mb-3",
      shiny::div(
        style = "min-width: 130px;",
        shiny::selectInput(
          ns("filter_level"),
          label = "Level",
          choices = c("All Levels" = "all", "Errors" = "error", "Warnings" = "warning", "Info" = "info"),
          selected = "all",
          width = "100%"
        )
      ),
      shiny::div(
        style = "min-width: 130px;",
        shiny::selectInput(
          ns("filter_sheet"),
          label = "Sheet",
          choices = c("All Sheets" = "all"),
          selected = "all",
          width = "100%"
        )
      ),
      shiny::div(
        style = "min-width: 130px;",
        shiny::selectInput(
          ns("filter_source"),
          label = "Source",
          choices = c("All Sources" = "all", "ODK" = "odk", "Custom" = "custom"),
          selected = "all",
          width = "100%"
        )
      ),
      shiny::div(
        style = "min-width: 130px;",
        shiny::selectInput(
          ns("filter_status"),
          label = "Status",
          choices = c("All Status" = "all", "Open" = "open", "Fixed" = "fixed", "Ignored" = "ignored"),
          selected = "all",
          width = "100%"
        )
      ),
      shiny::div(
        style = "padding-bottom: 0px;",
        shiny::uiOutput(ns("revalidate_btn"))
      ),
      shiny::div(
        style = "padding-bottom: 0px;",
        shiny::downloadButton(
          ns("download_issues"),
          label = "Download",
          icon = shiny::icon("download"),
          class = "btn-success"
        )
      )
    ),
    # Summary badges
    shiny::div(
      class = "issues-summary mb-3",
      shiny::uiOutput(ns("summary"))
    ),
    # Issues table with loading spinner
    shiny::div(
      class = "issues-table-container",
      shinycssloaders::withSpinner(
        DT::dataTableOutput(ns("issues_table"))
      )
    )
  )
}

#' Issues Log Module Server
#' @param id Module namespace ID
#' @param validation_results Reactive validation results (reactive or reactiveVal)
#' @param issue_status Reactive values for issue statuses
#' @param is_revalidating Reactive indicating if re-validation is in progress
#' @param upload_status Reactive indicating upload/validation status from upload module
#' @return List with selected_issue reactive
mod_issues_log_server <- function(id, validation_results, issue_status, is_revalidating = NULL, upload_status = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Selected issue for navigation
    selected_issue <- shiny::reactiveVal(NULL)
    
    # Combined loading state - check both revalidation and upload status
    is_loading <- shiny::reactive({
      revalidating <- !is.null(is_revalidating) && is_revalidating()
      uploading <- !is.null(upload_status) && upload_status() %in% c("uploading", "validating")
      revalidating || uploading
    })
    
    # Render loading indicator
    output$loading_indicator <- shiny::renderUI({
      if (is_loading()) {
        shiny::span(
          class = "text-info",
          shiny::icon("spinner", class = "fa-spin me-1"),
          "Validating..."
        )
      } else {
        NULL
      }
    })
    
    # Render re-validate button (can be disabled during validation)
    output$revalidate_btn <- shiny::renderUI({
      loading <- is_loading()
      
      shiny::actionButton(
        ns("btn_revalidate"),
        label = if (loading) "Validating..." else "Re-validate",
        icon = shiny::icon(if (loading) "spinner" else "refresh", 
                          class = if (loading) "fa-spin" else NULL),
        class = "btn-primary",
        disabled = if (loading) "disabled" else NULL
      )
    })
    
    # Update sheet filter based on results
    shiny::observe({
      results <- validation_results()
      if (is.null(results) || nrow(results$issues) == 0) {
        shiny::updateSelectInput(session, "filter_sheet", choices = c("All Sheets" = "all"))
        return()
      }
      
      sheets <- unique(results$issues$sheet)
      has_system_issues <- any(is.na(sheets))
      sheets <- sheets[!is.na(sheets)]
      
      choices <- c("All Sheets" = "all")
      if (length(sheets) > 0) {
        choices <- c(choices, setNames(sheets, stringr::str_to_title(sheets)))
      }
      if (has_system_issues) {
        choices <- c(choices, "System/Global" = "system")
      }
      
      shiny::updateSelectInput(session, "filter_sheet", choices = choices)
    })
    
    # Filtered issues
    filtered_issues <- shiny::reactive({
      results <- validation_results()
      if (is.null(results) || nrow(results$issues) == 0) {
        return(create_empty_results())
      }
      
      issues <- results$issues
      
      # Apply status from issue_status reactive
      if (!is.null(issue_status)) {
        statuses <- issue_status()
        if (!is.null(statuses) && length(statuses) > 0) {
          for (issue_id in names(statuses)) {
            idx <- which(issues$id == as.integer(issue_id))
            if (length(idx) > 0) {
              issues$status[idx] <- statuses[[issue_id]]
            }
          }
        }
      }
      
      # Filter by level
      if (!is.null(input$filter_level) && input$filter_level != "all") {
        issues <- issues[issues$level == input$filter_level, ]
      }
      
      # Filter by sheet
      if (!is.null(input$filter_sheet) && input$filter_sheet != "all") {
        if (input$filter_sheet == "system") {
          # Show only system-level issues (no sheet assigned)
          issues <- issues[is.na(issues$sheet), ]
        } else {
          # Show only issues for the selected sheet
          issues <- issues[!is.na(issues$sheet) & issues$sheet == input$filter_sheet, ]
        }
      }
      
      # Filter by source
      if (!is.null(input$filter_source) && input$filter_source != "all") {
        issues <- issues[!is.na(issues$source) & issues$source == input$filter_source, ]
      }
      
      # Filter by status
      if (!is.null(input$filter_status) && input$filter_status != "all") {
        issues <- issues[issues$status == input$filter_status, ]
      }
      
      issues
    })
    
    # Render summary
    output$summary <- shiny::renderUI({
      results <- validation_results()
      if (is.null(results)) {
        return(shiny::div(class = "text-muted", "No validation results"))
      }
      
      # Get filtered issues for display counts
      issues <- filtered_issues()
      total_results <- results$summary
      
      # Calculate filtered counts
      filtered_errors <- sum(issues$level == "error")
      filtered_warnings <- sum(issues$level == "warning")
      filtered_info <- sum(issues$level == "info")
      filtered_total <- nrow(issues)
      
      # Check if filters are active
      is_filtered <- (!is.null(input$filter_level) && input$filter_level != "all") || 
                     (!is.null(input$filter_sheet) && input$filter_sheet != "all") || 
                     (!is.null(input$filter_source) && input$filter_source != "all") ||
                     (!is.null(input$filter_status) && input$filter_status != "all")
      
      shiny::div(
        class = "summary-badges",
        if (filtered_errors > 0) shiny::span(class = "badge bg-danger", paste(filtered_errors, "Errors")),
        if (filtered_warnings > 0) shiny::span(class = "badge bg-warning text-dark", paste(filtered_warnings, "Warnings")),
        if (filtered_info > 0) shiny::span(class = "badge bg-info", paste(filtered_info, "Info")),
        shiny::span(class = "badge bg-secondary", paste(filtered_total, "Total")),
        if (is_filtered && filtered_total != total_results$total) {
          shiny::span(class = "text-muted ms-2", sprintf("(of %d)", total_results$total))
        }
      )
    })
    
    # Render issues table
    output$issues_table <- DT::renderDataTable({
      # Show spinner while loading (return NULL triggers withSpinner)
      if (is_loading()) {
        return(NULL)
      }
      
      issues <- filtered_issues()
      
      if (nrow(issues) == 0) {
        return(DT::datatable(
          data.frame(Message = "No issues found"),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }
      
      # Prepare display data
      display_data <- issues |>
        dplyr::select(id, level, source, sheet, row, field, message, status) |>
        dplyr::mutate(
          level = dplyr::case_when(
            level == "error" ~ '<span class="badge bg-danger">ERROR</span>',
            level == "warning" ~ '<span class="badge bg-warning text-dark">WARN</span>',
            level == "info" ~ '<span class="badge bg-info">INFO</span>',
            TRUE ~ level
          ),
          source = dplyr::case_when(
            source == "odk" ~ '<span class="badge bg-primary">ODK</span>',
            source == "custom" ~ '<span class="badge bg-secondary">Custom</span>',
            TRUE ~ ifelse(is.na(source), "-", source)
          ),
          status = dplyr::case_when(
            status == "open" ~ '<span class="status-open">&#9675; Open</span>',
            status == "fixed" ~ '<span class="status-fixed">&#10003; Fixed</span>',
            status == "ignored" ~ '<span class="status-ignored">&#8212; Ignored</span>',
            TRUE ~ status
          ),
          row = ifelse(is.na(row), "-", as.character(row)),
          field = ifelse(is.na(field), "-", field),
          sheet = ifelse(is.na(sheet), "-", sheet)
        )
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          dom = "lrtip",
          order = list(list(0, "asc")),
          columnDefs = list(
            list(className = "dt-center", targets = c(0, 1, 2, 3, 4, 7)),
            list(width = "50px", targets = 0),
            list(width = "70px", targets = 1),
            list(width = "70px", targets = 2),
            list(width = "70px", targets = 3),
            list(width = "50px", targets = 4),
            list(width = "100px", targets = 5)
          )
        ),
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        colnames = c("ID", "Level", "Source", "Sheet", "Row", "Field", "Message", "Status")
      )
    })
    
    # Handle row selection
    shiny::observeEvent(input$issues_table_rows_selected, {
      selected_row <- input$issues_table_rows_selected
      if (!is.null(selected_row) && length(selected_row) > 0) {
        issues <- filtered_issues()
        if (selected_row <= nrow(issues)) {
          issue <- issues[selected_row, ]
          selected_issue(list(
            id = issue$id,
            sheet = issue$sheet,
            row = issue$row,
            field = issue$field,
            level = issue$level,
            source = issue$source,
            message = issue$message,
            rule_id = issue$rule_id,
            status = issue$status
          ))
        }
      }
    })
    
    # Re-validate button click
    revalidate_trigger <- shiny::reactiveVal(0)
    shiny::observeEvent(input$btn_revalidate, {
      revalidate_trigger(revalidate_trigger() + 1)
    })
    
    # Download issues as Excel
    output$download_issues <- shiny::downloadHandler(
      filename = function() {
        paste0("xlsform_validation_issues_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        issues <- filtered_issues()
        
        if (is.null(issues) || nrow(issues) == 0) {
          # Create empty workbook with message
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Validation Issues")
          openxlsx::writeData(wb, "Validation Issues", data.frame(Message = "No issues found"))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          return()
        }
        
        # Prepare export data (without HTML formatting)
        export_data <- issues |>
          dplyr::select(id, level, source, sheet, row, field, message, status) |>
          dplyr::mutate(
            level = toupper(level),
            source = ifelse(is.na(source), "-", source),
            row = ifelse(is.na(row), "-", as.character(row)),
            field = ifelse(is.na(field), "-", field),
            sheet = ifelse(is.na(sheet), "-", sheet)
          )
        
        names(export_data) <- c("ID", "Level", "Source", "Sheet", "Row", "Field", "Message", "Status")
        
        # Create workbook
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Validation Issues")
        openxlsx::writeData(wb, "Validation Issues", export_data)
        
        # Define colors per level (Error=Red, Warning=Yellow, Info=Blue)
        level_colors <- c(
          "ERROR" = "#FFCDD2",
          "WARNING" = "#FFF9C4",
          "INFO" = "#BBDEFB"
        )
        
        # Apply row styles based on level
        for (level_name in names(level_colors)) {
          rows <- which(export_data$Level == level_name) + 1  # +1 for header row
          
          if (length(rows) > 0) {
            style <- openxlsx::createStyle(
              fgFill = level_colors[[level_name]]
            )
            
            openxlsx::addStyle(
              wb,
              sheet = "Validation Issues",
              style = style,
              rows = rows,
              cols = 1:ncol(export_data),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
        
        # Make header bold
        header_style <- openxlsx::createStyle(
          textDecoration = "bold",
          fgFill = "#E9ECEF"
        )
        openxlsx::addStyle(
          wb,
          "Validation Issues",
          style = header_style,
          rows = 1,
          cols = 1:ncol(export_data),
          gridExpand = TRUE
        )
        
        # Auto-width columns
        openxlsx::setColWidths(
          wb,
          "Validation Issues",
          cols = 1:ncol(export_data),
          widths = "auto"
        )
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # External trigger for selecting next issue (from cleaning panel Skip button)
    select_next_issue <- shiny::reactiveVal(0)
    
    # Handle external request to select next issue
    shiny::observeEvent(select_next_issue(), {
      if (select_next_issue() == 0) return()
      
      issues <- filtered_issues()
      current <- selected_issue()
      
      if (is.null(issues) || nrow(issues) == 0) return()
      
      # Find current index
      current_idx <- 1
      if (!is.null(current) && !is.null(current$id)) {
        current_idx <- which(issues$id == current$id)
        if (length(current_idx) == 0) current_idx <- 0
      }
      
      # Get next index
      next_idx <- current_idx + 1
      if (next_idx > nrow(issues)) {
        # Wrap around to first issue
        next_idx <- 1
      }
      
      # Select the row in DataTable
      proxy <- DT::dataTableProxy("issues_table")
      DT::selectRows(proxy, next_idx)
      
      # Also update selected_issue directly for immediate feedback
      issue <- issues[next_idx, ]
      selected_issue(list(
        id = issue$id,
        sheet = issue$sheet,
        row = issue$row,
        field = issue$field,
        level = issue$level,
        source = issue$source,
        message = issue$message,
        rule_id = issue$rule_id,
        status = issue$status
      ))
    }, ignoreInit = TRUE)
    
    # External trigger for selecting previous issue
    select_prev_issue <- shiny::reactiveVal(0)
    
    # Handle external request to select previous issue
    shiny::observeEvent(select_prev_issue(), {
      if (select_prev_issue() == 0) return()
      
      issues <- filtered_issues()
      current <- selected_issue()
      
      if (is.null(issues) || nrow(issues) == 0) return()
      
      # Find current index
      current_idx <- 1
      if (!is.null(current) && !is.null(current$id)) {
        current_idx <- which(issues$id == current$id)
        if (length(current_idx) == 0) current_idx <- 2
      }
      
      # Get previous index
      prev_idx <- current_idx - 1
      if (prev_idx < 1) {
        # Wrap around to last issue
        prev_idx <- nrow(issues)
      }
      
      # Select the row in DataTable
      proxy <- DT::dataTableProxy("issues_table")
      DT::selectRows(proxy, prev_idx)
      
      # Also update selected_issue directly for immediate feedback
      issue <- issues[prev_idx, ]
      selected_issue(list(
        id = issue$id,
        sheet = issue$sheet,
        row = issue$row,
        field = issue$field,
        level = issue$level,
        source = issue$source,
        message = issue$message,
        rule_id = issue$rule_id,
        status = issue$status
      ))
    }, ignoreInit = TRUE)
    
    # Return values
    return(list(
      selected_issue = selected_issue,
      revalidate_trigger = revalidate_trigger,
      select_next_issue = select_next_issue,
      select_prev_issue = select_prev_issue,
      filtered_issues = filtered_issues
    ))
  })
}

