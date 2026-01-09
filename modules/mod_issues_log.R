# Issues Log Module for XLS-Validator
# Displays validation issues with clickable rows for navigation

#' Issues Log Module UI
#' @param id Module namespace ID
mod_issues_log_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Header with title
    shiny::tags$h4("Validation Log", class = "mb-3"),
    
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
          ns("filter_status"),
          label = "Status",
          choices = c("All Status" = "all", "Open" = "open", "Fixed" = "fixed", "Ignored" = "ignored"),
          selected = "all",
          width = "100%"
        )
      ),
      shiny::div(
        style = "padding-bottom: 0px;",
        shiny::actionButton(
          ns("btn_revalidate"),
          label = "Re-validate",
          icon = shiny::icon("refresh"),
          class = "btn-outline-primary"
        )
      )
    ),
    # Summary badges
    shiny::div(
      class = "issues-summary mb-3",
      shiny::uiOutput(ns("summary"))
    ),
    # Issues table
    shiny::div(
      class = "issues-table-container",
      DT::dataTableOutput(ns("issues_table"))
    )
  )
}

#' Issues Log Module Server
#' @param id Module namespace ID
#' @param validation_results Reactive validation results
#' @param issue_status Reactive values for issue statuses
#' @return List with selected_issue reactive
mod_issues_log_server <- function(id, validation_results, issue_status) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Selected issue for navigation
    selected_issue <- shiny::reactiveVal(NULL)
    
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
        dplyr::select(id, level, sheet, row, field, message, status) |>
        dplyr::mutate(
          level = dplyr::case_when(
            level == "error" ~ '<span class="badge bg-danger">ERROR</span>',
            level == "warning" ~ '<span class="badge bg-warning text-dark">WARN</span>',
            level == "info" ~ '<span class="badge bg-info">INFO</span>',
            TRUE ~ level
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
            list(className = "dt-center", targets = c(0, 1, 2, 3, 6)),
            list(width = "50px", targets = 0),
            list(width = "80px", targets = 1),
            list(width = "80px", targets = 2),
            list(width = "50px", targets = 3),
            list(width = "100px", targets = 4)
          )
        ),
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        colnames = c("ID", "Level", "Sheet", "Row", "Field", "Message", "Status")
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
            field = issue$field
          ))
        }
      }
    })
    
    # Re-validate button click
    revalidate_trigger <- shiny::reactiveVal(0)
    shiny::observeEvent(input$btn_revalidate, {
      revalidate_trigger(revalidate_trigger() + 1)
    })
    
    # Return values
    return(list(
      selected_issue = selected_issue,
      revalidate_trigger = revalidate_trigger
    ))
  })
}

