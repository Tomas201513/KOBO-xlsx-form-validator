# Export Module for XLS-Validator
# Handles applying changes and downloading corrected XLSForm
# Also provides export functionality for validation issues

#' Export Module UI
#' @param id Module namespace ID
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "export-container",
      shiny::div(
        class = "export-status",
        shiny::uiOutput(ns("changes_summary"))
      ),
      shiny::div(
        class = "export-actions",
        shiny::actionButton(
          ns("btn_apply"),
          label = "Apply Changes",
          icon = shiny::icon("check"),
          class = "btn-success"
        ),
        shiny::downloadButton(
          ns("btn_download"),
          label = "Download XLSX",
          class = "btn-primary"
        ),
        shiny::actionButton(
          ns("btn_clear"),
          label = "Clear Changes",
          icon = shiny::icon("undo"),
          class = "btn-outline-secondary"
        )
      )
    ),
    shiny::hr(),
    shiny::div(
      class = "export-issues-section",
      shiny::h6(
        shiny::icon("file-export"),
        " Export Issues Report"
      ),
      shiny::div(
        class = "export-actions",
        shiny::downloadButton(
          ns("btn_download_issues_csv"),
          label = "Download CSV",
          class = "btn-outline-primary btn-sm"
        ),
        shiny::downloadButton(
          ns("btn_download_issues_xlsx"),
          label = "Download Excel",
          class = "btn-outline-primary btn-sm"
        )
      ),
      shiny::div(
        class = "mt-2",
        shiny::uiOutput(ns("issues_summary"))
      )
    ),
    shiny::hr(),
    shiny::div(
      class = "changes-detail",
      shiny::uiOutput(ns("changes_table"))
    )
  )
}

#' Export Module Server
#' @param id Module namespace ID
#' @param xlsform_data Reactive XLSForm data (original)
#' @param working_data Reactive working data (with live edits)
#' @param change_tracker Reactive change tracker
#' @param validation_results Reactive validation results (optional)
#' @param config Application configuration
#' @return List with applied reactive (triggers when changes are applied)
mod_export_server <- function(id, xlsform_data, working_data = NULL, change_tracker, config, validation_results = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track when changes are applied
    changes_applied <- shiny::reactiveVal(0)
    
    # Applied data (XLSForm with changes committed)
    applied_data <- shiny::reactiveVal(NULL)
    
    # Render changes summary
    output$changes_summary <- shiny::renderUI({
      tracker <- change_tracker()
      if (is.null(tracker)) return(NULL)
      
      cell_count <- count_changes(tracker)
      row_op_count <- count_row_operations(tracker)
      total_count <- cell_count + row_op_count
      
      if (total_count == 0) {
        shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"),
          " No pending changes"
        )
      } else {
        summary <- summarize_changes(tracker)
        sheets_info <- if (length(summary) > 0) {
          paste(names(summary), as.vector(summary), sep = ": ", collapse = ", ")
        } else {
          ""
        }
        
        details <- c()
        if (cell_count > 0) details <- c(details, sprintf("%d cell edit(s)", cell_count))
        if (row_op_count > 0) details <- c(details, sprintf("%d row operation(s)", row_op_count))
        
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          sprintf(" %d pending change(s) ", total_count),
          shiny::tags$small(paste0("(", paste(details, collapse = ", "), ")"))
        )
      }
    })
    
    # Render issues summary
    output$issues_summary <- shiny::renderUI({
      if (is.null(validation_results)) return(NULL)
      
      results <- validation_results()
      if (is.null(results) || is.null(results$issues)) {
        return(shiny::tags$small(class = "text-muted", "No validation results available"))
      }
      
      issues <- results$issues
      if (nrow(issues) == 0) {
        return(shiny::tags$small(class = "text-success", "No issues to export"))
      }
      
      errors <- sum(issues$level == "error")
      warnings <- sum(issues$level == "warning")
      infos <- sum(issues$level == "info")
      
      shiny::tags$small(
        class = "text-muted",
        sprintf("%d issue(s): %d error(s), %d warning(s), %d info", 
                nrow(issues), errors, warnings, infos)
      )
    })
    
    # Render changes detail table
    output$changes_table <- shiny::renderUI({
      tracker <- change_tracker()
      if (is.null(tracker)) return(NULL)
      
      changes <- get_changes(tracker)
      row_ops <- tracker$row_operations
      
      if (nrow(changes) == 0 && nrow(row_ops) == 0) {
        return(NULL)
      }
      
      shiny::tagList(
        # Cell changes
        if (nrow(changes) > 0) {
          shiny::div(
            class = "changes-list mt-3",
            shiny::h6("Cell Edits:"),
            shiny::tags$table(
              class = "table table-sm table-striped",
              shiny::tags$thead(
                shiny::tags$tr(
                  shiny::tags$th("Sheet"),
                  shiny::tags$th("Row"),
                  shiny::tags$th("Column"),
                  shiny::tags$th("Change"),
                  shiny::tags$th("Comment")
                )
              ),
              shiny::tags$tbody(
                lapply(seq_len(nrow(changes)), function(i) {
                  change <- changes[i, ]
                  shiny::tags$tr(
                    shiny::tags$td(change$sheet),
                    shiny::tags$td(change$row),
                    shiny::tags$td(change$column),
                    shiny::tags$td(
                      shiny::tags$span(class = "text-danger", shiny::tags$del(substr(as.character(change$old_value), 1, 20))),
                      " → ",
                      shiny::tags$span(class = "text-success", substr(as.character(change$new_value), 1, 20))
                    ),
                    shiny::tags$td(
                      class = "text-muted",
                      style = "font-size: 0.8em;",
                      if (nchar(change$comment) > 0) change$comment else "-"
                    )
                  )
                })
              )
            )
          )
        },
        # Row operations
        if (nrow(row_ops) > 0) {
          shiny::div(
            class = "changes-list mt-3",
            shiny::h6("Row Operations:"),
            shiny::tags$table(
              class = "table table-sm table-striped",
              shiny::tags$thead(
                shiny::tags$tr(
                  shiny::tags$th("Sheet"),
                  shiny::tags$th("Operation"),
                  shiny::tags$th("Row"),
                  shiny::tags$th("Details")
                )
              ),
              shiny::tags$tbody(
                lapply(seq_len(nrow(row_ops)), function(i) {
                  op <- row_ops[i, ]
                  details <- switch(op$operation,
                    "move" = sprintf("Moved to row %d", op$target_row),
                    "duplicate" = "Duplicated",
                    "add" = "New row added",
                    "delete" = "Row deleted",
                    ""
                  )
                  shiny::tags$tr(
                    shiny::tags$td(op$sheet),
                    shiny::tags$td(
                      class = switch(op$operation,
                        "add" = "text-success",
                        "delete" = "text-danger",
                        "text-info"
                      ),
                      stringr::str_to_title(op$operation)
                    ),
                    shiny::tags$td(op$row),
                    shiny::tags$td(class = "text-muted", details)
                  )
                })
              )
            )
          )
        }
      )
    })
    
    # Apply changes button
    shiny::observeEvent(input$btn_apply, {
      # Use working_data if available
      data <- if (!is.null(working_data)) working_data() else xlsform_data()
      tracker <- change_tracker()
      
      if (is.null(data) || is.null(tracker)) {
        shiny::showNotification(
          "No data to apply changes to",
          type = "error"
        )
        return()
      }
      
      total_ops <- count_total_operations(tracker)
      if (total_ops == 0) {
        shiny::showNotification(
          "No changes to apply",
          type = "warning"
        )
        return()
      }
      
      # Apply changes
      updated_data <- apply_changes(tracker, data)
      applied_data(updated_data)
      
      # Trigger notification
      changes_applied(changes_applied() + 1)
      
      shiny::showNotification(
        sprintf("Applied %d change(s) successfully!", total_ops),
        type = "message"
      )
    })
    
    # Clear changes button
    shiny::observeEvent(input$btn_clear, {
      # This needs to be handled by the parent (main app)
      # by resetting the change_tracker
      shiny::showNotification(
        "Use the spreadsheet module to revert changes",
        type = "warning"
      )
    })
    
    # Download XLSForm handler
    output$btn_download <- shiny::downloadHandler(
      filename = function() {
        data <- xlsform_data()
        if (!is.null(data) && !is.null(data$file_name)) {
          create_download_filename(data$file_name)
        } else {
          create_download_filename("xlsform.xlsx")
        }
      },
      content = function(file) {
        # Prefer working_data if available (contains live edits)
        data <- if (!is.null(working_data)) working_data() else xlsform_data()
        tracker <- change_tracker()
        cfg <- if (shiny::is.reactive(config)) config() else config
        
        if (is.null(data)) {
          # Create empty file
          writexl::write_xlsx(list(error = data.frame(message = "No data")), file)
          return()
        }
        
        # Use applied data if available, otherwise apply current changes
        if (!is.null(applied_data())) {
          write_xlsform(applied_data(), file)
        } else if (!is.null(tracker) && count_total_operations(tracker) > 0) {
          write_with_changes(data, tracker, file)
        } else {
          write_xlsform(data, file)
        }
        
        shiny::showNotification(
          "Download ready!",
          type = "message"
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    # Download Issues CSV handler
    output$btn_download_issues_csv <- shiny::downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("validation_issues_", timestamp, ".csv")
      },
      content = function(file) {
        if (is.null(validation_results)) {
          write.csv(data.frame(message = "No validation results"), file, row.names = FALSE)
          return()
        }
        
        results <- validation_results()
        if (is.null(results) || is.null(results$issues) || nrow(results$issues) == 0) {
          write.csv(data.frame(message = "No issues found"), file, row.names = FALSE)
          shiny::showNotification("No issues to export", type = "warning")
          return()
        }
        
        # Prepare export data
        export_df <- results$issues[, c("id", "level", "sheet", "row", "field", "message", "rule_id", "status")]
        
        write.csv(export_df, file, row.names = FALSE)
        shiny::showNotification(
          sprintf("Exported %d issue(s) to CSV", nrow(export_df)),
          type = "message"
        )
      },
      contentType = "text/csv"
    )
    
    # Download Issues Excel handler
    output$btn_download_issues_xlsx <- shiny::downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("validation_issues_", timestamp, ".xlsx")
      },
      content = function(file) {
        if (is.null(validation_results)) {
          writexl::write_xlsx(data.frame(message = "No validation results"), file)
          return()
        }
        
        results <- validation_results()
        if (is.null(results) || is.null(results$issues) || nrow(results$issues) == 0) {
          writexl::write_xlsx(data.frame(message = "No issues found"), file)
          shiny::showNotification("No issues to export", type = "warning")
          return()
        }
        
        # Prepare export data
        export_df <- results$issues[, c("id", "level", "sheet", "row", "field", "message", "rule_id", "status")]
        
        # Create workbook with summary and details sheets
        summary_df <- data.frame(
          Metric = c("Total Issues", "Errors", "Warnings", "Info", "Export Date"),
          Value = c(
            nrow(export_df),
            sum(export_df$level == "error"),
            sum(export_df$level == "warning"),
            sum(export_df$level == "info"),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
        )
        
        writexl::write_xlsx(
          list(
            Summary = summary_df,
            Issues = export_df
          ),
          file
        )
        
        shiny::showNotification(
          sprintf("Exported %d issue(s) to Excel", nrow(export_df)),
          type = "message"
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    # Return values
    return(list(
      changes_applied = changes_applied,
      applied_data = applied_data
    ))
  })
}
