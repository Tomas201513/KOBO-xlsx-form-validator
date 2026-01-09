# Export Module for XLS-Validator
# Handles applying changes and downloading corrected XLSForm

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
    shiny::div(
      class = "changes-detail",
      shiny::uiOutput(ns("changes_table"))
    )
  )
}

#' Export Module Server
#' @param id Module namespace ID
#' @param xlsform_data Reactive XLSForm data
#' @param change_tracker Reactive change tracker
#' @param config Application configuration
#' @return List with applied reactive (triggers when changes are applied)
mod_export_server <- function(id, xlsform_data, change_tracker, config) {
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
      
      count <- count_changes(tracker)
      
      if (count == 0) {
        shiny::div(
          class = "alert alert-secondary",
          shiny::icon("info-circle"),
          " No pending changes"
        )
      } else {
        summary <- summarize_changes(tracker)
        sheets_info <- paste(
          names(summary),
          as.vector(summary),
          sep = ": ",
          collapse = ", "
        )
        
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          sprintf(" %d pending change(s) ", count),
          shiny::tags$small(paste0("(", sheets_info, ")"))
        )
      }
    })
    
    # Render changes detail table
    output$changes_table <- shiny::renderUI({
      tracker <- change_tracker()
      if (is.null(tracker)) return(NULL)
      
      changes <- get_changes(tracker)
      
      if (nrow(changes) == 0) {
        return(NULL)
      }
      
      shiny::div(
        class = "changes-list mt-3",
        shiny::h6("Pending Changes:"),
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th("Sheet"),
              shiny::tags$th("Row"),
              shiny::tags$th("Column"),
              shiny::tags$th("Old Value"),
              shiny::tags$th("New Value")
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
                  class = "text-danger",
                  shiny::tags$del(substr(as.character(change$old_value), 1, 30))
                ),
                shiny::tags$td(
                  class = "text-success",
                  substr(as.character(change$new_value), 1, 30)
                )
              )
            })
          )
        )
      )
    })
    
    # Apply changes button
    shiny::observeEvent(input$btn_apply, {
      data <- xlsform_data()
      tracker <- change_tracker()
      
      if (is.null(data) || is.null(tracker)) {
        shiny::showNotification(
          "No data to apply changes to",
          type = "error"
        )
        return()
      }
      
      if (count_changes(tracker) == 0) {
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
        sprintf("Applied %d change(s) successfully!", count_changes(tracker)),
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
    
    # Download handler
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
        data <- xlsform_data()
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
        } else if (!is.null(tracker) && count_changes(tracker) > 0) {
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
    
    # Return values
    return(list(
      changes_applied = changes_applied,
      applied_data = applied_data
    ))
  })
}


