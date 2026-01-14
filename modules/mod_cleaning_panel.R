# Cleaning Panel Module for XLS-Validator
# Sidebar panel for viewing issue details and applying corrections with comments

#' Cleaning Panel Module UI
#' @param id Module namespace ID
mod_cleaning_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "cleaning-panel",
      
      # Panel header
      shiny::div(
        class = "cleaning-panel-header",
        shiny::h5(
          shiny::icon("broom"),
          " Cleaning Panel"
        )
      ),
      
      # Issue details section
      shiny::div(
        class = "cleaning-section",
        shiny::uiOutput(ns("issue_details"))
      ),
      
      # Correction form section
      shiny::div(
        class = "cleaning-section",
        shiny::uiOutput(ns("correction_form"))
      ),
      
      # Action buttons
      shiny::div(
        class = "cleaning-actions",
        shiny::uiOutput(ns("action_buttons"))
      ),
      
      shiny::hr(),
      
      # Change history section
      shiny::div(
        class = "cleaning-section",
        shiny::div(
          class = "d-flex justify-content-between align-items-center mb-2",
          shiny::h6(
            shiny::icon("history"),
            " Change History",
            class = "mb-0"
          ),
          shiny::actionButton(
            ns("btn_clear_history"),
            label = NULL,
            icon = shiny::icon("trash-alt"),
            class = "btn btn-sm btn-outline-secondary",
            title = "Clear all changes"
          )
        ),
        shiny::div(
          class = "change-history",
          shiny::uiOutput(ns("change_history"))
        )
      )
    )
  )
}

#' Cleaning Panel Module Server
#' @param id Module namespace ID
#' @param selected_issue Reactive selected issue from issues log
#' @param xlsform_data Reactive original XLSForm data
#' @param working_data Reactive working data
#' @param change_tracker Reactive change tracker
#' @param validation_results Reactive validation results
#' @param issues_module Issues log module return value (for Skip integration)
#' @return List with correction_applied trigger and updated tracker
mod_cleaning_panel_server <- function(id, selected_issue, xlsform_data, working_data,
                                       change_tracker, validation_results, 
                                       issues_module = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger for when a correction is applied
    correction_applied <- shiny::reactiveVal(0)
    
    # Track next issue index for Skip functionality
    current_issue_index <- shiny::reactiveVal(1)
    
    # Get all issues from validation results
    all_issues <- shiny::reactive({
      results <- validation_results()
      if (is.null(results) || nrow(results$issues) == 0) {
        return(NULL)
      }
      results$issues
    })
    
    # Get current cell value
    current_value <- shiny::reactive({
      issue <- selected_issue()
      data <- working_data()
      
      if (is.null(issue) || is.null(data)) return(NULL)
      if (is.na(issue$row) || is.na(issue$field)) return(NULL)
      
      get_cell_value(data, issue$sheet, issue$row, issue$field)
    })
    
    # Render issue details
    output$issue_details <- shiny::renderUI({
      issue <- selected_issue()
      
      if (is.null(issue)) {
        return(
          shiny::div(
            class = "text-center text-muted py-4",
            shiny::icon("hand-pointer", class = "fa-2x mb-2"),
            shiny::p("Select an issue from the log to view details")
          )
        )
      }
      
      level_class <- switch(issue$level,
        "error" = "bg-danger",
        "warning" = "bg-warning text-dark",
        "info" = "bg-info",
        "bg-secondary"
      )
      
      level_label <- switch(issue$level,
        "error" = "ERROR",
        "warning" = "WARNING",
        "info" = "INFO",
        toupper(issue$level)
      )
      
      shiny::div(
        class = "issue-details-card",
        
        # Issue header
        shiny::div(
          class = "d-flex justify-content-between align-items-center mb-2",
          shiny::span(
            class = paste("badge", level_class),
            level_label
          ),
          shiny::span(
            class = "text-muted small",
            sprintf("Issue #%d", issue$id)
          )
        ),
        
        # Location info
        shiny::div(
          class = "issue-location mb-2",
          shiny::tags$table(
            class = "table table-sm table-borderless mb-0",
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-muted", style = "width: 60px;", "Sheet:"),
                shiny::tags$td(shiny::tags$strong(toupper(issue$sheet)))
              ),
              shiny::tags$tr(
                shiny::tags$td(class = "text-muted", "Row:"),
                shiny::tags$td(shiny::tags$strong(ifelse(is.na(issue$row), "-", issue$row)))
              ),
              shiny::tags$tr(
                shiny::tags$td(class = "text-muted", "Field:"),
                shiny::tags$td(shiny::tags$strong(ifelse(is.na(issue$field), "-", issue$field)))
              )
            )
          )
        ),
        
        # Issue message
        shiny::div(
          class = "issue-message p-2 rounded",
          style = "background: #f8f9fa; border-left: 3px solid var(--xls-warning);",
          shiny::p(class = "mb-0 small", issue$message)
        )
      )
    })
    
    # Render correction form
    output$correction_form <- shiny::renderUI({
      issue <- selected_issue()
      curr_val <- current_value()
      
      if (is.null(issue)) {
        return(NULL)
      }
      
      # Check if issue has a specific field (cell-level issue)
      if (is.na(issue$field) || is.na(issue$row)) {
        return(
          shiny::div(
            class = "alert alert-info small",
            shiny::icon("info-circle"),
            " This is a form-level issue. Manual correction may not apply."
          )
        )
      }
      
      shiny::div(
        class = "correction-form mt-3",
        
        # Current value display
        shiny::div(
          class = "mb-3",
          shiny::tags$label(
            class = "form-label small text-muted",
            "Current Value:"
          ),
          shiny::div(
            class = "current-value p-2 rounded",
            if (is.null(curr_val) || is.na(curr_val) || nchar(curr_val) == 0) {
              shiny::tags$em(class = "text-muted", "(empty)")
            } else {
              shiny::tags$code(curr_val)
            }
          )
        ),
        
        # Corrected value input
        shiny::div(
          class = "mb-3",
          shiny::textAreaInput(
            ns("corrected_value"),
            label = shiny::tags$span(
              class = "small",
              "Corrected Value:"
            ),
            value = if (!is.null(curr_val) && !is.na(curr_val)) curr_val else "",
            rows = 2,
            width = "100%",
            placeholder = "Enter the corrected value..."
          )
        ),
        
        # Comment/reason input
        shiny::div(
          class = "mb-3",
          shiny::textAreaInput(
            ns("correction_comment"),
            label = shiny::tags$span(
              class = "small",
              shiny::icon("comment"),
              " Reason/Comment:"
            ),
            value = "",
            rows = 2,
            width = "100%",
            placeholder = "Why is this change being made?"
          )
        )
      )
    })
    
    # Update corrected_value input when issue changes
    shiny::observeEvent(selected_issue(), {
      curr_val <- current_value()
      if (!is.null(curr_val) && !is.na(curr_val)) {
        shiny::updateTextAreaInput(session, "corrected_value", value = curr_val)
      } else {
        shiny::updateTextAreaInput(session, "corrected_value", value = "")
      }
      shiny::updateTextAreaInput(session, "correction_comment", value = "")
    })
    
    # Render action buttons
    output$action_buttons <- shiny::renderUI({
      issue <- selected_issue()
      
      if (is.null(issue)) {
        return(NULL)
      }
      
      can_fix <- !is.na(issue$field) && !is.na(issue$row)
      
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        if (can_fix) {
          shiny::actionButton(
            ns("btn_apply_fix"),
            label = "Apply Fix",
            icon = shiny::icon("check"),
            class = "btn btn-success btn-sm flex-grow-1"
          )
        },
        shiny::actionButton(
          ns("btn_skip"),
          label = "Skip",
          icon = shiny::icon("forward"),
          class = "btn btn-outline-secondary btn-sm"
        ),
        shiny::actionButton(
          ns("btn_ignore"),
          label = "Ignore",
          icon = shiny::icon("eye-slash"),
          class = "btn btn-outline-warning btn-sm"
        )
      )
    })
    
    # Handle Apply Fix button
    shiny::observeEvent(input$btn_apply_fix, {
      issue <- selected_issue()
      tracker <- change_tracker()
      
      if (is.null(issue) || is.null(tracker)) return()
      if (is.na(issue$field) || is.na(issue$row)) return()
      
      new_value <- input$corrected_value
      comment <- input$correction_comment
      curr_val <- current_value()
      
      # Add change to tracker
      tracker <- add_change(
        tracker,
        sheet = issue$sheet,
        row = as.integer(issue$row),
        column = issue$field,
        old_value = if (!is.null(curr_val) && !is.na(curr_val)) curr_val else "",
        new_value = new_value,
        comment = comment
      )
      
      # Update tracker (this will propagate to parent)
      correction_applied(correction_applied() + 1)
      
      shiny::showNotification(
        sprintf("Fix applied to %s row %d", issue$sheet, issue$row),
        type = "message",
        duration = 2
      )
      
      # Clear comment field
      shiny::updateTextAreaInput(session, "correction_comment", value = "")
      
      # Return updated tracker
      change_tracker(tracker)
    })
    
    # Handle Skip button - advance to next issue
    shiny::observeEvent(input$btn_skip, {
      # Use issues_module integration if available
      if (!is.null(issues_module) && !is.null(issues_module$select_next_issue)) {
        # Calculate next index BEFORE triggering async update
        # (selected_issue() would return old value after trigger due to async execution)
        next_idx <- 1
        total_issues <- 0
        
        if (!is.null(issues_module$filtered_issues)) {
          issues <- issues_module$filtered_issues()
          current <- selected_issue()
          
          if (!is.null(issues) && nrow(issues) > 0) {
            total_issues <- nrow(issues)
            
            if (!is.null(current) && !is.null(current$id)) {
              current_idx <- which(issues$id == current$id)
              if (length(current_idx) > 0) {
                # Calculate what the next index WILL BE
                next_idx <- if (current_idx >= nrow(issues)) 1 else current_idx + 1
              }
            }
          }
        }
        
        # Now trigger the issues log to select next issue
        current_val <- issues_module$select_next_issue()
        issues_module$select_next_issue(current_val + 1)
        
        # Show notification with pre-calculated index
        if (total_issues > 0) {
          shiny::showNotification(
            sprintf("Issue %d of %d", next_idx, total_issues),
            type = "message",
            duration = 2
          )
        }
      } else {
        # Fallback to old behavior
        issues <- all_issues()
        current <- selected_issue()
        
        if (is.null(issues) || is.null(current)) return()
        
        # Find current index
        current_idx <- which(issues$id == current$id)
        if (length(current_idx) == 0) current_idx <- 1
        
        # Get next index
        next_idx <- current_idx + 1
        if (next_idx > nrow(issues)) {
          shiny::showNotification("No more issues", type = "message", duration = 2)
          return()
        }
        
        current_issue_index(next_idx)
        
        shiny::showNotification(
          sprintf("Skipped to issue %d of %d", next_idx, nrow(issues)),
          type = "message",
          duration = 2
        )
      }
    })
    
    # Handle Ignore button
    shiny::observeEvent(input$btn_ignore, {
      issue <- selected_issue()
      if (is.null(issue)) return()
      
      shiny::showNotification(
        sprintf("Issue #%d marked as ignored", issue$id),
        type = "warning",
        duration = 2
      )
    })
    
    # Handle Clear History button
    shiny::observeEvent(input$btn_clear_history, {
      tracker <- change_tracker()
      if (is.null(tracker)) return()
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Clear All Changes?",
          "This will remove all pending changes. This action cannot be undone.",
          footer = shiny::tagList(
            shiny::actionButton(ns("confirm_clear"), "Clear All", class = "btn-danger"),
            shiny::modalButton("Cancel")
          )
        )
      )
    })
    
    shiny::observeEvent(input$confirm_clear, {
      change_tracker(create_change_tracker())
      shiny::removeModal()
      shiny::showNotification("All changes cleared", type = "message", duration = 2)
    })
    
    # Render change history
    output$change_history <- shiny::renderUI({
      tracker <- change_tracker()
      
      if (is.null(tracker)) {
        return(shiny::div(class = "text-muted small", "No changes yet"))
      }
      
      history <- get_change_history(tracker)
      row_ops <- tracker$row_operations
      
      if (nrow(history) == 0 && nrow(row_ops) == 0) {
        return(shiny::div(class = "text-muted small text-center py-2", "No changes yet"))
      }
      
      shiny::div(
        class = "change-history-list",
        
        # Cell edits
        if (nrow(history) > 0) {
          lapply(seq_len(min(nrow(history), 10)), function(i) {
            change <- history[i, ]
            shiny::div(
              class = "change-history-item mb-2 p-2 rounded",
              style = "background: #f8f9fa; border-left: 3px solid var(--xls-success);",
              shiny::div(
                class = "d-flex justify-content-between",
                shiny::span(
                  class = "small",
                  shiny::strong(sprintf("%s[%d]", change$sheet, change$row)),
                  " → ",
                  shiny::tags$code(class = "small", change$column)
                ),
                shiny::actionButton(
                  ns(paste0("undo_", change$id)),
                  label = NULL,
                  icon = shiny::icon("undo"),
                  class = "btn btn-sm btn-link p-0",
                  title = "Undo this change"
                )
              ),
              shiny::div(
                class = "small text-truncate",
                style = "max-width: 200px;",
                change$change_summary
              ),
              if (nchar(change$comment) > 0) {
                shiny::div(
                  class = "small text-muted fst-italic mt-1",
                  shiny::icon("comment", class = "me-1"),
                  substr(change$comment, 1, 50),
                  if (nchar(change$comment) > 50) "..."
                )
              }
            )
          })
        },
        
        # Row operations
        if (nrow(row_ops) > 0) {
          shiny::div(
            class = "mt-2",
            lapply(seq_len(min(nrow(row_ops), 5)), function(i) {
              op <- row_ops[i, ]
              op_icon <- switch(op$operation,
                "add" = "plus",
                "delete" = "trash",
                "duplicate" = "copy",
                "move" = "arrows-alt-v",
                "edit"
              )
              op_color <- switch(op$operation,
                "add" = "success",
                "delete" = "danger",
                "info"
              )
              
              shiny::div(
                class = "change-history-item mb-1 p-2 rounded",
                style = sprintf("background: #f8f9fa; border-left: 3px solid var(--xls-%s);", op_color),
                shiny::span(
                  class = "small",
                  shiny::icon(op_icon, class = "me-1"),
                  sprintf("%s row %d (%s)", 
                          stringr::str_to_title(op$operation), 
                          op$row, 
                          op$sheet)
                )
              )
            })
          )
        },
        
        # Show more indicator
        if (nrow(history) > 10 || nrow(row_ops) > 5) {
          shiny::div(
            class = "text-center text-muted small mt-2",
            sprintf("... and %d more", max(0, nrow(history) - 10) + max(0, nrow(row_ops) - 5))
          )
        }
      )
    })
    
    # Handle undo buttons dynamically
    shiny::observe({
      tracker <- change_tracker()
      if (is.null(tracker) || nrow(tracker$changes) == 0) return()
      
      lapply(tracker$changes$id, function(change_id) {
        btn_id <- paste0("undo_", change_id)
        shiny::observeEvent(input[[btn_id]], {
          current_tracker <- change_tracker()
          if (!is.null(current_tracker)) {
            updated_tracker <- remove_change(current_tracker, change_id)
            change_tracker(updated_tracker)
            shiny::showNotification("Change undone", type = "message", duration = 2)
          }
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    # Return values
    return(list(
      correction_applied = correction_applied,
      skip_to_next = current_issue_index
    ))
  })
}



