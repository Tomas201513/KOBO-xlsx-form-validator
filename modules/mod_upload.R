# Upload Module for XLS-Validator
# Handles file upload and initial validation trigger

#' Upload Module UI
#' @param id Module namespace ID
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "upload-container",
      shiny::fileInput(
        ns("file"),
        label = NULL,
        accept = c(".xls", ".xlsx"),
        placeholder = "Choose XLSForm file...",
        buttonLabel = shiny::tags$span(
          shiny::icon("upload"),
          "Upload"
        )
      ),
      shiny::div(
        class = "upload-status",
        shiny::uiOutput(ns("status"))
      )
    )
  )
}

#' Upload Module Server
#' @param id Module namespace ID
#' @param config Reactive configuration
#' @return List with reactive values: file_path, xlsform_data, validation_results
mod_upload_server <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive values for module output
    rv <- shiny::reactiveValues(
      file_path = NULL,
      file_name = NULL,
      xlsform_data = NULL,
      validation_results = NULL,
      status = "idle",
      message = ""
    )
    
    # Handle file upload
    shiny::observeEvent(input$file, {
      req(input$file)
      
      rv$status <- "uploading"
      rv$message <- "Processing file..."
      
      # Use withProgress to allow UI to render loading state
      shiny::withProgress(
        message = "Processing XLSForm...",
        value = 0,
        {
          tryCatch({
            shiny::incProgress(0.1, detail = "Reading file...")
            
            # Get uploaded file info
            file_info <- input$file
            temp_path <- file_info$datapath
            original_name <- file_info$name
            
            # Copy to our temp directory
            cfg <- if (shiny::is.reactive(config)) config() else config
            safe_path <- copy_to_temp(temp_path, original_name, cfg)
            
            rv$file_path <- safe_path
            rv$file_name <- original_name
            rv$status <- "validating"
            rv$message <- "Running validation..."
            
            shiny::incProgress(0.3, detail = "Running validation checks...")
            
            # Run validation
            results <- validate_xlsform(safe_path, config = cfg)
            
            shiny::incProgress(0.5, detail = "Processing results...")
            
            rv$xlsform_data <- results$xlsform_data
            rv$validation_results <- results
            
            shiny::incProgress(0.1, detail = "Finalizing...")
            
            # Update status based on results
            if (results$summary$errors > 0) {
              rv$status <- "error"
              rv$message <- sprintf(
                "%d error(s), %d warning(s) found",
                results$summary$errors,
                results$summary$warnings
              )
            } else if (results$summary$warnings > 0) {
              rv$status <- "warning"
              rv$message <- sprintf(
                "Valid with %d warning(s)",
                results$summary$warnings
              )
            } else {
              rv$status <- "success"
              rv$message <- "Form is valid!"
            }
            
          }, error = function(e) {
            rv$status <- "error"
            rv$message <- paste("Error:", e$message)
            rv$xlsform_data <- NULL
            rv$validation_results <- NULL
          })
        }
      )
      
      # Reset the file input after processing to allow re-uploading
      # Using delay to ensure Shiny finishes processing the current event
      # shinyjs::reset() handles namespacing automatically within module context
      shinyjs::delay(100, shinyjs::reset("file"))
    })
    
    # Render status
    output$status <- shiny::renderUI({
      icon_class <- switch(rv$status,
                           "idle" = "info-circle",
                           "uploading" = "spinner fa-spin",
                           "validating" = "spinner fa-spin",
                           "success" = "check-circle",
                           "warning" = "exclamation-triangle",
                           "error" = "times-circle"
      )
      
      status_class <- switch(rv$status,
                             "idle" = "text-muted",
                             "uploading" = "text-info",
                             "validating" = "text-info",
                             "success" = "text-success",
                             "warning" = "text-warning",
                             "error" = "text-danger"
      )
      
      if (rv$status == "idle") {
        return(NULL)
      }
      
      shiny::div(
        class = paste("upload-message", status_class),
        shiny::icon(icon_class),
        shiny::span(rv$message)
      )
    })
    
    # Return reactive values
    return(list(
      file_path = shiny::reactive(rv$file_path),
      file_name = shiny::reactive(rv$file_name),
      xlsform_data = shiny::reactive(rv$xlsform_data),
      validation_results = shiny::reactive(rv$validation_results),
      status = shiny::reactive(rv$status)
    ))
  })
}

#' Trigger re-validation
#' @param upload_module Module return value
#' @param xlsform_data Updated XLSForm data
#' @param config Configuration
trigger_revalidation <- function(upload_module, xlsform_data, config) {
  # This would be called from the main app when re-validation is needed
  # Implementation depends on how the main app handles state
}
