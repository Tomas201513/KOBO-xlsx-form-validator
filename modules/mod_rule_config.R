# Rule Configuration Module for XLS-Validator
# Provides UI for enabling/disabling custom validation rules

#' Rule Configuration Module UI
#' @param id Module namespace ID
mod_rule_config_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "rule-config-header d-flex justify-content-between align-items-center mb-3",
      shiny::h6(
        shiny::icon("list-check"),
        " Custom Validation Rules"
      ),
      shiny::div(
        shiny::actionButton(
          ns("btn_enable_all"),
          label = "Enable All",
          icon = shiny::icon("check-double"),
          class = "btn-outline-success btn-sm me-2"
        ),
        shiny::actionButton(
          ns("btn_disable_all"),
          label = "Disable All",
          icon = shiny::icon("times"),
          class = "btn-outline-secondary btn-sm"
        )
      )
    ),
    shiny::div(
      class = "rule-config-info mb-3",
      shiny::tags$small(
        class = "text-muted",
        shiny::icon("info-circle"),
        " Changes take effect on next validation. Rule states are preserved during this session."
      )
    ),
    shiny::div(
      class = "rule-config-table",
      DT::DTOutput(ns("rules_table"))
    ),
    shiny::hr(),
    shiny::div(
      class = "rule-config-summary",
      shiny::uiOutput(ns("rules_summary"))
    )
  )
}

#' Rule Configuration Module Server
#' @param id Module namespace ID
#' @return List with reactive values indicating rule configuration state
mod_rule_config_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track rule states (for forcing refresh)
    rules_version <- shiny::reactiveVal(0)
    
    # Get current rules as reactive
    current_rules <- shiny::reactive({
      # Depend on version to force refresh
      rules_version()
      list_rules()
    })
    
    # Render rules table
    output$rules_table <- DT::renderDT({
      rules_df <- current_rules()
      
      if (nrow(rules_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No custom rules registered"),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }
      
      # Prepare display data
      display_df <- data.frame(
        Enabled = rules_df$enabled,
        ID = rules_df$id,
        Description = rules_df$description,
        Level = rules_df$level,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(
              targets = 0,
              render = DT::JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    if (data === true || data === 'TRUE') {",
                "      return '<span class=\"badge bg-success\">Enabled</span>';",
                "    } else {",
                "      return '<span class=\"badge bg-secondary\">Disabled</span>';",
                "    }",
                "  }",
                "  return data;",
                "}"
              )
            ),
            list(
              targets = 2,
              width = "50%"
            ),
            list(
              targets = 3,
              render = DT::JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    var cls = 'bg-info';",
                "    if (data === 'error') cls = 'bg-danger';",
                "    if (data === 'warning') cls = 'bg-warning text-dark';",
                "    return '<span class=\"badge ' + cls + '\">' + data + '</span>';",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        ),
        callback = DT::JS(
          "table.on('click', 'tbody tr', function() {",
          "  var data = table.row(this).data();",
          "  if (data) {",
          "    Shiny.setInputValue('", ns("row_clicked"), "', {",
          "      rule_id: data[1],",
          "      enabled: data[0],",
          "      time: new Date().getTime()",
          "    });",
          "  }",
          "});"
        ),
        escape = FALSE
      )
    })
    
    # Handle row click to toggle rule
    shiny::observeEvent(input$row_clicked, {
      info <- input$row_clicked
      if (is.null(info)) return()
      
      rule_id <- info$rule_id
      current_state <- info$enabled
      
      # Toggle the rule
      if (current_state == TRUE || current_state == "TRUE") {
        disable_rule(rule_id)
        shiny::showNotification(
          sprintf("Rule '%s' disabled", rule_id),
          type = "warning",
          duration = 2
        )
      } else {
        enable_rule(rule_id)
        shiny::showNotification(
          sprintf("Rule '%s' enabled", rule_id),
          type = "message",
          duration = 2
        )
      }
      
      # Force refresh
      rules_version(rules_version() + 1)
    })
    
    # Enable all button
    shiny::observeEvent(input$btn_enable_all, {
      rules_df <- current_rules()
      if (nrow(rules_df) == 0) return()
      
      for (rule_id in rules_df$id) {
        enable_rule(rule_id)
      }
      
      rules_version(rules_version() + 1)
      shiny::showNotification(
        "All rules enabled",
        type = "message",
        duration = 2
      )
    })
    
    # Disable all button
    shiny::observeEvent(input$btn_disable_all, {
      rules_df <- current_rules()
      if (nrow(rules_df) == 0) return()
      
      for (rule_id in rules_df$id) {
        disable_rule(rule_id)
      }
      
      rules_version(rules_version() + 1)
      shiny::showNotification(
        "All rules disabled",
        type = "warning",
        duration = 2
      )
    })
    
    # Render summary
    output$rules_summary <- shiny::renderUI({
      rules_df <- current_rules()
      
      if (nrow(rules_df) == 0) {
        return(shiny::tags$p(class = "text-muted", "No rules available"))
      }
      
      total <- nrow(rules_df)
      enabled <- sum(rules_df$enabled)
      disabled <- total - enabled
      
      shiny::div(
        class = "d-flex justify-content-between",
        shiny::tags$span(
          class = "text-muted",
          sprintf("Total: %d rule(s)", total)
        ),
        shiny::tags$span(
          shiny::tags$span(class = "badge bg-success me-1", sprintf("%d enabled", enabled)),
          shiny::tags$span(class = "badge bg-secondary", sprintf("%d disabled", disabled))
        )
      )
    })
    
    # Return current rules state
    return(list(
      current_rules = current_rules,
      rules_version = rules_version
    ))
  })
}




