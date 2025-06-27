#' Results Module UI
#'
#' @param id Module namespace
#'
#' @return HTML div with results content
#' @export
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    # Top download button
    shiny::div(
      class = "container-fluid bg-light border-bottom",
      style = "padding: 10px 0;",
      shiny::div(
        class = "container-fluid",
        style = "max-width: 1200px;",
        shiny::div(
          class = "d-flex justify-content-end",
          shiny::div(
            style = "text-align:center; margin-bottom: 15px;",
            shiny::downloadButton(
              ns("download_pdf"),
              label = "📄 Download PDF Report",
              class = "btn btn-primary btn-lg"
            )
          )
        )
      )
    ),
    shiny::div(
      class = "container-fluid",
      bslib::navset_card_tab(
        bslib::nav_panel(
          "Final Settlement",
          shiny::h4("💰 Final Settlement"),
          DT::dataTableOutput(ns("final_table"))
        ),
        bslib::nav_panel(
          "Expense Summary",
          shiny::h4("📊 Expense Summary"),
          DT::dataTableOutput(ns("summary_table"))
        ),
        bslib::nav_panel(
          "Expense Details",
          shiny::h4("📋 Expense Details"),
          DT::dataTableOutput(ns("expenses_table"))
        ),
        bslib::nav_panel(
          "Filter by Type",
          shiny::h4("🔍 Filter by Type"),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                ns("filter_type"),
                "Select Expense Type:",
                choices = NULL,
                selected = NULL
              )
            ),
            shiny::column(8, shiny::h5("Quick Summary:"), shiny::verbatimTextOutput(ns("type_summary")))
          ),
          shiny::br(),
          DT::dataTableOutput(ns("filtered_settlement_table"))
        )
      )
    )
  )
}

#' Results Module Server
#'
#' @param id Module namespace
#' @param calculations Reactive calculations result
#' @param start_date Reactive start date
#' @param end_date Reactive end date
#' @param absences_data Reactive absences data
#' @param exceptions_data Reactive exceptions data
#'
#' @export
mod_results_server <- function(id, calculations, start_date, end_date, absences_data, exceptions_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Display expenses table
    output$expenses_table <- DT::renderDataTable({
      shiny::req(calculations())
      
      if (is.null(calculations())) {
        return(DT::datatable(data.frame(
          Message = "⚠️ Please fix validation errors and recalculate"
        )))
      }
      
      calculations()$expenses |>
        dplyr::arrange(dplyr::desc(Date)) |>
        DT::datatable(
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        ) |>
        DT::formatCurrency("Amount", currency = "CHF ")
    })
    
    # Display summary table
    output$summary_table <- DT::renderDataTable({
      shiny::req(calculations())
      
      if (is.null(calculations())) {
        return(DT::datatable(data.frame(
          Message = "⚠️ Please fix validation errors and recalculate"
        )))
      }
      
      calculations()$summary_by_type |>
        DT::datatable(
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Summary by Expense Type"
        ) |>
        DT::formatCurrency(
          c("Total_Amount", "Total_Paid", "Total_Owed"),
          currency = "CHF "
        )
    })
    
    # Display final settlement table
    output$final_table <- DT::renderDataTable({
      shiny::req(calculations())
      
      if (is.null(calculations())) {
        return(DT::datatable(data.frame(
          Message = "⚠️ Please fix validation errors and recalculate"
        )))
      }
      
      calculations()$final_settlement |>
        dplyr::arrange(dplyr::desc(Final_Balance)) |>
        DT::datatable(
          options = list(pageLength = 15, scrollX = TRUE),
          rownames = FALSE,
          caption = "Final Settlement - Who Owes What"
        ) |>
        DT::formatCurrency(
          c("Total_Paid", "Total_Owed", "Final_Balance"),
          currency = "CHF "
        ) |>
        DT::formatStyle(
          "Final_Balance",
          backgroundColor = DT::styleInterval(0, c("#ffebee", "#e8f5e8")),
          fontWeight = "bold"
        )
    })
    
    # Update filter dropdown choices when calculations change
    shiny::observe({
      shiny::req(calculations())
      if (!is.null(calculations())) {
        expense_types <- unique(calculations()$detailed_calculations$Type)
        expense_types <- expense_types[!is.na(expense_types)]
        shiny::updateSelectInput(
          session,
          "filter_type",
          choices = expense_types,
          selected = expense_types[1]
        )
      }
    })
    
    # Display filtered settlement table
    output$filtered_settlement_table <- DT::renderDataTable({
      shiny::req(calculations(), input$filter_type)
      
      if (is.null(calculations()) || is.null(input$filter_type)) {
        return(DT::datatable(data.frame(
          Message = "⚠️ Please calculate expenses and select a type"
        )))
      }
      
      # Filter detailed calculations for selected type
      filtered_data <- calculations()$detailed_calculations |>
        dplyr::filter(Type == input$filter_type) |>
        dplyr::select(Person, Type, Total_Amount, Total_Paid, Share_Owed, Balance) |>
        dplyr::arrange(dplyr::desc(Balance))
      
      DT::datatable(
        filtered_data,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        caption = paste("Settlement for", input$filter_type, "expenses only")
      ) |>
        DT::formatCurrency(
          c("Total_Amount", "Total_Paid", "Share_Owed", "Balance"),
          currency = "CHF "
        ) |>
        DT::formatStyle(
          "Balance",
          backgroundColor = DT::styleInterval(0, c("#ffebee", "#e8f5e8")),
          fontWeight = "bold"
        )
    })
    
    # Display type summary
    output$type_summary <- shiny::renderText({
      shiny::req(calculations(), input$filter_type)
      
      if (is.null(calculations()) || is.null(input$filter_type)) {
        return("Select an expense type to see summary")
      }
      
      # Get summary for selected type
      filtered_data <- calculations()$detailed_calculations |>
        dplyr::filter(Type == input$filter_type)
      
      total_amount <- dplyr::first(filtered_data$Total_Amount)
      total_paid <- sum(filtered_data$Total_Paid)
      total_owed <- sum(filtered_data$Share_Owed)
      net_balance <- sum(filtered_data$Balance)
      
      # Count people who owe vs receive
      people_owe <- sum(filtered_data$Balance < 0)
      people_receive <- sum(filtered_data$Balance > 0)
      people_even <- sum(filtered_data$Balance == 0)
      
      paste(
        paste(
          "Total",
          input$filter_type,
          "expenses: CHF",
          sprintf("%.2f", total_amount)
        ),
        paste("Total paid: CHF", sprintf("%.2f", total_paid)),
        paste("Total owed: CHF", sprintf("%.2f", total_owed)),
        paste("Net balance: CHF", sprintf("%.2f", net_balance)),
        "",
        paste("People who owe money:", people_owe),
        paste("People who receive money:", people_receive),
        paste("People who are even:", people_even),
        sep = "\n"
      )
    })
    
    # Download handler for PDF report
    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("House_Expenses_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # Check if calculations exist
        if (is.null(calculations()) || is.null(calculations()$final_settlement)) {
          shiny::showNotification(
            "❌ No data to export. Please calculate expenses first.",
            type = "error"
          )
          return()
        }
        
        # Generate PDF report
        generate_pdf_report(
          calculations(), 
          start_date(), 
          end_date(), 
          absences_data(), 
          exceptions_data(), 
          file
        )
      },
      contentType = "application/pdf"
    )
  })
}

# Global variables for NSE
utils::globalVariables(c("Date", "Final_Balance", "Type", "Person", "Total_Amount", 
                        "Total_Paid", "Share_Owed", "Balance"))
