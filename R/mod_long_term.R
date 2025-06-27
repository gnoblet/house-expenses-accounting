# Global variables for NSE
utils::globalVariables(c("Amount", "Date", "Type", "Person", "Period", "Total_Amount", "Count"))

#' @importFrom magrittr %>%
NULL

#' Long Term Analysis Module UI
#'
#' @param id Module namespace
#'
#' @return HTML div with long term analysis content
#' @export
mod_long_term_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "container-fluid",
    style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
    
    shiny::h2("📈 Long Term Analysis", class = "text-center mb-4"),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "card",
          shiny::div(
            class = "card-header",
            shiny::h4("Analysis Controls", class = "mb-0")
          ),
          shiny::div(
            class = "card-body",
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::selectInput(
                  ns("analysis_type"),
                  "Analysis Type:",
                  choices = list(
                    "By Expense Type" = "type",
                    "By Person" = "person",
                    "Total Expenses" = "total"
                  ),
                  selected = "type"
                )
              ),
              shiny::column(
                4,
                shiny::selectInput(
                  ns("period_grouping"),
                  "Group By:",
                  choices = list(
                    "Month" = "month",
                    "Quarter" = "quarter",
                    "Year" = "year"
                  ),
                  selected = "month"
                )
              ),
              shiny::column(
                4,
                shiny::checkboxInput(
                  ns("include_shared"),
                  "Include Shared Expenses",
                  value = TRUE
                )
              )
            )
          )
        )
      )
    ),
    
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "card",
          shiny::div(
            class = "card-header",
            shiny::h4("📊 Interactive Chart", class = "mb-0")
          ),
          shiny::div(
            class = "card-body",
            ggiraph::girafeOutput(ns("long_term_chart"), height = "600px")
          )
        )
      )
    ),
    
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "card",
          shiny::div(
            class = "card-header",
            shiny::h4("📋 Summary Table", class = "mb-0")
          ),
          shiny::div(
            class = "card-body",
            DT::dataTableOutput(ns("long_term_table"))
          )
        )
      )
    )
  )
}

#' Long Term Analysis Module Server
#'
#' @param id Module namespace
#' @param expenses_data Reactive expenses data
#'
#' @export
mod_long_term_server <- function(id, expenses_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Long Term Analysis reactive data
    long_term_data <- shiny::reactive({
      shiny::req(expenses_data())
      
      df <- expenses_data()
      
      # Filter by shared expenses if requested
      if (!input$include_shared) {
        df <- df[df$Type != "Shared", ]
      }
      
      # Add period grouping
      df$Period <- dplyr::case_when(
        input$period_grouping == "month" ~ format(df$Date, "%Y-%m"),
        input$period_grouping == "quarter" ~ paste0(format(df$Date, "%Y"), "-Q", lubridate::quarter(df$Date)),
        input$period_grouping == "year" ~ format(df$Date, "%Y")
      )
      
      # Group by period and analysis type
      if (input$analysis_type == "type") {
        grouped_data <- df %>%
          dplyr::group_by(Period, Type) %>%
          dplyr::summarise(
            Total_Amount = sum(Amount, na.rm = TRUE),
            Count = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::arrange(Period, Type)
      } else if (input$analysis_type == "person") {
        grouped_data <- df %>%
          dplyr::group_by(Period, Person) %>%
          dplyr::summarise(
            Total_Amount = sum(Amount, na.rm = TRUE),
            Count = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::arrange(Period, Person)
      } else { # total
        grouped_data <- df %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            Total_Amount = sum(Amount, na.rm = TRUE),
            Count = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::arrange(Period) %>%
          dplyr::mutate(Category = "Total Expenses")
      }
      
      return(grouped_data)
    })
    
    # Long term chart output
    output$long_term_chart <- ggiraph::renderGirafe({
      shiny::req(long_term_data())
      
      data <- long_term_data()
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      # Create the plot based on analysis type
      if (input$analysis_type == "total") {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = Period, y = Total_Amount)) +
          ggiraph::geom_col_interactive(
            ggplot2::aes(tooltip = paste0("Period: ", Period, "\nTotal: CHF ", 
                                 sprintf("%.2f", Total_Amount), "\nTransactions: ", Count)),
            fill = "#3498db",
            alpha = 0.8
          ) +
          ggplot2::labs(
            title = paste("Total Expenses by", stringr::str_to_title(input$period_grouping)),
            x = stringr::str_to_title(input$period_grouping),
            y = "Amount (CHF)"
          )
      } else {
        color_var <- if (input$analysis_type == "type") "Type" else "Person"
        
        p <- ggplot2::ggplot(data, ggplot2::aes(x = Period, y = Total_Amount, fill = !!rlang::sym(color_var))) +
          ggiraph::geom_col_interactive(
            ggplot2::aes(tooltip = paste0("Period: ", Period, "\n", color_var, ": ", !!rlang::sym(color_var), 
                                 "\nAmount: CHF ", sprintf("%.2f", Total_Amount), 
                                 "\nTransactions: ", Count)),
            alpha = 0.8,
            position = "stack"
          ) +
          ggplot2::labs(
            title = paste("Expenses by", stringr::str_to_title(input$period_grouping), "and", stringr::str_to_title(gsub("_", " ", input$analysis_type))),
            x = stringr::str_to_title(input$period_grouping),
            y = "Amount (CHF)",
            fill = stringr::str_to_title(gsub("_", " ", color_var))
          ) +
          ggplot2::scale_fill_viridis_d(alpha = 0.8)
      }
      
      # Common styling
      p <- p +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          legend.position = "bottom"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_currency(prefix = "CHF "))
      
      # Create interactive plot
      ggiraph::girafe(
        ggobj = p,
        width_svg = 12,
        height_svg = 8,
        options = list(
          ggiraph::opts_hover_inv(css = "opacity:0.3;"),
          ggiraph::opts_hover(css = "stroke:black;stroke-width:2px;"),
          ggiraph::opts_tooltip(
            css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:0 0 10px rgba(0,0,0,0.5);"
          )
        )
      )
    })
    
    # Long term table output
    output$long_term_table <- DT::renderDataTable({
      shiny::req(long_term_data())
      
      data <- long_term_data()
      
      if (nrow(data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }
      
      # Format the data for display
      if (input$analysis_type == "total") {
        display_data <- data %>%
          dplyr::mutate(
            Amount = paste("CHF", sprintf("%.2f", Total_Amount)),
            Transactions = Count
          ) %>%
          dplyr::select(Period, Amount, Transactions)
      } else {
        category_col <- if (input$analysis_type == "type") "Type" else "Person"
        display_data <- data %>%
          dplyr::mutate(
            Amount = paste("CHF", sprintf("%.2f", Total_Amount)),
            Transactions = Count
          ) %>%
          dplyr::select(Period, !!rlang::sym(category_col), Amount, Transactions)
      }
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(0, "desc")) # Sort by period descending
        ),
        rownames = FALSE
      )
    })
  })
}
