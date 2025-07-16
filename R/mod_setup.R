#' Setup Module UI
#'
#' @param id Module namespace
#'
#' @return HTML div with setup page content
#' @export
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "container-fluid",
    style = "max-width: 1200px; margin: 0 auto; padding: 20px;",

    shiny::h2("House Expense Calculator", class = "text-center mb-4"),

    shiny::fluidRow(
      shiny::column(
        6,
        mod_data_upload_ui(ns("data_upload")),

        shiny::br(),

        shiny::div(
          class = "card",
          shiny::div(
            class = "card-header",
            shiny::h4("📅 Period Settings", class = "mb-0")
          ),
          shiny::div(
            class = "card-body",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::dateInput(
                  ns("start_date"),
                  "Start Date",
                  value = Sys.Date() - 90,
                  format = "dd M yyyy"
                )
              ),
              shiny::column(
                6,
                shiny::dateInput(
                  ns("end_date"),
                  "End Date",
                  value = Sys.Date(),
                  format = "dd M yyyy"
                )
              )
            )
          )
        )
      ),

      shiny::column(
        6,
        shiny::div(
          class = "card",
          shiny::div(
            class = "card-header",
            shiny::h4("👥 People & Expense Types", class = "mb-0")
          ),
          shiny::div(
            class = "card-body",
            shiny::textAreaInput(
              ns("people_list"),
              "People Names (one per line)",
              value = get_default_people_list(),
              height = "120px"
            ),
            shiny::textAreaInput(
              ns("expense_types"),
              "Expense Types (one per line)",
              value = get_default_expense_types(),
              height = "100px"
            ),
            shiny::textAreaInput(
              ns("shared_expense_types"),
              "Shared Expense Types - Always Split Equally (one per line)",
              value = get_default_shared_expense_types(),
              height = "80px"
            )
          )
        )
      )
    ),

    shiny::br(),

    shiny::div(
      class = "text-center",
      shiny::actionButton(
        ns("calculate"),
        "🧮 Calculate Expenses",
        class = "btn-primary btn-lg",
        style = "padding: 15px 30px; font-size: 18px;"
      )
    ),

    shiny::br(),

    # Instructions/Help section
    mod_instructions_ui(ns("instructions"))
  )
}

#' Setup Module Server
#'
#' @param id Module namespace
#'
#' @return Reactive values with calculation results and settings
#' @export
mod_setup_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Data upload module
    upload_data <- mod_data_upload_server("data_upload")

    # Instructions module
    mod_instructions_server("instructions")

    # Update date inputs when data is loaded
    shiny::observe({
      if (!is.null(upload_data$date_range)) {
        shiny::updateDateInput(
          session,
          "start_date",
          value = upload_data$date_range$start
        )
        shiny::updateDateInput(
          session,
          "end_date",
          value = upload_data$date_range$end
        )
      }
    })

    # Main calculation
    calculations <- shiny::eventReactive(input$calculate, {
      shiny::req(upload_data$expenses_data)

      result <- process_expenses(
        expenses_data = upload_data$expenses_data,
        start_date = input$start_date,
        end_date = input$end_date,
        people_list = input$people_list,
        expense_types = input$expense_types,
        shared_expense_types = input$shared_expense_types,
        absences_data = upload_data$absences_data,
        exceptions_data = upload_data$exceptions_data
      )

      # Handle errors
      if (!is.null(result$errors)) {
        shiny::showNotification(
          shiny::HTML(paste(
            "🚨 <strong>Validation Errors:</strong><br>",
            paste("• ", result$errors, collapse = "<br>")
          )),
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Show warnings
      if (!is.null(result$warnings) && length(result$warnings) > 0) {
        shiny::showNotification(
          shiny::HTML(paste(
            "⚠️ <strong>Warnings:</strong><br>",
            paste("• ", result$warnings, collapse = "<br>")
          )),
          type = "warning",
          duration = 8
        )
      }

      return(result)
    })

    return(list(
      calculations = calculations,
      start_date = shiny::reactive(input$start_date),
      end_date = shiny::reactive(input$end_date),
      absences_data = shiny::reactive(upload_data$absences_data),
      exceptions_data = shiny::reactive(upload_data$exceptions_data),
      expenses_data = shiny::reactive(upload_data$expenses_data)
    ))
  })
}
