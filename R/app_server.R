#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny observe observeEvent req showNotification withProgress incProgress reactive reactiveValues updateNavbarPage
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {
  # Initialize reactive values for shared state
  shared_values <- reactiveValues(
    people_list = character(0),
    expense_types = character(0),
    shared_expense_types = character(0),
    start_date = Sys.Date(),
    end_date = Sys.Date(),
    expenses_data = NULL,
    absences_data = NULL,
    exceptions_data = NULL,
    results = NULL
  )

  # Setup module handles everything including data upload and instructions
  setup_data <- mod_setup_server("setup")

  # Observe calculate button and perform calculations
  observeEvent(setup_data$calculations(), {
    req(setup_data$calculations())

    results <- setup_data$calculations()

    # Check if calculation was successful (no errors)
    if (!is.null(results) && is.null(results$errors)) {
      shared_values$results <- results

      showNotification(
        "✅ Calculation completed! Redirecting to results...",
        type = "message",
        duration = 3
      )

      # Navigate to results tab
      updateNavbarPage(
        session = session,
        inputId = "main_nav",
        selected = "results"
      )
    }
  })

  # Results module - pass reactive values
  mod_results_server(
    "results",
    calculations = reactive({
      shared_values$results
    }),
    start_date = reactive({
      if (is.null(setup_data$start_date())) {
        Sys.Date()
      } else {
        setup_data$start_date()
      }
    }),
    end_date = reactive({
      if (is.null(setup_data$end_date())) Sys.Date() else setup_data$end_date()
    }),
    absences_data = reactive({
      setup_data$absences_data()
    }),
    exceptions_data = reactive({
      setup_data$exceptions_data()
    })
  )

  # Long-term analysis module - pass reactive values
  mod_long_term_server(
    "long_term",
    expenses_data = reactive({
      setup_data$expenses_data()
    })
  )
}
