#' Instructions Module UI
#'
#' @param id Module namespace
#'
#' @return HTML div with instructions content
#' @export
mod_instructions_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "card bg-light",
    shiny::div(
      class = "card-header",
      shiny::h5("ℹ️ Instructions", class = "mb-0")
    ),
    shiny::div(
      class = "card-body",
      shiny::tags$ul(
        shiny::tags$li(
          "Upload your expenses CSV file with columns: Type, Reason, Date, Amount, Person"
        ),
        shiny::tags$li(
          "Optionally upload absences CSV (Person, Absent_Days) and exceptions CSV (Person, Type, Percentage)"
        ),
        shiny::tags$li(
          "Adjust the date range if needed - it will auto-update when you upload expenses"
        ),
        shiny::tags$li("Modify people names and expense types as needed"),
        shiny::tags$li(
          "Click 'Calculate Expenses' to see the results in the other tabs"
        )
      ),
      shiny::br(),
      shiny::h6("📥 Download Example Files:", class = "mb-2"),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::downloadButton(
          ns("download_expenses_example"),
          "📊 Expenses Example",
          class = "btn-outline-secondary btn-sm"
        ),
        shiny::downloadButton(
          ns("download_absences_example"),
          "🏖️ Absences Example",
          class = "btn-outline-secondary btn-sm"
        ),
        shiny::downloadButton(
          ns("download_exceptions_example"),
          "⚙️ Exceptions Example",
          class = "btn-outline-secondary btn-sm"
        )
      )
    )
  )
}

#' Instructions Module Server
#'
#' @param id Module namespace
#'
#' @export
mod_instructions_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Download handlers for example files
    output$download_expenses_example <- shiny::downloadHandler(
      filename = function() {
        "expenses_example.csv"
      },
      content = function(file) {
        example_path <- system.file(
          "app",
          "www",
          "examples",
          "expenses_example.csv",
          package = "houseexpenses"
        )
        if (file.exists(example_path)) {
          file.copy(example_path, file)
        } else {
          # Create a basic example if file doesn't exist
          example_data <- data.frame(
            Type = c("Normal", "Party", "Normal", "Alcohol"),
            Reason = c("Groceries", "Birthday Party", "Utilities", "Wine"),
            Date = c("01/06/2025", "15/06/2025", "20/06/2025", "25/06/2025"),
            Amount = c(45.50, 120.00, 85.30, 25.90),
            Person = c("Alice", "Bob", "Charlie", "Alice")
          )
          utils::write.csv(example_data, file, row.names = FALSE)
        }
      },
      contentType = "text/csv"
    )

    output$download_absences_example <- shiny::downloadHandler(
      filename = function() {
        "absences_example.csv"
      },
      content = function(file) {
        example_path <- system.file(
          "app",
          "www",
          "examples",
          "absences_example.csv",
          package = "houseexpenses"
        )
        if (file.exists(example_path)) {
          file.copy(example_path, file)
        } else {
          # Create a basic example if file doesn't exist
          example_data <- data.frame(
            Person = c("Alice", "Bob", "Charlie", "Diana"),
            Absent_Days = c(0, 5, 2, 0)
          )
          utils::write.csv(example_data, file, row.names = FALSE)
        }
      },
      contentType = "text/csv"
    )

    output$download_exceptions_example <- shiny::downloadHandler(
      filename = function() {
        "exceptions_example.csv"
      },
      content = function(file) {
        example_path <- system.file(
          "app",
          "www",
          "examples",
          "exceptions_example.csv",
          package = "houseexpenses"
        )
        if (file.exists(example_path)) {
          file.copy(example_path, file)
        } else {
          # Create a basic example if file doesn't exist
          example_data <- data.frame(
            Person = c("Alice", "Bob"),
            Type = c("Alcohol", "Alcohol"),
            Percentage = c(0.5, 0.0)
          )
          utils::write.csv(example_data, file, row.names = FALSE)
        }
      },
      contentType = "text/csv"
    )
  })
}
