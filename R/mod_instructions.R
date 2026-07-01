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
          "Upload your xlsx file — it must include the sheets: ",
          shiny::tags$strong("Expenses"), ", ",
          shiny::tags$strong("Person"), ", ",
          shiny::tags$strong("ExpenseType"), ", ",
          shiny::tags$strong("SharedExpenseType")
        ),
        shiny::tags$li(
          "Optionally add ", shiny::tags$strong("Absences"),
          " (Person, Absent_Days) and ", shiny::tags$strong("Exceptions"),
          " (Person, Type, Percentage) sheets"
        ),
        shiny::tags$li(
          "The date range auto-updates from your Expenses sheet — adjust if needed"
        ),
        shiny::tags$li(
          "Click ‘Calculate Expenses’ to see the results in the other tabs"
        )
      ),
      shiny::br(),
      shiny::h6("📥 Download Example File:", class = "mb-2"),
      shiny::downloadButton(
        ns("download_example_xlsx"),
        "📊 Download Example xlsx",
        class = "btn-outline-secondary btn-sm"
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
    output$download_example_xlsx <- shiny::downloadHandler(
      filename = function() {
        "input.xlsx"
      },
      content = function(file) {
        example_path <- system.file(
          "app", "www", "examples", "input.xlsx",
          package = "houseexpenses"
        )
        if (file.exists(example_path)) {
          file.copy(example_path, file)
        } else {
          stop("Example file not found.")
        }
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  })
}
