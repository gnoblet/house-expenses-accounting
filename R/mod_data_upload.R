#' Data Upload Module UI
#'
#' @param id Module namespace
#'
#' @return HTML div with data upload inputs
#' @export
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "card",
    shiny::div(
      class = "card-header",
      shiny::h4("📁 Data Upload", class = "mb-0")
    ),
    shiny::div(
      class = "card-body",
      shiny::fileInput(
        ns("xlsx_file"),
        "Upload Your Excel File (xlsx) with Expenses, Absences, and Exceptions sheets",
        accept = ".xlsx"
      )
    )
  )
}

#' Data Upload Module Server
#'
#' @param id Module namespace
#'
#' @return List of reactive values for uploaded data
#' @export
mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive values to store data
    values <- shiny::reactiveValues(
      expenses_data = NULL,
      absences_data = NULL,
      exceptions_data = NULL,
      date_range = NULL,
      people_list = character(0),
      expense_types = character(0),
      shared_expense_types = character(0)
    )

    # Process uploaded Excel file
    shiny::observeEvent(input$xlsx_file, {
      shiny::req(input$xlsx_file)

      temp_error <- NULL

      tryCatch(
        {
          # Read the Excel file path
          filepath <- input$xlsx_file$datapath

          # Get list of available sheets
          available_sheets <- readxl::excel_sheets(filepath)

          # Check required sheets before loading anything
          required_sheets <- c("Expenses", "Person", "ExpenseType", "SharedExpenseType")
          optional_sheets <- c("Absences", "Exceptions")
          missing_required <- setdiff(required_sheets, available_sheets)

          if (length(missing_required) > 0) {
            shiny::showModal(shiny::modalDialog(
              title = "❌ Missing required sheets",
              shiny::tags$p("Your file is missing the following required sheet(s):"),
              shiny::tags$ul(lapply(missing_required, shiny::tags$li)),
              shiny::tags$hr(),
              shiny::tags$p(shiny::tags$strong("Required sheets: "), paste(required_sheets, collapse = ", ")),
              shiny::tags$p(shiny::tags$strong("Optional sheets: "), paste(optional_sheets, collapse = ", ")),
              shiny::tags$p(shiny::tags$strong("Sheets found in your file: "), paste(available_sheets, collapse = ", ")),
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            ))
            return()
          }

          # Initialize flag for overall success
          all_loaded <- TRUE

          # Helper function to load and validate a sheet
          load_sheet <- function(sheet_name, validator_fn, assign_target) {
            if (!(sheet_name %in% available_sheets)) {
              message(paste("Sheet '", sheet_name, "' not found in file"))
              return(NULL)
            }

            df <- readxl::read_excel(
              filepath,
              sheet = sheet_name
            )

            # Validate columns and types
            validator_fn(df)

            # Special processing for expenses sheet
            if (sheet_name == "Expenses") {
              if ("Date" %in% names(df)) {
                df$Date <- lubridate::mdy(df$Date)
              }
              if ("Amount" %in% names(df)) {
                df$Amount <- as.numeric(df$Amount)
              }
            }

            df
          }

          # Load Expenses sheet
          tryCatch(
            {
              values$expenses_data <- load_sheet(
                "Expenses",
                check_expenses_input,
                "expenses_data"
              )

              # Calculate date range from expenses
              if (!is.null(values$expenses_data)) {
                min_date <- min(values$expenses_data$Date, na.rm = TRUE)
                max_date <- max(values$expenses_data$Date, na.rm = TRUE)
                values$date_range <- list(start = min_date, end = max_date)

                shiny::showNotification(
                  paste(
                    "✅ Expenses loaded:",
                    nrow(values$expenses_data),
                    "rows"
                  ),
                  type = "message"
                )

                shiny::showNotification(
                  paste0(
                    "📅 Date range: ",
                    format(min_date, "%d/%m/%Y"),
                    " to ",
                    format(max_date, "%d/%m/%Y")
                  ),
                  type = "message",
                  duration = 5
                )
              }
            },
            error = function(e) {
              shiny::showModal(shiny::modalDialog(
                title = "❌ Error in Expenses sheet",
                shiny::tags$pre(
                  style = "white-space: pre-wrap; word-break: break-word;",
                  e$message
                ),
                footer = shiny::modalButton("OK"),
                easyClose = TRUE
              ))
              all_loaded <<- FALSE
            }
          )

          # Load Absences sheet
          tryCatch(
            {
              values$absences_data <- load_sheet(
                "Absences",
                check_absences_input,
                "absences_data"
              )

              if (!is.null(values$absences_data)) {
                shiny::showNotification(
                  paste(
                    "✅ Absences loaded:",
                    nrow(values$absences_data),
                    "rows"
                  ),
                  type = "message"
                )
              }
            },
            error = function(e) {
              shiny::showNotification(
                paste("⚠️ No Absences data or error:", e$message),
                type = "warning"
              )
            }
          )

          # Load Exceptions sheet
          tryCatch(
            {
              values$exceptions_data <- load_sheet(
                "Exceptions",
                check_exceptions_input,
                "exceptions_data"
              )

              if (!is.null(values$exceptions_data)) {
                shiny::showNotification(
                  paste(
                    "✅ Exceptions loaded:",
                    nrow(values$exceptions_data),
                    "rows"
                  ),
                  type = "message"
                )
              }
            },
            error = function(e) {
              shiny::showNotification(
                paste("⚠️ No Exceptions data or error:", e$message),
                type = "warning"
              )
            }
          )

          # Load Person sheet
          tryCatch(
            {
              df <- readxl::read_excel(filepath, sheet = "Person")
              if (!("Person" %in% names(df))) {
                stop(paste0(
                  "Sheet 'Person' found but column 'Person' is missing.\n",
                  "Expected exactly one column named 'Person'.\n",
                  "Found columns: ", paste(names(df), collapse = ", ")
                ))
              }
              values$people_list <- as.character(df$Person[!is.na(df$Person)])
            },
            error = function(e) {
              shiny::showModal(shiny::modalDialog(
                title = "❌ Error in Person sheet",
                shiny::tags$pre(
                  style = "white-space: pre-wrap; word-break: break-word;",
                  e$message
                ),
                footer = shiny::modalButton("OK"),
                easyClose = TRUE
              ))
            }
          )

          # Load ExpenseType sheet
          tryCatch(
            {
              df <- readxl::read_excel(filepath, sheet = "ExpenseType")
              if (!("ExpenseType" %in% names(df))) {
                stop(paste0(
                  "Sheet 'ExpenseType' found but column 'ExpenseType' is missing.\n",
                  "Expected exactly one column named 'ExpenseType'.\n",
                  "Found columns: ", paste(names(df), collapse = ", ")
                ))
              }
              values$expense_types <- as.character(df$ExpenseType[!is.na(df$ExpenseType)])
            },
            error = function(e) {
              shiny::showModal(shiny::modalDialog(
                title = "❌ Error in ExpenseType sheet",
                shiny::tags$pre(
                  style = "white-space: pre-wrap; word-break: break-word;",
                  e$message
                ),
                footer = shiny::modalButton("OK"),
                easyClose = TRUE
              ))
            }
          )

          # Load SharedExpenseType sheet
          tryCatch(
            {
              df <- readxl::read_excel(filepath, sheet = "SharedExpenseType")
              if (!("SharedExpenseType" %in% names(df))) {
                stop(paste0(
                  "Sheet 'SharedExpenseType' found but column 'SharedExpenseType' is missing.\n",
                  "Expected exactly one column named 'SharedExpenseType'.\n",
                  "Found columns: ", paste(names(df), collapse = ", ")
                ))
              }
              values$shared_expense_types <- as.character(df$SharedExpenseType[!is.na(df$SharedExpenseType)])
            },
            error = function(e) {
              shiny::showModal(shiny::modalDialog(
                title = "❌ Error in SharedExpenseType sheet",
                shiny::tags$pre(
                  style = "white-space: pre-wrap; word-break: break-word;",
                  e$message
                ),
                footer = shiny::modalButton("OK"),
                easyClose = TRUE
              ))
            }
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("❌ Failed to parse Excel file:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })

    # Optional: Load default exceptions on start
    shiny::onSessionEnded(function() {
      # Cleanup when session ends
    })

    return(values)
  })
}
