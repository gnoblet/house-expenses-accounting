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
        ns("expenses_file"), 
        "Upload Expenses CSV", 
        accept = ".csv"
      ),
      shiny::fileInput(
        ns("absences_file"),
        "Upload Absences CSV (optional)",
        accept = ".csv"
      ),
      shiny::fileInput(
        ns("exceptions_file"),
        "Upload Exceptions CSV (optional - percentage participation)",
        accept = ".csv"
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
      date_range = NULL
    )
    
    # Load expenses file
    shiny::observeEvent(input$expenses_file, {
      shiny::req(input$expenses_file)
      
      tryCatch({
        # Read the CSV file
        df <- readr::read_csv(input$expenses_file$datapath, show_col_types = FALSE)
        
        # Standardize column names
        names(df) <- c("Type", "Reason", "Date", "Amount", "Person")
        
        # Convert date column
        df$Date <- lubridate::dmy(df$Date)
        
        # Convert Amount to numeric
        df$Amount <- as.numeric(df$Amount)
        
        # Validate columns and types
        tryCatch({
          check_expenses_input(df)
          values$expenses_data <- df
          
          # Update date range
          min_date <- min(df$Date, na.rm = TRUE)
          max_date <- max(df$Date, na.rm = TRUE)
          values$date_range <- list(start = min_date, end = max_date)
          
          shiny::showNotification(
            "✅ Expenses file loaded successfully!",
            type = "message"
          )
          shiny::showNotification(
            paste(
              "📅 Date range updated to cover all expenses:",
              format(min_date, "%d/%m/%Y"),
              "to",
              format(max_date, "%d/%m/%Y")
            ),
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Validation error in expenses file:", e$message),
            type = "error",
            duration = 10
          )
        })
      }, error = function(e) {
        shiny::showNotification(
          paste("❌ Error reading expenses file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Load absences file
    shiny::observeEvent(input$absences_file, {
      shiny::req(input$absences_file)
      
      tryCatch({
        df <- readr::read_csv(input$absences_file$datapath, show_col_types = FALSE)
        tryCatch({
          check_absences_input(df)
          values$absences_data <- df
          shiny::showNotification(
            "✅ Absences file loaded successfully!",
            type = "message"
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Validation error in absences file:", e$message),
            type = "error",
            duration = 10
          )
        })
      }, error = function(e) {
        shiny::showNotification(
          paste("❌ Error reading absences file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Load exceptions file
    shiny::observeEvent(input$exceptions_file, {
      shiny::req(input$exceptions_file)
      
      tryCatch({
        df <- readr::read_csv(input$exceptions_file$datapath, show_col_types = FALSE)
        tryCatch({
          check_exceptions_input(df)
          values$exceptions_data <- df
          shiny::showNotification(
            "✅ Exceptions file loaded successfully!",
            type = "message"
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("❌ Validation error in exceptions file:", e$message),
            type = "error",
            duration = 10
          )
        })
      }, error = function(e) {
        shiny::showNotification(
          paste("❌ Error reading exceptions file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Load default exceptions on start if available
    shiny::observe({
      if (is.null(values$exceptions_data)) {
        asset_path <- system.file("app", "www", "assets", "exceptions_default.csv", package = "houseexpenses")
        if (file.exists(asset_path)) {
          tryCatch({
            df <- readr::read_csv(asset_path, show_col_types = FALSE)
            values$exceptions_data <- df
            message("✓ Loaded default exceptions from assets/exceptions_default.csv")
          }, error = function(e) {
            message("Warning: Could not load default exceptions: ", e$message)
          })
        }
      }
    })
    
    return(values)
  })
}
