library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(scales)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(ggiraph)
library(ggplot2)
library(stringr)
library(viridis)

# Function to load default values from assets folder
load_default_from_assets <- function(filename, fallback_default) {
  asset_path <- file.path("assets", filename)
  if (file.exists(asset_path)) {
    tryCatch(
      {
        content <- readLines(asset_path, warn = FALSE)
        content <- content[content != ""] # Remove empty lines
        return(paste(content, collapse = "\n"))
      },
      error = function(e) {
        cat(
          "Warning: Could not read",
          asset_path,
          "- using fallback defaults\n"
        )
        return(fallback_default)
      }
    )
  } else {
    return(fallback_default)
  }
}

# Load defaults from assets or use fallbacks
default_people_list <- load_default_from_assets(
  "people_list_default.txt",
  "Alice\nBob\nCharlie\nDiana\nEve\nFrank\nGrace\nHenry"
)

default_expense_types <- load_default_from_assets(
  "expense_types_default.txt",
  "Normal\nParty\nAlcohol\nSpecial"
)

default_shared_expense_types <- load_default_from_assets(
  "shared_expense_types_default.txt",
  "Utilities\nSubscriptions\nInsurance"
)

ui <- div(
  
  page_navbar(
    title = "House Expense Calculator",
    id = "main_nav",
    
    nav_panel(
    "Setup & Calculate",
    value = "setup_page",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      h2("House Expense Calculator", class = "text-center mb-4"),
      
      fluidRow(
        column(
          6,
          div(
            class = "card",
            div(
              class = "card-header",
              h4("📁 Data Upload", class = "mb-0")
            ),
            div(
              class = "card-body",
              fileInput("expenses_file", "Upload Expenses CSV", accept = ".csv"),
              fileInput(
                "absences_file",
                "Upload Absences CSV (optional)",
                accept = ".csv"
              ),
              fileInput(
                "exceptions_file",
                "Upload Exceptions CSV (optional - percentage participation)",
                accept = ".csv"
              )
            )
          ),
          
          br(),
          
          div(
            class = "card",
            div(
              class = "card-header",
              h4("📅 Period Settings", class = "mb-0")
            ),
            div(
              class = "card-body",
              fluidRow(
                column(
                  6,
                  dateInput(
                    "start_date",
                    "Start Date",
                    value = Sys.Date() - 90,
                    format = "dd M yyyy"
                  )
                ),
                column(
                  6,
                  dateInput("end_date", "End Date", value = Sys.Date(), format = "dd M yyyy")
                )
              )
            )
          )
        ),
        
        column(
          6,
          div(
            class = "card",
            div(
              class = "card-header",
              h4("👥 People & Expense Types", class = "mb-0")
            ),
            div(
              class = "card-body",
              textAreaInput(
                "people_list",
                "People Names (one per line)",
                value = default_people_list,
                height = "120px"
              ),
              textAreaInput(
                "expense_types",
                "Expense Types (one per line)",
                value = default_expense_types,
                height = "100px"
              ),
              textAreaInput(
                "shared_expense_types",
                "Shared Expense Types - Always Split Equally (one per line)",
                value = default_shared_expense_types,
                height = "80px"
              )
            )
          )
        )
      ),
      
      br(),
      
      div(
        class = "text-center",
        actionButton(
          "calculate", 
          "🧮 Calculate Expenses", 
          class = "btn-primary btn-lg",
          style = "padding: 15px 30px; font-size: 18px;"
        )
      ),
      
      br(),
      
      # Instructions/Help section
      div(
        class = "card bg-light",
        div(
          class = "card-header",
          h5("ℹ️ Instructions", class = "mb-0")
        ),
        div(
          class = "card-body",
          tags$ul(
            tags$li("Upload your expenses CSV file with columns: Type, Reason, Date, Amount, Person"),
            tags$li("Optionally upload absences CSV (Person, Absent_Days) and exceptions CSV (Person, Type, Percentage)"),
            tags$li("Adjust the date range if needed - it will auto-update when you upload expenses"),
            tags$li("Modify people names and expense types as needed"),
            tags$li("Click 'Calculate Expenses' to see the results in the other tabs")
          ),
          br(),
          h6("📥 Download Example Files:", class = "mb-2"),
          div(
            class = "d-flex flex-wrap gap-2",
            downloadButton(
              "download_expenses_example",
              "📊 Expenses Example",
              class = "btn-outline-secondary btn-sm"
            ),
            downloadButton(
              "download_absences_example", 
              "🏖️ Absences Example",
              class = "btn-outline-secondary btn-sm"
            ),
            downloadButton(
              "download_exceptions_example",
              "⚙️ Exceptions Example", 
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Results",
    value = "results",
     # Top download button
  div(
    class = "container-fluid bg-light border-bottom",
    style = "padding: 10px 0;",
    div(
      class = "container-fluid",
      style = "max-width: 1200px;",
      div(
        class = "d-flex justify-content-end",
        downloadButton(
          "download_pdf",
          "📄 Download PDF Report",
          class = "btn-success"
        )
      )
    )
  ),
    div(
      class = "container-fluid",
      navset_card_tab(
        nav_panel(
          "Final Settlement",
          h4("💰 Final Settlement"),
          DT::dataTableOutput("final_table")
        ),
        nav_panel(
          "Expense Summary",
          h4("📊 Expense Summary"),
          DT::dataTableOutput("summary_table")
        ),
        nav_panel(
          "Expense Details",
          h4("📋 Expense Details"),
          DT::dataTableOutput("expenses_table")
        ),
        nav_panel(
          "Filter by Type",
          h4("🔍 Filter by Type"),
          fluidRow(
            column(
              4,
              selectInput(
                "filter_type",
                "Select Expense Type:",
                choices = NULL,
                selected = NULL
              )
            ),
            column(8, h5("Quick Summary:"), verbatimTextOutput("type_summary"))
          ),
          br(),
          DT::dataTableOutput("filtered_settlement_table")
        )
      )
    )
  ),
  
  nav_panel(
    "Long Term Analysis",
    value = "long_term_analysis",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      h2("📈 Long Term Analysis", class = "text-center mb-4"),
      
      fluidRow(
        column(
          12,
          div(
            class = "card",
            div(
              class = "card-header",
              h4("Analysis Controls", class = "mb-0")
            ),
            div(
              class = "card-body",
              fluidRow(
                column(
                  4,
                  selectInput(
                    "analysis_type",
                    "Analysis Type:",
                    choices = list(
                      "By Expense Type" = "type",
                      "By Person" = "person",
                      "Total Expenses" = "total"
                    ),
                    selected = "type"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "period_grouping",
                    "Group By:",
                    choices = list(
                      "Month" = "month",
                      "Quarter" = "quarter",
                      "Year" = "year"
                    ),
                    selected = "month"
                  )
                ),
                column(
                  4,
                  checkboxInput(
                    "include_shared",
                    "Include Shared Expenses",
                    value = TRUE
                  )
                )
              )
            )
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          12,
          div(
            class = "card",
            div(
              class = "card-header",
              h4("📊 Interactive Chart", class = "mb-0")
            ),
            div(
              class = "card-body",
              girafeOutput("long_term_chart", height = "600px")
            )
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          12,
          div(
            class = "card",
            div(
              class = "card-header",
              h4("📋 Summary Table", class = "mb-0")
            ),
            div(
              class = "card-body",
              DT::dataTableOutput("long_term_table")
            )
          )
        )
      )
    )
  )
)
)

server <- function(input, output, session) {
  # Reactive values to store data
  expenses_data <- reactiveVal(NULL)
  absences_data <- reactiveVal(NULL)
  exceptions_data <- reactiveVal(NULL)
  
  # Hide result tabs initially
  observe({
    hideTab(inputId = "main_nav", target = "results")
  })

  # Load expenses file
  observeEvent(input$expenses_file, {
    req(input$expenses_file)

    tryCatch(
      {
        # Read the CSV file
        df <- read_csv(input$expenses_file$datapath)

        # Standardize column names (handle different languages/formats)
        names(df) <- c("Type", "Reason", "Date", "Amount", "Person")

        # Convert date column
        df$Date <- dmy(df$Date)

        # Convert Amount to numeric (assuming it was CHF before)
        df$Amount <- as.numeric(df$Amount)

        expenses_data(df)

        # Update date range to cover all expenses
        min_date <- min(df$Date, na.rm = TRUE)
        max_date <- max(df$Date, na.rm = TRUE)

        updateDateInput(session, "start_date", value = min_date)
        updateDateInput(session, "end_date", value = max_date)

        showNotification(
          "✅ Expenses file loaded successfully!",
          type = "message"
        )
        showNotification(
          paste(
            "📅 Date range updated to cover all expenses:",
            format(min_date, "%d/%m/%Y"),
            "to",
            format(max_date, "%d/%m/%Y")
          ),
          type = "message",
          duration = 5
        )
      },
      error = function(e) {
        showNotification(
          paste("❌ Error loading expenses file:", e$message),
          type = "error"
        )
      }
    )
  })

  # Load absences file
  observeEvent(input$absences_file, {
    req(input$absences_file)

    tryCatch(
      {
        df <- read_csv(input$absences_file$datapath)
        absences_data(df)
        showNotification(
          "✅ Absences file loaded successfully!",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste("❌ Error loading absences file:", e$message),
          type = "error"
        )
      }
    )
  })

  # Load exceptions file
  observeEvent(input$exceptions_file, {
    req(input$exceptions_file)

    tryCatch(
      {
        df <- read_csv(input$exceptions_file$datapath)
        exceptions_data(df)
        showNotification(
          "✅ Exceptions file loaded successfully!",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste("❌ Error loading exceptions file:", e$message),
          type = "error"
        )
      }
    )
  })

  # Load default exceptions on app start if available
  observe({
    if (is.null(exceptions_data())) {
      asset_path <- file.path("assets", "exceptions_default.csv")
      if (file.exists(asset_path)) {
        tryCatch(
          {
            df <- read_csv(asset_path, show_col_types = FALSE)
            exceptions_data(df)
            cat(
              "✓ Loaded default exceptions from assets/exceptions_default.csv\n"
            )
          },
          error = function(e) {
            cat("Warning: Could not read", asset_path, ":", e$message, "\n")
          }
        )
      }
    }
  })

  # Main calculation
  calculations <- eventReactive(input$calculate, {
    req(expenses_data())

    # Get people list
    people <- trimws(strsplit(input$people_list, "\n")[[1]])
    people <- people[people != ""]

    # Get expense types
    expense_types <- trimws(strsplit(input$expense_types, "\n")[[1]])
    expense_types <- expense_types[expense_types != ""]

    # Get shared expense types (always split equally)
    shared_expense_types <- trimws(strsplit(input$shared_expense_types, "\n")[[
      1
    ]])
    shared_expense_types <- shared_expense_types[shared_expense_types != ""]

    # Combine all expense types for validation
    all_expense_types <- unique(c(expense_types, shared_expense_types))

    # Filter expenses by date range
    expenses <- expenses_data() |>
      filter(Date >= input$start_date & Date <= input$end_date)

    # Debug: Check if we have expenses in the date range
    if (nrow(expenses) == 0) {
      showNotification(
        "⚠️ No expenses found in the selected date range. Please check your date range or expense data.",
        type = "warning",
        duration = 8
      )
      return(NULL)
    }

    # Calculate total days in period
    total_days <- as.numeric(input$end_date - input$start_date) + 1

    # Validation checks
    validation_errors <- c()
    validation_warnings <- c()

    # Check if people names in expenses match the people list
    unique_people_in_expenses <- unique(expenses$Person)
    people_not_in_list <- setdiff(unique_people_in_expenses, people)
    people_not_in_expenses <- setdiff(people, unique_people_in_expenses)

    if (length(people_not_in_list) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "👥 People in expenses but not in people list:",
          paste(people_not_in_list, collapse = ", ")
        )
      )
    }

    if (length(people_not_in_expenses) > 0) {
      validation_warnings <- c(
        validation_warnings,
        paste(
          "🔍 People in list but not found in expenses:",
          paste(people_not_in_expenses, collapse = ", ")
        )
      )
    }

    # Check if expense types in expenses match the expense types list
    unique_types_in_expenses <- unique(expenses$Type)
    types_not_in_list <- setdiff(unique_types_in_expenses, expense_types)
    types_not_in_expenses <- setdiff(expense_types, unique_types_in_expenses)

    if (length(types_not_in_list) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "🏷️ Expense types in expenses but not in types list:",
          paste(types_not_in_list, collapse = ", ")
        )
      )
    }

    if (length(types_not_in_expenses) > 0) {
      validation_warnings <- c(
        validation_warnings,
        paste(
          "📝 Expense types in list but not found in expenses:",
          paste(types_not_in_expenses, collapse = ", ")
        )
      )
    }

    # Handle exceptions data
    if (is.null(exceptions_data())) {
      exceptions <- data.frame(
        Person = character(0),
        Type = character(0),
        Percentage = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      exceptions <- exceptions_data()

      # Validate exceptions data
      exceptions_people_not_in_list <- setdiff(
        unique(exceptions$Person),
        people
      )
      exceptions_types_not_in_list <- setdiff(
        unique(exceptions$Type),
        all_expense_types
      )

      if (length(exceptions_people_not_in_list) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "🚫 People in exceptions file but not in people list:",
            paste(exceptions_people_not_in_list, collapse = ", ")
          )
        )
      }

      if (length(exceptions_types_not_in_list) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "🚫 Expense types in exceptions file but not in types list:",
            paste(exceptions_types_not_in_list, collapse = ", ")
          )
        )
      }

      # Validate percentage values
      invalid_percentages <- exceptions$Percentage[
        exceptions$Percentage < 0 | exceptions$Percentage >= 1
      ]
      if (length(invalid_percentages) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "📊 Invalid percentage values in exceptions file (must be 0.0-0.99):",
            paste(invalid_percentages, collapse = ", ")
          )
        )
      }
    }

    # Create absence adjustments (default to 0 if no file uploaded)
    if (is.null(absences_data())) {
      absences <- data.frame(
        Person = people,
        Absent_Days = 0,
        stringsAsFactors = FALSE
      )
    } else {
      absences <- absences_data()

      # Validate absences data
      # Check if all people in absences are in the people list
      people_in_absences <- unique(absences$Person)
      absences_people_not_in_list <- setdiff(people_in_absences, people)
      absences_people_missing <- setdiff(people, people_in_absences)

      if (length(absences_people_not_in_list) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "🚫 People in absences file but not in people list:",
            paste(absences_people_not_in_list, collapse = ", ")
          )
        )
      }

      if (length(absences_people_missing) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "❌ People missing from absences file:",
            paste(absences_people_missing, collapse = ", ")
          )
        )
      }

      # Check for negative absent days
      negative_days <- absences$Absent_Days[absences$Absent_Days < 0]
      if (length(negative_days) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "⚠️ Negative absent days found:",
            paste(negative_days, collapse = ", ")
          )
        )
      }

      # Check for absent days exceeding period length
      excessive_days <- absences$Absent_Days[absences$Absent_Days > total_days]
      if (length(excessive_days) > 0) {
        validation_errors <- c(
          validation_errors,
          paste(
            "📅 Absent days exceeding period length (",
            total_days,
            " days):",
            paste(excessive_days, collapse = ", ")
          )
        )
      }
    }

    # If there are validation errors, show them and return NULL
    if (length(validation_errors) > 0) {
      showNotification(
        shiny::HTML(paste(
          "🚨 <strong>Validation Errors:</strong><br>",
          paste("• ", validation_errors, collapse = "<br>")
        )),
        type = "error",
        duration = 10
      )
      return(NULL)
    }

    # Show warnings if any (but continue with calculation)
    if (length(validation_warnings) > 0) {
      showNotification(
        shiny::HTML(paste(
          "⚠️ <strong>Warnings:</strong><br>",
          paste("• ", validation_warnings, collapse = "<br>")
        )),
        type = "warning",
        duration = 8
      )
    }

    # Show info about shared expense types if any
    shared_types_in_expenses <- intersect(
      shared_expense_types,
      unique(expenses$Type)
    )
    if (length(shared_types_in_expenses) > 0) {
      showNotification(
        shiny::HTML(paste(
          "⚖️ <strong>Shared Expense Types (Equal Split):</strong><br>",
          paste("•", shared_types_in_expenses, collapse = "<br>")
        )),
        type = "message",
        duration = 4
      )
    }

    # Show info about exceptions if any
    if (nrow(exceptions) > 0) {
      exception_summary <- exceptions |>
        group_by(Type) |>
        summarise(
          Exception_Details = paste(
            paste(
              Person,
              "(",
              scales::percent(Percentage, accuracy = 1),
              ")",
              sep = ""
            ),
            collapse = ", "
          ),
          .groups = "drop"
        )

      exception_msg <- paste(
        apply(exception_summary, 1, function(x) {
          paste("•", x["Type"], ":", x["Exception_Details"])
        }),
        collapse = "<br>"
      )

      showNotification(
        shiny::HTML(paste(
          "ℹ️ <strong>Expense Exceptions Applied:</strong><br>",
          exception_msg
        )),
        type = "message",
        duration = 6
      )
    }

    # Calculate presence ratios
    presence_ratios <- absences |>
      mutate(
        Present_Days = total_days - Absent_Days,
        Presence_Ratio = Present_Days / total_days
      )

    # Calculate expenses by type and person
    expense_summary <- expenses |>
      group_by(Type, Person) |>
      summarise(Total_Paid = sum(Amount, na.rm = TRUE), .groups = "drop") |>
      complete(Type, Person = people, fill = list(Total_Paid = 0))

    # Calculate total by expense type
    type_totals <- expenses |>
      group_by(Type) |>
      summarise(Total_Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

    # Calculate what each person owes (adjusted for absences)
    final_calculations <- expand_grid(
      Type = all_expense_types,
      Person = people
    ) |>
      left_join(type_totals, by = "Type") |>
      left_join(presence_ratios, by = c("Person" = "Person")) |>
      left_join(expense_summary, by = c("Type", "Person" = "Person")) |>
      mutate(
        Total_Amount = ifelse(is.na(Total_Amount), 0, Total_Amount),
        Total_Paid = ifelse(is.na(Total_Paid), 0, Total_Paid),
        Presence_Ratio = ifelse(is.na(Presence_Ratio), 1, Presence_Ratio)
      )

    # Calculate weighted shares that sum to the total amount for each expense type
    final_calculations <- final_calculations |>
      group_by(Type) |>
      mutate(
        # Check if this is a shared expense type (always split equally)
        Is_Shared_Type = Type %in% shared_expense_types,
        # For shared types: use equal split, for others: apply exceptions and absences
        Exception_Key = paste(Person, Type, sep = "-"),
        Exception_Percentage = case_when(
          Is_Shared_Type ~ 1.0, # Shared types always 100% for everyone
          Exception_Key %in%
            paste(exceptions$Person, exceptions$Type, sep = "-") ~
            exceptions$Percentage[match(
              Exception_Key,
              paste(exceptions$Person, exceptions$Type, sep = "-")
            )],
          TRUE ~ 1.0
        ),
        # For shared types: ignore presence ratio, for others: apply it
        Base_Ratio = ifelse(Is_Shared_Type, 1.0, Presence_Ratio),
        # Apply exception percentage to base ratio
        Adjusted_Presence_Ratio = Base_Ratio * Exception_Percentage,
        Total_Presence_Weight = sum(Adjusted_Presence_Ratio),
        Share_Owed = ifelse(
          Total_Presence_Weight > 0,
          (Total_Amount * Adjusted_Presence_Ratio) / Total_Presence_Weight,
          0
        ),
        Balance = Total_Paid - Share_Owed
      ) |>
      ungroup()

    # Create summary by type
    summary_by_type <- final_calculations |>
      group_by(Type) |>
      summarise(
        Total_Amount = first(Total_Amount),
        Total_Paid = sum(Total_Paid),
        Total_Owed = sum(Share_Owed),
        .groups = "drop"
      )

    # Create final settlement table
    final_settlement <- final_calculations |>
      group_by(Person) |>
      summarise(
        Total_Paid = sum(Total_Paid),
        Total_Owed = sum(Share_Owed),
        Final_Balance = sum(Balance),
        .groups = "drop"
      )

    list(
      expenses = expenses,
      summary_by_type = summary_by_type,
      final_settlement = final_settlement,
      detailed_calculations = final_calculations
    )
  })

  # Navigate to results tabs after successful calculation
  observe({
    req(calculations())
    if (!is.null(calculations())) {
      # Show result tabs
      showTab(inputId = "main_nav", target = "results")
      
      # Navigate to results tab
      updateNavbarPage(session, "main_nav", selected = "results")
      showNotification(
        "✅ Calculation complete! Navigate between the tabs to see results.",
        type = "message",
        duration = 3
      )
    }
  })

  # Display expenses table
  output$expenses_table <- DT::renderDataTable({
    req(calculations())

    if (is.null(calculations())) {
      return(DT::datatable(data.frame(
        Message = "⚠️ Please fix validation errors and recalculate"
      )))
    }

    calculations()$expenses |>
      arrange(desc(Date)) |>
      DT::datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) |>
      DT::formatCurrency("Amount", currency = "CHF ")
  })

  # Display summary table
  output$summary_table <- DT::renderDataTable({
    req(calculations())

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
    req(calculations())

    if (is.null(calculations())) {
      return(DT::datatable(data.frame(
        Message = "⚠️ Please fix validation errors and recalculate"
      )))
    }

    calculations()$final_settlement |>
      arrange(desc(Final_Balance)) |>
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

  # Download handler for PDF report
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("House_Expenses_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Check if calculations exist
      if (is.null(calculations()) || is.null(calculations()$final_settlement)) {
        showNotification(
          "❌ No data to export. Please calculate expenses first.",
          type = "error"
        )
        return()
      }

      cat("Creating PDF report with R Markdown...\n")

      # Prepare data
      final_settlement <- calculations()$final_settlement
      summary_by_type <- calculations()$summary_by_type
      expenses <- calculations()$expenses
      detailed_calculations <- calculations()$detailed_calculations

      absences_summary <- if (
        !is.null(absences_data()) && nrow(absences_data()) > 0
      ) {
        absences_data()
      } else {
        data.frame(Person = character(0), Absent_Days = numeric(0))
      }

      exceptions_summary <- if (
        !is.null(exceptions_data()) && nrow(exceptions_data()) > 0
      ) {
        exceptions_data()
      } else {
        data.frame(
          Person = character(0),
          Type = character(0),
          Percentage = numeric(0)
        )
      }

      # Create temporary R Markdown file
      temp_rmd <- tempfile(fileext = ".Rmd")

      # Generate R Markdown content
      rmd_content <- paste0(
        '
---
title: "House Expenses Report"
date: "',
        format(Sys.Date(), "%B %d, %Y"),
        '"
output: 
  pdf_document:
    latex_engine: xelatex
    number_sections: true
geometry: margin=0.8in
mainfont: Carlito
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{array}
  - \\usepackage{multirow}
  - \\usepackage{wrapfig}
  - \\usepackage{float}
  - \\usepackage{colortbl}
  - \\usepackage{pdflscape}
  - \\usepackage{tabu}
  - \\usepackage{threeparttable}
  - \\usepackage{threeparttablex}
  - \\usepackage[normalem]{ulem}
  - \\usepackage{makecell}
  - \\usepackage{xcolor}
  - \\definecolor{softgreen}{RGB}{144,190,109}
  - \\definecolor{softred}{RGB}{235,151,78}
params:
  start_date: "',
        input$start_date,
        '"
  end_date: "',
        input$end_date,
        '"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(kableExtra)
library(dplyr)
library(scales)
```

# Executive Summary

**Report Period:** ',
        format(input$start_date, "%B %d, %Y"),
        ' to ',
        format(input$end_date, "%B %d, %Y"),
        '

**Total Expenses:** CHF ',
        sprintf("%.2f", sum(expenses$Amount, na.rm = TRUE)),
        '

**Number of Transactions:** ',
        nrow(expenses),
        '

**People Involved:** ',
        length(unique(expenses$Person)),
        '

## Absences Summary

',
        if (nrow(absences_summary) > 0) {
          paste0(
            '```{r absences-table}
absences_data <- data.frame(
  Person = c("',
            paste(absences_summary$Person, collapse = '", "'),
            '"),
  Absent_Days = c(',
            paste(absences_summary$Absent_Days, collapse = ', '),
            ')
)

kable(absences_data, 
      col.names = c("Person", "Absent Days"),
      caption = "Absence Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```'
          )
        } else {
          "No absences recorded for this period."
        },
        '

## Exceptions Summary

',
        if (nrow(exceptions_summary) > 0) {
          paste0(
            '```{r exceptions-table}
exceptions_data <- data.frame(
  Person = c("',
            paste(exceptions_summary$Person, collapse = '", "'),
            '"),
  Type = c("',
            paste(exceptions_summary$Type, collapse = '", "'),
            '"),
  Percentage = c(',
            paste(exceptions_summary$Percentage, collapse = ', '),
            ')
)

exceptions_data$Percentage_Text <- paste0(round(exceptions_data$Percentage * 100, 1), "%")

kable(exceptions_data[,c("Person", "Type", "Percentage_Text")], 
      col.names = c("Person", "Expense Type", "Participation %"),
      caption = "Expense Participation Exceptions") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```'
          )
        } else {
          "No exceptions applied for this period."
        },
        '

# Final Settlement

```{r final-settlement}
final_data <- data.frame(
  Person = c("',
        paste(final_settlement$Person, collapse = '", "'),
        '"),
  Total_Paid = c(',
        paste(final_settlement$Total_Paid, collapse = ', '),
        '),
  Total_Owed = c(',
        paste(final_settlement$Total_Owed, collapse = ', '),
        '),
  Final_Balance = c(',
        paste(final_settlement$Final_Balance, collapse = ', '),
        ')
)

final_data <- final_data %>%
  arrange(desc(Final_Balance)) %>%
  mutate(
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Total_Owed_Text = paste0("CHF ", sprintf("%.2f", Total_Owed)),
    Final_Balance_Text = paste0("CHF ", sprintf("%.2f", Final_Balance)),
    Balance_Color = ifelse(Final_Balance >= 0, "green", "red")
  )

kable(final_data[,c("Person", "Total_Paid_Text", "Total_Owed_Text", "Final_Balance_Text")], 
      col.names = c("Person", "Total Paid", "Total Owed", "Final Balance"),
      caption = "Final Settlement - Who Owes What") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position")) %>%
  column_spec(4, color = ifelse(final_data$Final_Balance >= 0, "softgreen", "softred"), bold = TRUE)
```

# Summary by Expense Type

```{r summary-by-type}
summary_data <- data.frame(
  Type = c("',
        paste(summary_by_type$Type, collapse = '", "'),
        '"),
  Total_Amount = c(',
        paste(summary_by_type$Total_Amount, collapse = ', '),
        '),
  Total_Paid = c(',
        paste(summary_by_type$Total_Paid, collapse = ', '),
        '),
  Total_Owed = c(',
        paste(summary_by_type$Total_Owed, collapse = ', '),
        ')
)

summary_data <- summary_data %>%
  mutate(
    Total_Amount_Text = paste0("CHF ", sprintf("%.2f", Total_Amount)),
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Total_Owed_Text = paste0("CHF ", sprintf("%.2f", Total_Owed))
  )

kable(summary_data[,c("Type", "Total_Amount_Text", "Total_Paid_Text", "Total_Owed_Text")], 
      col.names = c("Expense Type", "Total Amount", "Total Paid", "Total Owed"),
      caption = "Summary by Expense Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```

# Expense Details

```{r expense-details}
expense_data <- data.frame(
  Date = as.Date(c("',
        paste(expenses$Date, collapse = '", "'),
        '")),
  Type = c("',
        paste(expenses$Type, collapse = '", "'),
        '"),
  Person = c("',
        paste(expenses$Person, collapse = '", "'),
        '"),
  Reason = c("',
        paste(gsub('"', '\\\\\\"', expenses$Reason), collapse = '", "'),
        '"),
  Amount = c(',
        paste(expenses$Amount, collapse = ', '),
        ')
)

expense_data <- expense_data %>%
  arrange(desc(Date)) %>%
  mutate(
    Date_Text = format(Date, "%Y-%m-%d"),
    Amount_Text = paste0("CHF ", sprintf("%.2f", Amount))
  )

kable(expense_data[,c("Date_Text", "Type", "Person", "Reason", "Amount_Text")], 
      col.names = c("Date", "Type", "Person", "Reason", "Amount"),
      caption = "All Expense Details") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position", "scale_down"))
```

# Detailed Calculations

```{r detailed-calculations}
detailed_data <- data.frame(
  Person = c("',
        paste(detailed_calculations$Person, collapse = '", "'),
        '"),
  Type = c("',
        paste(detailed_calculations$Type, collapse = '", "'),
        '"),
  Total_Paid = c(',
        paste(detailed_calculations$Total_Paid, collapse = ', '),
        '),
  Share_Owed = c(',
        paste(detailed_calculations$Share_Owed, collapse = ', '),
        '),
  Balance = c(',
        paste(detailed_calculations$Balance, collapse = ', '),
        ')
)

detailed_data <- detailed_data %>%
  filter(Total_Paid > 0 | Share_Owed > 0) %>%
  arrange(Type, desc(Balance)) %>%
  mutate(
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Share_Owed_Text = paste0("CHF ", sprintf("%.2f", Share_Owed)),
    Balance_Text = paste0("CHF ", sprintf("%.2f", Balance))
  )

kable(detailed_data[,c("Person", "Type", "Total_Paid_Text", "Share_Owed_Text", "Balance_Text")], 
      col.names = c("Person", "Type", "Paid", "Owed", "Balance"),
      caption = "Detailed Calculations by Person and Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position", "scale_down")) %>%
  column_spec(5, color = ifelse(detailed_data$Balance >= 0, "softgreen", "softred"), bold = TRUE)
```
'
      )

      # Write R Markdown content to temporary file
      writeLines(rmd_content, temp_rmd)

      # Render R Markdown document
      tryCatch(
        {
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            quiet = FALSE
          )

          cat("✓ PDF report created successfully with R Markdown\n")
          showNotification(
            "📄 PDF report generated successfully!",
            type = "message"
          )
        },
        error = function(e) {
          cat("❌ R Markdown PDF creation failed:", e$message, "\n")
          showNotification(
            paste("❌ Error creating PDF report:", e$message),
            type = "error"
          )

          # Create a simple CSV file as fallback
          tryCatch(
            {
              write.csv(
                calculations()$final_settlement,
                file,
                row.names = FALSE
              )
              showNotification(
                "⚠️ PDF creation failed - provided CSV file instead",
                type = "warning"
              )
            },
            error = function(e2) {
              cat("❌ CSV fallback also failed:", e2$message, "\n")
              writeLines("Error: Could not create report file", file)
              showNotification(
                "❌ Report creation failed completely",
                type = "error"
              )
            }
          )
        }
      )

      # Clean up temporary file
      if (file.exists(temp_rmd)) {
        file.remove(temp_rmd)
      }
    },
    contentType = "application/pdf"
  )

  # Download handlers for example files
  output$download_expenses_example <- downloadHandler(
    filename = function() {
      "expenses_example.csv"
    },
    content = function(file) {
      example_path <- file.path("examples", "expenses_example.csv")
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
        write.csv(example_data, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )
  
  output$download_absences_example <- downloadHandler(
    filename = function() {
      "absences_example.csv"
    },
    content = function(file) {
      example_path <- file.path("examples", "absences_example.csv")
      if (file.exists(example_path)) {
        file.copy(example_path, file)
      } else {
        # Create a basic example if file doesn't exist
        example_data <- data.frame(
          Person = c("Alice", "Bob", "Charlie", "Diana"),
          Absent_Days = c(0, 5, 2, 0)
        )
        write.csv(example_data, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )
  
  output$download_exceptions_example <- downloadHandler(
    filename = function() {
      "exceptions_example.csv"
    },
    content = function(file) {
      example_path <- file.path("examples", "exceptions_example.csv")
      if (file.exists(example_path)) {
        file.copy(example_path, file)
      } else {
        # Create a basic example if file doesn't exist
        example_data <- data.frame(
          Person = c("Alice", "Bob"),
          Type = c("Alcohol", "Alcohol"),
          Percentage = c(0.5, 0.0)
        )
        write.csv(example_data, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )

  # Update filter dropdown choices when calculations change
  observe({
    req(calculations())
    if (!is.null(calculations())) {
      expense_types <- unique(calculations()$detailed_calculations$Type)
      expense_types <- expense_types[!is.na(expense_types)]
      updateSelectInput(
        session,
        "filter_type",
        choices = expense_types,
        selected = expense_types[1]
      )
    }
  })

  # Display filtered settlement table
  output$filtered_settlement_table <- DT::renderDataTable({
    req(calculations(), input$filter_type)

    if (is.null(calculations()) || is.null(input$filter_type)) {
      return(DT::datatable(data.frame(
        Message = "⚠️ Please calculate expenses and select a type"
      )))
    }

    # Filter detailed calculations for selected type
    filtered_data <- calculations()$detailed_calculations |>
      filter(Type == input$filter_type) |>
      select(Person, Type, Total_Amount, Total_Paid, Share_Owed, Balance) |>
      arrange(desc(Balance))

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
  output$type_summary <- renderText({
    req(calculations(), input$filter_type)

    if (is.null(calculations()) || is.null(input$filter_type)) {
      return("Select an expense type to see summary")
    }

    # Get summary for selected type
    filtered_data <- calculations()$detailed_calculations |>
      filter(Type == input$filter_type)

    total_amount <- first(filtered_data$Total_Amount)
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
  
  # Long Term Analysis reactive data
  long_term_data <- reactive({
    req(expenses_data())
    
    df <- expenses_data()
    
    # Filter by shared expenses if requested
    if (!input$include_shared) {
      df <- df[df$Type != "Shared", ]
    }
    
    # Add period grouping
    df$Period <- case_when(
      input$period_grouping == "month" ~ format(df$Date, "%Y-%m"),
      input$period_grouping == "quarter" ~ paste0(format(df$Date, "%Y"), "-Q", quarter(df$Date)),
      input$period_grouping == "year" ~ format(df$Date, "%Y")
    )
    
    # Group by period and analysis type
    if (input$analysis_type == "type") {
      grouped_data <- df %>%
        group_by(Period, Type) %>%
        summarise(
          Total_Amount = sum(Amount, na.rm = TRUE),
          Count = n(),
          .groups = "drop"
        ) %>%
        arrange(Period, Type)
    } else if (input$analysis_type == "person") {
      grouped_data <- df %>%
        group_by(Period, Person) %>%
        summarise(
          Total_Amount = sum(Amount, na.rm = TRUE),
          Count = n(),
          .groups = "drop"
        ) %>%
        arrange(Period, Person)
    } else { # total
      grouped_data <- df %>%
        group_by(Period) %>%
        summarise(
          Total_Amount = sum(Amount, na.rm = TRUE),
          Count = n(),
          .groups = "drop"
        ) %>%
        arrange(Period) %>%
        mutate(Category = "Total Expenses")
    }
    
    return(grouped_data)
  })
  
  # Long term chart output
  output$long_term_chart <- renderGirafe({
    req(long_term_data())
    
    data <- long_term_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Create the plot based on analysis type
    if (input$analysis_type == "total") {
      p <- ggplot(data, aes(x = Period, y = Total_Amount)) +
        geom_col_interactive(
          aes(tooltip = paste0("Period: ", Period, "\nTotal: CHF ", 
                               sprintf("%.2f", Total_Amount), "\nTransactions: ", Count)),
          fill = "#3498db",
          alpha = 0.8
        ) +
        labs(
          title = paste("Total Expenses by", str_to_title(input$period_grouping)),
          x = str_to_title(input$period_grouping),
          y = "Amount (CHF)"
        )
    } else {
      color_var <- if (input$analysis_type == "type") "Type" else "Person"
      
      p <- ggplot(data, aes(x = Period, y = Total_Amount, fill = !!sym(color_var))) +
        geom_col_interactive(
          aes(tooltip = paste0("Period: ", Period, "\n", color_var, ": ", !!sym(color_var), 
                               "\nAmount: CHF ", sprintf("%.2f", Total_Amount), 
                               "\nTransactions: ", Count)),
          alpha = 0.8,
          position = "stack"
        ) +
        labs(
          title = paste("Expenses by", str_to_title(input$period_grouping), "and", str_to_title(gsub("_", " ", input$analysis_type))),
          x = str_to_title(input$period_grouping),
          y = "Amount (CHF)",
          fill = str_to_title(gsub("_", " ", color_var))
        ) +
        scale_fill_viridis_d(alpha = 0.8)
    }
    
    # Common styling
    p <- p +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom"
      ) +
      scale_y_continuous(labels = scales::label_currency(prefix = "CHF "))
    
    # Create interactive plot
    girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 8,
      options = list(
        opts_hover_inv(css = "opacity:0.3;"),
        opts_hover(css = "stroke:black;stroke-width:2px;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:0 0 10px rgba(0,0,0,0.5);"
        )
      )
    )
  })
  
  # Long term table output
  output$long_term_table <- DT::renderDataTable({
    req(long_term_data())
    
    data <- long_term_data()
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    # Format the data for display
    if (input$analysis_type == "total") {
      display_data <- data %>%
        mutate(
          Amount = paste("CHF", sprintf("%.2f", Total_Amount)),
          Transactions = Count
        ) %>%
        select(Period, Amount, Transactions)
    } else {
      category_col <- if (input$analysis_type == "type") "Type" else "Person"
      display_data <- data %>%
        mutate(
          Amount = paste("CHF", sprintf("%.2f", Total_Amount)),
          Transactions = Count
        ) %>%
        select(Period, !!sym(category_col), Amount, Transactions)
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
}

shinyApp(ui = ui, server = server)
