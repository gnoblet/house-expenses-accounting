library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(scales)

ui <- page_sidebar(
  title = "House Expense Calculator",
  sidebar = sidebar(
    width = 350,
    h4("Data Upload"),
    fileInput("expenses_file", "Upload Expenses CSV",
              accept = ".csv"),
    fileInput("absences_file", "Upload Absences CSV (optional)",
              accept = ".csv"),
    fileInput("exceptions_file", "Upload Exceptions CSV (optional - percentage participation)",
              accept = ".csv"),
    
    h4("Period Settings"),
    dateInput("start_date", "Start Date", value = Sys.Date() - 90),
    dateInput("end_date", "End Date", value = Sys.Date()),
    
    h4("People & Expense Types"),
    textAreaInput("people_list", "People Names (one per line)",
                  value = "Alice\nBob\nCharlie\nDiana\nEve\nFrank\nGrace\nHenry",
                  height = "120px"),
    textAreaInput("expense_types", "Expense Types (one per line)",
                  value = "Normal\nParty\nAlcohol\nSpecial",
                  height = "100px"),
    textAreaInput("shared_expense_types", "Shared Expense Types - Always Split Equally (one per line)",
                  value = "Utilities\nSubscriptions\nInsurance",
                  height = "80px"),
    
    actionButton("calculate", "Calculate Expenses", class = "btn-primary"),
    br(), br(),
    downloadButton("download_excel", "📊 Download Results as Excel", class = "btn-success")
  ),
  
  navset_card_tab(
    nav_panel("Final Settlement", 
              DT::dataTableOutput("final_table")),
    nav_panel("Expense Summary", 
              DT::dataTableOutput("summary_table")),
    nav_panel("Expense Details", 
              DT::dataTableOutput("expenses_table")),
    nav_panel("Filter by Type",
              fluidRow(
                column(4,
                  selectInput("filter_type", "Select Expense Type:",
                             choices = NULL,
                             selected = NULL)
                ),
                column(8,
                  h5("Quick Summary:"),
                  verbatimTextOutput("type_summary")
                )
              ),
              br(),
              DT::dataTableOutput("filtered_settlement_table"))
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store data
  expenses_data <- reactiveVal(NULL)
  absences_data <- reactiveVal(NULL)
  exceptions_data <- reactiveVal(NULL)
  
  # Load expenses file
  observeEvent(input$expenses_file, {
    req(input$expenses_file)
    
    tryCatch({
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
      
      showNotification("✅ Expenses file loaded successfully!", type = "message")
      showNotification(
        paste("📅 Date range updated to cover all expenses:", 
              format(min_date, "%d/%m/%Y"), "to", format(max_date, "%d/%m/%Y")),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("❌ Error loading expenses file:", e$message), type = "error")
    })
  })
  
  # Load absences file
  observeEvent(input$absences_file, {
    req(input$absences_file)
    
    tryCatch({
      df <- read_csv(input$absences_file$datapath)
      absences_data(df)
      showNotification("✅ Absences file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error loading absences file:", e$message), type = "error")
    })
  })
  
  # Load exceptions file
  observeEvent(input$exceptions_file, {
    req(input$exceptions_file)
    
    tryCatch({
      df <- read_csv(input$exceptions_file$datapath)
      exceptions_data(df)
      showNotification("✅ Exceptions file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error loading exceptions file:", e$message), type = "error")
    })
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
    shared_expense_types <- trimws(strsplit(input$shared_expense_types, "\n")[[1]])
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
      validation_errors <- c(validation_errors, 
        paste("👥 People in expenses but not in people list:", paste(people_not_in_list, collapse = ", ")))
    }
    
    if (length(people_not_in_expenses) > 0) {
      validation_warnings <- c(validation_warnings, 
        paste("🔍 People in list but not found in expenses:", paste(people_not_in_expenses, collapse = ", ")))
    }
    
    # Check if expense types in expenses match the expense types list
    unique_types_in_expenses <- unique(expenses$Type)
    types_not_in_list <- setdiff(unique_types_in_expenses, expense_types)
    types_not_in_expenses <- setdiff(expense_types, unique_types_in_expenses)
    
    if (length(types_not_in_list) > 0) {
      validation_errors <- c(validation_errors, 
        paste("🏷️ Expense types in expenses but not in types list:", paste(types_not_in_list, collapse = ", ")))
    }
    
    if (length(types_not_in_expenses) > 0) {
      validation_warnings <- c(validation_warnings, 
        paste("📝 Expense types in list but not found in expenses:", paste(types_not_in_expenses, collapse = ", ")))
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
      exceptions_people_not_in_list <- setdiff(unique(exceptions$Person), people)
      exceptions_types_not_in_list <- setdiff(unique(exceptions$Type), all_expense_types)
      
      if (length(exceptions_people_not_in_list) > 0) {
        validation_errors <- c(validation_errors, 
          paste("🚫 People in exceptions file but not in people list:", paste(exceptions_people_not_in_list, collapse = ", ")))
      }
      
      if (length(exceptions_types_not_in_list) > 0) {
        validation_errors <- c(validation_errors, 
          paste("🚫 Expense types in exceptions file but not in types list:", paste(exceptions_types_not_in_list, collapse = ", ")))
      }
      
      # Validate percentage values
      invalid_percentages <- exceptions$Percentage[exceptions$Percentage < 0 | exceptions$Percentage >= 1]
      if (length(invalid_percentages) > 0) {
        validation_errors <- c(validation_errors, 
          paste("📊 Invalid percentage values in exceptions file (must be 0.0-0.99):", paste(invalid_percentages, collapse = ", ")))
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
        validation_errors <- c(validation_errors, 
          paste("🚫 People in absences file but not in people list:", paste(absences_people_not_in_list, collapse = ", ")))
      }
      
      if (length(absences_people_missing) > 0) {
        validation_errors <- c(validation_errors, 
          paste("❌ People missing from absences file:", paste(absences_people_missing, collapse = ", ")))
      }
      
      # Check for negative absent days
      negative_days <- absences$Absent_Days[absences$Absent_Days < 0]
      if (length(negative_days) > 0) {
        validation_errors <- c(validation_errors, 
          paste("⚠️ Negative absent days found:", paste(negative_days, collapse = ", ")))
      }
      
      # Check for absent days exceeding period length
      excessive_days <- absences$Absent_Days[absences$Absent_Days > total_days]
      if (length(excessive_days) > 0) {
        validation_errors <- c(validation_errors, 
          paste("📅 Absent days exceeding period length (", total_days, " days):", paste(excessive_days, collapse = ", ")))
      }
    }
    
    # If there are validation errors, show them and return NULL
    if (length(validation_errors) > 0) {
      showNotification(
        shiny::HTML(paste("🚨 <strong>Validation Errors:</strong><br>", paste("• ", validation_errors, collapse = "<br>"))),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    # Show warnings if any (but continue with calculation)
    if (length(validation_warnings) > 0) {
      showNotification(
        shiny::HTML(paste("⚠️ <strong>Warnings:</strong><br>", paste("• ", validation_warnings, collapse = "<br>"))),
        type = "warning",
        duration = 8
      )
    }
    
    # Show info about shared expense types if any
    shared_types_in_expenses <- intersect(shared_expense_types, unique(expenses$Type))
    if (length(shared_types_in_expenses) > 0) {
      showNotification(
        shiny::HTML(paste("⚖️ <strong>Shared Expense Types (Equal Split):</strong><br>", 
                         paste("•", shared_types_in_expenses, collapse = "<br>"))),
        type = "message",
        duration = 4
      )
    }
    
    # Show info about exceptions if any
    if (nrow(exceptions) > 0) {
      exception_summary <- exceptions |>
        group_by(Type) |>
        summarise(Exception_Details = paste(
          paste(Person, "(", scales::percent(Percentage, accuracy = 1), ")", sep = ""), 
          collapse = ", "
        ), .groups = "drop")
      
      exception_msg <- paste(
        apply(exception_summary, 1, function(x) paste("•", x["Type"], ":", x["Exception_Details"])), 
        collapse = "<br>"
      )
      
      showNotification(
        shiny::HTML(paste("ℹ️ <strong>Expense Exceptions Applied:</strong><br>", exception_msg)),
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
    final_calculations <- expand_grid(Type = all_expense_types, Person = people) |>
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
          Is_Shared_Type ~ 1.0,  # Shared types always 100% for everyone
          Exception_Key %in% paste(exceptions$Person, exceptions$Type, sep = "-") ~ 
            exceptions$Percentage[match(Exception_Key, paste(exceptions$Person, exceptions$Type, sep = "-"))],
          TRUE ~ 1.0
        ),
        # For shared types: ignore presence ratio, for others: apply it
        Base_Ratio = ifelse(Is_Shared_Type, 1.0, Presence_Ratio),
        # Apply exception percentage to base ratio
        Adjusted_Presence_Ratio = Base_Ratio * Exception_Percentage,
        Total_Presence_Weight = sum(Adjusted_Presence_Ratio),
        Share_Owed = ifelse(Total_Presence_Weight > 0, 
                           (Total_Amount * Adjusted_Presence_Ratio) / Total_Presence_Weight, 
                           0),
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
      ) |>
      mutate(
        Status = ifelse(Final_Balance > 0, "To Receive", "To Pay"),
        Amount = abs(Final_Balance)
      )
    
    list(
      expenses = expenses,
      summary_by_type = summary_by_type,
      final_settlement = final_settlement,
      detailed_calculations = final_calculations
    )
  })
  
  # Display expenses table
  output$expenses_table <- DT::renderDataTable({
    req(calculations())
    
    if (is.null(calculations())) {
      return(DT::datatable(data.frame(Message = "⚠️ Please fix validation errors and recalculate")))
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
      return(DT::datatable(data.frame(Message = "⚠️ Please fix validation errors and recalculate")))
    }
    
    calculations()$summary_by_type |>
      DT::datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "Summary by Expense Type"
      ) |>
      DT::formatCurrency(c("Total_Amount", "Total_Paid", "Total_Owed"), currency = "CHF ")
  })
  
  # Display final settlement table
  output$final_table <- DT::renderDataTable({
    req(calculations())
    
    if (is.null(calculations())) {
      return(DT::datatable(data.frame(Message = "⚠️ Please fix validation errors and recalculate")))
    }
    
    calculations()$final_settlement |>
      arrange(desc(Final_Balance)) |>
      DT::datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        caption = "Final Settlement - Who Owes What"
      ) |>
      DT::formatCurrency(c("Total_Paid", "Total_Owed", "Final_Balance", "Amount"), currency = "CHF ") |>
      DT::formatStyle(
        "Final_Balance",
        backgroundColor = DT::styleInterval(0, c("#ffebee", "#e8f5e8")),
        fontWeight = "bold"
      )
  })
  
  # Download handler for Excel export
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("House_Expenses_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(calculations())
      
      if (is.null(calculations())) {
        showNotification("❌ No data to export. Please calculate expenses first.", type = "error")
        return()
      }
      
      # Create workbook
      wb <- createWorkbook()
      
      # Add Final Settlement sheet
      addWorksheet(wb, "Final Settlement")
      writeData(wb, "Final Settlement", calculations()$final_settlement)
      
      # Add Expense Summary sheet
      addWorksheet(wb, "Expense Summary")
      writeData(wb, "Expense Summary", calculations()$summary_by_type)
      
      # Add Expense Details sheet
      addWorksheet(wb, "Expense Details")
      writeData(wb, "Expense Details", calculations()$expenses)
      
      # Style the headers
      headerStyle <- createStyle(
        textDecoration = "bold",
        fgFill = "#4F81BD",
        fontColour = "white",
        border = "all"
      )
      
      # Apply header styling to all sheets
      addStyle(wb, "Final Settlement", headerStyle, rows = 1, cols = 1:ncol(calculations()$final_settlement), gridExpand = TRUE)
      addStyle(wb, "Expense Summary", headerStyle, rows = 1, cols = 1:ncol(calculations()$summary_by_type), gridExpand = TRUE)
      addStyle(wb, "Expense Details", headerStyle, rows = 1, cols = 1:ncol(calculations()$expenses), gridExpand = TRUE)
      
      # Auto-size columns
      setColWidths(wb, "Final Settlement", cols = 1:ncol(calculations()$final_settlement), widths = "auto")
      setColWidths(wb, "Expense Summary", cols = 1:ncol(calculations()$summary_by_type), widths = "auto")
      setColWidths(wb, "Expense Details", cols = 1:ncol(calculations()$expenses), widths = "auto")
      
      # Save workbook
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification("📊 Excel file downloaded successfully!", type = "message")
    }
  )
  
  # Update filter dropdown choices when calculations change
  observe({
    req(calculations())
    if (!is.null(calculations())) {
      expense_types <- unique(calculations()$detailed_calculations$Type)
      expense_types <- expense_types[!is.na(expense_types)]
      updateSelectInput(session, "filter_type", 
                       choices = expense_types,
                       selected = expense_types[1])
    }
  })
  
  # Display filtered settlement table
  output$filtered_settlement_table <- DT::renderDataTable({
    req(calculations(), input$filter_type)
    
    if (is.null(calculations()) || is.null(input$filter_type)) {
      return(DT::datatable(data.frame(Message = "⚠️ Please calculate expenses and select a type")))
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
      DT::formatCurrency(c("Total_Amount", "Total_Paid", "Share_Owed", "Balance"), currency = "CHF ") |>
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
      paste("Total", input$filter_type, "expenses: CHF", sprintf("%.2f", total_amount)),
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
}

shinyApp(ui = ui, server = server)

