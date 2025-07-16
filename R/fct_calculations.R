#' Process Expenses Data
#'
#' @param expenses_data Raw expenses data frame
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @param people_list List of people
#' @param expense_types List of expense types
#' @param shared_expense_types List of shared expense types
#' @param absences_data Absences data frame (optional)
#' @param exceptions_data Exceptions data frame (optional)
#'
#' @return List with processed calculations
#' @export
process_expenses <- function(
  expenses_data,
  start_date,
  end_date,
  people_list,
  expense_types,
  shared_expense_types,
  absences_data = NULL,
  exceptions_data = NULL
) {
  # Parse input lists
  people <- trimws(strsplit(people_list, "\n")[[1]])
  people <- people[people != ""]

  expense_types <- trimws(strsplit(expense_types, "\n")[[1]])
  expense_types <- expense_types[expense_types != ""]

  shared_expense_types <- trimws(strsplit(shared_expense_types, "\n")[[1]])
  shared_expense_types <- shared_expense_types[shared_expense_types != ""]

  # Combine all expense types for validation
  all_expense_types <- unique(c(expense_types, shared_expense_types))

  # Filter expenses by date range
  if (any(is.na(names(expenses_data)) | names(expenses_data) == "")) {
    rlang::abort("Expenses data has columns with missing or empty names.")
  }
  expenses <- expenses_data |>
    dplyr::filter(Date >= start_date & Date <= end_date)

  # Check if we have expenses in the date range
  if (nrow(expenses) == 0) {
    return(list(error = "No expenses found in the selected date range"))
  }

  # Calculate total days in period
  total_days <- as.numeric(end_date - start_date) + 1

  # Validation
  validation_result <- validate_data(
    expenses,
    people,
    all_expense_types,
    absences_data,
    exceptions_data,
    total_days
  )

  if (length(validation_result$errors) > 0) {
    return(list(
      errors = validation_result$errors,
      warnings = validation_result$warnings
    ))
  }

  # Process absences and exceptions
  absences <- process_absences(absences_data, people)
  exceptions <- process_exceptions(exceptions_data)

  # Calculate presence ratios
  presence_ratios <- absences |>
    dplyr::mutate(
      Present_Days = total_days - Absent_Days,
      Presence_Ratio = Present_Days / total_days
    )

  # Calculate expenses by type and person
  expense_summary <- expenses |>
    dplyr::group_by(Type, Person) |>
    dplyr::summarise(
      Total_Paid = sum(Amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(Type, Person = people, fill = list(Total_Paid = 0))

  # Calculate total by expense type
  type_totals <- expenses |>
    dplyr::group_by(Type) |>
    dplyr::summarise(Total_Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

  # Calculate final calculations
  final_calculations <- calculate_final_settlement(
    all_expense_types,
    people,
    type_totals,
    presence_ratios,
    expense_summary,
    shared_expense_types,
    exceptions
  )

  # Create summary by type
  summary_by_type <- final_calculations |>
    dplyr::group_by(Type) |>
    dplyr::summarise(
      Total_Amount = dplyr::first(Total_Amount),
      Total_Paid = sum(Total_Paid),
      Total_Owed = sum(Share_Owed),
      .groups = "drop"
    )

  # Create final settlement table
  final_settlement <- final_calculations |>
    dplyr::group_by(Person) |>
    dplyr::summarise(
      Total_Paid = sum(Total_Paid),
      Total_Owed = sum(Share_Owed),
      Final_Balance = sum(Balance),
      .groups = "drop"
    )

  list(
    expenses = expenses,
    summary_by_type = summary_by_type,
    final_settlement = final_settlement,
    detailed_calculations = final_calculations,
    warnings = validation_result$warnings,
    absences_summary = if (!is.null(absences_data)) {
      absences_data
    } else {
      data.frame()
    },
    exceptions_summary = if (!is.null(exceptions_data)) {
      exceptions_data
    } else {
      data.frame()
    }
  )
}

#' Validate Input Data
#'
#' @param expenses Expenses data frame
#' @param people List of people
#' @param all_expense_types List of all expense types
#' @param absences_data Absences data frame
#' @param exceptions_data Exceptions data frame
#' @param total_days Total days in period
#'
#' @return List with errors and warnings
validate_data <- function(
  expenses,
  people,
  all_expense_types,
  absences_data,
  exceptions_data,
  total_days
) {
  validation_errors <- c()
  validation_warnings <- c()

  # Check people
  unique_people_in_expenses <- unique(expenses$Person)
  people_not_in_list <- setdiff(unique_people_in_expenses, people)
  people_not_in_expenses <- setdiff(people, unique_people_in_expenses)

  if (length(people_not_in_list) > 0) {
    validation_errors <- c(
      validation_errors,
      paste(
        "People in expenses but not in people list:",
        paste(people_not_in_list, collapse = ", ")
      )
    )
  }

  if (length(people_not_in_expenses) > 0) {
    validation_warnings <- c(
      validation_warnings,
      paste(
        "People in list but not found in expenses:",
        paste(people_not_in_expenses, collapse = ", ")
      )
    )
  }

  # Check expense types
  unique_types_in_expenses <- unique(expenses$Type)
  types_not_in_list <- setdiff(unique_types_in_expenses, all_expense_types)

  if (length(types_not_in_list) > 0) {
    validation_errors <- c(
      validation_errors,
      paste(
        "Expense types in expenses but not in types list:",
        paste(types_not_in_list, collapse = ", ")
      )
    )
  }

  # Validate absences if provided
  if (!is.null(absences_data) && nrow(absences_data) > 0) {
    people_in_absences <- unique(absences_data$Person)
    absences_people_not_in_list <- setdiff(people_in_absences, people)

    if (length(absences_people_not_in_list) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "People in absences file but not in people list:",
          paste(absences_people_not_in_list, collapse = ", ")
        )
      )
    }

    # Check for invalid absent days
    if (any(absences_data$Absent_Days < 0)) {
      validation_errors <- c(validation_errors, "Negative absent days found")
    }

    if (any(absences_data$Absent_Days > total_days)) {
      validation_errors <- c(
        validation_errors,
        paste("Absent days exceeding period length (", total_days, " days)")
      )
    }
  }

  # Validate exceptions if provided
  if (!is.null(exceptions_data) && nrow(exceptions_data) > 0) {
    exceptions_people_not_in_list <- setdiff(
      unique(exceptions_data$Person),
      people
    )
    exceptions_types_not_in_list <- setdiff(
      unique(exceptions_data$Type),
      all_expense_types
    )

    if (length(exceptions_people_not_in_list) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "People in exceptions file but not in people list:",
          paste(exceptions_people_not_in_list, collapse = ", ")
        )
      )
    }

    if (length(exceptions_types_not_in_list) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "Expense types in exceptions file but not in types list:",
          paste(exceptions_types_not_in_list, collapse = ", ")
        )
      )
    }

    # Validate percentage values
    invalid_percentages <- exceptions_data$Percentage[
      exceptions_data$Percentage < 0 | exceptions_data$Percentage >= 1
    ]
    if (length(invalid_percentages) > 0) {
      validation_errors <- c(
        validation_errors,
        paste(
          "Invalid percentage values (must be 0.0-0.99):",
          paste(invalid_percentages, collapse = ", ")
        )
      )
    }
  }

  list(errors = validation_errors, warnings = validation_warnings)
}

#' Process Absences Data
#'
#' @param absences_data Raw absences data
#' @param people List of people
#'
#' @return Processed absences data frame
process_absences <- function(absences_data, people) {
  if (is.null(absences_data) || nrow(absences_data) == 0) {
    return(data.frame(
      Person = people,
      Absent_Days = 0,
      stringsAsFactors = FALSE
    ))
  }
  return(absences_data)
}

#' Process Exceptions Data
#'
#' @param exceptions_data Raw exceptions data
#'
#' @return Processed exceptions data frame
process_exceptions <- function(exceptions_data) {
  if (is.null(exceptions_data) || nrow(exceptions_data) == 0) {
    return(data.frame(
      Person = character(0),
      Type = character(0),
      Percentage = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  return(exceptions_data)
}

#' Calculate Final Settlement
#'
#' @param all_expense_types All expense types
#' @param people List of people
#' @param type_totals Type totals data frame
#' @param presence_ratios Presence ratios data frame
#' @param expense_summary Expense summary data frame
#' @param shared_expense_types Shared expense types
#' @param exceptions Exceptions data frame
#'
#' @return Final calculations data frame
calculate_final_settlement <- function(
  all_expense_types,
  people,
  type_totals,
  presence_ratios,
  expense_summary,
  shared_expense_types,
  exceptions
) {
  final_calculations <- tidyr::expand_grid(
    Type = all_expense_types,
    Person = people
  ) |>
    dplyr::left_join(type_totals, by = "Type") |>
    dplyr::left_join(presence_ratios, by = c("Person" = "Person")) |>
    dplyr::left_join(expense_summary, by = c("Type", "Person" = "Person")) |>
    dplyr::mutate(
      Total_Amount = ifelse(is.na(Total_Amount), 0, Total_Amount),
      Total_Paid = ifelse(is.na(Total_Paid), 0, Total_Paid),
      Presence_Ratio = ifelse(is.na(Presence_Ratio), 1, Presence_Ratio)
    )

  # Calculate weighted shares
  final_calculations <- final_calculations |>
    dplyr::group_by(Type) |>
    dplyr::mutate(
      Is_Shared_Type = Type %in% shared_expense_types,
      Exception_Key = paste(Person, Type, sep = "-"),
      Exception_Percentage = dplyr::case_when(
        Is_Shared_Type ~ 1.0,
        Exception_Key %in%
          paste(exceptions$Person, exceptions$Type, sep = "-") ~
          exceptions$Percentage[match(
            Exception_Key,
            paste(exceptions$Person, exceptions$Type, sep = "-")
          )],
        TRUE ~ 1.0
      ),
      Base_Ratio = ifelse(Is_Shared_Type, 1.0, Presence_Ratio),
      Adjusted_Presence_Ratio = Base_Ratio * Exception_Percentage,
      Total_Presence_Weight = sum(Adjusted_Presence_Ratio),
      Share_Owed = ifelse(
        Total_Presence_Weight > 0,
        (Total_Amount * Adjusted_Presence_Ratio) / Total_Presence_Weight,
        0
      ),
      Balance = Total_Paid - Share_Owed
    ) |>
    dplyr::ungroup()

  return(final_calculations)
}

# Global variables for NSE
utils::globalVariables(c(
  "Date",
  "Amount",
  "Type",
  "Person",
  "Absent_Days",
  "Present_Days",
  "Presence_Ratio",
  "Total_Paid",
  "Total_Amount",
  "Share_Owed",
  "Balance",
  "Is_Shared_Type",
  "Exception_Key",
  "Exception_Percentage",
  "Base_Ratio",
  "Adjusted_Presence_Ratio",
  "Total_Presence_Weight"
))
