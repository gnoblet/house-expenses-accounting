
#' Validate Input Data Columns and Types
#'
#' @param df The data frame to validate
#' @param required_cols Named list of required columns and their expected types (e.g., list(name = "character", amount = "numeric"))
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_input_data <- function(df, required_cols) {
  if (nrow(df) == 0) {
    stop("Sheet is empty — it must have at least one data row.")
  }

  expected_names <- names(required_cols)
  actual_names <- names(df)
  missing_cols <- setdiff(expected_names, actual_names)

  if (length(missing_cols) > 0) {
    msgs <- vapply(missing_cols, function(col) {
      lower_match <- actual_names[tolower(actual_names) == tolower(col)]
      if (length(lower_match) > 0) {
        paste0(
          "  • '", col, "' not found — did you mean '", lower_match[1],
          "'? Column names are case-sensitive."
        )
      } else {
        paste0("  • '", col, "' not found.")
      }
    }, character(1))

    stop(paste0(
      "Missing required column(s):\n",
      paste(msgs, collapse = "\n"),
      "\n\nExpected columns: ", paste(expected_names, collapse = ", "),
      "\nFound columns:    ", paste(actual_names, collapse = ", ")
    ))
  }

  for (col in expected_names) {
    expected_type <- required_cols[[col]]
    actual_type <- class(df[[col]])[1]
    if (expected_type == "numeric" && actual_type %in% c("integer", "numeric")) next
    if (expected_type == "integer" && actual_type %in% c("integer", "numeric")) next
    if (actual_type != expected_type) {
      stop(paste0(
        "Column '", col, "' should contain ", expected_type, " values",
        " but the values found look like ", actual_type, ".\n",
        "  First value: '", df[[col]][1], "'"
      ))
    }
  }

  for (col in expected_names) {
    na_rows <- which(is.na(df[[col]]) | (is.character(df[[col]]) & trimws(df[[col]]) == ""))
    if (length(na_rows) > 0) {
      shown <- head(na_rows, 5)
      stop(paste0(
        "Column '", col, "' has ", length(na_rows), " blank/missing value(s)",
        " in row(s): ", paste(shown, collapse = ", "),
        if (length(na_rows) > 5) paste0(" (and ", length(na_rows) - 5, " more)") else "",
        ".\nAll values in this column are required."
      ))
    }
  }

  TRUE
}

#' Check Expenses Input File Columns and Types
#'
#' @param df The expenses data frame
#' @return TRUE if valid, otherwise throws an error
#' @export
check_expenses_input <- function(df) {
  required <- list(
    Type = "character",
    Reason = "character",
    Date = "character",
    Amount = "numeric",
    Person = "character"
  )
  validate_input_data(df, required)

  date_pattern <- "^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])-(19|20)\\d{2}$"
  bad_rows <- which(!grepl(date_pattern, df$Date))
  if (length(bad_rows) > 0) {
    shown <- head(bad_rows, 5)
    examples <- paste0("'", df$Date[shown], "'", collapse = ", ")
    rlang::abort(paste0(
      "Date column must be in MM-DD-YYYY format (e.g. 03-25-2025).\n",
      "Found ", length(bad_rows), " invalid date(s) in row(s): ",
      paste(shown, collapse = ", "),
      if (length(bad_rows) > 5) paste0(" (and ", length(bad_rows) - 5, " more)") else "",
      "\nValues: ", examples
    ))
  }
}

#' Check Exceptions Input File Columns and Types
#'
#' @param df The exceptions data frame
#' @return TRUE if valid, otherwise throws an error
#' @export
check_exceptions_input <- function(df) {
  required <- list(
    Person = "character",
    Type = "character",
    Percentage = "numeric"
  )
  validate_input_data(df, required)

  bad_rows <- which(df$Percentage < 0 | df$Percentage >= 1)
  if (length(bad_rows) > 0) {
    stop(paste0(
      "Column 'Percentage' must be between 0.0 and 0.99 (exclusive of 1).\n",
      "Found invalid value(s) in row(s): ",
      paste(head(bad_rows, 5), collapse = ", "),
      "\nValues: ", paste(df$Percentage[head(bad_rows, 5)], collapse = ", ")
    ))
  }
}

#' Check Absences Input File Columns and Types
#'
#' @param df The absences data frame
#' @return TRUE if valid, otherwise throws an error
#' @export
check_absences_input <- function(df) {
  required <- list(
    Person = "character",
    Absent_Days = "numeric"
  )
  validate_input_data(df, required)

  neg_rows <- which(df$Absent_Days < 0)
  if (length(neg_rows) > 0) {
    stop(paste0(
      "Column 'Absent_Days' must be 0 or greater. Found negative value(s) in row(s): ",
      paste(head(neg_rows, 5), collapse = ", "),
      "\nValues: ", paste(df$Absent_Days[head(neg_rows, 5)], collapse = ", ")
    ))
  }
}
