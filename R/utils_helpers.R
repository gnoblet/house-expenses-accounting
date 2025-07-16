#' Load Default Values from Assets Folder
#'
#' @param filename The filename to load from assets folder
#' @param fallback_default Default value to use if file doesn't exist
#'
#' @return Character string with file content or fallback default
#' @export
load_default_from_assets <- function(filename, fallback_default) {
  asset_path <- system.file(
    "app",
    "www",
    "assets",
    filename,
    package = "houseexpenses"
  )
  if (file.exists(asset_path)) {
    tryCatch(
      {
        content <- readLines(asset_path, warn = FALSE)
        content <- content[content != ""] # Remove empty lines
        paste(content, collapse = "\n")
      },
      error = function(e) {
        message(
          "Warning: Could not read ",
          asset_path,
          " - using fallback defaults"
        )
        fallback_default
      }
    )
  } else {
    fallback_default
  }
}

#' Get Default People List
#'
#' @return Character string with default people list
#' @export
get_default_people_list <- function() {
  load_default_from_assets(
    "people_list_default.txt",
    "Alice\nBob\nCharlie\nDiana\nEve\nFrank\nGrace\nHenry"
  )
}

#' Get Default Expense Types
#''scale_fill_viridis_d' is not an exported object from 'namespace:viridis'

#' @return Character string with default expense types
#' @export
get_default_expense_types <- function() {
  load_default_from_assets(
    "expense_types_default.txt",
    "Normal\nParty\nAlcohol\nSpecial"
  )
}

#' Get Default Shared Expense Types
#'
#' @return Character string with default shared expense types
#' @export
get_default_shared_expense_types <- function() {
  load_default_from_assets(
    "shared_expense_types_default.txt",
    "Utilities\nSubscriptions\nInsurance"
  )
}

#' Validate Input Data Columns and Types
#'
#' @param df The data frame to validate
#' @param required_cols Named list of required columns and their expected types (e.g., list(name = "character", amount = "numeric"))
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_input_data <- function(df, required_cols) {
  missing_cols <- setdiff(names(required_cols), names(df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  for (col in names(required_cols)) {
    expected_type <- required_cols[[col]]
    actual_type <- class(df[[col]])[1]
    # Allow integer/numeric interchangeably
    if (expected_type == "numeric" && actual_type == "integer") {
      next
    }
    if (expected_type == "integer" && actual_type == "numeric") {
      next
    }
    if (actual_type != expected_type) {
      stop(paste0(
        "Column '",
        col,
        "' should be of type '",
        expected_type,
        "' but is '",
        actual_type,
        "'"
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

  # date is of format "MM/DD/YYYY"
  # check that first 2 digits between 01 and 12
  if (
    !all(grepl(
      "^(0[1-9]|1[1,2])-(0[1-9]|[12][0-9]|3[01])-(19|20)\d{2}$",
      df$Date
    ))
  ) {
    rlang::abort("Date column must be in MM-DD-YYYY format")
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
}
