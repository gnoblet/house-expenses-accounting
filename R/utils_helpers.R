#' Load Default Values from Assets Folder
#'
#' @param filename The filename to load from assets folder
#' @param fallback_default Default value to use if file doesn't exist
#'
#' @return Character string with file content or fallback default
#' @export
load_default_from_assets <- function(filename, fallback_default) {
  asset_path <- system.file("app", "www", "assets", filename, package = "houseexpenses")
  if (file.exists(asset_path)) {
    tryCatch({
      content <- readLines(asset_path, warn = FALSE)
      content <- content[content != ""] # Remove empty lines
      paste(content, collapse = "\n")
    }, error = function(e) {
      message("Warning: Could not read ", asset_path, " - using fallback defaults")
      fallback_default
    })
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
