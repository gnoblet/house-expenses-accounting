#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "House Expense Calculator",
      id = "main_nav",
      
      bslib::nav_panel(
        "Setup & Calculate",
        value = "setup_page",
        mod_setup_ui("setup")
      ),
      
      bslib::nav_panel(
        "Results",
        value = "results",
        mod_results_ui("results")
      ),
      
      bslib::nav_panel(
        "Long Term Analysis",
        value = "long_term_analysis",
        mod_long_term_ui("long_term")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "houseexpenses"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
