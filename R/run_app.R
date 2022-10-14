#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT datatable
#' @importFrom magrittr %>%
#' @import dplyr
#' @import forcats
#' @import ggcorrplot
#' @import tidyr
#' @import readxl
#' @import leaflet
#' @import leaflet.extras
#' @importFrom leaflet.extras2 addOpenweatherTiles
#' @import viridis
#' @import tibble
#' @import superheat
#' @import ggplot2
#' @import ggrepel
#' @import janitor
#' @import purrr
#' @importFrom ggiraphExtra ggRadar
#' @import stringr
#' @import shinyWidgets
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel))
#' @rawNamespace import(plotly, except = c(last_plot))
#' @rawNamespace import(leaflet.extras2, except = c(addSpinner, menuItem))
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
