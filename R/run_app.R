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
#' @importFrom plotly ggplotly
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import tibble
#' @import ggplot2
#' @import ggrepel
#' @import janitor
#' @import purrr
#' @importFrom ggiraphExtra ggRadar
#' @import stringr
#' @import shinyWidgets
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel))
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
