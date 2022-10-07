#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dataframes <- mod_data_import_server("data_import_1")

  mod_summary_stat_server("summary_stat_1", dataframes())
}
