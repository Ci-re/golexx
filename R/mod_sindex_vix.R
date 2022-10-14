#' sindex_vix UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sindex_vix_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(status = "info", title = "Raw Table Distribution", maximizable = TRUE, elevation = 3,
            width = 6, withSpinner(DT::dataTableOutput(ns("raw_distribution")))),
        box(status = "info", title = "Check Difference Distribution", maximizable = TRUE, elevation = 3,
            width = 6, withSpinner(DT::dataTableOutput(ns("check_mean_distribution")))),
        box(status = "info", title = "Traits Correlation with S-INDEX",
            width = 12, withSpinner(plotlyOutput(ns("correlation"), height = "800px", width = "100%"))),
        # box(width = 3),
        box(status = "info", title = "Heatmap: Genotypes against Traits ",
            width = 12,
            prettyCheckbox(
              inputId = ns("corr_heat"),
              label = "Switch plot",
              status = "success",
              outline = TRUE,
              value = TRUE
            ),
            withSpinner(plotOutput(ns("heatmaps"), width = "100%", height = "1200px"))),
        box(
          shinyWidgets::dropdown(
            animate = shinyWidgets::animateOptions(
              enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
              exit = shinyWidgets::animations$fading_exits$fadeOutLeftBig
            ),
            style = "pill",
            icon = icon("gear"),
            verify_fa = FALSE,
            status = "success",
            width = "300px",
            uiOutput(ns("top_frac")),
          ),
          title = "Check Difference: Genotypes traits against Checks",
          width = 12, maximizable = TRUE, elevation = 3,
          withSpinner(plotOutput(ns("check_diff_plot"), width = "100%", height = "2000px"))),
      )
    )
  )
}

#' sindex_vix Server Functions
#'
#' @noRd
mod_sindex_vix_server <- function(id, dataset, checks){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$raw_distribution <- DT::renderDataTable({
      dataframe <- dataset$sin_data %>% janitor::clean_names() %>% select(accession | where(is.numeric))
      DT::datatable(
        dataframe,
        filter = 'top',
        extensions = 'Buttons',
        options = list(
          paging= TRUE,
          searching = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          fixedColumns = TRUE,
          buttons = c('copy', 'excel','csv', 'pdf', 'print')
        ),
        class = 'display'
      )
    })

    output$check_mean_distribution <- DT::renderDataTable({
      req(checks)
      dataframe <- dataset$sin_data
      # print(checks)
      dataframe <- calculate_sindex_checkmean(dataframe, checks = checks)
      # print(dataframe)
      DT::datatable(
        dataframe,
        filter = 'top',
        extensions = 'Buttons',
        options = list(
          paging= TRUE,
          searching = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          fixedColumns = TRUE,
          buttons = c('copy', 'excel','csv', 'pdf', 'print')
        ),
        class = 'display'
      )
    })

    output$correlation <- renderPlotly({
      dat <- dataset$sin_data %>% select(accession | where(is.numeric))
      dat <- sindex_corrplot(dataframe = dat)
      ggplotly(dat)
    })


    output$heatmap_hint <- renderUI({

    })

    output$heatmaps <- renderPlot({
      # req(input$corr_heat)
      switch <- input$corr_heat
      # print(switch)
      sindex_dataframe <- dataset$sin_data %>% dplyr::select(accession | where(is.numeric))
      if(switch == TRUE){
        dat <- sup_heat_corr(sindex_dataframe, checks)
        return(dat)
      }else if(switch == FALSE) {
        dat <- sindex_heatmap(dataframe = sindex_dataframe)
        return(dat)
      }
    })

    output$top_frac <- renderUI({
      max_obs <- length(dataset$sin_data$accession)
      mid_obs <- median(1:max_obs)
      sliderInput(
        inputId = ns("top_frac"),
        label = "Select topfrac..",
        min = 1,
        step = 1,
        max = max_obs,
        value = c(1,20)
      )
    })

    output$check_diff_plot <- renderPlot({
      req(input$top_frac)
      # sorting <- input$sort_sindex\
      req(checks)
      sindex_dataframe <- dataset$sin_data
      dat <- sindex_dataframe %>% dplyr::select(accession | where(is.numeric))
      dataframe <- calculate_sindex_checkmean(dataframe = dat,checks = checks)
      dataframe <- dataframe %>% arrange(desc(sindex))
      barplot <- barplot_checkdiff(import_data = dataframe[min(input$top_frac):max(input$top_frac),])
      return(barplot)
    })

  })
}

## To be copied in the UI
# mod_sindex_vix_ui("sindex_vix_1")

## To be copied in the server
# mod_sindex_vix_server("sindex_vix_1")
