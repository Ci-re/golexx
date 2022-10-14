#' geo_vix UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geo_vix_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(

        box(status = "info", title = "Customize and Visualize",
            width = 12,
            maximizable = TRUE,
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
              uiOutput(ns("select_trait")),
              uiOutput(ns("select_accession")),
              uiOutput(ns("select_checks")),
              uiOutput(ns("weather"))
            )
        )
      ), fluidRow(
        box(status = "info", title = "Actual Performance accross environments, 'Toggle Check Difference'",
            uiOutput(ns("check_difference_toggler")),
            width = 12, withSpinner(plotlyOutput(ns("accession_map"), width = "100%", height = "800px"))),
        # box(width = 3, ),
        # box(width = 12, plotlyOutput(ns("check_map"))),
        box(status = "info", title = "Leaflet Animated Visualization, 'Click on Markers for more info'",
            width = 12,
            maximizable = TRUE,
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
              radioButtons(inputId = ns("weather2"), label = "Real time view:",
                           c("Rain" = "rainClassic",
                             "Temperature" = "temperature",
                             "Precipitation" = "precipitationClassic",
                             "Clouds" = "cloudsClassic",
                             "Pressure" = "pressure",
                             "Wind" = "wind"),
                           selected = "precipitationClassic")
        ),
        withSpinner(leafletOutput(outputId = ns("live_map"), width = "100%", "900px")))
      )
    )
  )
}

#' geo_vix Server Functions
#'
#' @noRd
mod_geo_vix_server <- function(id, dataset, checks){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$select_trait <- renderUI({
      pickerInput(
        inputId = ns("select_trait"),
        label = "Select trait",
        choices = c("Dry matter" = "dm", "Dry Yield" = "dyld",
                    "Fresh yield" = "fyld", "Sprout" = "sprout",
                    "Mosaic" = "mcmds", "Plant height" = "pltht"),
        selected = "dm",
        multiple = FALSE
      )
    })

    output$select_checks <- renderUI({
      pickerInput(
        inputId = ns("select_check"),
        label = "Checks..",
        choices = unique(dataset$env_data$accession),
        selected = get_checks(unique(dataset$env_data$accession)),
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })
    output$select_accession <- renderUI({
      pickerInput(
        inputId = ns("select_accession"),
        label = "Genotype..",
        choices = unique(dataset$env_data$accession),
        selected = dataset$env_data$accession[1],
        multiple = TRUE,
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })
    output$weather <- renderUI({
      pickerInput(
        inputId = ns("weather"),
        label = "Weather..",
        choices = c("Rainfall", "Temperature"),
        selected = "Rainfall",
        multiple = FALSE
      )
    })

    output$check_difference_toggler <- renderUI({
      prettyCheckbox(
        inputId = ns("check_difference_toggler"),
        label = "Toggle check-difference",
        status = "info", outline = TRUE,value = FALSE,
      )
    })

    output$accession_map <- renderPlotly({
      req(input$select_accession)
      req(input$select_check)
      req(input$weather)
      print(dataset$env_data)

      genotype <- input$select_accession
      weather_data <- input$weather
      checks <- input$select_check
      trait <- input$select_trait
      switchs <- input$check_difference_toggler
      rendered_map <- render_maps(dataframe = dataset$env_data, checks = checks, trait = trait,
                                  accession = genotype, weather = weather_data, switch = switchs)
      ggplotly(rendered_map)
    })

    output$live_map <- renderLeaflet({
      req(input$select_accession)
      genotype <- input$select_accession
      weather_data <- input$weather2
      checks <- input$select_check
      trait <- input$select_trait
      switchs <- input$check_difference_toggler
      render_leaflet_map <- render_leaflet_maps(dataframe = dataset$env_data, checks = checks, trait = trait,
                                                accession = genotype, weather = weather_data, switch = switchs)
      return(render_leaflet_map)
    })
  })
}

## To be copied in the UI
# mod_geo_vix_ui("geo_vix_1")

## To be copied in the server
# mod_geo_vix_server("geo_vix_1")
