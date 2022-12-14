#' summary_stat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# “simple”, “bordered”, “minimal”, “stretch”, “jelly”, “gradient”, “fill”, “material-circle”, “material-flat”, “pill”, “float”, “unite”

mod_summary_stat_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
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
            uiOutput(ns("select_traits")),
            uiOutput(ns("accession_range")),
            # uiOutput(ns("checks_select")),
            radioButtons(inputId = ns("plot_option"), label = "Select plot",
                         choices = c("Radar", "Line"), selected = "Line", inline = TRUE)
          ),
        width = 12,
        status = "info",
        maximizable = TRUE,
        elevation = 3,
        title = "Summary Statistics",
        uiOutput(outputId = ns("get_genotypes_info1")),
        uiOutput(outputId = ns("get_genotype_plot"))
        )
      ),
      fluidRow(
        box(
          width = 12,
          status = "info",
          maximizable = TRUE,
          elevation = 3,
          title = "Plot distribution of traits",
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
            uiOutput(ns("select_x")),
            uiOutput(ns("select_y")),
            uiOutput(ns("color_xy")),
            radioButtons(inputId = ns("linmod"), label = "Show lm",
                         choices = c("Yes", "No"), selected = "No", inline = TRUE)
          ),
          plotlyOutput(ns("distribution_plot"))
        ),
        box(
          width = 12,
          status = "info",
          maximizable = TRUE,
          elevation = 3,
          title = "Plot Checks Distribution with Traits",
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
          ),
          plotlyOutput(ns("checks_distribution"))
        )
      )
    )
  )
}

#' summary_stat Server Functions
#'
#' @noRd
mod_summary_stat_server <- function(id, dataset){
  # try(eval.parent(substitute(dataset)), silent = TRUE)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$select_traits <- renderUI({
      # choices = c("Dry yield" = "dyld", "Fresh Yield" = "fyld", "Dry Matter" = "dm",
      #             "Plant Height" = "pltht", "Sprout" = "sprout", "Mosaic" = "mcmds",
      #             "Harvest Index" = "hi"),
      env_data <- dataset$env_data

      traits <- unique(env_data$trait)
      traits <- lapply(traits, tolower)
      pickerInput(
        inputId = ns("select_traits"),
        label = "Select traits",
        choices = traits,
        multiple = TRUE,
        selected = traits[1:3],
        options = pickerOptions(
          title = "Top four traits",
          header = "Top four traits",
          style = "btn-primary",
          maxOptions = 3,
          maxOptionsText = "Please select at most 3 traits"
        )
      )
    })

    output$select_trait <- renderUI({
      env <- dataset$env_data %>% select(-accession)
      pickerInput(
        inputId = ns("select_trait"),
        label = "Select color var",
        choices = c(unique(env$trait)),
        multiple = FALSE,
        selected = "FYLD",
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 3
        )
      )
    })

    output$accession_range <- renderUI({
      sin_data <- dataset$sin_data
      maxObs <- length(sin_data$accession)
      minObs <-  1
      sliderInput(
        inputId = ns("accession_range"),
        label = "Range of Observation",
        min = 1,
        max = maxObs,
        value = c(1, 4),
        step = 1
      )
    })

    output$get_genotypes_info1 <- renderUI({
      sindex_dataset <- dataset$sin_data
      env_dataset <- dataset$env_data
      original_list <- unique(dataset$env_data$trait)
      selected_traits <- fix_listOfTop_Traits(original_list = original_list, top_traits = input$select_traits)

      # primary, secondary, info, success, warning, danger, gray-dark, gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive
      full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry matter",
                         "SPROUT" = "Sprout", "PLTHT" = "Plant Height", "RTNO" = "Root Number",
                         "DYLD" = "Dry yield", "FYLD" = "Fresh yield")
      box_color <- c("teal", "gray-dark", "info")
      icons <- c("chart-column", "chart-pie", "chart-line")
      fluidRow(
        lapply(1:length(selected_traits), function(k){
          datageh <- env_dataset %>% filter(trait == toupper(selected_traits[k]))
          if(toupper(selected_traits[k]) == "MCMDS"){
            datageh <- datageh %>% arrange(combined)
          }else {
            datageh <- datageh %>% arrange(desc(combined))
          }
          sin_d <- sindex_dataset %>% filter(accession == datageh$accession[1])
          # print(sin_d)
          keyvals <- list("accession" = datageh$accession[1], "value" = datageh$combined[1], "sindex" = sin_d$sindex)
          column(
            4,
            bs4Dash::infoBox(
              elevation = 2,
              iconElevation = 3,
              title = h5(keyvals["accession"], class = "clone-name"),
              value = paste("Value: ", keyvals["value"],"   Index: ",keyvals["sindex"]),
              width = NULL,
              subtitle = full_names[toupper(selected_traits[k])],
              icon = icon(icons[k], verify_fa = FALSE),
              color = box_color[k],
              fill = TRUE
            )
          )
        })
       )
    })

    output$get_genotype_plot <- renderUI({
      if(!is.null(input$accession_range)){
        accession_range <- input$accession_range
      } else {
        accession_range <- c(1,4)
      }
      env_dataset <- dataset$env_data
      original_list <- unique(dataset$env_data$trait)
      selected_traits <- fix_listOfTop_Traits(original_list = original_list, top_traits = input$select_traits)
      checks <- input$checks_select
      plot_choice <- input$plot_option
      fluidRow(
        lapply(1:length(selected_traits), function(k){
          datageh <- env_dataset %>% filter(trait == toupper(selected_traits[k]))
          if(toupper(selected_traits[k]) == "MCMDS"){
            datageh <- datageh %>% arrange(combined)
          }else {
            datageh <- datageh %>% arrange(desc(combined))
          }
          column(
            4,
            plotOutput(outputId = k, height = "0px"),
            output[[k]] <- renderPlot({
              if(plot_choice == "Radar"){
                plotRadar(
                  trait_to_plot = selected_traits[k],
                  imported_data = env_dataset,
                  checks = checks,
                  accession_range = accession_range
                )
              } else if(plot_choice == "Line"){
                linePlot(
                  trait_to_plot = selected_traits[k],
                  imported_data = env_dataset,
                  checks = checks,
                  accession_range = accession_range
                )
              } else {
                return(NULL)
              }
            })
          )
        })
      )
    })

    output$select_x <- renderUI({
      sin_data <- dataset$sin_data %>% select(-accession)
      pickerInput(
        inputId = ns("select_x"),
        label = "Select x",
        choices = c(colnames(sin_data)),
        multiple = FALSE,
        selected = "dyld",
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 3
        )
      )
    })

    output$select_y <- renderUI({
      sin_data <- dataset$sin_data %>% select(-accession)
      pickerInput(
        inputId = ns("select_y"),
        label = "Select y",
        choices = c(colnames(sin_data)),
        multiple = FALSE,
        selected = "fyld",
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 3
        )
      )
    })

    output$color_xy <- renderUI({
      sin_data <- dataset$sin_data
      pickerInput(
        inputId = ns("color_xy"),
        label = "Select color var",
        choices = c(colnames(sin_data)),
        multiple = FALSE,
        selected = "sindex",
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 3
        )
      )
    })

    output$distribution_plot <- renderPlotly({
      x_var <- input$select_x
      y_var <- input$select_y
      col_var <- input$color_xy
      lm <- input$linmod
      if(is.null(x_var) || is.null(y_var) || is.null(col_var)){
        x_var = "fyld"; y_var = "dyld"; col_var = "sindex"; lm = "Yes"
      } else {
        x_var = x_var; y_var = y_var; col_var = col_var; lm = lm
      }
      sin_data <- dataset$sin_data
      checks <- get_checks(accession_list = sin_data$accession)
      sin_data <- sin_data %>% mutate(category = if_else(accession %in% checks, "check", "genotype"))
      distribution_plot(x = x_var, y = y_var, data = sin_data, color = col_var, lm = lm, text = "accession")
    })

    output$checks_distribution <- renderPlotly({
      trait <- input$select_trait
      if(is.null(trait)){
        trait <- "FYLD"
      } else {
        trait <- trait
      }
      checks <- dataset$checks
      dataframe <- dataset$env_data %>% filter(accession  %in% checks)
      plot_checks(dataframe = dataframe, traits = trait)
    })
  })
}

## To be copied in the UI
# mod_summary_stat_ui("summary_stat_1")

## To be copied in the server
# mod_summary_stat_server("summary_stat_1")
