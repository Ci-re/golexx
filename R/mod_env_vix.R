#' env_vix UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_env_vix_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(status = "info", title = "Raw Table Distribution",
            maximizable = TRUE, elevation = 3,
            width = 6, withSpinner(DT::dataTableOutput(ns("raw_distribution_env")))),
        box(
          status = "info",
          title = "Check Difference Table",
          width = 6,
          uiOutput(ns("select_trait")),
          maximizable = TRUE, elevation = 3,
          withSpinner(DT::dataTableOutput(ns("check_mean_distribution_env")))
        ),
        box(status = "info", title = "Multi-environment Correlation",
            maximizable = TRUE, elevation = 3,
            width = 12, plotlyOutput(ns("correlation_env"), width = "100%", height = "500px")),
        box(status = "info", title = "Heatmaps: Genotypes by Environment",
            maximizable = TRUE, elevation = 3,
            width = 12, uiOutput(ns("corr_heat_env")), withSpinner(plotOutput(ns("heatmaps_env"), width = "100%", height = "1200px"))),
        box(status = "info", title = "Barplot Check Difference",
            maximizable = TRUE, elevation = 3,
            width = 12, uiOutput(ns("top_frac_env")), uiOutput(ns("bar_traits")),
            withSpinner(plotOutput(ns("check_diff_plot_env"), width = "100%")))
      )
    )
  )
}

#' env_vix Server Functions
#'
#' @noRd
mod_env_vix_server <- function(id, dataset, checks){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$raw_distribution_env <- DT::renderDataTable({
      env_data <- dataset$env_data
      DT::datatable(
        env_data,
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

    output$select_trait <- renderUI({
      pickerInput(
        inputId = ns("select_trait"),
        label = "Select trait",
        choices = c(unique(dataset$env_data$trait)),
        multiple = FALSE,
        selected = "FYLD",
        options = pickerOptions(
          liveSearch = TRUE,
          style = "btn-primary",
          `action-box` = TRUE,
          size = 10
        )
      )
    })


    output$corr_heat_env <- renderUI({
      prettyCheckbox(
        inputId = ns("corr_heat_env"), label = "Switch plot",
        status = "success", outline = TRUE,value = TRUE,
      )
    })


    output$top_frac_env <- renderUI({
      max_obs <- length(unique(dataset$env_data$accession))
      mid_obs <- median(1:max_obs)

      sliderInput(
        inputId = ns("top_frac_env"),
        label = "Select topfrac.." ,
        min = 1,
        step = 1,
        max = max_obs,
        value = c(1,5)
      )
    })

    output$check_mean_distribution_env <- DT::renderDataTable({
      req(checks)
      req(input$select_trait)
      env_data <- dataset$env_data %>% janitor::clean_names() %>% filter(trait == input$select_trait)
      env_data <- calculate_env_checkmean(env_data, checks)
      DT::datatable(
        env_data,
        filter = 'top',
        extensions = 'Buttons',
        options = list(
          searching = TRUE,
          extend = "collection",
          scrollX = TRUE,
          dom = 'Bfrtip',
          fixedColumns = TRUE,
          buttons = c('copy', 'excel','csv', 'pdf', 'print')
        ),
        class = 'display'
      )
    })

    output$correlation_env <- renderPlotly({
      req(input$select_trait)
      dataframe <- dataset$env_data %>% filter(trait == toupper(input$select_trait))
      not_all_na <- function(x) any(!is.na(x))
      dat <- dataframe %>% dplyr::select(where(not_all_na))
      corr_plt <- env_correlation(dat)
      ggplotly(corr_plt)
    })


    output$heatmaps_env <- renderPlot({
      req(input$select_trait)
      dat <- dataset$env_data
      not_all_na <- function(x) any(!is.na(x))
      dataframe <- dat %>% filter(trait == toupper(input$select_trait)) %>% dplyr::select(-trait) %>%
        dplyr::select(where(not_all_na))
      dataframe <- dataframe %>% arrange(desc(combined))
      if(input$corr_heat_env == TRUE){
        heat_plt <- env_heatmap(dataframe)
        return(heat_plt)
      }else{
        heat_plt <- env_superheat_corr(dataframe, checks)
        return(heat_plt)
      }
    })

    output$bar_traits <- renderUI({
      traits <- unique(dataset$env_data$trait)
      shinyWidgets::prettyRadioButtons(
        inputId = ns("bar_traits"),
        label = "Select traits: ",
        choices = c(traits),
        selected = "DM",
        status = "primary",
        shape = "square",
        outline = TRUE,
        animation = "rotate",
        fill = TRUE,
        inline = TRUE
      )
    })

    plot_height <- reactiveVal(250)

    plotht <- reactive({
      req(input$top_frac_env)
      val <- input$top_frac_env
      no_obs <- length(min(val):max(val))
      plotht <- as.numeric(no_obs) * 250
      plot_height(plotht)
      return(plot_height())
    })

    observe({
      output$check_diff_plot_env <- renderPlot({
        req(input$bar_traits)
        dat <- dataset$env_data
        print(input$bar_traits)
        trait_to_plot <- input$bar_traits

        not_all_na <- function(x) any(!is.na(x))
        dataframe <- dat %>%
          filter(trait == toupper(trait_to_plot)) %>%
          dplyr::select(where(not_all_na))

        dataframe <- dataframe %>% arrange(desc(combined))
        dat <- calculate_env_checkmean(dataframe, checks = checks)
        env_barplot_checkdiff <- env_barplot_checkdiff(dat[min(input$top_frac_env):max(input$top_frac_env),])
        # print(length(min(input$top_frac_env):max(input$top_frac_env)))
        return(env_barplot_checkdiff)
      }, height = plotht())
    })
  })
}

## To be copied in the UI
# mod_env_vix_ui("env_vix_1")

## To be copied in the server
# mod_env_vix_server("env_vix_1")
