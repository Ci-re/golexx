#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(6,
           box(width = 12,
            column(6,
             fileInput(inputId = ns("import_data"),
               label = "Upload BLUPS data",
               placeholder = "BLUPS DATA",
               width = "200px",
               accept = c("xlsx","xls"))
            )
        )),
        column(6,
               box(width = 12, textOutput(outputId = ns("user_prompt")),
                   uiOutput(outputId = ns("sindex_select_options")),
                   # uiOutput(outputId = ns("subset_table")),
                   uiOutput(outputId = ns("load_all"))))
      ),
      fluidRow(

       box(
          width = 12,
          status = "info",
          maximizable = TRUE,
          elevation = 3,
          title = "Modify/Select Options",
          uiOutput(ns("switch_table")),
          DT::dataTableOutput(outputId = ns("dataframes"))
        )
      )
    )
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    userFile <- reactive({
      req(input$import_data)
      file1 <- input$import_data
      validate(need(identical(tools::file_ext(file1$datapath), "xls"),"csv, xls or xlsx data needed"))
      return(file1)
    })

    check_sindex <- reactive({
      req(userFile())
      gen_data <- userFile()$datapath
      is_sindex <- check_si(gen_data)
      # print(is_sindex)
      return(is_sindex)
    })

    output$load_all <- renderUI({
      req(check_sindex())
      if(check_sindex() == "notavailable"){
          actionButton(
            inputId = ns("load_all"),
            label = "Load Data",
            icon = icon("check"),
            width = "200px",
            class = "btn-success"
          )
      } else {
        actionButton(
          inputId = ns("load_all"),
          label = "Load",
          icon = icon("check"),
          width = "200px",
          class = "btn-success"
        )
      }
      # actionButton(inputId = ns("load_all"), label = "Load", icon = icon("check"), status = "success")
    })

    output$user_prompt <- renderText({
      req(check_sindex())
      # print(check_sindex())
      if(check_sindex() == "available"){
        "Data Successfully Uploaded, Selection Index dataframe detected"
      } else {
        "Uploaded Successfully, No Selection index dataframe detected, Please Generate one below."
      }
    })

    observeEvent(userFile(), {
      if(check_sindex() == "notavailable"){
        output$sindex_select_options <- renderUI({
          prettyCheckbox(
            inputId = ns("sindex_choice"),
            label = "Would you like to calculate Selection Index?",
            status = "danger",
            value = FALSE,
            icon = icon("check"),
            fill  = TRUE,
            shape = "curve",
            outline = TRUE,
            animation = "smooth")
        })
      } else {
        return(NULL)
      }
    })

    # output$traits <- renderUI({
    #   req(input$sindex_choice)
    #   gen_data <- userFile()$datapath
    #   dat <- generate_sindex_table(gen_data)
    #   traits <- sort(colnames(dat)[-1])
    #   selectInput(inputId = ns("traits"),label = "Select trait for selection index",choices = traits, multiple = TRUE, selected = traits)
    # })
    #
    # output$weight_input <- renderUI({
    #   req(input$traits)
    #   print(input$sindex_choice)
    #   if(input$sindex_choice == TRUE){
    #     print(input$traits)
    #     # initial value for weights and to keep track of value
    #     # dat <- read_excel("../../git_workspace/Visualizations/BLUPS-UYT-40.xls") %>% janitor::clean_names()
    #     gen_data <- userFile()$datapath
    #     dat <- generate_sindex_table(gen_data)
    #     traits <- sort(colnames(dat))
    #     weightages <- rep(0, ncol(dat))
    #     # set names to simplify recover/storing value
    #     names(weightages) <- names(dat)
    #
    #     var_name <- input$traits
    #     if (!is.null(var_name)) {
    #       # lapply will return a list
    #       lapply(1:length(var_name), function(k){
    #
    #         numericInput(inputId = ns(paste0("var", k)),label =
    #                        paste('Input weight for',
    #                              # assign stored value
    #                              var_name[k]), weightages[[var_name[k]]]
    #         )
    #       })
    #     }
    #   }
    # })

    observeEvent(input$sindex_choice, {
      gen_data <- userFile()$datapath
      dat <- generate_sindex_table(gen_data)
      trait_acc <- sort(colnames(dat)[-1])
      traits <- sort(colnames(dat))
      weightages <- rep(0, ncol(dat))
      # set names to simplify recover/storing value
      names(weightages) <- names(dat)
      if(input$sindex_choice == TRUE){
        showModal(
          modalDialog(
            fluidRow(
              column(2),
              column(8,
                     selectInput(
                       inputId = ns("traits"),
                       label = "Select trait for selection index",
                       choices = trait_acc,
                       multiple = TRUE,
                       selected = trait_acc
                     ),
                     if (!is.null(trait_acc)) {
                       # var_name <- input$traits
                       # lapply will return a list
                       lapply(1:length(trait_acc), function(k){
                         numericInput(
                           inputId = ns(paste0("var", k)),
                           label = paste('Input weight for', trait_acc[k]),
                           value =  weightages[[trait_acc[k]]]
                         )
                       })
                     }
              ),
              column(2),
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(inputId = ns("calculate_si"), "Calculate", class = "btn-primary")
            ),
            easyClose = FALSE,
            size = 'm',
            fade = TRUE,
            title = "Enter weights for selection index"
          )
        )
      }
    })

    calculate_si <- reactive({
      req(input$calculate_si)
      # list_of_inputs <<- reactiveValuesToList(input)
      # print(list_of_inputs)
      gen_data <- userFile()$datapath
      dat <- generate_sindex_table(gen_data)
      traits <- input$traits
      weights <- rep(0, ncol(dat))
      names(weights) <- names(dat)
      if(!is.null(traits)){
        for(i in 1:length(traits)){
          if(!is.null(input[[paste0("var",i)]])){
            weights[[traits[[i]]]] <- input[[paste0("var", i)]]
          }
        }
        weight <- unlist(weights)
        final_dataframe <- calculate_selection_index(dat, weight)
        return(final_dataframe)
      }
    })

    observeEvent(calculate_si(), {
      removeModal()
    })

    blup_sindex_data <- eventReactive(input$load_all, {
      # req(check_sindex())
      print(check_sindex())
      gen_data <- userFile()$datapath
      get_gxe_data <- get_genotype_by_location_data(gen_data)
      # print(get_gxe_data)
      # sindex <- tryCatch(
      #   expr = {
      if(check_sindex() == "notavailable"){
        req(input$sindex_choice)
        print(input$sindex_choice)
        if(input$sindex_choice == TRUE){
          req(calculate_si())
          get_sindex_data <- calculate_si()
        }
      }else{
        get_sindex_data <- export_si_from_blups(datapath = gen_data)
      }
      #   },
      #   error = function(e){
      #     print(e)
      #   }
      # )
      # # print(sindex)
      dataframes_list <- list(env_data = get_gxe_data, sin_data = get_sindex_data)
        # print(dataframes_list)
      return(dataframes_list)
    })

    output$switch_table <- renderUI({
      prettyCheckbox(
        inputId = ns("switch_tab"),
        label = "Switch Table",
        status = "success",
        value = FALSE,
        # fill  = TRUE,
        shape = "curve",
        outline = TRUE,
        animation = "tada"
      )
    })

    output$dataframes <- DT::renderDataTable({
      req(blup_sindex_data())
      dat1 <- as.data.frame(blup_sindex_data()$sin_data)
      dat2 <- as.data.frame(blup_sindex_data()$env_data)
      print(input$switch_tab)
      if(input$switch_tab == TRUE){
        return(DT::datatable(
          dat2,
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
        ))
      } else if(input$switch_tab == FALSE){
        return(DT::datatable(
          dat1,
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
        ))
      }
    })
    return(blup_sindex_data)
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")