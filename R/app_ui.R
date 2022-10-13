#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4DashPage(
      title = "",
      skin = NULL,
      freshTheme = NULL,
      preloader = NULL,
      options = NULL,
      fullscreen = TRUE,
      help = FALSE,
      dark = NULL,
      scrollToTop = FALSE,
      header = bs4DashNavbar(
        title = dashboardBrand(
          title = "Vix",
          color = "white",
          href = "https://mrpackages.netlify.app/",
          image = "www/beans3.png",
          opacity = 0.8
        ),
        status = "light",
        fixed = TRUE,
        # HTML("<script type='text/javascript' src='https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js' data-name='bmc-button' data-slug='mrbean' data-color='#FFFFFF' data-emoji=''  data-font='Cookie' data-text='Buy MrBean a coffee' data-outline-color='#000' data-font-color='#000' data-coffee-color='#fd0' ></script>"),
        "Web Application for Data Analysis!",
        rightUi = bs4DropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          href = "http://buymeacoffee.com/mrbean",
          messageItem(
            from = "MrBean",
            message = "If you want to contribute...",
            time = "today", image = "www/beans3.png",
            href = "http://buymeacoffee.com/mrbean"
          )
        )
      ),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "info",
        elevation = 4,
        fixed = FALSE,
        bs4SidebarMenu(
          id = "tabs",
          bs4SidebarHeader("Menu"),
          bs4SidebarMenuItem(
            "Home",
            tabName = "home",
            icon = shiny::icon("home", verify_fa = FALSE)
          ),
          # Import data
          bs4SidebarMenuItem(
            "My Data",
            icon = shiny::icon("database"),
            startExpanded = F,
            bs4SidebarMenuItem(
              text = "Import Data",
              tabName = "Data",
              icon = shiny::icon("file-upload", verify_fa = FALSE)
            ),
            bs4SidebarMenuItem(
              text = "Summary stat",
              tabName = "summ_stat",
              icon = shiny::icon("chart-column")
            )
          ),
          bs4SidebarMenuItem(
            "Selection Index",
            tabName = "s_index",
            icon = shiny::icon("chart-column")
          ),
          bs4SidebarMenuItem(
            "Genotype by Env",
            tabName = "env",
            icon = shiny::icon("chart-column")
          ),
          bs4SidebarMenuItem(
            "Geographical Visuals",
            tabName = "geo",
            icon = shiny::icon("chart-column")
          )
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          # chooseSliderSkin("Modern"),
          bs4TabItem(
            tabName = "home",
          ),
          # Import data
          bs4TabItem(
            tabName = "Data",
            mod_data_import_ui("data_import_1")
          ),
          bs4TabItem(
            tabName = "summ_stat",
            mod_summary_stat_ui("summary_stat_1")
          ),
          bs4TabItem(
            tabName = "s_index",
            mod_sindex_vix_ui("sindex_vix_1")
          ),
          bs4TabItem(
            tabName = "env",
            mod_env_vix_ui("env_vix_1")
          ),
          bs4TabItem(
            tabName = "geo",
            mod_geo_vix_ui("geo_vix_1")
          )
        )
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
      app_title = "Golexx"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
