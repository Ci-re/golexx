
options(ggrepel.max.overlaps = Inf)
#' fix_listOfTop_Traits
#'
#' @param original_list
#' @param top_traits
#'
#' @return a list
#' @export
#' @examples
#' # check_si(datapath = "path/to/BLUPS.xls")
#'

fix_listOfTop_Traits <- function(original_list, top_traits){
  filt <- function(x){
    !(x %in% top_traits)
  }
  not_in_list <- original_list[sapply(original_list, filt) == TRUE]
  length_of_top_traits <- length(top_traits)
  more_to_add <- 3 - length_of_top_traits
  if(more_to_add > 0){
    for(i in 1:more_to_add){
      top_traits[length_of_top_traits + i] <- not_in_list[i]
    }
  }
  return(top_traits)
}

#' get_checks
#' @param accession_list the list of all the accession in your data
#'
#' @return a list of detected checks in the dataframe
#' @export
#' @examples
#' # get_checks(accession_list = dataframe$accession)
#'
get_checks <- function(accession_list){
  checks <- c()
  pattern <- c("IITA", "TMEB", "TME", "-", "tmeb", "iita")
  for(i in 1:length(accession_list)){
    for(j in 1:length(pattern)){
      if(str_detect(accession_list[i], pattern = pattern[j])){
        checks[length(checks) + 1] <- accession_list[i]
      }
    }
  }
  # print(unique(checks))
  return(unique(checks))
}


#' get_genotype_by_location_data
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @export
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
plotRadar <- function(trait_to_plot, imported_data, checks, accession_range){
  # print(accession_range)
  # imported_data <- get_genotype_by_location_data(datapath = "../../git_workspace/Visualizations/BLUPS-UYT-40.xls")
  # trait_to_plot <- "PLTHT"
  # checks <- get_checks(imported_data$accession)
  # accession_range <- c(1,4)
  trait_to_plot <- toupper(trait_to_plot)

  uytdata_arr <- imported_data %>% filter(trait == trait_to_plot) %>%  mutate(category = if_else(accession %in% checks, "Checks", "Genotype")) %>%
    janitor::clean_names() %>% select(-trait)
  if(trait_to_plot == "MCMDS"){
    uytdata_arr <- uytdata_arr %>% arrange(combined)
  }else {
    uytdata_arr <- uytdata_arr %>% arrange(desc(combined))
  }
  uytdatax <- uytdata_arr[min(accession_range):max(accession_range),]
  # colnames(uytdatax) <- paste0("loc_", 2:7)
  # print(uytdatax)
  p <- ggRadar(data = uytdatax,mapping = aes(group = accession), alpha = 0.1,  rescale = FALSE) +
    # geom_text_repel(max.overlaps = Inf, box.padding = 0.5, aes(group = accession)) +
    theme_light(base_size = 18) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.text = element_text(size = 10)) +
    guides(color = guide_legend(nrow = 4))
  return(p)
}

#' linePlot
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @export
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
linePlot <- function(trait_to_plot, imported_data, checks, accession_range){
  # listr <- c("dyld", "fyld", "")
  # imported_data <- read_csv("../../Visualizations/combo.csv")
  # trait_to_plot <- "MCMDS"
  # uytdata %>% View()
  trait_to_plot <- toupper(trait_to_plot)
  uytdata_arr <- imported_data %>% filter(trait == trait_to_plot) %>%
    mutate(category = if_else(accession %in% checks, "Checks", "Genotype")) %>%
    janitor::clean_names()
  if(trait_to_plot == "MCMDS"){
    uytdata_arr <- uytdata_arr %>% arrange(combined)
  }else {
    uytdata_arr <- uytdata_arr %>% arrange(desc(combined))
  }
  # if(trait_to_plot == "SPROUT" || trait_to_plot == "MCMDS"){
  #   uytdatax <- uytdata_arr[1:4,]
  # }else{
  #   uytdatax <- uytdata_arr %>% top_frac(.1, combined)
  # }
  uytdatax <- uytdata_arr[min(accession_range):max(accession_range),]
  x <- uytdatax %>% pivot_longer(-c(trait, accession, combined, category),
                                 names_to = "location", values_to = "values")
  full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry Matter",
                     "SPROUT" = "Sprout", "DYLD" = "Dry yield", "FYLD" = "Fresh yield", "PLTHT" = "Plant Height")

  p <- x %>% ggplot(aes(location, values, group = accession, colour = accession)) +
    geom_point() +
    geom_line()  +
    theme_grey() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#fcfcee",colour = "#cecece")) +
    # plot.caption = element_text("Hello",face = "italics")) +
    labs(x = "Location", y = trait_to_plot) +
    # guides(fill = "none") +
    geom_text_repel(max.overlaps = Inf, box.padding = 0.5,
                    aes(group = accession, label = if_else(location == "mokwa",accession,"")))
  # p <- ggplotly(p)
  # layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  return(p)
}

#' distribution_plot
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @export
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
distribution_plot <- function(x, y, data, color, lm){
  print(lm)
  # data <- export_si_from_blups("../../git_workspace/Visualizations/BLUPS-UYT-40.xls")
  p <- ggplot2::ggplot(data, mapping = aes_string(x = x, y = y)) +
    geom_point(mapping = aes_string(color = color, size = color))
    if(lm == "Yes"){
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
  return(plotly::ggplotly(p))
}

#' plot_checks
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @export
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
plot_checks <- function(dataframe, traits){
  # traits <- c("DM","DYLD","FYLD","PLTHT")
  traits <- toupper(triats)
  x <- dataframe %>% pivot_longer(-c(trait, accession, combined),
                                  names_to = "location", values_to = "values") %>% filter(trait %in% traits)
  # full_names <- list("DM" = "Dry Matter","DYLD" = "Dry yield", "FYLD" = "Fresh yield",
  #                    "PLTHT" = "Plant Height")
  p <- x %>% ggplot(aes(location, values, group = accession, colour = accession)) +
    geom_point() +
    geom_line()  +
    ggthemes::theme_hc() +
    theme(legend.position = "bottom", strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
          strip.text = element_text(colour = "white"),
          plot.background = element_rect(fill = "#fcfcee", colour = "#cecece")) +
    # plot.caption = element_text("Hello",face = "italics")) +
    labs(x = "Location") +
    # guides(fill = "none") +
    geom_text(aes(label = values), nudge_x = 0.25, nudge_y = 0.25, check_overlap = TRUE)
    # facet_wrap(~trait, scales = "free_y")
  p <- ggplotly(p)
  return(p)
}
