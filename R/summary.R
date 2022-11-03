
options(ggrepel.max.overlaps = Inf)
#' fix_listOfTop_Traits
#'
#' @param original_list the full list of traits
#' @param top_traits the traits selected
#'
#' @return a list
#' @examples
#' # fix_listOfTop_Traits(original_list = trait_list, top_traits = selected_traits)
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

#' plotRadar
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
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

  uytdata_arr <- imported_data %>% filter(trait == trait_to_plot) %>%
    mutate(category = if_else(accession %in% checks, "Checks", "Genotype")) %>%
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
  uytdatax <- uytdata_arr[min(accession_range):max(accession_range),]
  x <- uytdatax %>% pivot_longer(-c(trait, accession, category),
                                 names_to = "location", values_to = "values")
  full_names <- list("MCMDS" = "Cassava Mosaic", "HI" = "Harvest Index", "DM" = "Dry Matter",
                     "SPROUT" = "Sprout", "DYLD" = "Dry yield", "FYLD" = "Fresh yield",
                     "PLTHT" = "Plant Height", "RTWT" = "Root Weight", "SHTWT" = "Shoot Weight")

  p <- x %>% ggplot(aes(location, values, group = accession, colour = accession)) +
    geom_point() +
    geom_line()  +
    theme_dark() +
    theme(legend.position = "none", axis.text = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "#fcfcee",colour = "#cecece")) +
    labs(x = "Location", y = trait_to_plot) +
    geom_text_repel(max.overlaps = Inf, box.padding = 0.5,
                    aes(group = accession, label = if_else(location == "combined",accession,"")))
  return(p)
}


#' plot_finlay_chart
#' @param imported_data the dataframe used, containing locations and their means
#' @param trait_to_plot the selected trait to plot
#'
#' @return a 'finlay' kind of plot showing the performance of
#' accessions across different environments, scaled by the average
#'
#' @examples
#' # plot_finlay_chart(imported_data = mydata, trait_to_plot = "DYLD")
#'
plot_finlay_chart <- function(imported_data, trait_to_plot, checks, accession_range){
  trait_to_plot <- toupper(trait_to_plot)
  traits_all <- imported_data[min(accession_range):max(accession_range),] %>%
    filter(trait == trait_to_plot) %>%
    pivot_longer(
      names_to = "location",
      values_to = "values",
      cols = -c(accession, combined, trait)
    )

  location_mean <- traits_all %>%
    select(trait, combined, location, values) %>%
    group_by(location) %>%
    summarise(average = mean(values, na.rm=T)) %>%
    arrange(average)

  plot_data <- traits_all %>%
    right_join(location_mean, by = "location", suffix = c("", "")) %>%
    arrange(average) %>%
    mutate(category = if_else(accession %in% checks, "Checks", "Genotype"))

  p <- plot_data %>%
    ggplot(mapping = aes(average, values)) +
    geom_point(aes(color = values)) +
    geom_smooth(aes(group = accession), method = "lm", se = FALSE) +
    theme(axis.text = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(
      breaks = unique(plot_data$average),
      labels = unique(plot_data$location),
      guide = guide_axis(n.dodge = 2)
    )
  return(p)
}

#' distribution_plot
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
distribution_plot <- function(x, y, data, color, lm, text){
  # print(lm)
  # data <- export_si_from_blups("../../git_workspace/Visualizations/BLUPS-UYT-40.xls")
  p <- ggplot2::ggplot(data, mapping = aes_string(x = x, y = y)) +
    geom_point(mapping = aes_string(color = color, size = color, text = text)) +
    scale_color_viridis_c()
    if(lm == "Yes"){
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
  return(plotly::ggplotly(p))
}

#' plot_checks
#' @param datapath the exact path to your blups data
#' @param traits Selected trait to plot
#'
#' @return a stacked dataframe of all the traits and location in one dataframe
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
plot_checks <- function(dataframe, traits){
  # traits <- c("DM","DYLD","FYLD","PLTHT")
  traits <- toupper(traits)

  x <- dataframe %>%
    pivot_longer(
      cols = -c(trait, accession, combined),
      names_to = "location",
      values_to = "values"
    ) %>%
    filter(trait %in% traits)

  location_mean <- x %>%
    select(trait, combined, location, values) %>%
    group_by(location) %>%
    summarise(average = mean(values, na.rm=T)) %>%
    arrange(average)

  plot_data <- x %>%
    right_join(location_mean, by = "location", suffix = c("", "")) %>%
    arrange(location, average)

  averages <- plot_data %>% group_by(location) %>%
    summarize(average = unique(average))
  averages1 <- averages$average
  print(plot_data)
  p <- plot_data %>% ggplot(aes(average, values, colour = accession)) +
    geom_point() +
    ggplot2::geom_smooth(mapping = aes(group = accession), method = "lm", se = FALSE) +
    ggthemes::theme_hc() +
    theme(
      axis.text = element_text(angle = 45, size = 8, hjust = 1),
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
      strip.text = element_text(colour = "white"),
      plot.background = element_rect(fill = "#fcfcee", colour = "#cecece")) +
    labs(x = "Location") +
    geom_text(aes(label = values), check_overlap = TRUE) +
    scale_x_continuous(
      breaks = averages1,
      labels = unique(plot_data$location),
      guide = guide_axis(n.dodge = 1)
    )
    # guides(fill = "none") +
    # facet_wrap(~trait, scales = "free_y")
  return(p)
}

# library(tidyverse)
#
# x <- read.csv("../dat.csv")
# checks <- Golexx::get_checks(x$accession)
# Y <- x %>% filter(accession %in% checks) %>% plot_checks("DYLD")
# Y
