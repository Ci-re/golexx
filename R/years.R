#' multiyears_tidy_data
#'
#' @param dataframe A multiyears data frame in the regular format
#'
#' @return a tidied dataframe for visualization
#'
#' @examples
#' #multiyears_tidy_data(dataframe)
multiyears_tidy_data <- function(dataframe){
  # dataframe <- read_xls("../BLUEs_TRIAL.xls")
  new_data <- dataframe %>%
    janitor::clean_names() %>% wrangle_data()
  trial <- length(which(colnames(new_data) == "trial"))
  sn <- which(str_detect(colnames(new_data), "sn"))
  if(sn){
    new_data <- new_data %>% select(-sn)
  }
  if(!trial == 0){
    years <- unique(str_extract(dataframe$trial, "\\d{2}"))
    if(length(years) > 1){
      wrg <- new_data %>% mutate(year = as.numeric(str_extract(trial, "\\d{2}"))) %>%
        rowwise() %>%
        mutate(year = ifelse(year > 30, year + 1900, year + 2000)) %>%
        mutate_at("year", as.factor)
    }
    return(wrg)
  } else {
    return(new_data)
  }
}


#' multiyears_wrangler
#'
#' @param dataframe a dataframe for wrangling multiyears dataframe
#'
#' @return a wrangled dataframe
#' @export
#' @examples
#' multiyears_wrangler(dataframe)
multiyears_wrangler <- function(dataframe){
  # new_data <- read_xls("../../GolProj/BLUEs_TRIAL.xls") %>%
  new_data <- dataframe %>%
    janitor::clean_names() %>%
    wrangle_data() %>% multiyears_tidy_data() %>%
    select(-trial) %>%
    pivot_longer(cols = -c(accession, location, year), names_to = "trait", values_to = "values")

  dat <- new_data %>%
    pivot_wider(names_from = "location", values_from = "values", id_cols = c(accession, trait, year)) %>%
    janitor::clean_names() %>%
    mutate_at("trait", toupper) %>%
    pivot_longer(cols = -c(accession, trait, year), names_to = "location", values_to = "values") %>%
    unite("location", location:year, sep = "") %>%
    pivot_wider(id_cols = c(accession, trait), names_from = "location", values_from = "values") %>%
    mutate(combined = rowMeans(select_if(.,is.numeric), na.rm = TRUE)) %>%
    mutate(across(where(is.numeric), round, 2))
  return(dat)
}
