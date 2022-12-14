#' Datatoy
#'
#' @param datapath BLUPS data path to check for sindex sheet
#'
#' @return a string datatype, TRUE or FALSE
#' @export
#' @examples
#' # check_si(datapath = "path/to/BLUPS.xls")
check_si <- function(datapath = "",...){
  # datapath <- "../../git_workspace/Visualizations/BLUPS-UYT-40.xls"
  mysheets_fromexcel <- list()
  mysheetlist <- excel_sheets(datapath)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  # print(mysheetlist)
  list_of_traits <- c("sindex","index","si","selection index", "s-index", "selection-index")
  bools <- mysheetlist %in% list_of_traits
  bool_val <- "available"
  if(TRUE %in% bools){
    bool_val <- "available"
  } else {
    bool_val <- "notavailable"
  }
  return(bool_val)
}

#' export_si_from_blups
#'
#' @param datapath BLUPS data path to extract sindex sheet from xls file
#'
#' @return helps to extract the selection index data in BLUPS file
#' @export
#' @examples
#' # export_si_from_blups(datapath = "path/to/BLUPS.xls")
export_si_from_blups <- function(datapath) {
  # datapath <- "../../Visualizations/BLUPS-UYT-40.xls"
  mysheets_fromexcel <- list()
  mysheetlist <- excel_sheets(datapath)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  list_of_traits <- c("sindex","index","si","selection index")

  for (i in 1:length(mysheetlist)){
    if(mysheetlist[i]  %in% list_of_traits){
      tempdf <- read_excel(path=datapath, sheet = i)
      mysheets_fromexcel[[i]] <- tempdf
      assign(mysheetlist[i], tempdf)
    }
  }
  list_of_excels <- mysheets_fromexcel %>% purrr::discard(is.null)
  list_of_excels <- list_of_excels[[1]]
  sin_data <- as.data.frame(list_of_excels) %>% janitor::clean_names() %>% wrangle_data_sindex()
  # print(class(list_of_excels))
  return(sin_data)
}

#' generate_sindex_data
#' @param datapath BLUPS data path to check for sindex sheet
#'
#' @return a dataframe that includes selection index
#' @export
#' @examples
#' # generate_sindex_data(datapath = "path/to/BLUPS.xls")
#'
generate_sindex_table_from_excel <- function(datapath){
  stacked_data <- get_genotype_by_location_data(datapath)
  select_cols <- stacked_data %>% select(trait, accession, combined)
  new_df <- data.frame(accession = c(unique(select_cols$accession)))
  traits <- unique(select_cols$trait)
  for(i in 1:length(traits)){
    map_df <- select_cols %>% filter(trait == traits[i]) %>% select(accession, combined)
    colnames(map_df)[which(names(map_df) == "combined")] <- traits[i]
    new_df <- new_df %>% dplyr::left_join(map_df, by = "accession") %>% janitor::clean_names()
  }
  return(new_df)
}

#' generate_sindex_data_from_csv
#' @param dataframe BLUPS dataframe to extract sindex sheet
#'
#' @return a dataframe to calculate selection index
#' @export
#' @examples
#' # generate_sindex_data(dataframe = "dataframe")
#'
generate_sindex_table_from_csv <- function(dataframe){
  # stacked_data <- get_genotype_by_location_data(datapath)
  stacked_data <- dataframe %>% wrangle_data()
  select_cols <- stacked_data %>% select(trait, accession, combined)
  new_df <- data.frame(accession = c(unique(select_cols$accession)))
  traits <- unique(select_cols$trait)
  for(i in 1:length(traits)){
    map_df <- select_cols %>% filter(trait == traits[i]) %>% select(accession, combined)
    colnames(map_df)[which(names(map_df) == "combined")] <- traits[i]
    new_df <- new_df %>% dplyr::left_join(map_df, by = "accession") %>% janitor::clean_names()
  }
  return(new_df)
}


#' wrangle_data
#' @param imported_data check headers that are not spelt correctly and put necessary corrections
#'
#' @return a dataframe with the exact correct names of location
#' @export
#' @examples
#' # generate_sindex_data(dataframe = "blupscsvdata/exceldata")
#' # target headers are accession, combined, and trait
#'
wrangle_data <- function(imported_data){
  # print(imported_data)
  # imported_data <- read_csv("../../Visualizations/combo.csv")
  imported_data <- janitor::clean_names(imported_data)
  x <- colnames(imported_data)
  acc_ind <- which(str_detect(x,paste(c("ssion", "genotype", "gen", "acc", "landrace", "germpl", "clon", "clone"), collapse = '|')))
  colnames(imported_data)[acc_ind] <- "accession"
  comb_ind <- which(str_detect(x,paste(c("com", "combine", "avg", "average"), collapse = '|')))
  colnames(imported_data)[comb_ind] <- "combined"
  location <- which(str_detect(x,paste(c("loc", "location", "env", "environ","environment"), collapse = '|')))
  colnames(imported_data)[location] <- "location"
  trait_ind <- which(str_detect(x,paste(c("trait", "trts", "traits"), collapse = '|')))
  colnames(imported_data)[trait_ind] <- "trait"
  return(imported_data)
}

#' wrangle_data_sindex
#' @param imported_data rename headers of blups dataframe e.g ag_40 to Ago_owu, ib_12 = Ibadan
#'
#' @return a dataframe with the exact correct names of location
#' @export
#' @examples
#' # generate_sindex_data(datapath = "path/to/BLUPS.xls")
#'
wrangle_data_sindex <- function(imported_data){
  # print(imported_data)
  # imported_data <- read_excel("../../git_workspace/Visualizations/BLUPS-UYT-40.xls")
  imported_data <- imported_data %>% wrangle_data() %>% rename(gen = accession)
  x <- colnames(imported_data)
  sin_ind <- which(str_detect(x,paste(c("sindex","index","si","selection index", "s-index", "selection-index"), collapse = '|')))
  colnames(imported_data)[sin_ind] <- "sindex"
  imported_data <- imported_data %>% rename(accession = gen)
  return(imported_data)
}


#' generate_sindex_data
#' @param data rename headers of blups dataframe e.g ag_40 to Ago_owu, ib_12 = Ibadan
#'
#' @return a dataframe with the exact correct names of location
#' @export
#' @examples
#' # generate_sindex_data(datapath = "path/to/BLUPS.xls")
wrangle_colnames <- function(data){
  # print(data)
  piv <- data %>% janitor::clean_names() %>%
    pivot_longer(cols = -c(trait, accession, combined),names_to = "location", values_to = "values") %>%
    mutate(
      location = case_when(
        stringr::str_detect(location, paste(c("ag", "ago"), collapse = '|')) ~ "ago_owu",
        stringr::str_detect(location, paste(c("ib", "iba"), collapse = '|')) ~ "ibadan",
        stringr::str_detect(location, paste(c("mk", "mok"), collapse = '|')) ~ "mokwa",
        stringr::str_detect(location, paste(c("on", "onn"), collapse = '|')) ~ "onne",
        stringr::str_detect(location, paste(c("ot", "oto"), collapse = '|')) ~ "otobi",
        stringr::str_detect(location, paste(c("ub", "ubi"), collapse = '|')) ~ "ubiaja",
        stringr::str_detect(location, paste(c("ik", "ike"), collapse = '|')) ~ "ikenne",
      )
    )
  piv <- piv %>% pivot_wider(names_from = "location", values_from = "values")
  return(piv)
}


#' attach_location_coordinates
#' @param data rename headers of blups dataframe e.g ag_40 to Ago_owu, ib_12 = Ibadan
#'
#' @return a dataframe with the exact correct names of location
#' @export
#' @examples
#' # generate_sindex_data(datapath = "path/to/BLUPS.xls")
attach_location_coordinates <- function(dataframe){
  dat <- dataframe
  piv <- dat %>% pivot_longer(cols = -c(trait, accession, combined), names_to = "location", values_to = "values")
  locs <- data.frame(location = c("ibadan", "mokwa", "ago_owu", "onne", "otobi", "ubiaja", "ikenne"),
                     lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493, 6.8650),
                     long = c(3.9470, 5.0547, 4.3258, 7.1516, 8.0897, 6.3918, 3.7143))
  piv2 <- piv %>%
    mutate(
      long = case_when(
        str_detect(location, paste(c("ag", "ago"), collapse = '|')) ~ locs %>% filter(location=="ago_owu") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("ib", "iba"), collapse = '|')) ~ locs %>% filter(location=="ibadan") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("mk", "mok"), collapse = '|')) ~ locs %>% filter(location=="mokwa") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("on", "onn"), collapse = '|')) ~ locs %>% filter(location=="onne") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("ot", "oto"), collapse = '|')) ~ locs %>% filter(location=="otobi") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("ub", "ubi"), collapse = '|')) ~ locs %>% filter(location=="ubiaja") %>% dplyr::select(long) %>% as.numeric(),
        str_detect(location, paste(c("ik", "ike"), collapse = '|')) ~ locs %>% filter(location=="ikenne") %>% dplyr::select(long) %>% as.numeric()
      )
    ) %>%
    mutate(
      lat = case_when(
        str_detect(location, paste(c("ag", "ago"), collapse = '|')) ~ locs %>% filter(location=="ago_owu") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("ib", "iba"), collapse = '|')) ~ locs %>% filter(location=="ibadan") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("mk", "mok"), collapse = '|')) ~ locs %>% filter(location=="mokwa") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("on", "onn"), collapse = '|')) ~ locs %>% filter(location=="onne") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("ot", "oto"), collapse = '|')) ~ locs %>% filter(location=="otobi") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("ub", "ubi"), collapse = '|')) ~ locs %>% filter(location=="ubiaja") %>% dplyr::select(lat) %>% as.numeric(),
        str_detect(location, paste(c("ik", "ike"), collapse = '|')) ~ locs %>% filter(location=="ikenne") %>% dplyr::select(lat) %>% as.numeric()
      )
    )
  return(piv2)
}

#' get_genotype_by_location_data
#' @param datapath the exact path to your blups data
#'
#' @return a stacked dataframe of all the traits and location in one dataframe extracted from excel sheets
#' @export
#' @examples
#' # get_genotype_by_location_data(datapath = "path/to/BLUPS.xls")
#'
get_genotype_by_location_data <- function(datapath, ...){
  datapath <- as.character(datapath)
  mysheets_fromexcel <- list()
  # print("HITTT")
  mysheetlist <- excel_sheets(datapath)
  # print(mysheetlist)
  # class(mysheetlist)
  mysheetlist <- lapply(mysheetlist, tolower)
  mysheetlist <- as.character(mysheetlist)
  list_of_traits <- c("dm", "dyld", "fyld", "pltht", "mcmds", "sprout", "hi", "rtno", "rtwt", "shtwt")

  for (i in 1:length(mysheetlist)){
    if(mysheetlist[i]  %in% list_of_traits){
      tempdf <- read_excel(path=datapath, sheet = i)
      mysheets_fromexcel[[i]] <- tempdf
      assign(mysheetlist[i], tempdf)
    }
  }
  list_of_excels <- mysheets_fromexcel %>% purrr::discard(is.null)
  # print(wrangle_data(as.data.frame(list_of_excels[[1]])))
  stacked_data <- wrangle_data(as.data.frame(list_of_excels[1]))
  stacked_data <- wrangle_colnames(stacked_data)
  # print(stacked_data)
  for(i in 2:length(list_of_excels)){
    # print(class(list_of_excels[[i]]))
    drt <- wrangle_data(list_of_excels[[i]]) %>% wrangle_colnames()
    # drt <- drt %>%  pivot_wider(names_from = "location", values_from = "values")
    stacked_data <- dplyr::bind_rows(stacked_data, drt)
  }
  # print(stacked_data)
  return(stacked_data)
}


#' calculate_selection_index
#' @param dataframe The dataframe without the selection index
#' @param ... Enter traits and their values, check example below
#'
#' @return a dataframe with the calculated dataframe
#' @export
#' @examples
#' # Please do ensure that variable argument names matches table headers
#' # calculate_select_index(dataframe = dat, rtwt = 34, shtwt = 50, dm = 89)
#' # enter traits as list or vector
#' # traits = c(shtwt = 6, rtwt = 3, dm = 45, pltht = 23)
#' # calculate_selection_index(dataframe = dat, traits)
#'
#'
calculate_selection_index <- function(dataframe, ...){
  # print(dataframe)
  # dataframe <- generate_sindex_table("../../git_workspace/Visualizations/trial.xls")
  trait_weight <- list(...)
  dataframe <- dataframe %>% janitor::clean_names()
  # print(dataframe)
  # trait_weight <- c(shtwt = 15, rtwt = 15, dm = 20, sprout = 10, hi = 10, rtno = 15,  mcmds = -10, mcmdi = -10, pltht = 10, fyld = 20, dyld = 20)
  colnames_dataframe <- colnames(dataframe)
  acc_ind <- which(str_detect(colnames_dataframe,paste(c("ssion", "genotype", "gen", "aces", "landrace", "germpl", "clon", "clone"), collapse = '|')))
  colnames(dataframe)[acc_ind] <- "accession"
  dataframe2 <- dataframe %>% select(-accession)
  initial_trait_weight_val <- rep(0, ncol(dataframe2))
  names(initial_trait_weight_val) <- names(dataframe2)
  initial_trait_weight_val <- data.frame(unlist(initial_trait_weight_val))
  colnames(initial_trait_weight_val) <- "value"
  initial_trait_weight_val <- initial_trait_weight_val %>% rownames_to_column("trait")

  if(!is.null(trait_weight)){
    trait_weight <- data.frame(unlist(trait_weight))
    colnames(trait_weight)[1] = "value"
    trait_weight_dataframe <- trait_weight %>% rownames_to_column("trait")
    # final_dataframe <- transform(merge(initial_trait_weight_val, trait_weight_dataframe, all = TRUE))
    weight_df <- left_join(initial_trait_weight_val, trait_weight_dataframe, by = "trait") %>%
      mutate(value = if_else(is.na(value.y), 0, value.y)) %>% select(-c(value.x, value.y))
    nume_dataframe <- dataframe %>% select_if(is.numeric)
    scaled_df <- data.frame(scale(nume_dataframe, center = TRUE, scale = TRUE)) %>% mutate_if(is.numeric, round, 3)
    traits <- colnames(scaled_df)
    index <- weight_df %>% filter(trait%in%traits) %>% data.frame() %>% column_to_rownames(var = "trait")
    arr.wt <- as.matrix(index)
    # dat <- scaled_df %>% mutate(si = (rtwt*15) + (shtwt*15) +(dm*20) +(sprout*10) +(hi*10) +(rtno*15)+ (dyld*20) +(fyld*20) +(mcmds*-10) +(mcmdi*-10) +(pltht*10)) %>% cbind(dataframe$accession) %>% select(si)
    # cbind(dataframe,dat) %>% arrange(-si)
    arr.data <- data.matrix(scaled_df)
    SI = arr.data %*% arr.wt
    SI <- round(SI,3)
    final_data = cbind(dataframe, data.frame(SI))
    final_data <- final_data %>% rename(sindex = value) %>% arrange(-sindex) %>% wrangle_data_sindex()
    return(final_data)
  }
}

return_sindex_dataframe <- function(datapath, file_name, ext){
  if(ext == "xls" || ext == "xlsx"){
    gen_data <- datapath
    dat <- readxl::read_excel(gen_data)
    if("trial" %in% colnames(dat)){
      dat <- multiyears_wrangler(dat) %>% generate_sindex_table_from_csv()
    } else {
      dat <- generate_sindex_table_from_excel(gen_data)
    }
  } else if(ext == "csv") {
    gen_data <- read.csv(datapath)
    if("trial" %in% colnames(dat)){
      dat <- multiyears_wrangler(gen_data) %>% generate_sindex_table_from_csv()
    } else {
      dat <- generate_sindex_table_from_csv(gen_data)
    }
  }
  return(dat)
}

get_gxe_data <- function(datapath, ext){
  if(ext == "xls" || ext == "xlsx"){
    gen_data <- datapath
    dat <- readxl::read_excel(gen_data)
    print(dat)
    if("trial" %in% colnames(dat)){
      get_gxe_data <- multiyears_wrangler(dat)
    } else {
      get_gxe_data <- get_genotype_by_location_data(gen_data)
    }
  } else if(ext == "csv") {
    gen_data <- read.csv(datapath)
    if("trial" %in% colnames(dat)){
      dat <- readxl::read_excel(gen_data)
      get_gxe_data <- multiyears_wrangler(dat)
    } else {
      get_gxe_data <- wrangle_data(gen_data) %>% wrangle_colnames()
    }
  }
  return(get_gxe_data)
}
