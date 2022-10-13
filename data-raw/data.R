## code to prepare `data` dataset goes here

shapedata <- readRDS("data-raw/val.rds")
usethis::use_data(shapedata, overwrite = TRUE)

precdata <- readRDS("data-raw/prec_df.rds")
usethis::use_data(precdata, overwrite = TRUE)

tempdata <- readRDS("data-raw/temp_mean_nga_df.rds")
usethis::use_data(tempdata, overwrite = TRUE)
