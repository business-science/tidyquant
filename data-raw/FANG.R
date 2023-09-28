## code to prepare `FANG` dataset goes here
FANG <- readr::read_csv("data-raw/FANG.csv")
usethis::use_data(FANG, overwrite = TRUE)
