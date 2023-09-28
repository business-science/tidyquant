## code to prepare `FANG` dataset goes here
FANG <- readr::read_csv("data-raw/FANG.csv")
FANG <- FANG %>%
    mutate(symbol = ifelse(symbol == "FB", "META", symbol))
usethis::use_data(FANG, overwrite = TRUE)
