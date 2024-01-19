## code to prepare `DATASET` dataset goes here


#' dataset is stockprices for AAPL and TSLA
#' @description Adjusted stock prices for AAPL and TSLA
#' @format dataframe
#' @returns tibble


usethis::use_data(DATASET, overwrite = TRUE)


stocks <- tidyquant::tq_get(c("TSLA", "AAPL"),
                            from = "2010-01-01",
                            to = Sys.Date(),
                            get = "stock.prices") %>%
  dplyr::select(1,2,8) %>%
  tidyr::pivot_wider(names_from = "symbol",
                     values_from = "adjusted")


usethis::use_data(stocks, overwrite = T)

