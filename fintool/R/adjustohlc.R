#' Return a dataframe with adjusted OHLC for given tickers
#'
#' This function takes 3 inputs: a ticker or list of tickers, a start date and an end date and returns the adjusted OHLC
#' table.
#'
#'
#' @param tickers A ticker in quotes: "AAPL", or subset of tickers: c("AAPL", "TSLA") for example
#' @param start Start date
#' @param end End date description
#' @return An table with adjusted OHLC prices
#'
#' @examples
#' adjustohlc("AAPL", "2020-01-01", "2023-02-01")
#'
#' @import dplyr
#' @import tidyr
#' @import tidyquant
#' @import purrr
#' @import tools
#' @import timetk
#' @import quantmod
#' @export
adjustohlc <- function(tickers, start, end) {


  Symbol <- data <- mutated_data <- Date <- NULL
  all <- tidyquant::tq_get(x = tickers,
                           get = "stock.prices",
                           from = start,
                           to = end) %>%
    dplyr::rename_all(tools::toTitleCase) %>%
    #group_by before nesting
    dplyr::group_by(Symbol) %>%
    #creates new column "data" which is a list of two tibbles
    tidyr::nest() %>%
    #create a new column by taking the input which is the list of two tibbles from above which were created
    #from the nesting function. Define the list (.x in purrr::map) as the data column for purrr::map to read.
    #The function applies the xts coercion to .x (each element in the list) and the quantmod adjustment is applied to .x which
    #is also pulling the .x list I defined in purrr::map as the OHLC object
    dplyr::mutate(mutated_data = purrr::map(.x = data, .f = ~    timetk::tk_xts(data = .x, date_var = Date) %>%
                                              quantmod::adjustOHLC(.x, use.Adjusted = TRUE) %>%
                                              timetk::tk_tbl(rename_index = "Date"))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(mutated_data) %>%
    dplyr::rename(date = Date)

  return(all)
}




