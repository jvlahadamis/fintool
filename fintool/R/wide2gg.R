#' Create a chart using ggplot2 from a wide dataframe
#'
#' This function takes a wide dataframe as input and generates a ggplot line plot
#' using the ggplot2 library.
#'
#'
#' @param data A wide dataframe with columns 'date', series of names
#' @return A ggplot chart
#'
#' @examples
#' wide2gg(stocks)
#'
#'
#' @import ggplot2
#' @export



wide2gg <- function(data) {

    value <- series <- NULL

    plot <- data %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
    ggplot2::ggplot(aes(x = date,
                        y = value,
                        color = series)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Your Plot",
         y = "y",
         x = "x")
  return(plot)

}







