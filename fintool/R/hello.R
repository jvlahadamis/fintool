# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



hello <- function() {
  print("Hello, world!")
}



## 1.

wide2gg <- function(data) {

  plot <- data %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
    ggplot2::ggplot(aes(x = date,
                        y = value,
                        color = series)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = fig.title,
         y = "y",
         x = "x")
  return(plot)
}

##source for 1.
data <- data %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
  ggplot2::ggplot(aes(x = date,
                      y = value,
                      color = series)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = fig.title,
       y = "y",
       x = "x")




## 2.

rollReg <- function(data, windowsize) {


  reg_output <- data.frame()
  for (i in windowsize:nrow(data)) {
    # iterate with window size
    subset <- data[(i - windowsize + 1):i, ]
    # lm obj.
    model <- lm(rate_ret ~ uso_ret, data = subset)
    # Extracting values from lm model
    slope <- summary(model)$coefficients[2]
    pVal <- anova(model)$'Pr(>F)'[1]
    # Binding to pre-defined dataframe
    reg_output <- rbind(reg_output, data.frame(i = i, slope = slope, pVal = pVal))
  }

  return(reg_output)
}




## 3.

adjustprices <- function(ticker)
  {
#inputs


all <- tidyquant::tq_get(x = ticker,
                         get = "stock.prices",
                         from = "2010-01-01",
                         to = Sys.Date()) %>%
  dplyr::rename_all(tools::toTitleCase) %>%
  #group_by before nesting
  dplyr::group_by(Symbol) %>%
  #creates new column "data" which is a list of two tibbles
  tidyr::nest() %>%
  #create a new column by taking the input which is the list of two tibbles from above which were created from the nesting function. Define the list (.x in purrr::map) as the data column for purrr::map to read. The function applies the xts coercion to .x (each element in the list) and the quantmod adjustment is applied to .x which is also pulling the .x list I defined in purrr::map as the OHLC object
  dplyr::mutate(mutated_data = purrr::map(.x = data, .f = ~    timetk::tk_xts(data = .x, date_var = Date) %>%
                                            quantmod::adjustOHLC(.x, use.Adjusted = TRUE) %>%
                                            timetk::tk_tbl(rename_index = "Date"))) %>%
  dplyr::select(-data) %>%
  tidyr::unnest(mutated_data) %>%
  dplyr::rename(date = Date)




output
}






