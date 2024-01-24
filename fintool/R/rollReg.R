#' Univariant regression rolling regression
#'
#' This function takes a wide data frame and computes regression coefficients
#' and P-Values on a rolling basis on the defined dependent and independent columns
#'
#'
#' @param data A wide data frame with columns for dependent and independent variables.
#' @param window The size of the rolling window.
#' @param x The name of the independent variable.
#' @param y The name of the dependent variable.
#' @return The input data frame with additional columns for rolling regression coefficients and P-Values.
#' @return Appends columns with rolling regression coefficients and P-Values
#'
#' @examples
#' rollReg(mtcars, 5, "hp", "mpg")
#'
#'
#' @import dplyr
#' @importFrom stats as.formula lm
#' @export


rollReg <- function(data, window, x, y) {
  rollReg_i <- function(data, window, x, y) {
    reg_output <- data.frame()
    for (i in window:nrow(data)) {
      # iterate with window size
      subset <- data[(i - window + 1):i, ]
      # lm obj.
      model <- lm(formula = as.formula(paste0(y, "~", x)), data = subset)
      # Extracting values from lm model
      slope <- summary(model)$coefficients[2]
      pVal <- stats::anova(model)$'Pr(>F)'[1]
      # Binding to pre-defined dataframe
      reg_output <- rbind(reg_output, data.frame(i = i, slope = slope, pVal = pVal))
    }

    return(reg_output)
  }

  # get results
  reg_output <- rollReg_i(data, window, x, y)

  # full join with original df
  result <- data %>%
    dplyr::mutate(i = row_number()) %>%
    dplyr::full_join(reg_output, by = "i")
  return(result)
}

