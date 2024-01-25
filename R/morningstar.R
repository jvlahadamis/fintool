#' Morning Star Commodities API call
#' @description
#' Returns data from Morningstar API.
#' Credentials are needed for Morningstar.
#'
#' @section Current feeds supported:
#' \itemize{
#'    \item MGEX_Futures_continuous and MGEX_Futures
#'    \item ICE_Canada_FuturesPrices_continuous
#' }
#'
#' @param feed Motningstar Feed Table. `character`
#' @param contract Morningstar key. `character`
#' @param from From date yyyy-mm-dd. `character`
#' @param iuser Morningstar username/email as character - locally sourced for examples. `character`
#' @param ipassword Morningstar password as character - locally sourced for examples. `character`
#'
#' @returns wide data frame `tibble`
#'
#' @export getAgPrice
#' @author Brooklyn Holt, developed from Philippe Cote's getPrice
#' @examples
#' \dontrun{
#'
#' getAgPrice(feed = "MGEX_Futures_continuous",
#'           contract = "MWE_001_Month",
#'           from = "2000-01-01",
#'           iuser = username,
#'           ipassword = password)
#'
#' getAgPrice(feed = "MGEX_Futures",
#'           contract = "@AJC6F",
#'           from = "2000-01-01",
#'           iuser = username,
#'           ipassword = password)
#'
#' getAgPrice(feed = "ICE_Canada_FuturesPrices_continuous",
#'            contract = "BW_001_Month",
#'            from = "2000-01-01",
#'            iuser = username,
#'            ipassword = password)
#' }


getAgPrice <- function(feed = "MGEX_Futures_continuous",
                       contract = "MWE_001_Month",
                       from = "2000-01-01",
                       iuser = "a@abc.com",
                       ipassword = "password") {

  . <- opt <- value <- NULL

  mpurl <- "https://mp.morningstarcommodity.com/lds/feeds/MGEX_Futures_continuous/ts?Symbol=MWE_001_Month"
  userpw <- paste0(iuser,":",ipassword)
  if (feed %in% c("MGEX_Futures")) {
    URL <- httr::modify_url(url =  "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "contract=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("MGEX_Futures_continuous",
                  "ICE_Canada_FuturesPrices_continuous"
  )) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "contract=", contract, "&fromDateTime=", from))
  }

  httr::handle_reset(URL)
  x <- httr::GET(url = URL, httr::authenticate(user = iuser, password = ipassword, type = "basic"))
  x <- httr::content(x)

  if (length(x) > 0) {
    if (length(x %>% purrr::flatten() %>% .$series %>% .$values %>% purrr::flatten()) > 0) {
      out <- dplyr::tibble(
        date = as.character(lubridate::ymd(x %>% purrr::flatten() %>% purrr::flatten() %>% .$dates)) %>% lubridate::ymd(),
        opt = feed == "MGEX_Futures",
        value = ifelse(opt == T,
                       as.numeric(x %>% purrr::flatten() %>% purrr::flatten() %>% .$values %>% .[[4]] %>% purrr::flatten()),
                       as.numeric(x %>% purrr::flatten() %>% purrr::flatten() %>% .$values %>% .[[1]] %>% purrr::flatten())
        )
      ) %>%
        dplyr::select(-opt) %>%
        dplyr::mutate(value = ifelse(is.nan(value), NA, value))
    } else{
      out <- dplyr::tibble(date = character(),
                           value = numeric(),
                           fwdmnt = numeric(),
                           fwdyr = numeric())
    }
  }
  if (length(colnames(out)) == 2) {
    colnames(out)[2] <- sub("@", "", contract)
  }
  return(out)
}

#' Morning Star Commodities API multiple calls
#' @description
#' Returns multiple Morningstar calls from the getAgPrice functions.
#' Credentials are needed for Morningstar.
#'
#' @section Current feeds supported:
#' \itemize{
#'    \item MGEX_Futures_continuous and MGEX_Futures
#'    \item ICE_Canada_FuturesPrices_continuous
#' }
#'
#' @param feed Motningstar Feed Table. `character`
#' @param contracts Symbols vector. `character`
#' @param from From date yyyy-mm-dd. `character`
#' @param iuser Morningstar username/email as character - locally sourced for examples. `character`
#' @param ipassword Morningstar password as character - locally sourced for examples. `character`
#'
#' @returns wide data frame `tibble`
#'
#' @export getAgPrices
#' @author Brooklyn Holt, developed from Philippe Cote's getPrices
#' @examples
#' \dontrun{
#' getAgPrices(feed = "ICE_Canada_FuturesPrices_continuous",
#'             contracts = c("BW_001_Month", "RS_001_Month"),
#'             from = "2000-01-01",
#'             iuser = username,
#'             ipassword = password)
#' }
#'
getAgPrices <- function(feed = "ICE_Canada_FuturesPrices_continuous",
                        contracts = c("BW_001_Month", "RS_001_Month"),
                        from = "2000-01-01",
                        iuser = "a@abc.com",
                        ipassword = "password") {
  y <- getAgPrice(feed = feed, contract = contracts[1], from = from, iuser = iuser, ipassword = ipassword)
  for (c in contracts[-1]) {
    y <- merge(y,
               getAgPrice(feed = feed, contract = c, from = from, iuser = iuser, ipassword = ipassword),
               all = T
    )
  }
  y <- dplyr::as_tibble(y)
  return(y)
}
