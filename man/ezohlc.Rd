% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezohlc.R
\name{ezohlc}
\alias{ezohlc}
\title{Return a dataframe with adjusted OHLC for given tickers}
\usage{
ezohlc(tickers, start, end)
}
\arguments{
\item{tickers}{A ticker in quotes: "AAPL", or subset of tickers: c("AAPL", "TSLA") for example}

\item{start}{Start date}

\item{end}{End date description}
}
\value{
An table with adjusted OHLC prices
}
\description{
This function takes 3 inputs:    a ticker/subset of tickers, start date, and end date. The
output is the adjusted OHLC dataframe.
}
\examples{
ezohlc("AAPL", "2020-01-01", "2023-02-01")

}
