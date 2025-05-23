% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-helpers.R
\name{gg_arma}
\alias{gg_arma}
\title{Plot characteristic ARMA roots}
\usage{
gg_arma(data)
}
\arguments{
\item{data}{A mable containing models with AR and/or MA roots.}
}
\value{
A ggplot object the characteristic roots from ARMA components.
}
\description{
Produces a plot of the inverse AR and MA roots of an ARIMA model.
Inverse roots outside the unit circle are shown in red.
}
\details{
Only models which compute ARMA roots can be visualised with this function.
That is to say, the \code{glance()} of the model contains \code{ar_roots} and \code{ma_roots}.
}
\examples{
if (requireNamespace("fable", quietly = TRUE)) {
library(fable)
library(tsibble)
library(dplyr)

tsibbledata::aus_retail \%>\%
  filter(
    State == "Victoria",
    Industry == "Cafes, restaurants and catering services"
  ) \%>\%
  model(ARIMA(Turnover ~ pdq(0,1,1) + PDQ(0,1,1))) \%>\%
  gg_arma()
}
}
