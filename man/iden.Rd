% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iden.R
\name{iden}
\alias{iden}
\title{ARIMA Model Identification}
\usage{
iden(
  data.tsd,
  seasonal = tsp(data.tsd)[3],
  gamma = 1,
  m = 0,
  d = 0,
  D = 0,
  lag.max = 38,
  print.table = FALSE,
  seasonal.lags = FALSE
)
}
\arguments{
\item{data.tsd}{Time series data object (output from the tsd function).}

\item{seasonal}{Optional. The number of observations in a period for a seasonal timeseries. By default this value is automatically taken from the time-series data object (.tsd object).}

\item{gamma}{Optional. The Box-Cox transformation power parameter. The default value is gamma=1 (no transformation).}

\item{m}{Optional. The constant that is added to the response variable before the data are transformed. The default value is m=0.}

\item{d}{The number of regular differences carried out on the data. If a value is not specified then the default value is d=0.}

\item{D}{Optional. The number of seasonal differences carried out on the data. The default value is D=0.}

\item{lag.max}{Optional. The maximum number of lags at which to estimate the ACF and PACF. The default value is lag.max=38}

\item{print.table}{Optional. Default value is print.table = FALSE. If print.table = TRUE, R will output a table of the ACF and PACF estimates.}

\item{seasonal.lags}{if TRUE will label ACF and PACF lags in seasonal multiples. Default is FALSE.}
}
\value{
Invisible list containing the numerical values of the ACF and PACF estimates that were plotted.
}
\description{
Graphical outputs to help tentatively identify a ARIMA model. The plots produced are: plot of the original or transformed data, a range-mean plot of the transformed data, and plots of the sample ACF and PACF.
}
\examples{
iden(spot.tsd)

Passengers.ts <- ts(Passengers, freq=12, start=1949)
Passengers.tsd <- tsd(Passengers.ts, data.title='International Airline Passengers',
                      time.units='Year',response.units='Thousands of Passengers')
iden(Passengers.tsd)
iden(Passengers.tsd, d=1, D=1)
iden(Passengers.tsd, d=1, D=1, gamma=0)

}
