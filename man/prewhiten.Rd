% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prewhiten.R
\name{prewhiten}
\alias{prewhiten}
\title{Prewhitening}
\usage{
prewhiten(y.tsd, x.tsd, x.model, x.coefficients, y.model, y.coefficients)
}
\arguments{
\item{y.tsd}{The output (y) time series.}

\item{x.tsd}{The input (x) time series.}

\item{x.model}{The assumed model for x.}

\item{x.coefficients}{The model coefficients for x.}

\item{y.model}{(optional) The assumed model for y (default is the x model).}

\item{y.coefficients}{(optional) The model coefficients for y (default is the x model coefficcients).}
}
\value{
The numerical values of the cross correlation are invisibly returned.
}
\description{
Performs a prewhitening operation to allow one to identify the dynamic relationship betweenan input time series (x) and an output time series (y). By default both the x and the y time series are filtered with the same model to compute residuals and then the cross correlation function between those 'residuals' is plotted, to identify the lags at which y sepends on x in the dynamic regression model.
}
\examples{
iden(gasrx.tsd)
gasrx.ar4.out <- esti(gasrx.tsd, model=model.pdq(p=4))
prewhiten(gasry.tsd, gasrx.tsd, x.model=model.pdq(p=4),
      x.coefficients=coef(gasrx.ar4.out))

}
