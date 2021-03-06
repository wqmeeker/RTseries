% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iden.sim.R
\name{iden.sim}
\alias{iden.sim}
\title{Model Identification Using Simulated Data}
\usage{
iden.sim(realization.size = 100, model, lag.max = 20, plot.true = FALSE)
}
\arguments{
\item{realization.size}{Size of the sample realization to be generated (default is 100).}

\item{model}{ARMA model. If no model is specified, a random model will be generated.}

\item{lag.max}{Maximum lag for the ACF and PACF functions (default is 20).}

\item{plot.true}{If plot.true = TRUE the function true ACF/PACF functions will display as the model is revealed.}
}
\value{
the last simulated data set is returned invisibly.
}
\description{
This function will simulate samples from an ARMA model, plot the data, compute and plot sample ACF and PACF functions (using the \code{\link{iden}} function), and let user gain experience of tentatively identifying a time series model. After each simulation, the user will be prompted to ask if another set of simulated data (from the same model) should be genrated or not. When the answer is no (or n), the model and its parameters are displayed and the user is asked whether a new model should be simulated or not.
}
\examples{
iden.sim()
iden.sim(model=list(ar=c(.9, -.5)))
iden.sim(model=list(ar=c(.9,-.5),ma=c(.1,.1)))

}
