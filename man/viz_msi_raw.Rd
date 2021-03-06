% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{viz_msi_raw}
\alias{viz_msi_raw}
\title{small app to survey MSIsensor against expression}
\usage{
viz_msi_raw(df, inmeth = MASS::rlm, nvar = 3)
}
\arguments{
\item{df}{a data.frame instance}

\item{inmeth}{a method for geom_smooth}

\item{nvar}{number of variables to show in biplot}
}
\description{
small app to survey MSIsensor against expression
}
\note{
Use ask=FALSE if running example.
}
\examples{
if (interactive()) viz_msi_raw(BiocOncoTK::small_msi, nvar=3)
}
