% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{multiviz}
\alias{multiviz}
\title{visualize aspects of MSIsensor/expression relationships}
\usage{
multiviz(
  tum = "MESO",
  gene = "TYMS",
  intrans = log10pl1(p = 1),
  inmeth = "auto",
  topmsi = Inf,
  indata,
  nvar = 6
)
}
\arguments{
\item{tum}{a TCGA tumor code}

\item{gene}{a gene symbol used in the indata data.frame}

\item{intrans}{an instance of the trans() transformation method of scales package}

\item{inmeth}{a valid setting for method parameter for geom_smooth}

\item{topmsi}{maximum numeric value for x-axis when plotting against MSI value}

\item{indata}{a data.frame instance with values for acronym, gene, msival}

\item{nvar}{numeric() number of variables to show in biplot}
}
\description{
visualize aspects of MSIsensor/expression relationships
}
