% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rollMAC.R
\name{rollMAC}
\alias{rollMAC}
\title{Apply an Moving Average Cross-over}
\usage{
rollMAC(x, mashort, malong)
}
\arguments{
\item{x}{An xts object}

\item{mashort}{An integer indicating the number of periods in the short MA.}

\item{malong}{An integer indicating the number of periods in the long MA.}
}
\description{
Apply a moving average crossover to one or multiple columns of an xts object
in a rolling fashion.
}
