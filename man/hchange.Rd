% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hill.R
\name{hchange}
\alias{hchange}
\title{Hill Climbing Slight Random Change Method}
\usage{
hchange(par, lower, upper, dist, round = TRUE, ...)
}
\arguments{
\item{par}{Initial solution}

\item{lower}{Vector with lowest values for each dimension}

\item{upper}{Vector with highest values for each dimension}

\item{dist}{random distribution function}

\item{round}{use integer (TRUE) or continuous (FALSE) search}

\item{...}{Extra parameters for dist}
}
\value{
Updated solutions
}
\description{
Hill Climbing Slight Random Change Method
}
