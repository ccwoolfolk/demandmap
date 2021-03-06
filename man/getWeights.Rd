% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getWeights}
\alias{getWeights}
\title{Retrieve weights for a given set of labels from a source table}
\usage{
getWeights(labels, wts)
}
\arguments{
\item{labels}{vector of NAICS.id values, column names, etc. [n]}

\item{wts}{matrix[n, 2] with labels in col 1 and weights in col 2 (0.5, etc)}
}
\value{
Vector of numeric values repsenting weights for corres. labels
}
\description{
Retrieve weights for a given set of labels from a source table
}
