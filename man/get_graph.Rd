% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataAcquisition.R
\name{get_graph}
\alias{get_graph}
\title{get_graph}
\usage{
get_graph(DT, plot_var, group_var)
}
\arguments{
\item{DT}{A data.table.}

\item{plot_var}{A character vector that has the variables for y axis}

\item{group_var}{A character vector that has the variables for x axis -
grouping variable}
}
\value{
\eqn{plot_var * group_var} ggplot objects
}
\description{
Experimenting. Might not necessary. Plot box_plot for all
 variables in the \strong{Variable} table by the value group.
}
\examples{
get_graph(DT, plot_var, group_var)
}
