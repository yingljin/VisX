% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figure_functions.R
\name{make_hist}
\alias{make_hist}
\title{Visualize numeric variables}
\usage{
make_hist(df_hist)
}
\arguments{
\item{df_hist}{A wide-format data include all numeric variables to visualize}
}
\value{
Annotated histograms in numeric variable tab
}
\description{
This functions generates histograms and marks out mean and
one standard deviation band around the mean for each numeric variable
}
\details{
This function only run internally in VisX application because it depends on
 reactive values from user input
}
