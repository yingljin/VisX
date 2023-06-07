






#' Run the VisX application
#'
#' @description This function starts the VisX application for basic data pre-processing and variables selection
#' @details This application currently supports files in csv format.
#' @details Application interface will pop up after calling this function;
#' data can be uploaded in that interface;
#' close the interface or interrupt R to stop the application.
#'
#' @return Histograms for continuous variables;
#' @return Barplots for discrete variables;
#' @return Correlation diagram;
#' @return Variance inflation factors (VIFs) and R-squared;
#' @return Correlation coefficient and p values from correlation test
#'
#' @export
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import here
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols
#' @importFrom kableExtra kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra scroll_box
#' @importFrom car vif
#' @import knitr
#' @import magrittr
#' @import readr
#' @import stringr
#' @import tibble
#' @importFrom tidyr pivot_longer
#' @importFrom nnet multinom
#'


VisX <- function(){
  shinyApp(ui = ui, server = server, enableBookmarking = "server", options = list(width = 1600, height = 900))
}
