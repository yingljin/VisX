
#### histograms #####

#' Visualize numeric variables
#' @description This functions generates histograms and marks out mean and
#' one standard deviation band around the mean for each numeric variable
#' @details This function only run internally in VisX application because it depends on
#'  reactive values from user input
#'
#' @param df_hist A wide-format data include all numeric variables to visualize
#'
#' @return Annotated histograms in numeric variable tab
#' @import ggplot2
#'

make_hist <- function(df_hist){
  p <- df_hist %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    mutate(ave = mean(value, na.rm = T),
           sd = sd(value, na.rm = T),
           N = sum(!is.na(value))) %>%
    mutate(name = paste(name, " (N = ", N, ")", sep = "")) %>%
    select(-N) %>%
    ggplot(aes(x = value))+geom_histogram()+
    geom_vline(aes(xintercept = ave), col = "red", linetype = 1)+
    geom_vline(aes(xintercept = ave-sd), col = "red", linetype = 2)+
    geom_vline(aes(xintercept = ave+sd), col = "red", linetype = 2)+
    facet_wrap(~name, scales = "free")+
    theme(strip.text = element_text(size = 10))+
    labs(x = "", y = "")
  p
}

##### bar plots #####

#' Visualize categorical variables
#' @description This functions generates barplots for each categorical variable
#' @details This function only run internally in VisX application because it depends on
#'  reactive values from user input
#'
#' @param df_bar A wide-format data include all categorical variables to visualize
#'
#' @return Barplots in categorical variable tab
#' @import ggplot2
#'

make_bar <- function(df_bar){
  p <- df_bar %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    mutate(N = sum(!is.na(value))) %>%
    mutate(name = paste(name, " (N = ", N, ")", sep = "")) %>%
    select(-N) %>%
    ggplot(aes(x = value))+geom_histogram(stat = "count")+
    facet_wrap(~name, scales = "free_x")+
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
          strip.text = element_text(size = 10))
  p
}
