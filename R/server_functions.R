
#### update data before transformation #####

#' Initialize data and generate reactive input panel
#' @description Update data sets reactively with data initialization,
#' as well as input panel in following tabs
#' @details This function only run internally in VisX application because it depends on
#'  reactive values from user input
#' @param df_lst A list of datasets generated from user upload
#'
#' @return A list of initialized datasets
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols
#'
update_reactive_df <- function(df_lst){

  # if the data has categorical  variables
  if(sum(sapply(df_lst$df_cat, function(x){!is.numeric(x)}))>0){
    df_lst$options = df_lst$df_cat %>%
      dummy_cols(ignore_na = T, remove_most_frequent_dummy = F) %>%
      clean_names(case = "none") %>%
      select(is.numeric) %>%
      colnames()
    df_lst$default = df_lst$df_cat %>%
      dummy_cols(ignore_na = T, remove_most_frequent_dummy = T) %>%
      clean_names(case = "none") %>%
      select(is.numeric) %>%
      colnames()
    df_lst$df_num = df_lst$df_cat %>%
      dummy_cols(ignore_na = T, remove_most_frequent_dummy = T) %>%
      clean_names(case = "none") %>%
      select(is.numeric)}
  # if data has no categorical variables
  else{
    df_lst$options = df_lst$df_cat %>% colnames()
    df_lst$default = df_lst$df_cat %>% colnames()
    df_lst$df_num = df_lst$df_cat
  }

}

##### Update data after transformation #####

#' Update data after pre-processing operation
#'
#' @param df_lst A list of datasets
#' @description Update data sets reactively with pre-processing operations specified by user,
#' as well as input panel in following tabs
#' @details This function only run internally in VisX application because it depends on
#'  reactive values from user input
#' @return A list of pre-processed datasets
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols

update_transform_df <- function(df_lst){

  # if data has categorical variables
  if(sum(sapply(df_lst$df_cat, function(x){!is.numeric(x)}))>0){
    df_lst$df_num = df_lst$df_cat %>%
      select(-all_of(df_lst$remove)) %>%
      dummy_cols(ignore_na = T, remove_most_frequent_dummy = T, remove_selected_columns = T) %>%
      clean_names(case = "none") %>%
      select(is.numeric)
    df_lst$default = df_lst$df_num %>% colnames()
    df_lst$options = df_lst$df_cat %>%
      dummy_cols(ignore_na = T, remove_most_frequent_dummy = F, remove_selected_columns = T) %>%
      clean_names(case = "none") %>%
      colnames()
  }
  else{
    df_lst$df_num = df_lst$df_cat %>%
      select(-all_of(df_lst$remove))
    df_lst$default = df_lst$df_num %>% colnames()
    df_lst$options = c(df_lst$default, df_lst$remove)
  }
  df_lst$df_all = df_lst$df_cat
}

#### Data for correlation diagram #####

#' Prepare data for correlation diagram
#' @description This function finds the proper data set for correlation diagram and statistics
#' @details This function only run internally in VisX application because it depends on
#'  reactive values from user input.
#'  The purpose is to allow users to apply pre-processing operations iteratively without
#'  resetting to the initial status.
#'
#' @param df_lst A list of datasets with all pre-processed and selected variables
#'
#' @return Data frame to visualize and used to computed multicollinearity/correlation statistics

df_for_figure <- function(df_lst){
   # to save current status of correlation panel
  if(is.null(df_lst$new_df_num)){
    df <- as.matrix(df_lst$df_num)
  }
  else{
    df <- as.matrix(df_lst$new_df_num)
  }
  return(df)
}



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
