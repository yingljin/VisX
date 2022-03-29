
#### update data before transformation #####

#' Update the data reactively
#'
#' @param df_lst
#'
#' @return
#' @export
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols
#'
#' @examples
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
#'
#' @param df_lst
#'
#' @return
#' @export
#'
#' @examples
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

#' Function to make histograms
#'
#' @param df_hist
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
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

#' Function to make barplots
#'
#' @param df_bar
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
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
