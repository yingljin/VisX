

#' Check for columns that are empty/identical
#'
#' @param df dataframe to check
#'
#' @return a warning message if there exists empty columns or columns with no variation in the data
#'
#' @examples
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = NA, x4 = 1)
#' data_check(df)

data_check <- function(df){

  # check for empty columns
  empty_col <- apply(df, 2, function(x){sum(!(is.na(x)|x==""|is.null(x)))})
  empty_name <- colnames(df)[empty_col==0]
  if(length(empty_name)>0){
    empty_name <- paste(empty_name, collapse = ", ")
    mes1 = paste("Warning:", empty_name, "missing for all observations")
    warning(mes1)
  }

  # check for columns with no variation
  identical_col <- apply(df, 2, function(x)length(unique(x)))
  identical_name <- colnames(df)[identical_col==1]
  identical_name <- identical_name[!identical_name %in% empty_name]
  if(length(identical_name)>0){
    identical_name <- paste(identical_name, collapse = ", ")
    mes2 = paste("Warning:", identical_name, "with same value for all observations")
    warning(mes2)
  }
}

