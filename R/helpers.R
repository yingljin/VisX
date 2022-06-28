# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format


#' Correlation matrix
#' @description Calculate pairwise correlation matrix with significance
#'
#' @param cor_value association matrix
#' @param cor_p significance of association test
#' @param removeTriangle display correlation table as upper or lower triangular matrix
#' @param result output format as none, html or latex
#'
#' @return Correlation coefficient and significance of correlation test between each pair of varaibles
#' @export
#' @examples
#' data(mtcars)
#' types <- rep("numeric", ncol(mtcars))
#' test <- pairwise_cor(mtcars, types)
#' corstars <-function(test$cor_value, test$cor_p)
#'
corstars <-function(cor_value, cor_p,
                    removeTriangle=c("lower"),
                    result=c("none", "html", "latex")){


  R <- cor_value # Matrix of correlation coeficients
  p <- cor_p # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- round(R, 2)

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(R))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(R)
  colnames(Rnew) <- paste(colnames(R), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])

  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    Rnew <- cbind(Rnew[1:length(Rnew)-1,])
  }

  ## remove last column and return the correlation matrix
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}


#' Adjusted R-squared
#' @description Compute adjusted R-squared for each variable
#' @details Adjusted R-squared is a measure of collinearity,
#' defined as how one variable is linearly associated with all other variables in the data set.
#' @param df data frame or matrix
#'
#' @return A table with values of adjusted R squared of each variable
#'
#' @examples
#' df <- data.frame(X1 = rnorm(100), X2 = rnorm(100), X3 = rnorm(100))
#' get_r2j(df)
get_r2j <- function(df) {
  r2_j <- numeric(ncol(df))
  for(j in 1:ncol(df)) {
    r2_j[j] <- summary(lm(df[,j] ~ as.matrix(df[,-j])))$r.squared
  }
  r2_j
}



