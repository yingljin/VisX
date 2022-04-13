# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format


#' Correlation matrix
#' @description Calculate pairwise correlation matrix with significance
#'
#' @param x data frame or matrix
#' @param method method to calculate correlation coefficient; spearman or pearson
#' @param removeTriangle display correlation table as upper or lower triangular matrix
#' @param result output format as none, html or latex
#'
#' @return Correlation coefficient and significance of correlation test between each pair of varaibles
#' @export
#' @import corrr
#' @importFrom Hmisc rcorr
#' @examples
#' data(mtcars)
#' corstars(mtcars, method = "spearman")
corstars <-function(x, method=c("spearman", "pearson"), removeTriangle=c("lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix

  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

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



