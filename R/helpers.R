# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format

#' Create a table for correlation with significance
#'
#' @param x a data frame or matrix
#' @param method spearman or pearson
#' @param removeTriangle display correlation table as upper or lower trianglur matrix
#' @param result output as none, html or latex
#'
#' @return correlation and significance of input data set
#' @export
#' @import corrr
#' @importFrom Hmisc rcorr
#' @examples
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


#' Computed adjusted R-squared
#'
#' @param df a data frame or matrix
#'
#' @return a table with adjusted R squared of each variables verses all other variables
#' @export
#'
#' @examples
get_r2j <- function(df) {
  r2_j <- numeric(ncol(df))
  for(j in 1:ncol(df)) {
    r2_j[j] <- summary(lm(df[,j] ~ as.matrix(df[,-j])))$r.squared
  }
  r2_j
}



