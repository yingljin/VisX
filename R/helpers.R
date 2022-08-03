
#### correlation matrix #####
#' Correlation and association matrix
#' @description Calculate pairwise correlation matrix with significance marked by stars.
#' Columns and rows are grouped by variable type
#'
#' @param cor_value association matrix
#' @param cor_p significance of association test
#' @param var_type type of variables that correlation and association is calculated for
#'
#' @return Correlation coefficient and significance of correlation test between each pair of variables,
#' as well as variable type corresponding to rows and columns for display in shiny app
#' @export
#' @examples
#' data(mtcars)
#' library(VisX)
#' types <- rep("numeric", ncol(mtcars))
#' test <- pairwise_cor(mtcars, types)
#' type <- c("numeric", "factor",  rep("numeric", 5), rep("factor", 2), rep("ordinal", 2))
#' corstars(test$cor_value, test$cor_p, type)
#'
corstars <-function(cor_value, cor_p, var_type){


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

  ## order rows and columns by variable type
  var_type <- factor(var_type, levels = c("numeric", "factor", "ordinal"))
  idx <- order(var_type)
  Rnew <- Rnew[idx, idx]

  ## remove lower triangle of correlation matrix and empty row and columns
  Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  Rnew <- Rnew[1:length(Rnew)-1, 2:ncol(Rnew)]

  ## index for group rows and columns
  row_id <- var_type[idx][1:length(var_type)-1]
  col_id <- var_type[idx][-1]

  return(list(Rnew=Rnew, row_id=row_id, col_id=col_id))
}


##### statistics #####
#' R-squared
#' @description Compute R-squared for each variable versus all other covariates
#' @details R-squared is a measure of collinearity,
#' defined as how one variable is linearly associated with all other variables in the data set.
#' @param df data frame
#' @param type type corresponding to columns in df
#'
#' @importFrom nnet multinom
#' @importFrom DescTools PseudoR2
#'
#' @return A vector with values of R squared of each variable
#'
#' @examples
#' df <- data.frame(X1 = rnorm(100), X2 = rnorm(100), X3 = rnorm(100))
#' get_r2j(df, rep("numeric", 3))

get_r2 <- function(df, type) {

  r2 <- numeric(ncol(df))
  vars <- colnames(df)
  for(i in seq_along(r2)){
    # formula
    f <- as.formula(paste(vars[i], "~ ."))
    # numeric
    if(type[i] == "numeric"){
      r2[i] <- summary(lm(f, df))$r.squared
    }
    # factor
    else if(type[i] == "factor"){
      mult_fit <- multinom(f, data = df, model = T)
      r2[i] <- PseudoR2(mult_fit, which = "Nagelkerke")

    }
    # ordinal
    else if(type[i] == "ordinal"){
      df[, i] <- as.numeric(as.factor(df[, i]))
      r2[i] <- summary(lm(f, df))$r.squared
    }
  }

  return(r2)
}


