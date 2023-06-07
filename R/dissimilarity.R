
# this script writes functions for calculating measures of association
# between two variables

#### association ####

#' Calculate association measure between a pair of variables
#'
#' @param df a dataframe with 2 columns
#' @param type a vector with 2 elements corresponding to types of variables in df.
#' Types can be "numeric", "ordinal" or "factor",
#' but cannot both be numeric.
#'
#' @return values and type of association measure,
#' as well as p value from corresponding association test
#'
#' @details The following associated measures and tests are implemented dependent on variable type:
#'
#' factor vs numeric, factor or ordinal: Pseudo R^2 and p value from multinomial regression
#'
#' ordinal vs ordinal or numeric: GK gamma and GK gamma correlation test
#'
#' @importFrom nnet multinom
#' @importFrom DescTools PseudoR2
#' @importFrom car Anova
#' @importFrom MESS gkgamma
#'
#' @examples
#' data1 <- data.frame(x = rnorm(10), y = rbinom(10, 1, 0.5))
#' # second variable as factor
#' type1 <- c("numeric", "factor")
#' pair_cor(data1, type1)
#' # second variable as ordinal
#' type2 <- c("numeric", "ordinal")
#' pair_cor(data1, type2)
#'
pair_cor <- function(df, type){

  # check
  if(type[1]=="numeric"&type[2]=="numeric"){
    stop("Not supported")
  }
  # pseudo R^2
  if("factor" %in% type){
    # multinomial regression
    varnames <- colnames(df)
    f <- ifelse(length(unique(df[, 1])) <=  length(unique(df[, 2])),
                paste(varnames[1], "~", varnames[2]),
                paste(varnames[2], "~", varnames[1]))
    f <- as.formula(f)
    mult_fit <- try(multinom(f, data = df, model = T, trace = FALSE), silent = TRUE)
    cor_type <- "pseudoR2"

    if(!("try-error" %in% class(mult_fit)) ) {
      cor_value <- PseudoR2(mult_fit, which = "Nagelkerke")
      cor_value <- sqrt(cor_value)
      discard <- capture.output(cor_p <- Anova(mult_fit, trace = FALSE)$`Pr(>Chisq)`)
    } else if (all(df[,1] == df[,2])) {
      cor_value <- 1
      cor_p <- NA
    } else {
      cor_value <- 1
      cor_p <- NA
    }

  }
  # GK gamma
  else{
    #cor_value <- GoodmanKruskalGamma(df[, 1], df[, 2])
    cor_test <- gkgamma(table(df))
    cor_value <- cor_test$estimate
    cor_p <- cor_test$p.value
    cor_type <- "GKgamma"
  }

  return(list(cor_value = cor_value, cor_type = cor_type, cor_p = cor_p))
}

##### pairwise association #####

#' Calculate pariwise association of data with mixed types of variables
#'
#' @param df dataframe with mixed types of variables
#' @param var_type a charater vector corresponding to types of variables in df
#'  if not provided, will guess.
#'
#' @return matrices with measures, types and p values of association
#'
#' @details The following associated measures and tests are implemented dependent on variable type:
#'
#' factor vs numeric, factor or ordinal: Pseudo R^2 and p value from multinomial regression
#'
#' ordinal vs ordinal or numeric: GK gamma and GK gamma correlation test
#'
#' numeric vs numeric: Spearman correlation and p value
#'
#' @importFrom Hmisc rcorr
#' @importFrom nnet multinom
#' @importFrom DescTools PseudoR2
#' @importFrom car Anova
#' @importFrom MESS gkgamma
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
#' data1 <- data.frame(x = rnorm(10),
#'  y = rbinom(10, 1, 0.5),
#' z = rbinom(10, 5, 0.5))
#' type1 <- c("numeric", "factor", "ordinal")
#' pairwise_cor(data1, type1)
#'
pairwise_cor <- function(df, var_type = NULL){

  # Guesses if not specified
  if(!length(var_type)) {
    types <- sapply(df, class)
    var_type <- factor(types,
                    levels = c("numeric", "integer", "factor", "character", "logical", "NULL"),
                    labels = c("numeric", "numeric", "factor", "factor", "factor", "factor"))
  }


  # set-up
  p <- ncol(df)
  id_num <- which(var_type == "numeric")
  id_non_num <- which(var_type != "numeric")

  # container
  cor_value_mat <- matrix(NA, p, p)
  cor_type_mat <- matrix(NA, p, p)
  cor_p_mat <- matrix(NA, p, p)

  colnames(cor_value_mat) <- rownames(cor_value_mat) <- colnames(df)
  colnames(cor_type_mat) <- rownames(cor_type_mat) <- colnames(df)
  colnames(cor_p_mat) <- rownames(cor_p_mat) <- colnames(df)

  # calculate correlation
  ## numeric variables alone
  if(!is.null(id_num) & length(id_num!=0)){
    num_cor <-  rcorr(as.matrix(df[, id_num]), type = "spearman")
    cor_value_mat[id_num, id_num] <- num_cor$r
    cor_p_mat[id_num, id_num] <- num_cor$P
    cor_type_mat[id_num, id_num] <- "spearman"
  }

  ## other types
  if(!is.null(id_non_num)){
    for(i in  1:p){
      for(j in id_non_num){
        this_cor <- pair_cor(df[, c(i, j)], var_type[c(i, j)])
        cor_value_mat[i, j] <- cor_value_mat[j, i] <- this_cor$cor_value
        cor_type_mat[i, j] <- cor_type_mat[j, i] <- this_cor$cor_type
        cor_p_mat[i, j] <- cor_p_mat[j, i] <- this_cor$cor_p
      }
    }
  }

  return(list(cor_value = cor_value_mat, cor_type = cor_type_mat, cor_p = cor_p_mat, var_type = var_type))
}


