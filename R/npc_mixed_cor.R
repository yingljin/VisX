
#' Network Plot of Association
#'
#' @param cor_value a matrix with values of association measures
#' @param cor_type a matrix with types of association measures
#' @param cor_p a matrix with p values of association tests
#' @param var_type a character vector with types of variables association is measured on
#' @param min_cor minimum association to be visualized
#' @param show_signif whether or not to visualize significance of association
#' @param sig.level maximum significance to be visualized
#' @param overlay if TRUE, only show significant association above min_cor threshold
#' @param legend if TURE, how legend
#' @param repel if TRUE, text labels of variable names repel each other
#' @param label_size size of text labels of variable names
#'
#' @return A network plot of association
#'
#' @importFrom Hmisc rcorr
#' @importFrom nnet multinom
#' @importFrom DescTools PseudoR2
#' @importFrom car Anova
#' @importFrom MESS gkgamma
#' @import ggplot2
#' @import ggnewscale
#' @import ggrepel
#' @importFrom stats as.formula ave cmdscale lm rnorm sd
#' @importFrom utils read.csv sessionInfo
#'
#' @examples
#' data("mtcars")
#' type1 <- c(rep("numeric", 7), rep("factor", 2), rep("ordinal", 2))
#' test_cor1 <- pairwise_cor(mtcars, type1)
#' npc_mixed_cor(test_cor1$cor_value, test_cor1$cor_type, test_cor1$cor_p, type1)
#'
#'
npc_mixed_cor <- function (cor_value, cor_type, cor_p, var_type,
                           sig.level = 0.05, min_cor = 0.3,
                           show_signif = FALSE,
                           legend = TRUE, repel = TRUE, label_size = 5,
                           overlay = TRUE){

  # var_type <- relevel(as.factor(var_type), ref = "numeric")
  var_type <- factor(var_type, levels = c("numeric", "factor", "ordinal"))
  ## A few checks
  if(min_cor < 0 || min_cor > 1){
    stop("min_cor must be a value ranging from zero to one.")
  }
  if(!isSymmetric(cor_value) | !isSymmetric(cor_type)){
    stop("Correlation matrix or type not valid")
  }

  # create dissimilarity matrix
  dissim <- 1-abs(cor_value)

  # Calculate coordinates of variables
  points <-
    if (ncol(dissim) == 1) {
      matrix(c(0, 0), ncol = 2, dimnames = list(colnames(dissim)))
    }  else if (ncol(dissim) == 2) {
      matrix(c(0, -0.1, 0, 0.1), ncol = 2, dimnames = list(colnames(dissim)))
    }  else {
      suppressWarnings(cmdscale(dissim, k = 2))
    }
  if (ncol(points) < 2) {
    cont_flag <- FALSE
    shift_matrix <- matrix(1, nrow = nrow(dissim), ncol = ncol(dissim))
    diag(shift_matrix) <- 0
    for (shift in 10^(-6:-1)) {
      shifted_distance <- dissim + shift * shift_matrix
      points <- suppressWarnings(stats::cmdscale(shifted_distance))
      if (ncol(points) > 1) {
        cont_flag <- TRUE
        break
      }
    }
    if (!cont_flag)
      rlang::abort("Can't generate network plot.\nAttempts to generate 2-d coordinates failed.")
    rlang::warn("Plot coordinates derived from correlation matrix have dimension < 2.\nPairwise distances have been adjusted to facilitate plotting.")
  }
  points <- data.frame(points)
  colnames(points) <- c("x", "y")
  points$id <- rownames(points)

  # edges according to value of correlation
  proximity <- abs(cor_value)
  proximity[upper.tri(proximity)] <- NA
  diag(proximity) <- NA
  proximity[proximity < min_cor] <- NA
  n_paths <- sum(!is.na(proximity))
  # do not do this if no correlation is over threshold
  if(n_paths>0){
    paths <- data.frame(matrix(nrow = n_paths, ncol = 7))
    colnames(paths) <- c("x", "y", "xend", "yend", "proximity", "sign", "type")
    # proximity - color, transparancy, thickness of edges
    # sign - direction of correlation
    path <- 1
    for (row in 1:nrow(proximity)) {
      for (col in 1:ncol(proximity)) {
        path_proximity <- proximity[row, col]
        if (!is.na(path_proximity)) {
          path_sign <- sign(cor_value[row, col])
          x <- points$x[row]
          y <- points$y[row]
          xend <- points$x[col]
          yend <- points$y[col]
          paths[path, ] <- c(x, y, xend, yend, path_proximity,
                             path_sign, cor_type[row, col])
          path <- path + 1
        }
      }
    }
    paths <- paths %>% mutate_at(vars(-type), as.numeric)}

  # edges based on significance
  if(show_signif){
    proximity2 <- cor_p
    proximity2[upper.tri(proximity2)] <- NA
    diag(proximity2) <- NA
    proximity2[proximity2 > sig.level] <- NA
    ### whether or not show all significance
    if(overlay){
    proximity2[is.na(proximity)] <- NA
    }
    n_paths2 <- sum(!is.na(proximity2))
    # do not do this if no correlation is over threshold
    if(n_paths2>0){
      paths2 <- data.frame(matrix(nrow = n_paths2, ncol = 5))
      colnames(paths2) <- c("x", "y", "xend", "yend", "proximity")
      path <- 1
      for (row in 1:nrow(proximity2)) {
        for (col in 1:ncol(proximity2)) {
          path_proximity <- proximity2[row, col]
          if (!is.na(path_proximity)) {
            x <- points$x[row]
            y <- points$y[row]
            xend <- points$x[col]
            yend <- points$y[col]
            paths2[path, ] <- c(x, y, xend, yend, path_proximity)
            path <- path + 1
          }
        }
      }
      }
  }

  # plot correlation
  ## color by correlation type, point shape by variable type
  color_var <- ifelse(paths$type=="pseudoR2", "nodir", "dir")
  paths <- split(paths, f = color_var)
  ## add var_type in points
  ## plot using different colorscales

  ## first, plot variable as points
  npc <- ggplot()+
    geom_point(data = points, aes(x,y, shape = var_type), size = 3,
             alpha=1, colour = "black")+
    scale_shape_manual(name = "Variable type",
                       breaks = c("numeric", "factor", "ordinal"),
                       values = c("numeric"=1, "factor"=2, "ordinal"=5),
                       labels = c("numeric", "nominal",  "ordinal"))


  ## Then, the correlations as edges
  ## directional
  if(!is.null(paths$dir)){
    npc <- npc+
      geom_curve(data = paths$dir,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     alpha = proximity, size = proximity,
                     color = proximity*sign))+
      scale_alpha(limits = c(0, 1))+
      scale_size(limits = c(0, 1))+
      scale_color_gradientn(name = "Spearman/GKgamma", limits = c(-1, 1),
                            colors = c("indianred2","white", "skyblue1"))
  }
  ## indirectional
  if(!is.null(paths$nodir)){
    npc <- npc+new_scale_color()+
      geom_curve(data = paths$nodir,
                 aes(x = x, y = y, xend = xend, yend = yend,
                    alpha = proximity, size = proximity,
                    color = proximity))+
      scale_color_gradientn(name = "PseudoR", limits = c(0, 1),
                            colors = c("white", "darkorange"))
    }

  # add significance level
  if(show_signif & n_paths2>0){
    npc <- npc+
      geom_curve(data = paths2,
                aes(x = x, y = y, xend = xend, yend = yend, alpha = 0.5))
  }

  # add variable names and legend
  npc_labs <- list(# variable label
              if(repel)
                ggrepel::geom_text_repel(data = points,aes(x, y, label = id),
                                         fontface = "bold",size = label_size,
                                         segment.size = 0,
                                         segment.color = "white"),
              if(!repel)
                geom_text(data = points,aes(x, y, label = id),
                          fontface = "bold",
                          size = label_size),
              expand_limits(x = c(min(points$x) - 0.1, max(points$x) +0.1),
                            y = c(min(points$y) - 0.1, max(points$y) +0.1)),
              theme_void(),
              guides(size = "none", alpha = "none"),
              # legend
              if (legend)
                theme(legend.text = element_text(size = 10),
                      legend.title = element_text(face = "bold"),
                      legend.key.size = unit(0.7, "cm"),
                      legend.spacing.y = unit(0.5, "cm")),
              if (!legend)
                theme(legend.position = "none"))
  # fianl plot
  npc+npc_labs
}




