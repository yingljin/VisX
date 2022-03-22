#' Create a correlation network plot with significance lines
#'
#' @param df the data set to visualize
#' @param method spearman or pearson
#' @param sig,level the level at which to display significance lines
#' @param min_cor the threshold correlation to display
#' @param legend legend for correaltion
#' @param colours see network_plot
#' @param repel see network_plot
#' @param colors see network_plot
#' @import corrr
#' @import rlang
#' @import ggrepel
#' @importFrom Hmisc rcorr
#' @importFrom stats cmdscale

network_plot_wsig <- function (df, method = "spearman", sig.level = 0.05,
                               min_cor = 0.3, legend = TRUE, overlay = TRUE,
                               colours = c("indianred2","white", "skyblue1"),
                               repel = TRUE, colors) {
  ## A few checks
  if (min_cor < 0 || min_cor > 1) {
    stop("min_cor must be a value ranging from zero to one.")
  }
  if (!missing(colors))
    colours <- colors

  # create correlation matrix
  rdf <- correlate(df, method = method)
  pdf <- rcorr(as.matrix(df), type = method)$P

  # code borrowed from network_plot
  # for correlation plot
  rdf <- as_matrix(rdf, diagonal = 1)
  distance <- 1 - abs(rdf)
  points <-
    if (ncol(rdf) == 1) {
      matrix(c(0, 0), ncol = 2, dimnames = list(colnames(rdf)))
  }  else if (ncol(rdf) == 2) {
    matrix(c(0, -0.1, 0, 0.1), ncol = 2, dimnames = list(colnames(rdf)))
  }  else {
    suppressWarnings(cmdscale(distance, k = 2))
  }
  if (ncol(points) < 2) {
    cont_flag <- FALSE
    shift_matrix <- matrix(1, nrow = nrow(rdf), ncol = ncol(rdf))
    diag(shift_matrix) <- 0
    for (shift in 10^(-6:-1)) {
      shifted_distance <- distance + shift * shift_matrix
      points <- suppressWarnings(cmdscale(shifted_distance))
      if (ncol(points) > 1) {
        cont_flag <- TRUE
        break
      }
    }
    if (!cont_flag)
      abort("Can't generate network plot.\nAttempts to generate 2-d coordinates failed.")
      warn("Plot coordinates derived from correlation matrix have dimension < 2.\nPairwise distances have been adjusted to facilitate plotting.")
  }
  points <- data.frame(points)
  colnames(points) <- c("x", "y")
  points$id <- rownames(points)

  proximity <- abs(rdf)
  proximity[upper.tri(proximity)] <- NA
  diag(proximity) <- NA
  proximity[proximity < min_cor] <- NA
  n_paths <- sum(!is.na(proximity))
  paths <- data.frame(matrix(nrow = n_paths, ncol = 6))
  colnames(paths) <- c("x", "y", "xend", "yend", "proximity", "sign")
  path <- 1
  for (row in 1:nrow(proximity)) {
    for (col in 1:ncol(proximity)) {
      path_proximity <- proximity[row, col]
      if (!is.na(path_proximity)) {
        path_sign <- sign(rdf[row, col])
        x <- points$x[row]
        y <- points$y[row]
        xend <- points$x[col]
        yend <- points$y[col]
        paths[path, ] <- c(x, y, xend, yend, path_proximity,
                           path_sign)
        path <- path + 1
      }
    }
  }

  ## code from Ryan for p value plot
  proximity2 <- abs(pdf)
  proximity2[upper.tri(proximity2)] <- NA
  diag(proximity2) <- NA
  proximity2[proximity2 > sig.level] <- NA
  ### whether or not show all significance
  if(overlay){
    proximity2[is.na(proximity)] <- NA
  }
  n_paths2 <- sum(!is.na(proximity2))
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


  plot_ <- list(
    # Original correlations (from network_plot)
    geom_curve(
      data = paths,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        alpha = proximity,
        size = proximity,
        colour = proximity * sign
      )
    ),
    # significant correlations (new)
    geom_curve(
      data = paths2,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend),
        color = "black",
        alpha = .5

    ),
    scale_alpha(limits = c(0,
                           1)),
    scale_size(limits = c(0, 1)),
    scale_colour_gradientn(limits = c(-1, 1), colors = colours),
    geom_point(
      data = points,
      aes(x,
          y),
      size = 3,
      shape = 19,
      colour = "white"
    ),
    if (repel)
      geom_text_repel(
        data = points,
        aes(x, y, label = id),
        fontface = "bold",
        size = 5,
        segment.size = 0,
        segment.color = "white"
      ),
    if (!repel)
      geom_text(
        data = points,
        aes(x, y, label = id),
        fontface = "bold",
        size = 5
      ),
    expand_limits(
      x = c(min(points$x) - 0.1, max(points$x) +
              0.1),
      y = c(min(points$y) - 0.1, max(points$y) +
              0.1)
    ),
    theme_void(),
    guides(size = "none", alpha = "none"),
    if (legend)
      labs(colour = NULL),
    if (!legend)
      theme(legend.position = "none")
  )
  ggplot() + plot_
}

# network_plot_wsig(df_test, overlay = F)
#

