agitated <- function(x, nsets = 20) {
  assert_is(x, "list")  
  if (is.null(names(x))) {
    stop("Input must be named")
  }
  n <- seq(length(x) - 1, 2)
  x[] <- lapply(x, unique)
  x <- x[order(sapply(x, length))]
  grids <- lapply(n, 
    function(i) {
      grid <- combn(names(x), i)
      sapply(
        seq_len(ncol(grid)), 
        function(i) names(x) %in% grid[, i]
      )
    }
  )
  grids <- do.call(cbind, grids)
  rownames(grids) <- names(x)
  intersections <- find_intersections(x, grids)

  not_empty <- intersections > 0
  intersections <- intersections[not_empty]
  grids <- grids[, not_empty]
  # order <- order(apply(grids, 2, sum))
  order <- order(intersections, decreasing = TRUE)
  intersections <- intersections[order]
  grids <- grids[, order]
  grids <- grids[, seq_len(nsets)]
  intersections <- intersections[seq_len(nsets)]
  mdf <- reshape2::melt(grids)
  mdf$Var1 <- factor(mdf$Var1, levels = names(x))
  mdf$Var2 <- factor(mdf$Var2)
  dots <- ggplot(mdf, aes(x = Var2, y = Var1, color = value)) + 
    geom_point(na.rm = TRUE) + 
    scale_color_manual(
      guide = FALSE,
      limits = c("TRUE", "FALSE"), 
      values = c("black", NA)
    ) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(expand = expand_scale(mult = 0, add = 1)) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank())
  sidebar <- ggplot(
      mapping = aes(
        x = factor(names(x), levels = names(x)), 
        y = sapply(x, length))
    ) + 
    geom_bar(stat = "identity") +
    coord_flip() + 
    labs(x = NULL, y = "Input set size") +
    scale_x_discrete(position = "top") +
    scale_y_continuous(trans = "reverse")
  topbar <- ggplot(mapping = aes(x = seq_along(intersections), y = intersections)) + 
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Set size") +
    scale_x_discrete(expand = expand_scale(mult = 0, add = 0.5)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  topcorner <- empty_plot()
  plot_grid(
    topcorner, topbar, sidebar, dots, 
    nrow = 2, 
    align = "hv",
    axis = "ltbr")
}

#' @importFrom ggplot2 ggplot aes
#'                     geom_bar geom_point
#'                     scale_x_discrete scale_x_continuous scale_y_continuous
#'                     expand_scale
#'                     theme labs theme_void element_blank
#' @importFrom cowplot plot_grid

find_intersections <- function(x, grids) {
  sapply(seq_len(ncol(grids)), function(i) {
    length(intersection(x, grids[, i]))
  })
}

empty_plot <- function() {
  ggplot() + theme_void()
}

intersection <- function(x, which) {
  Reduce(intersect, x[which])
}

example_data <- function() {
  example <- lapply(1:10, function(i) {
    sample(letters, 10, replace=TRUE)
  })
  names(example) <- letters[1:10]
  example
}
