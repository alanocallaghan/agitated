#' An alternative to upset plots
#' @param x A list
#' @param nsets Maximum number of sets to be shown.
#' @param exclusive Should the intersections shown be exclusive? If yes, each 
#'  entry is shown only once in the top bar plot.
#' @return Whatever cowplot::plot_grid returns
#' @export
agitated <- function(
    x, 
    nsets = 20, 
    exclusive = TRUE, 
    intersection_order = c("frequency", "degree")) {

  assert_is(x, "list")  
  intersection_order <- match.arg(intersection_order)
  if (is.null(names(x))) {
    stop("Input must be named")
  }
  n <- seq(length(x), 2)
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
  grids <- cbind(grids, sapply(names(x), function(n) names(x) == n, USE.NAMES=FALSE))
  rownames(grids) <- names(x)
  intersections <- find_intersections(x, grids)

  if (exclusive) {
    for (i in seq_len(ncol(grids) - 1)) {
      for (j in (i + 1):ncol(grids)) {
        intersections[[j]] <- setdiff(intersections[[j]], intersections[[i]])
      }
    }
  }
  intersections <- vapply(intersections, length, numeric(1))

  not_empty <- intersections > 0
  intersections <- intersections[not_empty]
  ## Test this
  grids <- grids[, not_empty, drop = FALSE]
  if (intersection_order == "frequency") {
    order <- order(intersections, decreasing = TRUE)
  } else order <- seq_along(intersections)
  intersections <- intersections[order]
  grids <- grids[, order, drop = FALSE]
  nsets <- min(nsets, length(intersections))
  grids <- grids[, seq_len(nsets), drop = FALSE]
  intersections <- intersections[seq_len(nsets)]
  mdf <- reshape2::melt(grids)
  mdf$Var1 <- factor(mdf$Var1, levels = names(x))
  mdf$Var2 <- factor(mdf$Var2)
  dots <- ggplot(mdf, aes_string(x = "Var2", y = "Var1", color = "value")) + 
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
        y = vapply(x, length, numeric(1)))
    ) + 
    geom_bar(stat = "identity") +
    coord_flip() + 
    labs(x = NULL, y = "Input set size") +
    scale_x_discrete(position = "top") +
    scale_y_continuous(trans = "reverse", breaks = integer_breaks)
  topbar <- ggplot(mapping = aes(x = seq_along(intersections), y = intersections)) + 
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Set size") +
    scale_y_continuous(breaks = integer_breaks) +
    scale_x_discrete(expand = expand_scale(mult = 0, add = 0.5)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  topcorner <- empty_plot()
  plot_grid(
    topcorner, topbar, sidebar, dots, 
    nrow = 2, 
    align = "hv")
}


integer_breaks <- function(x) {
  seq(
    from = 0,
    to = ceiling(max(x)), 
    by = max(round(max(x) / 5), 1)
  )
}

find_intersections <- function(x, grids) {
  lapply(seq_len(ncol(grids)), function(i) {
    intersection(x, grids[, i])
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
