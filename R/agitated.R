#' An alternative to upset plots
#' @param x A list
#' @param nsets Maximum number of sets to be shown.
#' @param exclusive Should the intersections shown be exclusive? If yes, each 
#'  entry is shown only once in the top bar plot.
#' @param intersection_order Sort by "frequency" or "degree"?
#' @return Whatever cowplot::plot_grid returns
#' @export
agitated <- function(
    x, 
    nsets = 20, 
    exclusive = TRUE, 
    intersection_order = c("frequency", "degree")) {

  if (!inherits(x, "list")) {
    stop("Input must be a list of sets.")
  }
  intersection_order <- match.arg(intersection_order)
  if (is.null(names(x))) {
    stop("Input must be named")
  }
  x[] <- lapply(x, unique)
  
  n <- seq(length(x), 2)
  x <- x[order(sapply(x, length))]
  mat <- list_to_matrix(x)
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
  intersections <- numeric(ncol(grids))
  for (i in seq_len(ncol(grids))) {
    column <- grids[, i]
    intersects <- apply(mat[, column, drop = FALSE], 1, all)
    if (exclusive) {
      mat[intersects, column] <- FALSE
    }
    intersections[[i]] <- sum(intersects)
  }

  not_empty <- intersections > 0
  intersections <- intersections[not_empty]
  ## Test this
  grids <- grids[, not_empty, drop = FALSE]
  if (intersection_order == "frequency") {
    order <- order(intersections, decreasing = TRUE)
  } else {
    order <- seq_along(intersections)
  }
  intersections <- intersections[order]
  grids <- grids[, order, drop = FALSE]
  nsets <- min(nsets, length(intersections))
  grids <- grids[, seq_len(nsets), drop = FALSE]
  intersections <- intersections[seq_len(nsets)]

  mdf <- reshape2::melt(grids)
  mdf$Var1 <- factor(mdf$Var1, levels = names(x))
  mdf$Var2 <- factor(mdf$Var2)


  dots <- ggplot(mdf, aes_string(x = "Var2", y = "Var1", color = "value")) + 
    geom_point(
      na.rm = TRUE,
      size = 2.2,
      shape = 16
    ) + 
    geom_path(
      data = mdf[mdf$value, ],
      aes(group = Var2),
      size = 1.1
    ) +
    scale_color_manual(
      guide = FALSE,
      limits = c("TRUE", "FALSE"), 
      values = c("black", NA)
    ) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(expand = expand_scale(mult = 0, add = 1)) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )

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


list_to_matrix <- function(x) {
  elements <- Reduce(union, x)
  mat <- matrix(
    nrow = length(elements), 
    ncol = length(x),
    dimnames = list(elements, names(x))
  )
  mat[, ] <- sapply(x,
    function(y) {
      elements %in% y
    }
  )

  mat
}

integer_breaks <- function(x) {
  seq(
    from = 0,
    to = ceiling(max(x)), 
    by = max(round(max(x) / 5), 1)
  )
}


empty_plot <- function() {
  ggplot() + theme_void()
}

example_data <- function() {
  example <- lapply(1:10, function(i) {
    sample(letters, 10, replace=TRUE)
  })
  names(example) <- LETTERS[1:10]
  example
}
