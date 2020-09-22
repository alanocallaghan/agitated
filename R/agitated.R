#' An alternative to the UpSetR package for upset plots.
#' @param x A list or presence/absence matrix.
#' @param nsets Maximum number of sets to be shown.
#' @param exclusive Should the intersections shown be exclusive? If yes, each 
#'  entry is shown only once in the top bar plot.
#' @param intersection_order Sort by "frequency" or "degree"?
#' @param sort_sets Controls whether the input sets are re-ordered based on 
#' descending size.
#' @param title,subtitle Plot title and subtitle.
#' @return Whatever cowplot::plot_grid returns
#' @examples 
#' 
#' data <- agitated:::example_data()
#' agitated(data)
#' 
#' agitated(data, nsets = 10)
#' 
#' agitated(data, exclusive = FALSE)
#' 
#' agitated(data, intersection_order = "degree")
#' 
#' agitated(data, sort_sets = FALSE)
#' 
#' agitated(data, title = "title", subtitle = "subtitle")
#' @export
agitated <- function(
    x, 
    nsets = 20, 
    exclusive = TRUE,
    intersection_order = c("frequency", "degree"),
    sort_sets = TRUE,
    title = NULL,
    subtitle = NULL
  ) {

  intersection_order <- match.arg(intersection_order)
  if (is.matrix(x)) {
    if (is.null(colnames(x))) {
      stop("x must have column names")
    }
    if (any(!x %in% c(1, 0))) {
      stop("x must be a binary matrix")
    }
  } else if (is.list(x) & !is.data.frame(x)) {
    if (is.null(names(x))) {
      stop("Input must be a named list or a matrix")
    }
  } else {
    stop("Input must be a named list or a matrix")
  }

  if (is.list(x)) {
    x[] <- lapply(x, unique)
    n <- seq(length(x), 1)
    mat <- list_to_matrix(x)
  } else {
    n <- seq(ncol(x), 1)
  }
  names <- colnames(mat)
  mat_original <- mat
  if (sort_sets) {
    mat <- mat[, order(colSums(mat))]
  }
  grids <- lapply(n,
    function(i) {
      grid <- combn(colnames(mat), i)
      vapply(
        seq_len(ncol(grid)), 
        function(i) {
          colnames(mat) %in% grid[, i, drop = TRUE]
        },
        FUN.VALUE = logical(ncol(mat))
      )
    }
  )
  grids <- do.call(cbind, grids)
  rownames(grids) <- colnames(mat)
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
  mdf[["Var1"]] <- factor(mdf[["Var1"]], levels = names)
  mdf[["Var2"]] <- factor(mdf[["Var2"]])


  dots <- ggplot(mdf, 
      aes_string(x = "Var2", y = "Var1", color = "value")
    ) + 
    geom_point(
      na.rm = TRUE,
      size = 2.2,
      shape = 16
    ) + 
    geom_path(
      data = mdf[mdf[["value"]], ],
      aes_string(group = "Var2"),
      size = 1.1
    ) +
    scale_color_manual(
      guide = FALSE,
      limits = c("TRUE", "FALSE"), 
      values = c("black", NA)
    ) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(expand = expansion(mult = 0, add = 1)) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )

  sidebar <- ggplot(
      mapping = aes(
        x = factor(names, levels = names),
        y = apply(mat_original, 2, sum)
      )
    ) + 
    geom_bar(stat = "identity") +
    coord_flip() + 
    labs(x = NULL, y = "Input set size") +
    scale_x_discrete(position = "top") +
    scale_y_continuous(trans = "reverse", breaks = integer_breaks)


  topbar <- ggplot(mapping = aes(x = seq_along(intersections), y = intersections)) + 
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Set size", title = title, subtitle = subtitle) +
    scale_y_continuous(breaks = integer_breaks) +
    scale_x_discrete(expand = expansion(mult = 0, add = 0.5)) +
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
