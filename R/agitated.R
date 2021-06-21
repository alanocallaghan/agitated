#' An alternative to the UpSetR package for upset plots.
#' @param x A list or presence/absence matrix.
#' @param nsets Numeric scalar specifying the maximum number of sets to be shown.
#' @param exclusive Logical scalar controlling whether the intersections should
#'  be exclusive.
#' @param intersection_order Character scalar controlling whether sorting of
#' intersections is done by \code{"frequency"} (size of intersection) or
#' \code{"degree"} (number of intersecting sets).
#' @param sort_sets Logical scalar controlling whether the input sets are 
#' re-ordered based on descending size.
#' @param title,subtitle Character scalars specifying plot title and subtitle.
#' @param return_plots Logical scalar specifying whether the ggplot2 objects
#' should be returned directly (\code{TRUE}) or if 
#' \code{\link[cowplot]{plot_grid}} should be called on these objects instead.
#' @return If \code{return_plots = TRUE}, the function returns a list of three
#' ggplot2 objects.
#' If \code{return_plots = TRUE},
#' the function calls \code{\link[cowplot]{plot_grid}} on the plot objects.
#' @references
#'  UpSetR: an R package for the visualization of intersecting sets and their properties
#'  Jake R Conway, Alexander Lex, Nils Gehlenborg
#'  <doi:10.1093/bioinformatics/btx364>.
#' @examples 
#' 
#' data <- example_data()
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
    subtitle = NULL,
    return_plots = FALSE
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
    x <- list_to_matrix(x)
  }
  n <- seq(ncol(x), 1)
  names <- colnames(x)
  mat_original <- x
  if (sort_sets) {
    x <- x[, order(colSums(x))]
  }
  if (any(grepl("\\s", names))) {
    stop("Category names cannot contain spaces")
  }
  grids <- lapply(n,
    function(i) {
      grid <- combn(colnames(x), i)
      vapply(
        seq_len(ncol(grid)), 
        function(i) {
          colnames(x) %in% grid[, i, drop = TRUE]
        },
        FUN.VALUE = logical(ncol(x))
      )
    }
  )
  grids <- do.call(cbind, grids)
  rownames(grids) <- colnames(x)
  intersections <- setNames(numeric(ncol(grids)), colnames(grids))
  for (i in seq_len(ncol(grids))) {
    column <- grids[, i]
    intersects <- apply(x[, column, drop = FALSE], 1, all)
    if (exclusive) {
      x[intersects, column] <- FALSE
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
  if (return_plots) {
    return(list(topbar = topbar, sidebar = sidebar, dots = dots))
  }
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

#' Example data for creating UpSet plots.
#' 
#' @return A list of class memberships.
example_data <- function() {
  example <- lapply(1:10, function(i) {
    sample(letters, 10, replace=TRUE)
  })
  names(example) <- LETTERS[1:10]
  example
}
