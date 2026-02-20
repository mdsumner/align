#' Create a grid specification
#'
#' A grid specification defined by dimension and extent. Resolution,
#' coordinates, cell indices, and row/column mappings are derived on demand.
#'
#' Dimension is `c(ncol, nrow)`, default `c(1L, 1L)` Extent is `c(xmin, xmax, ymin, ymax)`.
#'
#' When `extent` is `NULL`, defaults to unit cell space: `c(0, ncol, 0, nrow)`.
#' This gives an identity mapping where column index = x coordinate and row
#' index = y coordinate.
#'
#' @param dimension integer vector `c(ncol, nrow)`
#' @param extent numeric vector `c(xmin, xmax, ymin, ymax)`, or `NULL` for
#'   unit cell space
#' @return A `vast` object (named list with class)
#' @export
#' @examples
#' vast(c(100L, 200L))
#' vast(c(500L, 300L), extent = c(140, 160, -50, -30))
vast <- function(dimension = c(1L, 1L), extent = NULL) {
  dimension <- as.integer(dimension)
  stopifnot(length(dimension) == 2L, all(dimension > 0L))

  if (is.null(extent)) {
    extent <- c(0, dimension[1], 0, dimension[2])
  }
  extent <- as.double(extent)
  stopifnot(
    length(extent) == 4L,
    extent[2] > extent[1],
    extent[4] > extent[3]
  )

  structure(
    list(dimension = dimension, extent = extent),
    class = "vast"
  )
}

#' Test if an object is a vast grid specification
#'
#' @param x object to test
#' @return logical
#' @export
is_vast <- function(x) {
  inherits(x, "vast")
}

#' @export
print.vast <- function(x, ...) {
  r <- res(x)
  cat(sprintf("<vast> %d x %d (%.6g x %.6g)\n",
              x$dimension[1], x$dimension[2], r[1], r[2]))
  cat(sprintf("  x: [%g, %g]\n  y: [%g, %g]\n",
              x$extent[1], x$extent[2], x$extent[3], x$extent[4]))
  invisible(x)
}

#' @export
format.vast <- function(x, ...) {
  r <- res(x)
  sprintf("<vast> %d x %d (%.6g x %.6g) x:[%g, %g] y:[%g, %g]",
          x$dimension[1], x$dimension[2], r[1], r[2],
          x$extent[1], x$extent[2], x$extent[3], x$extent[4])
}
