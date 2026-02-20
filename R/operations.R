#' @param extent numeric `c(xmin, xmax, ymin, ymax)` to crop to
#' @rdname crop
#' @export
crop.vast <- function(x, extent, ...) {
  extent <- as.double(extent)
  stopifnot(length(extent) == 4L)
  r <- res(x)

  ## snap inward to cell boundaries
  xmin <- x$extent[1] + ceiling((extent[1] - x$extent[1]) / r[1]) * r[1]
  xmax <- x$extent[1] + floor((extent[2] - x$extent[1]) / r[1]) * r[1]
  ymin <- x$extent[3] + ceiling((extent[3] - x$extent[3]) / r[2]) * r[2]
  ymax <- x$extent[3] + floor((extent[4] - x$extent[3]) / r[2]) * r[2]

  new_dim <- as.integer(round(c((xmax - xmin) / r[1],
                                (ymax - ymin) / r[2])))

  if (any(new_dim < 1L)) {
    stop("crop extent produces zero or negative dimension")
  }

  vast(new_dim, c(xmin, xmax, ymin, ymax))
}

#' @param extent numeric `c(xmin, xmax, ymin, ymax)` to expand to
#' @rdname expand
#' @export
expand.vast <- function(x, extent, ...) {
  extent <- as.double(extent)
  stopifnot(length(extent) == 4L)
  r <- res(x)

  ## snap outward to cell boundaries
  xmin <- x$extent[1] + floor((extent[1] - x$extent[1]) / r[1]) * r[1]
  xmax <- x$extent[1] + ceiling((extent[2] - x$extent[1]) / r[1]) * r[1]
  ymin <- x$extent[3] + floor((extent[3] - x$extent[3]) / r[2]) * r[2]
  ymax <- x$extent[3] + ceiling((extent[4] - x$extent[3]) / r[2]) * r[2]

  new_dim <- as.integer(round(c((xmax - xmin) / r[1],
                                (ymax - ymin) / r[2])))
  vast(new_dim, c(xmin, xmax, ymin, ymax))
}

#' @param n integer number of cells to remove from each edge (default `1L`).
#'   A single value applies to all four edges. A vector of two applies
#'   `c(x_edges, y_edges)`. A vector of four applies
#'   `c(left, right, bottom, top)`.
#' @rdname shrink
#' @export
shrink.vast <- function(x, n = 1L, ...) {
  n <- as.integer(n)
  if (length(n) == 1L) n <- rep(n, 4L)
  if (length(n) == 2L) n <- rep(n, each = 2L)  ## c(x, y) -> c(left, right, bottom, top)
  stopifnot(length(n) == 4L)

  r <- res(x)
  new_dim <- c(x$dimension[1] - n[1] - n[2],
               x$dimension[2] - n[3] - n[4])
  if (any(new_dim < 1L)) stop("shrink removes more cells than exist")

  new_extent <- c(
    x$extent[1] + n[1] * r[1],
    x$extent[2] - n[2] * r[1],
    x$extent[3] + n[3] * r[2],
    x$extent[4] - n[4] * r[2]
  )
  vast(new_dim, new_extent)
}

#' @param n integer number of cells to add to each edge (default `1L`).
#'   A single value applies to all four edges. A vector of two applies
#'   `c(x_edges, y_edges)`. A vector of four applies
#'   `c(left, right, bottom, top)`.
#' @rdname grow
#' @export
grow.vast <- function(x, n = 1L, ...) {
  n <- as.integer(n)
  if (length(n) == 1L) n <- rep(n, 4L)
  if (length(n) == 2L) n <- rep(n, each = 2L)  ## c(x, y) -> c(left, right, bottom, top)
  stopifnot(length(n) == 4L)

  r <- res(x)
  new_dim <- c(x$dimension[1] + n[1] + n[2],
               x$dimension[2] + n[3] + n[4])
  new_extent <- c(
    x$extent[1] - n[1] * r[1],
    x$extent[2] + n[2] * r[1],
    x$extent[3] - n[3] * r[2],
    x$extent[4] + n[4] * r[2]
  )
  vast(new_dim, new_extent)
}

#' @param factor numeric scaling factor. Values > 1 increase resolution
#'   (more cells, same extent). Values < 1 decrease resolution.
#'   Must produce exact integer dimensions.
#' @rdname resize
#' @export
resize.vast <- function(x, factor, ...) {
  factor <- as.double(factor)
  stopifnot(length(factor) == 1L, factor > 0)

  new_dim_d <- x$dimension * factor
  new_dim <- as.integer(round(new_dim_d))

  if (any(abs(new_dim_d - new_dim) > sqrt(.Machine$double.eps))) {
    stop("factor does not produce integer dimensions")
  }
  if (any(new_dim < 1L)) {
    stop("resize produces zero or negative dimension")
  }

  vast(new_dim, x$extent)
}
