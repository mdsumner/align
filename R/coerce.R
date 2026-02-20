#' Create a vast from extent and resolution
#'
#' Dimension is derived. Extent is adjusted (expanded) to honour exact
#' resolution — the returned extent may be larger than the input if the
#' extent is not evenly divisible by the resolution.
#'
#' @param extent numeric `c(xmin, xmax, ymin, ymax)`
#' @param resolution numeric resolution, scalar (square cells) or
#'   `c(dx, dy)`
#' @return a `vast` object
#' @export
#' @examples
#' vast_res(c(140, 160, -50, -30), 0.04)
vast_res <- function(extent, resolution) {
  extent <- as.double(extent)
  resolution <- as.double(rep_len(resolution, 2L))
  stopifnot(
    length(extent) == 4L,
    all(resolution > 0)
  )

  dimension <- as.integer(ceiling(c(
    diff(extent[1:2]) / resolution[1],
    diff(extent[3:4]) / resolution[2]
  )))

  ## adjust extent to honour exact resolution (expand from xmin/ymin anchor)
  extent[2] <- extent[1] + dimension[1] * resolution[1]
  extent[4] <- extent[3] + dimension[2] * resolution[2]
  vast(dimension, extent)
}

#' @rdname as_vast
#' @export
as_vast.vast <- function(x, ...) x

#' @rdname as_vast
#' @export
as_vast.SpatRaster <- function(x, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra is required for as_vast.SpatRaster")
  }
  vast(c(ncol(x), nrow(x)), as.vector(terra::ext(x)))
}

#' @rdname as_vast
#' @export
as_vast.BasicRaster <- function(x, ...) {
  ## raster package objects (RasterLayer, RasterBrick, etc.)
  vast(c(ncol(x), nrow(x)),
       c(x@extent@xmin, x@extent@xmax, x@extent@ymin, x@extent@ymax))
}

#' @rdname as_vast
#' @export
as_vast.list <- function(x, ...) {
  ## expect a list with $dimension and $extent (e.g. controlledburn output)
  stopifnot(!is.null(x$dimension), !is.null(x$extent))
  vast(x$dimension, x$extent)
}
