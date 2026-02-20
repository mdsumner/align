# Generics defined by align
#
# ncol() and nrow() are base generics — we just provide methods.
# Everything else is ours.

#' Grid resolution
#'
#' @param x a `vast` object
#' @param ... ignored
#' @return numeric vector `c(dx, dy)`
#' @export
res <- function(x, ...) UseMethod("res")

#' Number of cells
#'
#' @param x a `vast` object
#' @param ... ignored
#' @return integer
#' @export
ncell <- function(x, ...) UseMethod("ncell")

#' Cell-centre x coordinates
#'
#' @param x a `vast` object
#' @param ... ignored
#' @return numeric vector of length `ncol(x)`
#' @export
x <- function(x, ...) UseMethod("x")

#' Cell-centre y coordinates
#'
#' @param x a `vast` object
#' @param ... ignored
#' @return numeric vector of length `nrow(x)`
#' @export
y <- function(x, ...) UseMethod("y")

#' Crop a grid to a smaller extent
#'
#' Restrict the extent, snapping inward to cell boundaries.
#' Resolution is unchanged.
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
crop <- function(x, ...) UseMethod("crop")

#' Expand a grid to a larger extent
#'
#' Widen the extent, snapping outward to cell boundaries.
#' Resolution is unchanged. The inverse of [crop()].
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
expand <- function(x, ...) UseMethod("expand")

#' Shrink a grid by removing cells from the edges
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
shrink <- function(x, ...) UseMethod("shrink")

#' Grow a grid by adding cells to the edges
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
grow <- function(x, ...) UseMethod("grow")

#' Resize a grid by an integer factor
#'
#' Change resolution by an integer factor. The new lattice nests exactly
#' within the old one.
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
resize <- function(x, ...) UseMethod("resize")

#' Snap a grid to a parent lattice
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return a `vast` object
#' @export
snap <- function(x, ...) UseMethod("snap")

#' Partition a grid into tiles
#'
#' @param x a `vast` object
#' @param ... passed to methods
#' @return list of `vast` objects
#' @export
tiles <- function(x, ...) UseMethod("tiles")

#' Coerce to a vast grid specification
#'
#' @param x object to coerce
#' @param ... passed to methods
#' @return a `vast` object
#' @export
as_vast <- function(x, ...) UseMethod("as_vast")
