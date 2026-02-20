# The alignment prison:
#
# Two vast objects are compatible iff their cell boundaries coincide:
# 1. Same origin (modulo resolution)
# 2. Commensurate resolution (one is an exact integer multiple of the other)
#
# If these hold, every index operation produces exact integer results.
# If they don't, we refuse to proceed.

#' Test whether two grids share a lattice
#'
#' Returns `TRUE` if the cell boundaries of `g2` fall exactly on cell
#' boundaries of `g1` (or vice versa). Both origin alignment and
#' commensurate resolution are checked.
#'
#' @param g1,g2 `vast` objects
#' @param tol numeric tolerance for floating-point comparisons
#' @return logical
#' @export
is_aligned <- function(g1, g2, tol = sqrt(.Machine$double.eps)) {
  stopifnot(is_vast(g1), is_vast(g2))

  r1 <- res(g1)
  r2 <- res(g2)

  ## use the finer resolution as the reference
  rmin <- pmin(r1, r2)

  ## commensurate resolution: each must be an integer multiple of the finer
  ratio1 <- r1 / rmin
  ratio2 <- r2 / rmin
  if (any(abs(ratio1 - round(ratio1)) > tol)) return(FALSE)
  if (any(abs(ratio2 - round(ratio2)) > tol)) return(FALSE)

  ## origin alignment: offset between origins must be an integer number

  ## of the finer cells
  offset_x <- (g1$extent[1] - g2$extent[1]) / rmin[1]
  offset_y <- (g1$extent[3] - g2$extent[3]) / rmin[2]
  abs(offset_x - round(offset_x)) < tol &&
    abs(offset_y - round(offset_y)) < tol
}

#' Integer offset between aligned grids
#'
#' Returns the column and row offset from the origin of `parent` to the
#' origin of `child`. Both grids must have the same resolution.
#'
#' @param child,parent `vast` objects with the same resolution
#' @param tol numeric tolerance
#' @return named integer vector `c(col = ..., row = ...)`
#' @export
offset <- function(child, parent, tol = sqrt(.Machine$double.eps)) {
  stopifnot(is_vast(child), is_vast(parent))
  r <- res(parent)
  rc <- res(child)

  if (any(abs(r - rc) > tol * pmax(abs(r), abs(rc), 1))) {
    stop("offset requires identical resolution; use snap() or resize() first")
  }

  col_off <- round((child$extent[1] - parent$extent[1]) / r[1])
  row_off <- round((parent$extent[4] - child$extent[4]) / r[2])

  c(col = as.integer(col_off), row = as.integer(row_off))
}

#' @param parent `vast` object whose lattice to snap to
#' @rdname snap
#' @export
snap.vast <- function(x, parent, ...) {
  stopifnot(is_vast(parent))

  r <- res(parent)
  rx <- res(x)

  ## check commensurate
  ratio <- rx / r
  int_ratio <- round(ratio)
  tol <- sqrt(.Machine$double.eps)
  if (any(abs(ratio - int_ratio) > tol)) {
    stop("resolutions are not commensurate; snap requires integer-related resolutions")
  }

  ## snap x's extent to parent's cell boundaries
  xmin <- parent$extent[1] + round((x$extent[1] - parent$extent[1]) / r[1]) * r[1]
  xmax <- parent$extent[1] + round((x$extent[2] - parent$extent[1]) / r[1]) * r[1]
  ymin <- parent$extent[3] + round((x$extent[3] - parent$extent[3]) / r[2]) * r[2]
  ymax <- parent$extent[3] + round((x$extent[4] - parent$extent[3]) / r[2]) * r[2]

  new_dim <- as.integer(round(c((xmax - xmin) / rx[1],
                                (ymax - ymin) / rx[2])))
  vast(new_dim, c(xmin, xmax, ymin, ymax))
}
