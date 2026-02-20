# Index conventions:
#
# Cells are numbered 1..ncell, left-to-right then top-to-bottom.
# Cell 1 is top-left. This is the rasterImage / image convention.
#
# Row 1 is the top row (y = ymax side).
# Column 1 is the left column (x = xmin side).

#' Convert xy coordinates to cell indices
#'
#' Points outside the extent return `NA`.
#'
#' @param g a `vast` object
#' @param xy two-column numeric matrix (or two-element vector for a single point)
#' @return integer vector of cell indices (1-based)
#' @export
cell_from_xy <- function(g, xy) {
  xy <- matrix(as.double(xy), ncol = 2L)
  rc <- rowcol_from_xy(g, xy)
  cell_from_rowcol(g, rc[, 1L], rc[, 2L])
}

#' Convert cell indices to xy coordinates (cell centres)
#'
#' @param g a `vast` object
#' @param cell integer vector of cell indices (1-based)
#' @return two-column numeric matrix
#' @export
xy_from_cell <- function(g, cell) {
  rc <- rowcol_from_cell(g, cell)
  cbind(x_from_col(g, rc[, 2L]),
        y_from_row(g, rc[, 1L]))
}

#' Convert row and column to cell index
#'
#' @param g a `vast` object
#' @param row integer row indices (1-based, from top)
#' @param col integer column indices (1-based, from left)
#' @return integer vector of cell indices
#' @export
cell_from_rowcol <- function(g, row, col) {
  nc <- g$dimension[1]
  nr <- g$dimension[2]
  out <- (as.integer(row) - 1L) * nc + as.integer(col)
  out[row < 1L | row > nr | col < 1L | col > nc] <- NA_integer_
  out
}

#' Convert cell index to row and column
#'
#' @param g a `vast` object
#' @param cell integer vector of cell indices (1-based)
#' @return two-column integer matrix: `row`, `col`
#' @export
rowcol_from_cell <- function(g, cell) {
  cell <- as.integer(cell)
  nc <- g$dimension[1]
  row <- ((cell - 1L) %/% nc) + 1L
  col <- ((cell - 1L) %%  nc) + 1L
  bad <- cell < 1L | cell > (g$dimension[1] * g$dimension[2])
  row[bad] <- NA_integer_
  col[bad] <- NA_integer_
  cbind(row = row, col = col)
}

#' Convert xy coordinates to row and column
#'
#' Points outside the extent return `NA`.
#'
#' @param g a `vast` object
#' @param xy two-column numeric matrix
#' @return two-column integer matrix: `row`, `col`
#' @export
rowcol_from_xy <- function(g, xy) {
  xy <- matrix(as.double(xy), ncol = 2L)
  col <- col_from_x(g, xy[, 1L])
  row <- row_from_y(g, xy[, 2L])
  cbind(row = row, col = col)
}

#' Column index from x coordinate
#'
#' @param g a `vast` object
#' @param x numeric vector of x coordinates
#' @return integer column indices (1-based), `NA` if outside extent
#' @export
col_from_x <- function(g, x) {
  dx <- diff(g$extent[1:2]) / g$dimension[1]
  col <- as.integer(floor((x - g$extent[1]) / dx)) + 1L
  ## right edge: x == xmax lands in last column
  col[x == g$extent[2]] <- g$dimension[1]
  col[col < 1L | col > g$dimension[1]] <- NA_integer_
  col
}

#' Row index from y coordinate
#'
#' Row 1 is at the top (ymax side).
#'
#' @param g a `vast` object
#' @param y numeric vector of y coordinates
#' @return integer row indices (1-based), `NA` if outside extent
#' @export
row_from_y <- function(g, y) {
  dy <- diff(g$extent[3:4]) / g$dimension[2]
  row <- as.integer(floor((g$extent[4] - y) / dy)) + 1L
  ## bottom edge: y == ymin lands in last row
  row[y == g$extent[3]] <- g$dimension[2]
  row[row < 1L | row > g$dimension[2]] <- NA_integer_
  row
}

#' X coordinate from column index (cell centre)
#'
#' @param g a `vast` object
#' @param col integer column indices (1-based)
#' @return numeric x coordinates
#' @export
x_from_col <- function(g, col) {
  dx <- diff(g$extent[1:2]) / g$dimension[1]
  g$extent[1] + (col - 0.5) * dx
}

#' Y coordinate from row index (cell centre)
#'
#' Row 1 is at the top (ymax side).
#'
#' @param g a `vast` object
#' @param row integer row indices (1-based)
#' @return numeric y coordinates
#' @export
y_from_row <- function(g, row) {
  dy <- diff(g$extent[3:4]) / g$dimension[2]
  g$extent[4] - (row - 0.5) * dy
}
