test_that("cell_from_rowcol and rowcol_from_cell are inverses", {
  g <- vast(c(10L, 20L))
  cells <- c(1L, 10L, 11L, 100L, 200L)
  rc <- rowcol_from_cell(g, cells)
  cells2 <- cell_from_rowcol(g, rc[, "row"], rc[, "col"])
  expect_identical(cells2, cells)
})

test_that("cell 1 is top-left", {
  g <- vast(c(10L, 5L))
  rc <- rowcol_from_cell(g, 1L)
  expect_identical(rc[1, ], c(row = 1L, col = 1L))
})

test_that("cell ncol is top-right", {
  g <- vast(c(10L, 5L))
  rc <- rowcol_from_cell(g, 10L)
  expect_identical(rc[1, ], c(row = 1L, col = 10L))
})

test_that("cell ncol+1 is start of second row", {
  g <- vast(c(10L, 5L))
  rc <- rowcol_from_cell(g, 11L)
  expect_identical(rc[1, ], c(row = 2L, col = 1L))
})

test_that("last cell is bottom-right", {
  g <- vast(c(10L, 5L))
  rc <- rowcol_from_cell(g, 50L)
  expect_identical(rc[1, ], c(row = 5L, col = 10L))
})

test_that("out-of-range cells return NA", {
  g <- vast(c(10L, 5L))
  rc <- rowcol_from_cell(g, c(0L, 51L, -1L))
  expect_true(all(is.na(rc)))
})

test_that("out-of-range rowcol returns NA", {
  g <- vast(c(10L, 5L))
  expect_identical(cell_from_rowcol(g, 0L, 1L), NA_integer_)
  expect_identical(cell_from_rowcol(g, 1L, 11L), NA_integer_)
  expect_identical(cell_from_rowcol(g, 6L, 1L), NA_integer_)
})

test_that("xy_from_cell and cell_from_xy round-trip for cell centres", {
  g <- vast(c(20L, 15L), c(100, 200, -50, 50))
  cells <- c(1L, 50L, 100L, 200L, 300L)
  xy <- xy_from_cell(g, cells)
  cells2 <- cell_from_xy(g, xy)
  expect_identical(cells2, cells)
})

test_that("cell centres are in correct position", {
  g <- vast(c(4L, 3L))  # unit grid
  xy <- xy_from_cell(g, 1L)
  expect_equal(xy[1, ], c(0.5, 2.5))  # top-left cell centre
})

test_that("xy outside extent returns NA", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  expect_identical(cell_from_xy(g, c(-1, 5)), NA_integer_)
  expect_identical(cell_from_xy(g, c(5, -1)), NA_integer_)
  expect_identical(cell_from_xy(g, c(11, 5)), NA_integer_)
})

test_that("xy on exact right/bottom edge lands in last cell", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  ## xmax edge
  expect_false(is.na(cell_from_xy(g, c(10, 5))))
  expect_equal(col_from_x(g, 10), 10L)
  ## ymin edge
  expect_false(is.na(cell_from_xy(g, c(5, 0))))
  expect_equal(row_from_y(g, 0), 10L)
})

test_that("col_from_x and x_from_col are consistent", {
  g <- vast(c(100L, 50L), c(-180, 180, -90, 90))
  cols <- 1:100
  xs <- x_from_col(g, cols)
  cols2 <- col_from_x(g, xs)
  expect_identical(cols2, cols)
})

test_that("row_from_y and y_from_row are consistent", {
  g <- vast(c(100L, 50L), c(-180, 180, -90, 90))
  rows <- 1:50
  ys <- y_from_row(g, rows)
  rows2 <- row_from_y(g, ys)
  expect_identical(rows2, rows)
})

test_that("rasterImage convention: matrix(vals, nrow, byrow=TRUE)", {
  ## demonstrate that cell ordering matches matrix(, nrow, byrow = TRUE)
  g <- vast(c(3L, 2L))
  ## cells 1..6, row-major from top-left
  cells <- seq_len(ncell(g))
  rc <- rowcol_from_cell(g, cells)
  ## filling a matrix by row: row 1 gets cells 1,2,3; row 2 gets 4,5,6
  m <- matrix(cells, nrow = nrow(g), ncol = ncol(g), byrow = TRUE)
  for (k in seq_along(cells)) {
    expect_equal(m[rc[k, "row"], rc[k, "col"]], cells[k])
  }
})
