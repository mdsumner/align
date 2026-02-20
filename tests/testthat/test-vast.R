test_that("vast constructor works with dimension only", {
  g <- vast(c(100L, 200L))
  expect_s3_class(g, "vast")
  expect_identical(g$dimension, c(100L, 200L))
  expect_identical(g$extent, c(0, 100, 0, 200))
})

test_that("vast constructor works with dimension and extent", {
  g <- vast(c(500L, 300L), extent = c(140, 160, -50, -30))
  expect_identical(g$dimension, c(500L, 300L))
  expect_identical(g$extent, c(140, 160, -50, -30))
})

test_that("vast coerces dimension to integer", {
  g <- vast(c(10, 20))
  expect_identical(g$dimension, c(10L, 20L))
})

test_that("vast rejects bad inputs", {
  expect_error(vast(c(0L, 10L)))
  expect_error(vast(c(-1L, 10L)))
  expect_error(vast(c(10L, 10L), c(5, 3, 0, 10)))  # xmax < xmin
  expect_error(vast(c(10L, 10L), c(0, 10, 5, 3)))   # ymax < ymin
  expect_error(vast(c(10L)))  # wrong length
})

test_that("is_vast works", {
  expect_true(is_vast(vast(c(10L, 10L))))
  expect_false(is_vast(list(dimension = c(10L, 10L))))
})

test_that("res is derived correctly", {
  g <- vast(c(500L, 300L), c(140, 160, -50, -30))
  r <- res(g)
  expect_equal(r[1], 20 / 500)
  expect_equal(r[2], 20 / 300)
})

test_that("ncol, nrow, ncell work", {
  g <- vast(c(100L, 200L))
  expect_identical(ncol(g), 100L)
  expect_identical(nrow(g), 200L)
  expect_identical(ncell(g), 20000L)
})

test_that("unit grid has resolution 1x1", {
  g <- vast(c(100L, 200L))
  expect_equal(res(g), c(1, 1))
})

test_that("x and y give cell centres", {
  g <- vast(c(4L, 3L))
  expect_equal(x(g), c(0.5, 1.5, 2.5, 3.5))
  expect_equal(y(g), c(2.5, 1.5, 0.5))  # top to bottom
})

test_that("x and y have correct length", {
  g <- vast(c(100L, 200L), c(0, 10, 0, 20))
  expect_length(x(g), 100L)
  expect_length(y(g), 200L)
})

test_that("print and format work", {
  g <- vast(c(10L, 20L))
  expect_output(print(g), "<vast>")
  expect_type(format(g), "character")
})
