## ---- crop / expand ----

test_that("crop preserves resolution", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- crop(g, c(2, 8, 3, 7))
  expect_equal(res(h), res(g))
})

test_that("crop snaps inward", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))  # res 1x1
  ## request doesn't land on cell boundaries
  h <- crop(g, c(0.3, 9.7, 0.3, 9.7))
  ## should snap inward: xmin=1, xmax=9, ymin=1, ymax=9
  expect_equal(h$extent[1], 1)
  expect_equal(h$extent[2], 9)
  expect_equal(h$extent[3], 1)
  expect_equal(h$extent[4], 9)
  expect_identical(h$dimension, c(8L, 8L))
})

test_that("expand snaps outward", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  h <- expand(g, c(-0.3, 10.3, -0.3, 10.3))
  expect_equal(h$extent[1], -1)
  expect_equal(h$extent[2], 11)
  expect_equal(h$extent[3], -1)
  expect_equal(h$extent[4], 11)
  expect_identical(h$dimension, c(12L, 12L))
})

test_that("crop(expand(g, e), g$extent) returns g", {
  g <- vast(c(50L, 30L), c(100, 150, -40, -10))
  e <- c(80, 170, -60, 10)
  h <- expand(g, e)
  g2 <- crop(h, g$extent)
  expect_identical(g2$dimension, g$dimension)
  expect_equal(g2$extent, g$extent)
})

test_that("crop to own extent is no-op", {
  g <- vast(c(50L, 30L), c(100, 150, -40, -10))
  h <- crop(g, g$extent)
  expect_identical(h$dimension, g$dimension)
  expect_equal(h$extent, g$extent)
})

test_that("crop rejects extent that produces zero dimension", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  expect_error(crop(g, c(4.5, 5.4, 0, 10)), "zero or negative")
})

## ---- shrink / grow ----

test_that("shrink default removes 1 cell from each edge", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  h <- shrink(g)
  expect_identical(h$dimension, c(8L, 8L))
  expect_equal(h$extent, c(1, 9, 1, 9))
})

test_that("grow default adds 1 cell to each edge", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  h <- grow(g)
  expect_identical(h$dimension, c(12L, 12L))
  expect_equal(h$extent, c(-1, 11, -1, 11))
})

test_that("shrink(grow(g)) returns g", {
  g <- vast(c(20L, 15L), c(100, 200, -50, 50))
  expect_identical(shrink(grow(g))$dimension, g$dimension)
  expect_equal(shrink(grow(g))$extent, g$extent)
})

test_that("grow(shrink(g)) returns g", {
  g <- vast(c(20L, 15L), c(100, 200, -50, 50))
  expect_identical(grow(shrink(g))$dimension, g$dimension)
  expect_equal(grow(shrink(g))$extent, g$extent)
})

test_that("shrink with n=2 vector applies per-axis", {
  g <- vast(c(20L, 10L), c(0, 20, 0, 10))  # res 1x1
  h <- shrink(g, c(2L, 3L))
  ## x: remove 2 from each side -> 16 cols

  ## y: remove 3 from each side -> 4 rows
  expect_identical(h$dimension, c(16L, 4L))
})

test_that("shrink with n=4 vector applies per-edge", {
  g <- vast(c(20L, 10L), c(0, 20, 0, 10))  # res 1x1
  h <- shrink(g, c(1L, 2L, 3L, 0L))  # left=1, right=2, bottom=3, top=0
  expect_identical(h$dimension, c(17L, 7L))
  expect_equal(h$extent, c(1, 18, 3, 10))
})

test_that("shrink rejects n that removes all cells", {
  g <- vast(c(10L, 10L))
  expect_error(shrink(g, 5L))
})

## ---- resize ----

test_that("resize by integer factor", {
  g <- vast(c(10L, 10L), c(0, 10, 0, 10))
  h <- resize(g, 2L)
  expect_identical(h$dimension, c(20L, 20L))
  expect_equal(h$extent, g$extent)
  expect_equal(res(h), c(0.5, 0.5))
})

test_that("resize by fractional factor", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- resize(g, 0.5)
  expect_identical(h$dimension, c(50L, 50L))
  expect_equal(h$extent, g$extent)
})

test_that("resize(resize(g, 2), 0.5) returns g", {
  g <- vast(c(50L, 30L), c(100, 150, -40, -10))
  g2 <- resize(resize(g, 2L), 0.5)
  expect_identical(g2$dimension, g$dimension)
  expect_equal(g2$extent, g$extent)
})

test_that("resize rejects non-integer-producing factor", {
  g <- vast(c(10L, 10L))
  expect_error(resize(g, 0.3), "integer dimensions")
})

## ---- alignment ----

test_that("grid is aligned with itself", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  expect_true(is_aligned(g, g))
})

test_that("crop produces aligned grid", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- crop(g, c(2, 8, 2, 8))
  expect_true(is_aligned(g, h))
})

test_that("resize produces aligned grid", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- resize(g, 2L)
  expect_true(is_aligned(g, h))
})

test_that("misaligned grids detected", {
  g1 <- vast(c(100L, 100L), c(0, 10, 0, 10))
  g2 <- vast(c(100L, 100L), c(0.05, 10.05, 0, 10))  # shifted origin
  expect_false(is_aligned(g1, g2))
})

test_that("offset returns named integer vector", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- crop(g, c(2, 8, 3, 7))
  off <- offset(h, g)
  expect_type(off, "integer")
  expect_named(off, c("col", "row"))
  expect_equal(off[["col"]], 20L)
  expect_equal(off[["row"]], 30L)
})

test_that("offset is zero for identical grids", {
  g <- vast(c(50L, 50L), c(0, 10, 0, 10))
  off <- offset(g, g)
  expect_identical(off, c(col = 0L, row = 0L))
})

test_that("offset errors on mismatched resolution", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  h <- resize(g, 2L)
  expect_error(offset(h, g), "resolution")
})

test_that("snap aligns a grid to parent lattice", {
  parent <- vast(c(10L, 10L), c(0, 10, 0, 10))  # res 1x1
  child <- vast(c(5L, 5L), c(2.3, 7.3, 1.7, 6.7))  # res 1x1 but shifted
  snapped <- snap(child, parent)
  expect_true(is_aligned(snapped, parent))
})

## ---- tiles ----

test_that("tiles cover the full grid", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  tt <- tiles(g, c(30L, 30L))
  ## total cells across tiles should equal ncell(g)
  total <- sum(vapply(tt, ncell, integer(1)))
  expect_equal(total, ncell(g))
})

test_that("tiles are aligned to parent", {
  g <- vast(c(100L, 100L), c(0, 10, 0, 10))
  tt <- tiles(g, c(30L, 30L))
  for (tile in tt) {
    expect_true(is_aligned(g, tile))
  }
})

test_that("tile dimensions are correct", {
  g <- vast(c(10L, 10L))
  tt <- tiles(g, c(3L, 3L))
  ## 4 tiles in each direction (3+3+3+1), 16 tiles total
  expect_length(tt, 16)
  ## edge tiles are smaller
  dims <- t(vapply(tt, function(t) t$dimension, integer(2)))
  expect_true(all(dims[, 1] <= 3L))
  expect_true(all(dims[, 2] <= 3L))
})

test_that("single tile for small grid", {
  g <- vast(c(5L, 5L))
  tt <- tiles(g, c(256L, 256L))
  expect_length(tt, 1)
  expect_identical(tt[[1]]$dimension, g$dimension)
})

## ---- vast_res ----

test_that("vast_res derives dimension", {
  g <- vast_res(c(0, 10, 0, 10), 0.5)
  expect_identical(g$dimension, c(20L, 20L))
  expect_equal(res(g), c(0.5, 0.5))
})

test_that("vast_res expands extent for non-divisible resolution", {
  g <- vast_res(c(0, 10, 0, 10), 3)
  ## 10/3 = 3.33 -> ceil = 4, extent becomes 0..12
  expect_identical(g$dimension, c(4L, 4L))
  expect_equal(g$extent[2], 12)
  expect_equal(g$extent[4], 12)
})

test_that("vast_res accepts scalar resolution", {
  g <- vast_res(c(0, 100, 0, 50), 10)
  expect_equal(res(g), c(10, 10))
})

## ---- as_vast ----

test_that("as_vast.vast is identity", {
  g <- vast(c(10L, 10L))
  expect_identical(as_vast(g), g)
})

test_that("as_vast.list works for controlledburn-like output", {
  obj <- list(dimension = c(100L, 200L), extent = c(0, 10, 0, 20))
  g <- as_vast(obj)
  expect_s3_class(g, "vast")
  expect_identical(g$dimension, c(100L, 200L))
})
