## align + controlledburn integration demo
##
## Run this locally where controlledburn is installed.
## It demonstrates:
##   1. burn polygons to sparse runs
##   2. wrap the grid as a vast object
##   3. crop to a subwindow, compute offset
##   4. materialise only the subwindow (no full-grid allocation)
##   5. plot with rasterImage (the happy path)

library(align)

## -- materialise_chunk: the key integration function --
##
## Takes a burn result (sparse runs on a parent grid) and materialises
## only the cells within a target vast subwindow.
##
## index:  4-column matrix (col_start, col_end, row, poly_id), 1-based
## parent: vast object for the full burn grid
## target: vast object for the subwindow to fill (must be aligned to parent)
##
## Returns a matrix(nrow = nrow(target), ncol = ncol(target)) with poly_id
## values (or NA for unfilled cells).

materialise_chunk <- function(index, parent, target) {
  stopifnot(is_vast(parent), is_vast(target))

  if (!is_aligned(target, parent)) {
    stop("target is not aligned to parent grid")
  }

  off <- offset(target, parent)  ## c(col = ..., row = ...)
  nr <- nrow(target)
  nc <- ncol(target)

  ## filter runs to those intersecting the target window
  row_lo <- off[["row"]] + 1L
  row_hi <- off[["row"]] + nr
  col_lo <- off[["col"]] + 1L
  col_hi <- off[["col"]] + nc

  keep <- index[, 3L] >= row_lo &
          index[, 3L] <= row_hi &
          index[, 2L] >= col_lo &
          index[, 1L] <= col_hi

  runs <- index[keep, , drop = FALSE]

  ## allocate output — byrow convention: matrix(, nrow, ncol)
  m <- matrix(NA_real_, nrow = nr, ncol = nc)

  if (nrow(runs) == 0L) return(m)

  ## reindex: shift to target-local coordinates
  local_row   <- runs[, 3L] - off[["row"]]
  local_start <- pmax(runs[, 1L] - off[["col"]], 1L)
  local_end   <- pmin(runs[, 2L] - off[["col"]], nc)

  for (i in seq_len(nrow(runs))) {
    cols <- local_start[i]:local_end[i]
    m[local_row[i], cols] <- runs[i, 4L]
  }
  m
}


## -- helper: plot a materialised chunk with rasterImage --

plot_chunk <- function(m, target, col = NULL, ...) {
  if (is.null(col)) {
    ids <- sort(unique(as.vector(m[!is.na(m)])))
    if (length(ids) == 0) ids <- 1
    pal <- hcl.colors(max(length(ids), 1), "Set2")
    col_map <- setNames(pal[seq_along(ids)], ids)
    col_m <- matrix(NA_character_, nrow = nrow(m), ncol = ncol(m))
    for (id in ids) col_m[m == id] <- col_map[as.character(id)]
  } else {
    col_m <- col
  }

  plot(NULL, xlim = target$extent[1:2], ylim = target$extent[3:4],
       asp = 1, xlab = "x", ylab = "y", ...)
  rasterImage(as.raster(col_m),
              target$extent[1], target$extent[3],
              target$extent[2], target$extent[4])
}


## ==================================================================
## DEMO: run this part where controlledburn + silicate are installed
## ==================================================================

if (requireNamespace("controlledburn", quietly = TRUE) &&
    requireNamespace("silicate", quietly = TRUE)) {

  pols <- silicate::inlandwaters

  ## define the parent grid
  ext <- unlist(lapply(silicate::sc_vertex(pols), range))
  dm <- c(500L, 400L)

  ## burn
  r <- controlledburn:::burn_polygon(pols, extent = ext, dimension = dm)

  ## unpack to 1-based index matrix
  index <- matrix(unlist(r, use.names = FALSE), ncol = 4L, byrow = TRUE) + 1L

  ## wrap as vast
  parent <- vast(dm, ext)
  cat("Parent grid:\n")
  print(parent)

  ## crop to a subwindow
  mid_x <- mean(ext[1:2])
  mid_y <- mean(ext[3:4])
  sub_ext <- c(mid_x - 2, mid_x + 2, mid_y - 2, mid_y + 2)
  target <- crop(parent, sub_ext)
  cat("\nTarget subwindow:\n")
  print(target)

  cat("\nOffset from parent:\n")
  print(offset(target, parent))

  ## materialise just the subwindow
  m <- materialise_chunk(index, parent, target)
  cat("\nMaterialised chunk: ", nrow(m), "x", ncol(m),
      "with", sum(!is.na(m)), "filled cells\n")

  ## plot it
  plot_chunk(m, target, main = "Cropped subwindow from controlledburn")

  ## also show full materialisation for comparison
  m_full <- materialise_chunk(index, parent, parent)
  plot_chunk(m_full, parent, main = "Full grid")

} else {
  message("controlledburn and/or silicate not available — ",
          "running synthetic test only")
}


## ==================================================================
## Synthetic test (runs everywhere)
## ==================================================================

## fake a burn result: a few horizontal runs on a 20x15 grid
parent <- vast(c(20L, 15L))

## index columns: col_start, col_end, row, poly_id (1-based)
index <- rbind(
  c( 3, 10,  2, 1),  # row 2, cols 3-10, polygon 1
  c( 5, 15,  3, 1),  # row 3, cols 5-15
  c( 8, 18,  4, 1),  # row 4, cols 8-18
  c( 2,  7, 10, 2),  # row 10, cols 2-7, polygon 2
  c( 3,  8, 11, 2),  # row 11, cols 3-8
  c( 4,  6, 12, 2)   # row 12, cols 4-6
)

## materialise the full grid
m_full <- materialise_chunk(index, parent, parent)
cat("\nSynthetic full grid:\n")
print(m_full)

## now crop to a subwindow and materialise just that
target <- crop(parent, c(4, 12, 3, 13))
cat("\nTarget:\n")
print(target)
cat("Offset:", offset(target, parent), "\n")

m_sub <- materialise_chunk(index, parent, target)
cat("\nSubwindow:\n")
print(m_sub)

## verify: the subwindow matches the corresponding region of the full grid
off <- offset(target, parent)
m_expected <- m_full[
  (off[["row"]] + 1):(off[["row"]] + nrow(target)),
  (off[["col"]] + 1):(off[["col"]] + ncol(target))
]
stopifnot(identical(m_sub, m_expected))
cat("\n✓ Subwindow matches full grid extraction.\n")

## test with grow/shrink
grown <- grow(parent, 2L)
m_grown <- materialise_chunk(index, parent, grown)
## the interior should match
off_g <- offset(parent, grown)
m_interior <- m_grown[
  (off_g[["row"]] + 1):(off_g[["row"]] + nrow(parent)),
  (off_g[["col"]] + 1):(off_g[["col"]] + ncol(parent))
]
stopifnot(identical(m_interior, m_full))
cat("✓ Grown grid interior matches full grid.\n")

## tiles test
tt <- tiles(parent, c(8L, 8L))
cat("\nTiling into", length(tt), "tiles:\n")
for (i in seq_along(tt)) {
  m_tile <- materialise_chunk(index, parent, tt[[i]])
  off_t <- offset(tt[[i]], parent)
  m_ref <- m_full[
    (off_t[["row"]] + 1):(off_t[["row"]] + nrow(tt[[i]])),
    (off_t[["col"]] + 1):(off_t[["col"]] + ncol(tt[[i]]))
  ]
  stopifnot(identical(m_tile, m_ref))
}
cat("✓ All", length(tt), "tiles match full grid.\n")
