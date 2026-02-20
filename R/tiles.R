#' @param tile_dim integer `c(ncol, nrow)` for each tile. Edge tiles
#'   may be smaller.
#' @rdname tiles
#' @export
tiles.vast <- function(x, tile_dim = c(256L, 256L), ...) {
  tile_dim <- as.integer(rep_len(tile_dim, 2L))
  stopifnot(all(tile_dim > 0L))

  r <- res(x)
  nx <- ceiling(x$dimension[1] / tile_dim[1])
  ny <- ceiling(x$dimension[2] / tile_dim[2])

  out <- vector("list", nx * ny)
  k <- 0L
  for (j in seq_len(ny)) {
    for (i in seq_len(nx)) {
      col_start <- (i - 1L) * tile_dim[1]
      row_start <- (j - 1L) * tile_dim[2]
      nc <- min(tile_dim[1], x$dimension[1] - col_start)
      nr <- min(tile_dim[2], x$dimension[2] - row_start)

      ext <- c(
        x$extent[1] + col_start * r[1],
        x$extent[1] + (col_start + nc) * r[1],
        x$extent[4] - (row_start + nr) * r[2],
        x$extent[4] - row_start * r[2]
      )
      k <- k + 1L
      out[[k]] <- vast(c(nc, nr), ext)
    }
  }
  out
}
