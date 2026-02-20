#' @export
res.vast <- function(x, ...) {
  c(diff(x$extent[1:2]) / x$dimension[1],
    diff(x$extent[3:4]) / x$dimension[2])
}

#' @export
dim.vast <- function(x) {
  ## R convention: c(nrow, ncol) — reversed from internal c(ncol, nrow)
  rev(x$dimension)
}

#' @export
ncell.vast <- function(x, ...) x$dimension[1] * x$dimension[2]

#' @export
#' @rdname x
x.vast <- function(x, ...) {
  dx <- diff(x$extent[1:2]) / x$dimension[1]
  x$extent[1] + (seq_len(x$dimension[1]) - 0.5) * dx
}

#' @export
#' @rdname y
y.vast <- function(x, ...) {
  dy <- diff(x$extent[3:4]) / x$dimension[2]
  x$extent[4] - (seq_len(x$dimension[2]) - 0.5) * dy
}

