
# align

Grid specification primitive for R. A `vast` object is six numbers — dimension and extent — and everything else is arithmetic.

## Install

```r
remotes::install_github("mdsumner/align")
```

## What it does

```r
library(align)

g <- vast(c(500L, 300L), extent = c(140, 160, -50, -30))
g
#> <vast> 500 x 300 (0.04 x 0.0666667)
#>   x: [140, 160]
#>   y: [-50, -30]
```

Resolution, coordinates, and cell indices are derived on demand. Nothing is stored beyond `dimension` (ncol, nrow) and `extent` (xmin, xmax, ymin, ymax).

```r
res(g)
#> [1] 0.04000000 0.06666667

xy_from_cell(g, 1L)          # top-left cell centre
cell_from_xy(g, c(150, -40)) # which cell contains this point?
```

## Grid operations

Crop and expand snap to cell boundaries. Resolution is preserved, dimensions adjust.

```r
h <- crop(g, c(145, 155, -45, -35))
expand(h, g$extent)  # back to the original

shrink(g)       # remove 1 cell from each edge
grow(g, 5L)     # add 5 cells to each edge
resize(g, 2L)   # double the resolution, same extent
```

`shrink` and `grow` are inverses. `crop` and `expand` are inverses. `resize(g, k)` and `resize(g, 1/k)` are inverses.

## The alignment prison

Two grids are compatible if their cell boundaries coincide — same origin modulo resolution, commensurate resolutions. If they are, every index operation produces exact integer results. If they aren't, `align` refuses to proceed.

```r
is_aligned(g, crop(g, c(145, 155, -45, -35)))
#> [1] TRUE

is_aligned(g, vast(c(500L, 300L), c(140.01, 160.01, -50, -30)))
#> [1] FALSE

offset(crop(g, c(142, 158, -48, -32)), g)
#> col row
#>  50  30
```

## Tiles

Partition a grid into subwindows. Pure index arithmetic — no data, no I/O.

```r
tiles(g, c(256L, 256L))
#> list of vast objects, each 256x256 or smaller at edges
```

## Constructors

```r
vast(c(ncol, nrow))                          # unit cell space
vast(c(ncol, nrow), c(xmin, xmax, ymin, ymax))
vast_res(c(xmin, xmax, ymin, ymax), res)     # derive dimension from resolution

as_vast(terra_raster)
as_vast(list(dimension = ..., extent = ...)) # e.g. controlledburn output
```

## What it does NOT do

Store raster values. Read or write files. Handle CRS. Do geometry operations. Allocate large arrays.

`align` is six numbers and arithmetic on them.

## See also

[vaster](https://github.com/hypertidy/vaster), [grout](https://github.com/hypertidy/grout), [controlledburn](https://github.com/hypertidy/controlledburn)

