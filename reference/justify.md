# Align one set of cells with another set

If the header cells of a table aren't aligned to the left, right, top or
bottom of the data cells that they describe, then use `justify()` to
re-align them, using a second set of cells as a guide.

## Usage

``` r
justify(header_cells, corner_cells)
```

## Arguments

- header_cells:

  Data frame of data cells with at least the columns 'row' and 'column',
  which are `numeric` or `integer`.

- corner_cells:

  Data frame of header cells with at least the columns 'row' and
  'column', which are numeric/integer vectors. The same length as
  `header_cells`.

## Examples

``` r
header_cells <- tibble::tibble(row = c(1L, 1L, 1L, 1L),
                               col = c(3L, 5L, 8L, 10L),
                               value = LETTERS[1:4])
corner_cells <- tibble::tibble(row = c(2L, 2L, 2L, 2L),
                               col = c(1L, 4L, 6L, 9L))
justify(header_cells, corner_cells)
#> # A tibble: 4 × 3
#>     row   col value
#>   <int> <int> <chr>
#> 1     2     1 A    
#> 2     2     4 B    
#> 3     2     6 C    
#> 4     2     9 D    
```
