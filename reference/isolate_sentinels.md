# Move sentinel values into a separate column leaving NA behind

A sentinel value, takes the place of a value that isn't available for
some reason. `isolate_sentinels()` removes these values from a column of
data into a separate column, and optionally converts the data left
behind into an appropriate data type.

## Usage

``` r
isolate_sentinels(.data, col, sentinels, into = "sentinel")
```

## Arguments

- .data:

  A data frame.

- col:

  The name of the column of data containing sentinel values.

- sentinels:

  A vector of sentinel values to be removed.

- into:

  A name to give the new column of sentinel values.

## Examples

``` r
x <- data.frame(name = c("Matilda", "Nicholas", "Olivia", "Paul"),
                score = c(10, "confidential", "N/A", 12),
                stringsAsFactors = FALSE)
x
#>       name        score
#> 1  Matilda           10
#> 2 Nicholas confidential
#> 3   Olivia          N/A
#> 4     Paul           12
isolate_sentinels(x, score, c("confidential", "N/A"))
#>       name score     sentinel
#> 1  Matilda    10         <NA>
#> 2 Nicholas  <NA> confidential
#> 3   Olivia  <NA>          N/A
#> 4     Paul    12         <NA>
isolate_sentinels(x, score, c("confidential", "N/A"), "flag")
#>       name score         flag
#> 1  Matilda    10         <NA>
#> 2 Nicholas  <NA> confidential
#> 3   Olivia  <NA>          N/A
#> 4     Paul    12         <NA>
```
