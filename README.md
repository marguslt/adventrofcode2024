## Day 1: Historian Hysteria

<https://adventofcode.com/2024/day/1>

``` r
source("aoc.R")

test_in <- 
"3   4
4   3
2   5
1   3
3   9
3   3"

(parsed_lists <- aoc_table(test_in, col.names = c("a", "b")))
#>   a b
#> 1 3 4
#> 2 4 3
#> 3 2 5
#> 4 1 3
#> 5 3 9
#> 6 3 3
str(parsed_lists)
#> 'data.frame':    6 obs. of  2 variables:
#>  $ a: int  3 4 2 1 3 3
#>  $ b: int  4 3 5 3 9 3
```

#### part1: diff of ordered list summed

``` r
# Ex: 11

parsed_lists |> 
  with(sort(a) - sort(b)) |> 
  abs() |> 
  sum() 
#> [1] 11

# Same with lapply() & do.call():
parsed_lists |> 
  lapply(sort) |> 
  do.call(what = `-`) |> 
  abs() |> 
  sum()
#> [1] 11
```

#### part2: similarity score

Multiply each `a` with its count in `b`, sum

``` r
# Ex: 31

parsed_lists |> 
  with(table(b)[as.character(a)] * a) |> 
  sum(na.rm = TRUE)
#> [1] 31
```
## Day 2: Red-Nosed Reports

<https://adventofcode.com/2024/day/2>

``` r
source("aoc.R")

test_in <- 
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

parsed_reports <- 
  aoc_lines(test_in) |> 
  strsplit(" ") |> 
  lapply(as.integer)
  
str(parsed_reports)
#> List of 6
#>  $ : int [1:5] 7 6 4 2 1
#>  $ : int [1:5] 1 2 7 8 9
#>  $ : int [1:5] 9 7 6 2 1
#>  $ : int [1:5] 1 3 2 4 5
#>  $ : int [1:5] 8 6 4 4 1
#>  $ : int [1:5] 1 3 6 7 9
```

#### part1: count strictly monotonic level sequences where step is at most 3

``` r
# Ex: 2

diff_within_tol <- \(x) all(abs(x) <= 3) 

diff_is_strictly_monotonic  <- function(x){
  signs <- sign(x) |> unique() 
  length(signs) == 1 && signs[1] != 0
} 

parsed_reports |> 
  lapply(diff) |> 
  sapply(\(x) diff_within_tol(x) && diff_is_strictly_monotonic(x)) |> 
  sum()
#> [1] 2
```

#### part2: tolerate a single bad level

``` r
# Ex: 4
parsed_reports |> 
  sapply(
    \(row) sapply(
      seq_along(row), \(idx) {
        dd <- diff(row[-idx])
        diff_within_tol(dd) && diff_is_strictly_monotonic(dd)
      } 
    ) |> any()
  ) |> sum()
#> [1] 4
```
