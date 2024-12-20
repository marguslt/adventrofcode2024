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
## Day 3: Mull It Over

<https://adventofcode.com/2024/day/3>

``` r
source("aoc.R")

corrupted_mem_1 <- aoc_lines("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
corrupted_mem_2 <- aoc_lines("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
```

#### part1: add up real `mul()` instructions

``` r
# Ex: 161
corrupted_mem_1 |> 
  lapply(\(line) regmatches(line, gregexpr("(?<=mul\\()\\d+,\\d+(?=\\))", line, perl = TRUE))[[1]]) |> 
  unlist() |> 
  strsplit(",") |> 
  do.call(what = rbind) |> 
  `class<-`("numeric") |> 
  print() |> 
  apply(1, prod) |> 
  sum()
#>      [,1] [,2]
#> [1,]    2    4
#> [2,]    5    5
#> [3,]   11    8
#> [4,]    8    5
#> [1] 161
```

#### part2: handle `do()` & `don't()` instructions

Find locations for `do()` & `don't()` matches,
split input text index range by `do`/`don't` intervals,
keep only `do` ranges by checking first values in splits against `dos`;
return only matches where start value is within do_ranges.

``` r
# Ex: 48
do_matches <- function(x){
  dos <- c(1, gregexpr("do\\(\\)", x, perl = TRUE)[[1]])
  donts <- gregexpr("don't\\(\\)", x, perl = TRUE)[[1]]
  dd_splits <- split(1:nchar(x), findInterval(1:nchar(x), sort(c(dos, donts)))) 
  do_ranges <- dd_splits[sapply(dd_splits, `[`, 1) %in% dos] |> do.call(what = c)
  
  matches <- gregexpr("(?<=mul\\()\\d+,\\d+(?=\\))", x, perl = TRUE)
  regmatches(x, matches)[[1]][matches[[1]] %in% do_ranges]
}

corrupted_mem_2 |> 
  paste0(collapse = "") |> 
  do_matches() |> 
  strsplit(",") |> 
  do.call(what = rbind) |> 
  `class<-`("numeric") |> 
  print() |> 
  apply(1, prod) |> 
  sum()
#>      [,1] [,2]
#> [1,]    2    4
#> [2,]    8    5
#> [1] 48
```
