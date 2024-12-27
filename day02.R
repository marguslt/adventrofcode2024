#' ## Day 2: Red-Nosed Reports
#' https://adventofcode.com/2024/day/2
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
# -------------------------------------------------------------------------
#' ### part1: count safe reports
#' Count strictly monotonic level sequences where step is at most 3
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

# -------------------------------------------------------------------------
#' ### part2: tolerate a single bad level
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

