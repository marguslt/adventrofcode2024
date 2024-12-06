#' ## Day 1: Historian Hysteria
#' https://adventofcode.com/2024/day/1
source("aoc.R")

test_in <- 
"3   4
4   3
2   5
1   3
3   9
3   3"

(parsed_lists <- aoc_table(test_in, col.names = c("a", "b")))
str(parsed_lists)
# -------------------------------------------------------------------------
#' #### part1: diff of ordered list summed
# Ex: 11
parsed_lists |> 
  with(sort(a) - sort(b)) |> 
  abs() |> 
  sum() 

# Same with lapply() & do.call():
parsed_lists |> 
  lapply(sort) |> 
  do.call(what = `-`) |> 
  abs() |> 
  sum()

# -------------------------------------------------------------------------
#' #### part2: similarity score  
#' Multiply each `a` with its count in `b`, sum
# Ex: 31
parsed_lists |> 
  with(table(b)[as.character(a)] * a) |> 
  sum(na.rm = TRUE)

