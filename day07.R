#' ## Day 7: Bridge Repair 
#' https://adventofcode.com/2024/day/7
source("aoc.R")

test_in <- 
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

# make sure we'd not have to deal with scientific notation when coercing numeric to char  
options(scipen = 20)

calibration_df <- 
  test_in |>
  aoc_table(sep = ":", col.names = c("test_val", "equation"), strip.white = TRUE) |>
  within(equation <- strsplit(equation, " ") |> sapply(as.numeric))
calibration_df

# -------------------------------------------------------------------------
#' ### part1: detect correct equations
#' Detect which equations can produce test value. 
#' Numbers in equations are combined with operators (`+`, `*`), evaluated always from left to right.
#' Use recursion to build equation tree
# Ex: 3749

# Recursively test equations, in each call take first 2 elements, 
# apply operator and pass it along with remaining equation values;
# if number of equation values is reduced to 2, check if it matches final value;  
# `||` to evaluate from left to right and stop

test_equation <- function(equation, test_val, debug_ = FALSE){
  if (debug_) paste0(equation, collapse = ", ") |> paste0(" ? ", test_val, "; ") |> message(appendLF = FALSE)
  if (length(equation) == 2){
    if (debug_) sprintf("sum : %d; prod : %d", sum(equation), prod(equation)) |> message()
    return( sum(equation) ==  test_val || prod(equation) ==  test_val )
  } 
  return(
      test_equation(c( sum(equation[1:2]), equation[-(1:2)]), test_val, debug_) ||  
      test_equation(c(prod(equation[1:2]), equation[-(1:2)]), test_val, debug_)
  )
}
calibration_df |> 
  subset(subset = mapply(test_equation, equation, test_val, debug_ = TRUE), select = test_val) |> 
  sum()
  

# -------------------------------------------------------------------------
#' ### part2: add additional concat operator
#' Add another branch with new operator to equation tree;  
#' This naive approach now takes ~2min with actual puzzle input, 
#' let's switch to `furrr` for parallel processing.
# Ex: 11387
library(furrr)

num_concat <- \(x) as.character(x) |> paste0(collapse = "") |> as.numeric()

test_verif2 <- function(equation, test_val){
  if (length(equation) == 2){
    return(
      num_concat(equation) == test_val || sum(equation) ==  test_val || prod(equation) == test_val
    )
  } 
  return(
    test_verif2(c(num_concat(equation[1:2]), equation[-(1:2)]), test_val) ||  
    test_verif2(c(       sum(equation[1:2]), equation[-(1:2)]), test_val) ||  
    test_verif2(c(      prod(equation[1:2]), equation[-(1:2)]), test_val)
  )
}

plan(multisession, workers = parallelly::availableCores(omit = 1))

calibration_df |> 
  subset(subset = future_map2_lgl(equation, test_val, test_verif2, .progress = TRUE), select = test_val) |> 
  sum()
