#' ## Day 11: Plutonian Pebbles 
#' https://adventofcode.com/2024/day/11
source("aoc.R")

test_in <- "125 17"
stones <- 
  aoc_lines(test_in) |> 
  strsplit(" ") |> 
  _[[1]] |> 
  as.numeric() |> 
  as.list()
lobstr::tree(stones)
# -------------------------------------------------------------------------
#' #### part1: count stones after 25 blinks
#' A collection of numbered stones change each time we blink, according to 1st matching rule:  
#' - *0* turns into *1*
#' - stone with even number of digits is split into 2, new numbers are left and right half of digits
#' - if no rules applied, number is multiplied by 2024  
#' 
#' Naive approach: implement `blink()` as a function that takes a stone and returns a list of stone(s), 
#' recursively apply this on input list 25 times to create a nested list, flatten resulting list and count items.
#' Apparently blinking does not generate too many unique values, 
#' to speed things up, blink in batches of five and cache batch results with `memoise`.
# Ex: 55312
library(memoise)

blink <- function(stone){
  if (isTRUE(all.equal(stone,0))){
    return(list(1))
    
  } else if (nchar(stone) %% 2 == 0) {
    l <- nchar(stone)
    return(
      c(substr(stone, 1, l / 2), substring(stone, (l / 2) + 1)) |> 
        as.numeric() |> 
        as.list()
    )
  } else {
    return(list(stone * 2024))
  }
}

# blink once:
rapply(stones, blink, how = "list") |> lobstr::tree()
rapply(stones, blink, how = "list") |> unlist(recursive = TRUE)

# blink at a single stone n times, return a flat vector of stones;
# cached through memoise
blink_n <- memoise(function(stone, n = 5){
  Reduce(
    \(x, ...) rapply(x, blink, how = "list"), 
    x = 1:n, init = list(stone), simplify = FALSE
  ) |> 
  unlist(recursive = TRUE)
})
blink_n(stones[[1]], n = 5) 

# apply blink_n(..., n = 5) 5 times (5*5=25 blinks in total) on each input stone, 
# after each blink_n, flatten list of stones to a vector;
# get the length of final vector
Reduce(
  f = \(stones, ...) lapply(stones, blink_n, n = 5) |> unlist(), 
  x = 1:5, init = stones
) |> length()

# -------------------------------------------------------------------------
#' #### part2: count stones after 75 blinks
#' Previous solution does not scale too well after 30 or so blinks, 
#' instead keep track of the number of unique stones. 
#' Use `fastmap` for fast key-value storage, each key presents unique stone 
#' number, values are stone counts.

library(fastmap)
options(scipen = 999)

pebbles <- fastmap(missing_default = 0)
stones |> 
  setNames(unlist(stones)) |> 
  lapply(\(x) 1) |> 
  pebbles$mset(.list = _)
pebbles$mget(pebbles$keys()) |> lobstr::tree()

for (i in 1:75){
  # keep `pre_blink` state, `pebbles` will hold current blink state 
  pre_blink <- pebbles$clone()
  pebbles$reset()
  for (k in pre_blink$keys()) {
    # not vectorized, blink at every unique stone, one at a time;
    # update values, pebbles$get() is configured to return 0 for non-existing keys 
    v <- pre_blink$get(k)
    if (as.numeric(k) == 0) {
      pebbles$set("1", v + pebbles$get("1"))
    } else if (nchar(k) %% 2 == 0){
      left_  <- substr(k, start = 1, stop = nchar(k) %/% 2) |> as.numeric() |> as.character()
      right_ <- substr(k, start = nchar(k) %/% 2 + 1, stop = 999) |> as.numeric() |> as.character()
      pebbles$set(left_, v + pebbles$get(left_))
      pebbles$set(right_, v + pebbles$get(right_))
    } else {
      k_ <- as.character(as.numeric(k) * 2024)
      pebbles$set(k_, v + pebbles$get(k_))
    }
  }
  if (i %% 25 == 0){
    sprintf(
      "Unique stones after blink %.2d: %4.d; sum: %16.f", 
      i, pebbles$size(), pebbles$as_list() |> unlist() |> sum()
    ) |> message()
  } 
}
pebbles$as_list() |> unlist() |> sum()

