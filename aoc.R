# AOC 2024 utilities

#' `readLines()` wrapper for reading puzzle inputs from literal strings and files
#'
#' @param input Path to a puzzle input file or literal data
#' @param ... Passed to `readLines()`
#'
#' @return string vector, same as readLines()
#' @export

aoc_lines <- function(input, ...){
  if(!file.exists(input)) {
    input <- textConnection(input)
  }
  readLines(input, ...)
}

#' `read.table()` wrapper for reading puzzle inputs from literal strings and files
#'
#' @param input Path to a puzzle input file or literal data
#' @param ...Passed to `readLines()` 
#'
#' @return A data frame, same as `read.table()`
#' @export
#'
#' @examples
aoc_table <- function(input, ...){
  if(file.exists(input)) {
    read.table(file = input, ...)
  } else {
    read.table(text = input, ...)
  }
}

# options
options(reprex.advertise = FALSE)


# usage
# render with:
# reprex::reprex(input = "day01.R")