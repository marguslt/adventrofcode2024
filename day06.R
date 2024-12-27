#' ## Day 6: Guard Gallivant 
#' https://adventofcode.com/2024/day/6
source("aoc.R")

test_in <- 
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
m <- 
  aoc_lines(test_in) |> 
  strsplit("") |> 
  do.call(what = rbind)

dbg_print <- function(obstr_pos, start_pos, m, msg = ""){
  m[obstr_pos] <- "O"
  m[start_pos] <- "^"
  cat(msg, obstr_pos) |> message()
  rbind(c(1:9,0),m) |> 
    cbind(c(" ",1:9,0), y = _) |> 
    apply(1, paste0, collapse = "|") |> 
    paste0(collapse = "\n") |> 
    paste0("\n") |> 
    cat() |> 
    message()
}

# -------------------------------------------------------------------------
#' ### part1: predict the guard's route
#' Predict guards path starting from `^` (indicates direction) until she leaves 
#' the map area and count distinct positions.  
#' When facing obstacles , `#`, guard starts to turn clockwise. 
#' - navigation helper to get next location from current directiion & position
#' - mark current position in `m` with `X`, turn until path ahead is clear move forwards
#' - repeat until *subscript out of bounds* error (guard has left map area)
# Ex: 41

# navigation helper
nav <-  list(
  u = list(trn = "r", nxt = \(pos) pos + c(-1, 0)),
  r = list(trn = "d", nxt = \(pos) pos + c( 0, 1)),
  d = list(trn = "l", nxt = \(pos) pos + c( 1, 0)),
  l = list(trn = "u", nxt = \(pos) pos + c( 0,-1))
)

pos <- start_pos <- which(m == "^", arr.ind = TRUE)
dir <- "u"
try(
  repeat{
    m[pos] <- "X"
    while(m[nav[[dir]]$nxt(pos)] == "#") dir <- nav[[dir]]$trn
    pos <- nav[[dir]]$nxt(pos)
  }
)

dbg_print(NA, start_pos, m)
sum(m == "X")

# -------------------------------------------------------------------------
#' ### part2: count looping obstruction positions
#' Place new obstruction to catch guard in a loop, find number of suitable obstruction positions  
#' Brute force though all previously marked locations (start point removed) 
#' - start with matrix from part1 (or original)
#' - add an obstacle on to path
#' - restart tracing from the start position, mark positions with current moving direction
#' - if current position was already marked with current direction, we've created a loop
# Ex: 6

is_loop_obstruction <- function(obstr_pos, start_pos, m, nav){
  # obstr_pos input is likely a numeric vector of length 2, 
  # make it an array so it would work as an array index
  obstr_pos <- array(obstr_pos, dim = 1:2)
  m[obstr_pos] <- "#"
  pos <- start_pos
  dir <- "u"
  tryCatch(
    repeat{
      # return if we already have visited the same position from the same(!) direction
      if (m[pos] == dir) {
        # dbg_print(obstr_pos, start_pos, m, "loop obstr. @")
        # 1st loop obstr. @ 9 2
        #  |1|2|3|4|5|6|7|8|9|0
        # 1|.|.|.|.|#|.|.|.|.|.
        # 2|.|.|.|.|u|r|r|r|r|#
        # 3|.|.|.|.|u|.|.|.|d|.
        # 4|.|.|#|.|u|.|.|.|d|.
        # 5|.|.|u|r|r|r|r|#|d|.
        # 6|.|.|u|.|u|.|d|.|d|.
        # 7|.|#|u|l|^|l|d|l|d|.
        # 8|.|X|u|X|X|X|d|X|#|.
        # 9|#|O|l|l|l|l|d|X|.|.
        # 0|.|.|.|.|.|.|#|X|.|.
   
        return(TRUE)
      }
      m[pos] <- dir
      while(m[nav[[dir]]$nxt(pos)] == "#") {
        dir <- nav[[dir]]$trn
      }
      pos <- nav[[dir]]$nxt(pos)
    },
    # out of bounds subscript, guard has found a way out
    error = \(e) FALSE
  )
}

which(m == "X", arr.ind = TRUE) |> 
  as.data.frame() |> 
  print(max = 10) |>  
  subset(!(row == start_pos[,"row"] & col == start_pos[,"col"])) |> 
  apply(1, \(pos) is_loop_obstruction(obstr_pos = pos, start_pos = start_pos, m = m, nav = nav)) |> 
  sum()

