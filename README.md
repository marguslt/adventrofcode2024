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
## Day 4: Ceres Search

<https://adventofcode.com/2024/day/4>

``` r
source("aoc.R")

test_in <- 
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

m <- 
  aoc_lines(test_in) |>
  strsplit("") |> 
  do.call(what = rbind)
m
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,] "M"  "M"  "M"  "S"  "X"  "X"  "M"  "A"  "S"  "M"  
#>  [2,] "M"  "S"  "A"  "M"  "X"  "M"  "S"  "M"  "S"  "A"  
#>  [3,] "A"  "M"  "X"  "S"  "X"  "M"  "A"  "A"  "M"  "M"  
#>  [4,] "M"  "S"  "A"  "M"  "A"  "S"  "M"  "S"  "M"  "X"  
#>  [5,] "X"  "M"  "A"  "S"  "A"  "M"  "X"  "A"  "M"  "M"  
#>  [6,] "X"  "X"  "A"  "M"  "M"  "X"  "X"  "A"  "M"  "A"  
#>  [7,] "S"  "M"  "S"  "M"  "S"  "A"  "S"  "X"  "S"  "S"  
#>  [8,] "S"  "A"  "X"  "A"  "M"  "A"  "S"  "A"  "A"  "A"  
#>  [9,] "M"  "A"  "M"  "M"  "M"  "X"  "M"  "M"  "M"  "M"  
#> [10,] "M"  "X"  "M"  "X"  "A"  "X"  "M"  "A"  "S"  "X"
```

#### part1: count occurrences of “XMAS” – horizontal, vertical, diagonal, also written backwards

- generate a list of array indices for left-to-right and diagonals (main diag. + parallel to main)
- extract strings through array indices, count “XMAS” occurrences
- rotate matrix 3x, extracting and counting in every cycle

``` r
# Ex: 18

# Generate a list of array indices to subset rows, main diagonal and its parallels from a matrix
# @param sqr_m_dim Square matrix dimension (row or column count)
# @param min_diag_length minimum diagonal length, > 1 excludes corners
# @returns List of array indices
make_arrind_lst <- function(sqr_m_dim, min_diag_length = 1){
  m_seq <- 1:sqr_m_dim

  # parallel to diag, lower triangle
  diag_lower_tri <- 
    lapply(
      1:(sqr_m_dim - min_diag_length), 
      \(x) cbind(row = tail(m_seq, -x), col = head(m_seq, -x))
    )
  # upper triangle
  diag_upper_tri <- lapply(diag_lower_tri, \(arr_ind) arr_ind[,2:1])
  
  c(
    # rows, left to right
    row = expand.grid(row = 1:sqr_m_dim, col = 1:sqr_m_dim) |> 
      split(1:sqr_m_dim) |> 
      lapply(as.matrix),
    lo = rev(diag_lower_tri),
    # main diagonal
    diag = list(cbind(row = 1:sqr_m_dim, col = 1:sqr_m_dim)), 
    up = diag_upper_tri
  )
}

# rotate matrix
rotate_cw <- \(m) t(apply(m, 2, rev))

search_word <- "XMAS"
arrind_lst <- make_arrind_lst(dim(m)[1], min_diag_length = nchar(search_word))

# test with un-roatet matrix
lapply(arrind_lst, \(arr_ind) paste0(m[arr_ind], collapse = "")) |> str()
#> List of 23
#>  $ row.1 : chr "MMMSXXMASM"
#>  $ row.2 : chr "MSAMXMSMSA"
#>  $ row.3 : chr "AMXSXMAAMM"
#>  $ row.4 : chr "MSAMASMSMX"
#>  $ row.5 : chr "XMASAMXAMM"
#>  $ row.6 : chr "XXAMMXXAMA"
#>  $ row.7 : chr "SMSMSASXSS"
#>  $ row.8 : chr "SAXAMASAAA"
#>  $ row.9 : chr "MAMMMXMMMM"
#>  $ row.10: chr "MXMXAXMASX"
#>  $ lo1   : chr "SAMX"
#>  $ lo2   : chr "XMXMA"
#>  $ lo3   : chr "XXSAMX"
#>  $ lo4   : chr "MMAMMXM"
#>  $ lo5   : chr "ASAMSAMA"
#>  $ lo6   : chr "MMASMASMS"
#>  $ diag  : chr "MSXMAXSAMX"
#>  $ up1   : chr "MASAMXXAM"
#>  $ up2   : chr "MMXSXASA"
#>  $ up3   : chr "SXMMAMS"
#>  $ up4   : chr "XMASMA"
#>  $ up5   : chr "XSAMM"
#>  $ up6   : chr "MMMX"

counts <- vector(mode = "integer", length = 4)
for(i in seq_along(counts)){
  if (i > 1) m <- rotate_cw(m)
  counts[i] <- 
    sapply(arrind_lst, \(arr_ind) paste0(m[arr_ind], collapse = "")) |> 
    stringr::str_count(search_word) |> 
    print() |> 
    sum()
}
#>  [1] 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0
#>  [1] 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 1 0 0 0 0 1 0 0
#>  [1] 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 1 1 0 0 0 1 0 1
#>  [1] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
sum(counts)
#> [1] 18
```

#### part2: find two “MAS” in the shape of an X

Search for “A”-locations (3x3 sub-matrices),
exclude cases that are on the edge and check
if we can get “MS” or “SM” from both diagonals.

``` r
# Ex: 9

# Example "MAS" to search for:
# M.S
# .A.
# M.S

which(m == "A", arr.ind = TRUE) |> 
  as.data.frame() |> 
  # exclude edge locations
  subset(!(row %in% c(1, nrow(m)) | col %in% c(1, ncol(m)))) |> 
  # split by row, add dim attributes to get valid array index
  asplit(1) |> 
  lapply(array, dim = 1:2) |> 
  sapply(\(arr.ind){
    # check 3x3 sub matrix corners
    paste0(m[arr.ind + c(1, 1)], m[arr.ind + c(-1,-1)]) %in% c("MS", "SM") & 
    paste0(m[arr.ind + c(1,-1)], m[arr.ind + c(-1, 1)]) %in% c("MS", "SM")  
  }) |> 
  sum()
#> [1] 9
```
## Day 5: Print Queue

<https://adventofcode.com/2024/day/5>

``` r
source("aoc.R")
library(igraph, warn.conflicts = FALSE)

test_in <- 
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

parsed_lst <- 
  aoc_lines(test_in) |> 
  strsplit("[|,]") |>
  {\(l) split(l, cumsum(lengths(l) == 0))}() |> 
  setNames(c("rules", "updates")) |> 
  lapply(\(x) x[lengths(x) > 0])

str(parsed_lst, list.len = 3)
#> List of 2
#>  $ rules  :List of 21
#>   ..$ : chr [1:2] "47" "53"
#>   ..$ : chr [1:2] "97" "13"
#>   ..$ : chr [1:2] "97" "61"
#>   .. [list output truncated]
#>  $ updates:List of 6
#>   ..$ : chr [1:5] "75" "47" "61" "53" ...
#>   ..$ : chr [1:5] "97" "61" "53" "29" ...
#>   ..$ : chr [1:3] "75" "29" "13"
#>   .. [list output truncated]
```

#### part1: count updates that comply with all page ordering rules

Rule `47|53` means that if both pages occur in update, 47 must be printed before 53,
though not essentially immediately before 53.
Elves need to know the middle page number of each correct update, answer is some of those.
- use pages in rules as edge lists (to-from) to build a graph
- generate a sequence of vertex pairs from rules and check if all are adjacent
- check rule compliance, subset valid updates, extract middle page, sum

``` r
# Ex: 143
check_page_order <- function(pages, g){
  # 75,47,61,53,29 -> from = 75, to = 47; ... ; from = 53, to = 29
  mapply(
    from = head(pages, -1), 
    to = pages[-1],
    FUN = \(from, to) are_adjacent(g, from, to) 
  )
}

g <- 
  parsed_lst$rules |> 
  do.call(what = rbind) |> 
  graph_from_data_frame()
plot(g)
```

![](img/day05_reprex_files_unnamed-chunk-4-1.png)<!-- -->

``` r

rule_compliance <- sapply(parsed_lst$updates, check_page_order, g = g)
is_valid <- sapply(rule_compliance, all)

parsed_lst$updates[is_valid] |> 
  sapply(\(x) x[ceiling(length(x) / 2)]) |> 
  as.numeric() |> 
  sum()
#> [1] 143
```

#### part2: fix failed updates

Swap 1st failed location and next position until rule check passes

``` r
# Ex: 123
# 97,13,75,29,47 >> 97,75,47,29,13
mapply(
  u  = parsed_lst$updates[!is_valid], 
  rc = rule_compliance[!is_valid], 
  FUN = \(u, rc) {
    while (!all(rc)){
      inv_idx <- which.min(rc)
      u[c(inv_idx, inv_idx + 1)] <- u[c(inv_idx + 1, inv_idx)]
      rc <- check_page_order(u, g)
    }
    u[ceiling(length(u) / 2)] |> as.numeric()
  }
) |> sum()
#> [1] 123
```
## Day 6: Guard Gallivant

<https://adventofcode.com/2024/day/6>

``` r
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
```

#### part1: predict the guard’s route

Predict guards path starting from `^` (indicates direction) until she leaves
the map area and count distinct positions.  
When facing obstacles , `#`, guard starts to turn clockwise.
- navigation helper to get next location from current directiion & position
- mark current position in `m` with `X`, turn until path ahead is clear move forwards
- repeat until *subscript out of bounds* error (guard has left map area)

``` r
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
#> Error in m[nav[[dir]]$nxt(pos)] : subscript out of bounds

dbg_print(NA, start_pos, m)
#>  NA
#> 
#>  |1|2|3|4|5|6|7|8|9|0
#> 1|.|.|.|.|#|.|.|.|.|.
#> 2|.|.|.|.|X|X|X|X|X|#
#> 3|.|.|.|.|X|.|.|.|X|.
#> 4|.|.|#|.|X|.|.|.|X|.
#> 5|.|.|X|X|X|X|X|#|X|.
#> 6|.|.|X|.|X|.|X|.|X|.
#> 7|.|#|X|X|^|X|X|X|X|.
#> 8|.|X|X|X|X|X|X|X|#|.
#> 9|#|X|X|X|X|X|X|X|.|.
#> 0|.|.|.|.|.|.|#|X|.|.
#> 
sum(m == "X")
#> [1] 41
```

#### part2: place new obstruction to catch guard in a loop, find number of suitable obstruction positions

Brute force though all previously marked locations (start point removed)
- start with matrix from part1 (or original)
- add an obstacle on to path
- restart tracing from the start position, mark positions with current moving direction
- if current position was already marked with current direction, we’ve created a loop

``` r
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
#>   row col
#> 1   8   2
#> 2   9   2
#> 3   5   3
#> 4   6   3
#> 5   7   3
#>  [ reached 'max' / getOption("max.print") -- omitted 36 rows ]
#> [1] 6
```
## Day 7: Bridge Repair

<https://adventofcode.com/2024/day/7>

``` r
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
#>   test_val      equation
#> 1      190        10, 19
#> 2     3267    81, 40, 27
#> 3       83         17, 5
#> 4      156         15, 6
#> 5     7290   6, 8, 6, 15
#> 6   161011    16, 10, 13
#> 7      192     17, 8, 14
#> 8    21037  9, 7, 18, 13
#> 9      292 11, 6, 16, 20
```

#### part1: detect which equations can produce test value

Numbers in equations are combined with operators (`+`, `*`), evaluated always from left to right.
Use recursion to build equation tree

``` r
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
#> 10, 19 ? 190; sum : 29; prod : 190
#> 81, 40, 27 ? 3267; 121, 27 ? 3267; sum : 148; prod : 3267
#> 17, 5 ? 83; sum : 22; prod : 85
#> 15, 6 ? 156; sum : 21; prod : 90
#> 6, 8, 6, 15 ? 7290; 14, 6, 15 ? 7290; 20, 15 ? 7290; sum : 35; prod : 300
#> 84, 15 ? 7290; sum : 99; prod : 1260
#> 48, 6, 15 ? 7290; 54, 15 ? 7290; sum : 69; prod : 810
#> 288, 15 ? 7290; sum : 303; prod : 4320
#> 16, 10, 13 ? 161011; 26, 13 ? 161011; sum : 39; prod : 338
#> 160, 13 ? 161011; sum : 173; prod : 2080
#> 17, 8, 14 ? 192; 25, 14 ? 192; sum : 39; prod : 350
#> 136, 14 ? 192; sum : 150; prod : 1904
#> 9, 7, 18, 13 ? 21037; 16, 18, 13 ? 21037; 34, 13 ? 21037; sum : 47; prod : 442
#> 288, 13 ? 21037; sum : 301; prod : 3744
#> 63, 18, 13 ? 21037; 81, 13 ? 21037; sum : 94; prod : 1053
#> 1134, 13 ? 21037; sum : 1147; prod : 14742
#> 11, 6, 16, 20 ? 292; 17, 16, 20 ? 292; 33, 20 ? 292; sum : 53; prod : 660
#> 272, 20 ? 292; sum : 292; prod : 5440
#> [1] 3749
```

#### part2: add additional concatenation operator

Add another branch with new operator to equation tree;  
This naive approach now takes ~2min with actual puzzle input,
let’s switch to `furrr` for parallel processing.

``` r
# Ex: 11387
library(furrr)
#> Loading required package: future

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
#> [1] 11387
```
