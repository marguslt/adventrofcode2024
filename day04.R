#' ## Day 4: Ceres Search 
#' https://adventofcode.com/2024/day/4
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
# -------------------------------------------------------------------------
#' ### part1: count occurrences of "XMAS" 
#' Can be horizontal, vertical, diagonal, also written backwards.  
#' - generate a list of array indices for left-to-right and diagonals (main diag. + parallel to main)
#' - extract strings through array indices, count "XMAS" occurrences
#' - rotate matrix 3x, extracting and counting in every cycle
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

counts <- vector(mode = "integer", length = 4)
for(i in seq_along(counts)){
  if (i > 1) m <- rotate_cw(m)
  counts[i] <- 
    sapply(arrind_lst, \(arr_ind) paste0(m[arr_ind], collapse = "")) |> 
    stringr::str_count(search_word) |> 
    print() |> 
    sum()
}
sum(counts)


# -------------------------------------------------------------------------
#' ### part2: find two "MAS" in the shape of an X
#' Search for "A"-locations (3x3 sub-matrices), 
#' exclude cases that are on the edge and check 
#' if we can get "MS" or "SM" from both diagonals.
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
