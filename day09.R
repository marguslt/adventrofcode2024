#' ## Day 9: Disk Fragmenter 
#' https://adventofcode.com/2024/day/9
source("aoc.R")
library(tidyverse, warn.conflicts = FALSE)

options(scipen = 20)
test_in <- "2333133121414131402"

# -------------------------------------------------------------------------
#' #### part1: increase continuous free space by compacting files 
#' File map is sequence of single-digit block counts, starting with file block count 
#' and followed by free block count; ID of each file is a file sequence, starting from zero.
#' Move files 1 block a time from the end of disk to rightmost free block to fill all gaps,
#' and calculate new disk checksum.
#' - complete disk as a vector, `NA`s at free locations
#' - keep track of disk state through 2 vectros, one for free blocks and another for used blocks
# Ex: 1928
prn_disk <- \(disk) paste0(disk, collapse = "") |> str_replace_all("NA", ".")
dbg_header <- function (disk){
  sprintf("idx %66s", paste0(rep(c(1:9,0), length.out = length(disk)), collapse = "")) |> message()
  sprintf("%70s", prn_disk(disk)) |> message()
}

# checksum: each block's position multiplied by file id, summed
# file ids for checksum start with 0
checksum <- \(disk) sum(disk * (seq_along(disk) - 1), na.rm = TRUE)

disk_map <- 
  aoc_lines(test_in) |>
  str_split_1("") |> 
  # append 0 to get even number of values for 2-col matrix
  as.numeric() |> c(0) |> 
  matrix(ncol = 2, byrow = TRUE)  |> 
  `colnames<-`(c("len_file", "len_free")) |> 
  as_tibble() |> 
  mutate(
    # file id-s start from 0
    file_id = row_number() - 1,
    start_file = (cumsum(len_file + len_free) + 1) |> lag(default = 1),
    start_free = start_file + len_file,
    files = pick(len_file:file_id) |> pmap(\(len_file, len_free, file_id) c(rep(file_id, len_file), rep(NA, len_free)))
  )
disk_map |> 
  mutate(files = map_chr(files, paste, collapse = ",")) |> 
  relocate(file_id, .before = 1)

# whole disk, each item is single block, values are file id-s
(disk <- list_c(disk_map$files))
# free block idxs to move files to
(head_free <- which(is.na(disk)))
# used block idxs to move files from, reversed
(tail_used <- which(!is.na(disk)) |> rev())

# loop through head_free & tail_used, 
# swap block in disk vector;
# stop when tail meets head
idx <- 1
{
  dbg_header(disk)
  while (head_free[idx] < tail_used[idx]) {
    disk[c(head_free[idx], tail_used[idx])] <- disk[c(tail_used[idx], head_free[idx])]
    sprintf("%2s: disk[%2s] <-%s-> disk[%2s] %s", idx, head_free[idx], disk[head_free[idx]], tail_used[idx], prn_disk(disk)) |> message()
    idx <- idx + 1
  }
}
checksum(disk)

# -------------------------------------------------------------------------
#' #### part2: instead of moving blocks move whole files
#' Move whole files in order of decreasing file ID number.
#' - create another copy of a disk vector, `disk_free_lenghts`, to keep track of free locations
#' - 
#' - 
# Ex: 2858
# used with pmap; len_file, len_free are column names in input df
free_lengths <- \(len_file, len_free, ...) c(rep(0, len_file), rev(seq_len(len_free)))

(disk <- list_c(disk_map$files))
# generate disk_free_lengths from disk_map$len_file & disk_map$len_free 
(disk_free_lengths <- pmap(disk_map, free_lengths) |> list_c())

# disk map ordered by descending file id 
rev_dm <- 
  disk_map |> 
  select(id = file_id, start = start_file, len = len_file) |> 
  arrange(desc(id)) |> 
  print()

# loop though rev_dm rows, swap whole blocks in disk
for(rev_dm_ptr in seq_len(nrow(rev_dm))){
  f <- rev_dm[rev_dm_ptr,]
  free_ptr <- which.max(disk_free_lengths >= f$len)
  
  if (free_ptr > 1 && free_ptr < f$start) {
    from_range <- f$start:(f$start + f$len - 1)
    to_range <- free_ptr:(free_ptr + f$len - 1)
    disk[c(to_range, from_range)] <- disk[c(from_range, to_range)]
    disk_free_lengths[to_range] <- 0
    
    sprintf(
      "%2s: disk[%5s] <-%s-> disk[%5s] %s", rev_dm_ptr,
      paste0(sprintf("%.2d", range(from_range)), collapse = ":"),
      disk[free_ptr],
      paste0(sprintf("%.2d", range(to_range))  , collapse = ":"),
      prn_disk(disk)
    ) |>
    message()
  }
}
checksum(disk) 
