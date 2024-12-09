#' ## Day 9: Disk Fragmenter 
#' https://adventofcode.com/2024/day/9
source("aoc.R")
library(tidyverse, warn.conflicts = FALSE)

options(scipen = 20)
test_in <- "2333133121414131402"

# -------------------------------------------------------------------------
#' #### part1: 
# Ex: 1928
prn_disk <- \(disk) paste0(disk, collapse = "") |> str_replace_all("NA", ".")
dbg_header <- function (disk){
  sprintf("idx %66s", paste0(rep(c(1:9,0), length.out = length(disk)), collapse = "")) |> message()
  sprintf("%70s", prn_disk(disk)) |> message()
}

# file ids for checksum start with 0
checksum <- \(disk) sum(disk * (seq_along(disk) - 1), na.rm = TRUE)

disk_map <- 
  aoc_lines(test_in) |>
  str_split_1("") |> 
  # append 0
  as.numeric() |> c(0) |> 
  matrix(ncol = 2, byrow = TRUE)  |> 
  `colnames<-`(c("len_file", "len_free")) |> 
  as_tibble() |> 
  mutate(
    file_id = row_number() - 1,
    start_file = (cumsum(len_file + len_free) + 1) |> lag(default = 1),
    start_free = start_file + len_file,
    files = pick(len_file:file_id) |> pmap(\(len_file, len_free, file_id) c(rep(file_id, len_file), rep(NA, len_free)))
  )
disk_map |> 
  mutate(files = map_chr(files, paste, collapse = ",")) |> 
  relocate(file_id, .before = 1)

disk <- list_c(disk_map$files)
head_free <- which(is.na(disk))
tail_used <- which(!is.na(disk)) |> rev()
idx <- 1
{ 
  dbg_header(disk)
  while (head_free[idx] < tail_used[idx]) {
    disk[c(head_free[idx], tail_used[idx])] <- disk[c(tail_used[idx], head_free[idx])]
    sprintf("%2s: disk[%2s] <-%s-> disk[%2s] %s", idx, head_free[idx], disk[head_free[idx]], tail_used[idx], prn_disk(disk)) |> message()
    idx <- idx + 1
  }
  checksum(disk)
}
# -------------------------------------------------------------------------
#' #### part2: 
# Ex: 
free_lengths <- \(len_file, len_free, ...) c(rep(0, len_file), rev(seq_len(len_free)))

disk <- list_c(disk_map$files)
disk_free_lengths <- pmap(disk_map, free_lengths) |> list_c()

rev_dm <- 
  disk_map |> 
  select(id = file_id, start = start_file, len = len_file) |> 
  arrange(desc(id)) |> 
  print()

for(rev_dm_ptr in seq_len(nrow(rev_dm))){
  f <- rev_dm[rev_dm_ptr,]
  free_ptr <- which.max(disk_free_lengths >= f$len)
  
  if (free_ptr > 1 && free_ptr < f$start) {
    from_range <- f$start:(f$start + f$len - 1)
    to_range <- free_ptr:(free_ptr + f$len - 1)
    disk[c(to_range, from_range)] <- disk[c(from_range, to_range)]
    disk_free_lengths[to_range] <- 0
    
    # sprintf(
    #   "%2s: disk[%5s] <-%s-> disk[%5s] %s", rev_dm_ptr,
    #   paste0(sprintf("%.2d", range(from_range)), collapse = ":"),
    #   disk[free_ptr],
    #   paste0(sprintf("%.2d", range(to_range))  , collapse = ":"),
    #   prn_disk(disk)
    # ) |>
    # message()
  }
}
checksum(disk) 
