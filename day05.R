#' ## Day 5: Print Queue 
#' https://adventofcode.com/2024/day/5
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
  
# -------------------------------------------------------------------------
#' #### part1: count updates that comply with all page ordering rules
#' Rule `47|53` means that if both pages occur in update, 47 must be printed before 53,
#' though not essentially immediately before 53. 
#' Elves need to know the middle page number of each correct update, answer is some of those.
#' - use pages in rules as edge lists (to-from) to build a graph
#' - generate a sequence of vertex pairs from rules and check if all are adjacent
#' - check rule compliance, subset valid updates, extract middle page, sum
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
# plot(g)

rule_compliance <- sapply(parsed_lst$updates, check_page_order, g = g)
is_valid <- sapply(rule_compliance, all)

parsed_lst$updates[is_valid] |> 
  sapply(\(x) x[ceiling(length(x) / 2)]) |> 
  as.numeric() |> 
  sum()

# -------------------------------------------------------------------------
#' #### part2: fix failed updates
#' Swap 1st failed location and next position until rule check passes  
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

