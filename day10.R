#' ## Day 10: Hoof It 
#' https://adventofcode.com/2024/day/10
source("aoc.R")
library(igraph, warn.conflicts = FALSE)

test_in <- 
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

m <- 
  aoc_lines(test_in) |> 
  strsplit("") |> 
  do.call(what = rbind) |> 
  `mode<-`("integer")
m  
# -------------------------------------------------------------------------
#' #### part1: score hiking trail trailheads
#' Trail starts from elevation 0 and ends at elevation 9, it has even, gradual, uphill slope.
#' Trailheads are points at elevation 0, trailhead score is the number of reachable elevation 9 points.
#' - make directed lattice graph with mutual edges
#' - height as vertex attribute
#' - subgraph with only edges where elevation step is (+)1
#' - for each `height == 0` vertex, get a subcomponent reachable from that vertex, make it a subgraph
#' - from each subgraph find shortest paths from `height == 0` vertex (a single vertex) to every 
#' `height == 9` vertex
#' - count paths
# Ex: 36

# directed AND mutual graph, 2 directed edges between every adjecent vertex
g <- 
  make_(
    lattice(dimvector = dim(m), directed = TRUE, mutual = TRUE),
    with_vertex_(
      # x, y, color for plotting; reverse y to get the same visual layout as in input m
      height = as.vector(m),
      x = rep(1:ncol(m), each  = nrow(m)),
      y = rev(rep(1:nrow(m), times = ncol(m))),
      color = grDevices::terrain.colors(n = 10)[as.vector(m) + 1]
    )
  )
V(g)$name <- V(g)
E(g)$name <- E(g)

# use edge list (from / to) ...
as_data_frame(g) |> print(max = 12)
# ... to get edges that link vertices where height difference is +1:
gradual_uphill_edges <- with(as_data_frame(g), V(g)$height[to] - V(g)$height[from] == 1)
g <- subgraph.edges(g, E(g)[gradual_uphill_edges])

# subgraphs for each vertex with height == 0
g_lst <- 
  V(g)[height == 0] |> 
  lapply(\(v) subgraph(g, subcomponent(g, v, mode = "out"))) 

# paths from V(g_sub)[height == 0] (one per subgraph) to every V(g_sub)[height == 9]
paths_0_9 <- lapply(g_lst, \(g_sub) shortest_paths(g_sub, V(g_sub)[height == 0], V(g_sub)[height == 9], output = "epath")$epath)
lengths(paths_0_9) |> sum()

# -------------------------------------------------------------------------
#' ##### viz
#' Heights (vertices) and remaining edges that form paths, colored by a number of paths they are part of.
edge_score <- 
  paths_0_9 |> 
  lapply(do.call, what = c) |> 
  lapply(names) |> 
  do.call(what = c) |> 
  table()
  
E(g)$score <- 0
E(g)[names(edge_score)]$score <- as.vector(edge_score)

# shift sequential_pal 1 level up
E(g)$color <- E(g)$score |> cut(5) |> scales::dscale(\(n) sequential_pal(n+1)[-1])
E(g)[score == 0]$color <- "gray80"
# mark trailheads and highest points
V(g)[height %in% c(0, 9)]$frame.color <- "darkred"

withr::with_par(
  list(mar = c(0,0,0,0)),
  plot(
    g, 
    vertex.label = V(g)$height, 
    vertex.label.cex = 2, 
    vertex.shape = "square", 
    vertex.frame.width = 3 
  )
)
# -------------------------------------------------------------------------
#' #### part2: calculate trailhead ratings
#' Rating is a number of all distinct hiking trails starting from that trailhead. 
#' Instead of `shortest_paths()` use `all_shortest_paths()` , extract and count edge paths.
# Ex: 81
all_paths_0_9 <- 
  lapply(g_lst, \(g_sub) {
    all_shortest_paths(g_sub, V(g_sub)[height == 0], V(g_sub)[height == 9])$epaths
  })
         
lengths(all_paths_0_9) |> sum()
