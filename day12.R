#' ## Day 12: Garden Groups 
#' https://adventofcode.com/2024/day/12
source("aoc.R")
library(igraph, warn.conflicts = FALSE)

test_in <- 
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

m <- 
  aoc_lines(test_in) |> 
  strsplit("") |> 
  do.call(what = rbind) 
m

# -------------------------------------------------------------------------
#' ### part1: find area and perimeter for each region
#' We need to multiply area of each region by its perimeter to get fence price and add up all prices for the answer.  
#' `igraph` strategy:  
#' - make lattice graph
#' - store plan types in vertex attributes
#' - split vertex set by plant types, subgraph from each split
#' - decompose to store each region sharing the same plant type in its own subgraph
#' - unlist one level
#' - subgraph *area* is its plot (vertex) count and *perimeter* is the sum of outside plot sides, i.e. `sum(4 - degree(g)` 
#' (plots with degree 4 are surrounded by plots from that same region, plots with degree 1 are connected to the region by just one side and 
#' plots with degree 0 are single-plot regions, all 4 sides being outside sides)
#' - calculate prices for each region and sum
# Ex: 1930
g <- 
  make_(
    lattice(dim(m)),
    with_vertex_(plant = as.vector(m))
  )

regions <- 
  split(V(g), V(g)$plant) |> 
  lapply(\(vids) induced_subgraph(g, vids)) |> 
  lapply(decompose) |> 
  do.call(what = c) |> 
  lapply(\(g) list(area = vcount(g), perimeter = sum(4 - degree(g))))

sapply(regions, \(reg) reg$area * reg$perimeter) |> sum()

# -------------------------------------------------------------------------
#' ### part2: find number of sides for each region
#' New fence price for a region is now calculated by multiplying its area by its number of sides.
#' - upscale matrix & lattice graph by a factor of 3 (1x1 plot becomes 3x3)
#' - from each region, count clusters of degree-3 vertices as sides (outer & inner edge sections, excluding all corners)
# Ex: 1206

# upscale original grid
m3 <- m[rep(1:nrow(m), each=3), rep(1:ncol(m), each=3)]
g3 <- 
  make_(
    lattice(dim(m3)),
    with_vertex_(
      plant = as.vector(m3),
      x = which(!is.na(m3), arr.ind = T)[,"col"]
    )
  )

# get region side count through degree-3 vertex componets in each region
side_count <- 
  split(V(g3), V(g3)$plant) |> 
  lapply(\(vids) induced_subgraph(g3, vids)) |> 
  lapply(decompose) |> 
  unlist(recursive = FALSE) |> 
  sapply(\(g) subgraph(g, degree(g) == 3) |> count_components())

# use `regions` from part1 for area
mapply(
  side = side_count, 
  reg = regions, 
  FUN = \(side, reg) side * reg$area
) |> sum()

# -------------------------------------------------------------------------
#' ### viz
#+ fold=TRUE

plot_part1 <- function(m){
  g <- 
    make_(
      lattice(dim(m)),
      with_vertex_(
        plant = factor(as.vector(m)),
        label = factor(as.vector(m)),
        x = rep(1:ncol(m), each  = nrow(m)),
        y = rev(rep(1:nrow(m), times = ncol(m))),
        color = grDevices::hcl.colors(n = length(table(m)))[factor(as.vector(m))]
      )
    )
  
  region_edges <- with(as_data_frame(g), V(g)$plant[from] == V(g)$plant[to])
  g_regions <- subgraph.edges(g, E(g)[region_edges], delete.vertices = FALSE) 
  plot(g_regions, 
       main = "part1\ngarden plot degrees",
       vertex.label = degree(g_regions), 
       vertex.label.color = "gray99", 
       vertex.label.cex = 1.8, 
       vertex.shape = "square", 
       vertex.size = 18, 
       edge.width = 22,
       edge.color = "gray30"
  )
}

plot_part2 <- function(m3){
  g3 <- 
    make_(
      lattice(dim(m3)),
      with_vertex_(
        plant = factor(as.vector(m3)),
        label = factor(as.vector(m3)),
        x = rep(1:ncol(m3), each  = nrow(m3)),
        y = rev(rep(1:nrow(m3), times = ncol(m3))),
        color = grDevices::hcl.colors(n = length(table(m3)))[factor(as.vector(m3))]
      )
    )
  
  region_edges <- with(as_data_frame(g3), V(g3)$plant[from] == V(g3)$plant[to])
  g_regions <- subgraph.edges(g3, E(g3)[region_edges], delete.vertices = FALSE) 
  V(g_regions)$degree <- degree(g_regions)
  
  region_sides <- with(as_data_frame(g_regions), V(g_regions)$degree[from] == 3 & V(g_regions)$degree[to] == 3)
  g_sides <- subgraph.edges(g_regions, E(g_regions)[region_sides], delete.vertices = FALSE) 
  plot(g_sides, 
       main = "part2\nupscaled garden plots,\nregion sides as components",
       vertex.label = NA, 
       vertex.shape = "square", 
       vertex.size = 5, 
       vertex.frame.color = c(NA, "violet")[(V(g_sides)$degree == 3) + 1],
       vertex.frame.width = 2.5,
       edge.width = 5,
       edge.color = "gray30" 
      )
}

#+ plot, fig.dim=c(11,6)
withr::with_par(
  list(mar = c(0,0,4,0), mfrow = c(1,2)),
  { plot_part1(m); plot_part2(m3) }
)





