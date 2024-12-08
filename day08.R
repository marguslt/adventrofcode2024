#' ## Day 8: Resonant Collinearity 
#' https://adventofcode.com/2024/day/8
source("aoc.R")

library(purrr)
test_in <- 
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

m <- 
  aoc_lines(test_in) |>
  strsplit("") |> 
  do.call(what = rbind)

# -------------------------------------------------------------------------
#' #### part1: find unique locations of antinodes
#' For each pair of antennas that share the same frequencey there's a pair of antinodes.
#' Use complex numbers for coordinates and calculate antinodes with *a1 = z1 + (z1 - z2); a2 = z2 + (z2 - z1)*, 
#' remove results that are outside of map area
# Ex: 14

# z: a vector of complex numbers of length 2 holding coordinates for an antenna pair,
# vectorized form to calculate a1 = z1 + (z1 - z2); a2 = z2 + (z2 - z1)
antinodes <- \(z) 2*z[1:2] - z[2:1]

# locations from matrix to dataframe
antenna_locations <- 
  which(m != ".", arr.ind = TRUE) |> 
  as.data.frame() |> 
  within({
    freq <- m[cbind(row, col)]
    z    <- complex(real = col, imaginary = row)
  })
antenna_locations

# cycle through pairs of antennas within each frequency, 
# find antinodes and remove results that fall outside of map area,
# count unique locations
anti_locations <- 
  split(antenna_locations, ~freq) |> 
  lapply(
    \(df) {
      a <- combn(df$z, 2) |> apply(2, antinodes)
      a[Re(a) >= 1 & Re(a) <= ncol(m) & 
        Im(a) >= 1 & Im(a) <= nrow(m)]
    }
  )
anti_locations |> 
  do.call(what = c) |> 
  unique() |> 
  length()

# -------------------------------------------------------------------------
#' #### part2: antinodes now occur at any grid position exactly in line with at least two antennas of the same frequency
#' Each antenna pair forms a linear equation, *y = i0 + tan(phi) \* x*, calculate *y* for every grid column (1:50).  
#' Antenna pair is presented as a pair of complex numbers ( *z1* & *z2* ) 
#' and we are after a line that cuts though z1 & z2. 
# Ex: 34
equations <- 
  split(antenna_locations, ~freq) |> 
  lapply(
    \(df) {
      # sorted combinations for consistency
      combn(df$z, 2, FUN = sort) |> 
      t() |> 
      `colnames<-`(c("z1", "z2")) |> 
      as.data.frame() |> 
      within({
        # slope from diff argument for slope 
        slope  <- tan(Arg(z2 - z1))
        # intercept (from similar triangles)
        intercept  <- Im(z1) - slope * Re(z1)
      })
    }
  )
equations

# shape x-range as a matrix (n lines x matrix columns) for vectorized calculation  
y_pos <- 
  equations |> 
  lapply(\(df) with(df, intercept + slope * matrix(1:ncol(m), byrow = TRUE, ncol = ncol(m), nrow = nrow(df)))) |> 
  do.call(what = rbind)
round(y_pos, 2)

# keep only near-integer positions ..
y_pos_rnd <- round(y_pos)
y_pos_rnd[abs(y_pos - y_pos_rnd) > 1e-6] <- NA
# .. that are still within map area
y_pos_rnd[!(y_pos_rnd %in% seq_len(ncol(m)))] <- NA
# total number of unique positions
apply(y_pos_rnd, 2, table) |> lengths() |> sum()

# -------------------------------------------------------------------------
#' #### viz
ufreq <- unique(antenna_locations$freq)
pal <- 
  length(ufreq) |> 
  hcl.colors("Dark 2") |> 
  setNames(ufreq)

par(
  mar = c(2,2,2,2)+.1,
  pty = "s"
)

# input points (filled)
with(antenna_locations, plot(z, pch = 20, cex = 5, col = pal[freq], xlim = c(1, ncol(m)), ylim = c(nrow(m), 1)))

# lines 
names(equations) |> 
  lapply(\(freq) apply(equations[[freq]], 1, \(row_) abline(row_["intercept"], row_["slope"], col = pal[freq], lty = 2))) |> 
  invisible()

# input point labels
with(antenna_locations, points(z, pch = freq, cex = 1.5, col = "white"))

# points (part 2, diamond)
for(x_pos in seq_len(ncol(y_pos_rnd))){
  y <- y_pos_rnd[,x_pos] |> na.omit() |> unique()
  x <- rep(x_pos, length(y))
  points(x, y, cex = 3, lwd = 2, pch = 5, col = "gray50")
}

# part1 points, circles
names(anti_locations) |>
  lapply(\(freq){
    data.frame(freq, z = anti_locations[[freq]])
  }) |> 
  do.call(what = rbind) |> 
  with(points(z, pch = 1, cex = 5, lwd = 3, col = pal[freq]))
grid(nx = ncol(m), ny = nrow(m))
