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

#' Create and open R boilerplate for specific date, download input data
#'
#' @param year 
#' @param day 
#' @param aoc_template a template file, with `glue` placeholders `{aoc_title}` &`{aoc_url}`
#'
#' @returns
#' @export
#'
aoc_get <- function(year, day, aoc_template = "day__.R"){
  # year <- 2024; day <- 4
  aoc_path <- gsub(x = aoc_template, pattern = "_+", replacement = sprintf("%.2d", day))
  if(file.exists(aoc_path)){
    aoc_path <- 
      sprintf(
        "%s_%s", 
        fs::path_ext_remove(aoc_path), 
        strftime(Sys.time(), "%Y%m%d%H%M%S")
      ) |> 
      fs::path_ext_set(
        fs::path_ext(aoc_path)
      )
  }
  
  aoc_url <- glue::glue("https://adventofcode.com/{year}/day/{day}")
  aoc_title <- 
    rvest::read_html(aoc_url) |> 
    rvest::html_element("h2") |> 
    rvest::html_text(trim = TRUE) |> 
    gsub(x = _, pattern = "---", replacement = "") |> 
    trimws()
  
  in_path <- fs::path(fs::dir_create("_in"), sprintf("%.2d.txt", day))
  # usethis::edit_r_environ("user")
  try({
    download.file(paste0(aoc_url, "/input"), in_path, headers = c("Cookie" = Sys.getenv("AOC_COOKIE")))
    rstudioapi::documentOpen(in_path)
  })
  
  readLines(aoc_template) |> 
    paste0(collapse = "\n") |> 
    glue::glue() |> 
    writeLines(aoc_path)
  rstudioapi::documentOpen(aoc_path) |> invisible()
}

aoc_reprex_img <- function(file){
  # file <- "day05_reprex_files/figure-gfm/unnamed-chunk-4-1.png"
  path_ <- fs::file_copy(file, fs::dir_create("img") / paste0(fs::path_split(file)[[1]][-2], collapse = "_"), overwrite = TRUE)
  return(path_)
}

# options
options(reprex.advertise = FALSE)
#knitr::opts_knit$set(upload.fun = identity) 
knitr::opts_knit$set(upload.fun = aoc_reprex_img) 
knitr::opts_chunk$set(dev = "ragg_png")
par(mar=c(0,0,0,0)+.1)

# std_out_err = TRUE
aoc_reprex <- function(dayR, readme_append = FALSE, ...){
  readme <- NA
  on.exit(try(close(readme)))
  
  if (readme_append){
    readme <- file("README.md", open ="a")
  }
  
  for (f in fs::dir_ls(glob = dayR)){
    reprex_lines <- reprex::reprex(input = f, ...)
    if (readme_append){
      writeLines(reprex_lines, readme)
    }
  }
}

# aoc_reprex("day0?.R", readme_append = T)

# usage
# render with:
# reprex::reprex(input = "day03.R")
# cat day03_reprex.md >> README.md
# git add README.md day03.R
# git commit --date="2024-12-03 12:00:00" -m "day 03"

# restore version:
# git checkout c87f657969f0bb9b17c29fdc53f436347b5e43f7 -- README.md


