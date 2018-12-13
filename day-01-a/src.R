if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse"
)

d <- readLines("day-01/input.txt")

ans <- d %>% as.numeric() %>% sum()

writeLines(as.character(ans), "day-01/ans.txt")
