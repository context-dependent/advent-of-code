if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse",
  "stringdist"
)

input <- readLines("day-02/input.txt")



# PART 1: CHECKSUM --------------------------------------------------------

get_checksum <- function(box_labels) {
  
  str_split(box_labels, "") %>% 
    map(table) %>%
    map(
    ~ tibble(
        twice = 2 %in% .x,
        thrice = 3 %in% .x
      )
    ) %>% 
    bind_rows() %>% 
    summarize_all(sum) %>% 
    mutate(
      checksum = twice * thrice
    )
  
}

ans <- get_checksum(input)

write_csv(ans, "day-02/answer_part-01.csv")



# PART 2: FIND BOXES WITH NEARLY THE SAME NAME ----------------------------

get_close <- function(box_labels) {
  
  dist_mat <- stringdist::stringdistmatrix(box_labels, box_labels)
  indices <- which(dist_mat == 1, arr.ind = TRUE)[, 1]
  similar <- box_labels[indices]
  
  charlist <- similar %>% str_split("", simplify = TRUE)
  common_chars <- charlist[1, charlist[1, ] == charlist[2, ]] %>% 
    paste0(collapse = "")
  
  common_chars
  
}

ans <- get_close(input)

write_lines(ans, "day-02/answer_part-02.txt")

