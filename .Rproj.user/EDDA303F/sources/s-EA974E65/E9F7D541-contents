if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse"
)


input <- readLines("day-05/input.txt")


pattern <- paste0(
  str_c(letters, LETTERS, collapse = "|"), "|",
  str_c(LETTERS, letters, collapse = "|")
)


react <- function(polymer_string) {
  
  in_length <- nchar(polymer_string)
  
  res <- polymer_string %>% str_remove_all(pattern)
  
  out_length <- nchar(res)
  
  if(in_length == out_length) {
    return(res)
  } 
  
  react(res)
  
}

reacted_polymer <- react(input)

output_length <- reacted_polymer %>% nchar()

cat(
  glue::glue("Answer 1: the polymer's final length is {output_length}\n"),
  file = "day-05/answer.txt",
  sep = "\n"
  )


get_shortest_drop <- function(input) {
  
  drop_tests <- str_c("[", letters, LETTERS, "]")
  
  res <- drop_tests %>% 
    map_int(
    ~ str_remove_all(
        input, .x
      ) %>% 
      react() %>% 
      nchar()
    ) %>% 
    min()
  
  res
}

best_output <- get_shortest_drop(input)

cat(
  glue::glue("Answer 2: the shortest result of dropping one unit pair is {best_output}"),
  file = "day-05/answer.txt",
  append = TRUE,
  sep = "\n"
)

