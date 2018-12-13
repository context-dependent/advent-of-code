if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse"
)

input <- readLines("day-02/input.txt")

d <- 
  
  tibble(
    freq_change = as.numeric(input)
  ) %>%
  mutate(
    freq_current = cumsum(freq_change)
  ) %>% 
   
  select(-freq_change)


get_first_repeat <- function(d) {
  
  test <- d %>% 
    group_by(
      freq_current
    ) %>% 
    mutate(
      freq_count = row_number()
    ) 
  
  if(max(test$freq_count > 1)) {
    res <- test %>% filter(freq_count == 2) %>% pull(freq_current) %>% first()
    return(res)
  }
  
  d_next <- d %>% mutate(freq_current = freq_current + last(freq_current))
  
  d_new <- bind_rows(d, d_next)
  
  get_first_repeat(d_new)
  
}

ans <- get_first_repeat(d)

write_lines(ans, "day-01-b/answer.txt")

