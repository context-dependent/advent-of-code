if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse",
  "stringdist"
)

input <- readLines("day-03/input.txt")

clean_claims <- function(claims) {
  input %>% 
    str_remove(
      "^#"
    ) %>% 
    str_split(
      "(\\s|[^\\d])+",
      simplify = TRUE
    ) %>% 
    as_tibble() %>% 
    set_names(
      c("id", "x_start", "y_start", "width", "height")
    ) %>% 
    mutate_all(as.integer) %>% 
    mutate(
      x_max = x_start + width, 
      y_min = 1000 - y_start - height + 1,
      y_max = 1000 - y_start,
      x_min = x_start + 1, 
    ) %>% 
    select(matches("^id|min|max")) 
}


plot_claim <- function(id, x_min, x_max, y_min, y_max, browse = FALSE) {
  
  if(browse) browser()
  
  coords <- expand.grid(
    id = id, 
    x = x_min:x_max, 
    y = y_min:y_max
  ) %>% 
  
  as_tibble()
  
  coords
  
  
}

claims <- clean_claims(input) 

coords_used <- claims %>% pmap_dfr(plot_claim, browse = FALSE)

used_count <- coords_used %>% 
  count(x, y) 

overlapping_sq_in <- used_count %>% 
  filter(n > 1) %>% 
  nrow()

ans_01 <- glue::glue(
  "Answer 1: {overlapping_sq_in} square inches of the fabric are claimed by multiple elves"
  )


non_overlapping_id <- 
  coords_used %>% 
  left_join(used_count) %>% 
  group_by(id) %>% 
  summarize(max_z = max(n)) %>% 
  filter(max_z == 1) %>% 
  pull(id)

ans_02 <- glue::glue(
  "Answer 2: Claim #{non_overlapping_id} is the only one that doesnt overlap"
)

cat(ans_01, ans_02, file = "day-03/answer.txt", sep = "\n")
