if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  "tidyverse"
)

input <- readLines("day-04/input.txt")

clean_naps <- function(entries) {
  
  d <- tibble(
      raw = entries
    ) %>% 
    
    mutate(
      event_time = raw %>% 
        str_extract("(?<=\\[).+(?=\\])") %>% 
        lubridate::ymd_hm(), 
      event = str_extract(raw, "falls asleep|begins shift|wakes up"),
      guard_id = str_extract(raw, "(?<=#)\\d+") %>% as.numeric()
    ) %>% 
    
    arrange(
      event_time
    ) %>% 
    
    
    fill(
      guard_id
    ) %>% 
  
    select(
      -raw
    ) %>% 
    
    group_by(
      event
    ) %>% 
    
    filter(
      event != "begins shift"
    ) %>% 
    
    mutate(
      nap_id = row_number()
    ) %>% 
    
    spread(
      event, event_time
    ) %>% 
    
    janitor::clean_names() %>% 
    
    mutate(
      mins_asleep = map2(
        falls_asleep, 
        wakes_up,
      ~ lubridate::minute(.x):(lubridate::minute(.y)-1)
      ),
      nap_duration = wakes_up - falls_asleep
    ) %>% 
    
    ungroup()
}

naps <- clean_naps(input)

total_nappage <- naps %>% 
  group_by(guard_id) %>% 
  summarize(
    nap_duration_total = sum(nap_duration),
    mins_asleep = list(unlist(mins_asleep))
  )

sleepiest_guard <- total_nappage %>% 
  filter(nap_duration_total == max(nap_duration_total)) %>% 
  unnest() %>% 
  ungroup() %>% 
  count(
    guard_id,
    nap_duration_total, 
    mins_asleep
  ) %>% 
  arrange(
    -n
  ) %>% 
  slice(1) %>% 
  mutate(checksum = guard_id * mins_asleep) %>% 
  pull(checksum)

ans_01 <- glue::glue("Answer 1: The sleepiest guard checksum = {sleepiest_guard}")


sleepiest_minute <- total_nappage %>%
  unnest() %>% 
  ungroup() %>% 
  count(guard_id, mins_asleep) %>% 
  arrange(-n) %>% 
  slice(1) %>% 
  mutate(checksum = guard_id * mins_asleep) %>% 
  pull(checksum)



ans_02 <- glue::glue("Answer 2: The sleepiest minute checksum = {sleepiest_minute}")

cat(ans_01, ans_02, sep = "\n", file = "day-04/answer.txt")
