#Cross-sectional analysis of award yield (degrees and certificates awarded per 100 students enrolled)
# for CA community colleges across 3 academic years.

library("tidyverse")
StudentHeadcounts <- read.csv("StudentHeadcounts.csv")
ProgAwards <- read.csv("ProgAwards.csv")

glimpse(StudentHeadcounts)

awards_clean <- ProgAwards %>%
  rename(college_label = X, award_type = X.1) %>%
  mutate(
    across(starts_with("Annual"), ~ parse_number(as.character(.x))),
    is_total = !is.na(college_label) & college_label != "",
    college  = if_else(is_total, str_remove(college_label, " Total$"), NA_character_)
  ) %>%
  fill(college, .direction = "down") %>%
  filter(!str_detect(college, "State of California")) %>%
  pivot_longer(starts_with("Annual"), names_to = "year", values_to = "n_awards") %>%
  mutate(year = str_replace_all(year, "Annual\\.(\\d{4})\\.(\\d{4})", "\\1-\\2"))

college_totals <- awards_clean %>% filter(is_total)
detail         <- awards_clean %>% filter(!is_total)

# CHECK: detail rows should add up to each college's Total row (expect 0 rows)
detail %>%
  group_by(college, year) %>%
  summarise(detail_sum = sum(n_awards, na.rm = TRUE), .groups = "drop") %>%
  left_join(college_totals %>% select(college, year, total = n_awards),
            by = c("college", "year")) %>%
  filter(detail_sum != total)

headct_long <- StudentHeadcounts %>%
  slice(-(1:2)) %>%
  setNames(c("college", "2023-2024", "2024-2025", "2025-2026")) %>%
  pivot_longer(-college, names_to = "year", values_to = "headcount") %>%
  mutate(headcount = parse_number(as.character(headcount)))

# CHECK: do college names line up between the two files? (expect 0 rows)
anti_join(distinct(college_totals, college), distinct(headct_long, college),
          by = "college")
