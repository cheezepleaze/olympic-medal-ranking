library(tidyverse)
library(rvest)


# note: scraped tables depend on current wiki page structure (a/o 24 Feb 2026)
s24_url <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"
w26_url <- "https://en.wikipedia.org/wiki/2026_Winter_Olympics_medal_table"
s24_page <- read_html(s24_url)
w26_page <- read_html(w26_url)

# found/assumed: medal count table is 3rd HTML table in medal table wiki pages
get_medal_table <- function(page, table_index = 3) {
    tables <- html_table(page, fill = TRUE)
    tables[[table_index]]
}

s24_medal_table <- get_medal_table(s24_page)
w26_medal_table <- get_medal_table(w26_page)

# standardize column names: total olympic medal count wiki page uses medal icons
colnames(w26_medal_table) <- c(
  "Rank", # wiki table uses gold-priority / gold-only ranking
  "Team",
  "Gold",
  "Silver",
  "Bronze",
  "Total"
)

colnames(s24_medal_table) <- c(
  "Rank",
  "Team",
  "Gold",
  "Silver",
  "Bronze",
  "Total"
)

# clean up tibbles
s24_medal_table <- s24_medal_table %>%
  select(-Rank) %>% # remove "Rank" column, ranking is recomputed
  filter(!str_detect(Team, "^Totals")) %>% # don't need to have full overall totals
  mutate(
    Team = Team %>%
        str_remove_all("\\[.*?\\]") %>% # remove citations/endnotes
        str_remove_all("[^\\p{L}\\d\\s]") %>% # remove non-letter symbols
        str_remove_all("\\.mw\\-parser\\-output.*") %>% # remove HTML/CSS artifacts
        str_trim(),
    across(-Team, as.numeric),
    Games = "Paris 2024",
  )

w26_medal_table <- w26_medal_table %>%
  select(-Rank) %>%
  filter(!str_detect(Team, "^Totals")) %>%
  mutate(
    Team = Team %>%
        str_remove_all("\\[.*?\\]") %>%
        str_remove_all("[^\\p{L}\\d\\s]") %>%
        str_remove_all("\\.mw\\-parser\\-output.*") %>%
        str_trim(),
    across(-Team, as.numeric),
    Games = "Milan-Cortina 2026",
  )

# combine games tables into one tibble
all_medals_table <- bind_rows(s24_medal_table, w26_medal_table)

compute_medal_score <- function(df, w_gold, w_silver, w_bronze) {

  df %>%
    mutate(
      score = 
        Gold * w_gold + 
        Silver * w_silver + 
        Bronze * w_bronze
    )
}

rank_countries <- function(df, w_gold, w_silver, w_bronze) {

  df %>%
    compute_medal_score(w_gold, w_silver, w_bronze) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number())
}

weights_equal      <- c(1, 1, 1)
weights_gold_heavy <- c(5, 2, 1) # arbitrary weights for EDA

rank_count <- rank_countries(w26_medal_table, weights_equal[1], weights_equal[2], weights_equal[3])
rank_gold_only <- w26_medal_table %>%
  arrange(desc(Gold), desc(Silver), desc(Bronze)) %>%
  mutate(rank_gold_only = row_number())
rank_gold_heavy <- rank_countries(w26_medal_table, weights_gold_heavy[1], weights_gold_heavy[2], weights_gold_heavy[3])

# compare rankings, noting delta from total counts to gold-only and gold-heavy priorities
comparison <- rank_count %>%
  select(Team, rank_equal = rank, score_equal = score) %>%
  left_join(
    rank_gold_heavy %>% select(Team, rank_gold_heavy = rank, score_gold_heavy = score),
    by = "Team"
  ) %>%
  left_join(
    rank_gold_only %>% select(Team, rank_gold_only),
    by = "Team"
  ) %>%
  mutate(
    delta_gold_only = rank_gold_only - rank_equal,
    delta_gold_heavy = rank_gold_heavy - rank_equal
  )