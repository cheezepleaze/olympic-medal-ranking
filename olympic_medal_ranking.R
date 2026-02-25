library(tidyverse)
library(rvest)


# note: scraped tables depend on current wiki page structure (a/o 24 Feb 2026)
s24_url <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"
w26_url <- "https://en.wikipedia.org/wiki/2026_Winter_Olympics_medal_table"

scrape_wiki_medal_table <- function(url, games_label, table_index = 3) {
  read_html(url) %>%
    html_table(fill = TRUE) %>%
    .[[table_index]] %>%
    rename(Team = NOC) %>%
    select(-Rank) %>% # remove "Rank" column, re-ranking based on strategy
    filter(!str_detect(Team, "^Totals")) %>%
    mutate(
      Team = Team %>%
        str_remove_all("\\[.*?\\]") %>% # remove citations/endnotes
        str_remove_all("[^\\p{L}\\d\\s]") %>% # remove non-letter symbols
        str_remove_all("\\.mw\\-parser\\-output.*") %>% # remove HTML/CSS artifacts
        str_trim(),
      across(-Team, as.numeric),
      Games = games_label
    )
}

s24_medals <- scrape_wiki_medal_table(s24_url, "Paris 2024")
w26_medals <- scrape_wiki_medal_table(w26_url, "Milan-Cortina 2026")

# combine games tables into one tibble
all_medals <- bind_rows(s24_medals, w26_medals)

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

medal_weights <- tibble(
    philosophy = c("equal", "gold_heavy"), # current: arbitrary gold_heavy weights
    w_gold = c(1, 3),
    w_silver = c(1, 2),
    w_bronze = c(1, 1)
)

rank_gold_priority <- function(medals_data) {
  medals_data %>%
    arrange(
      Games,
      desc(Gold),
      desc(Silver),
      desc(Bronze)
    ) %>%
    group_by(Games) %>%
    mutate(
      rank = row_number()
    ) %>%
    ungroup()
}

rank_count <- rank_countries(w26_medals, weights_equal[1], weights_equal[2], weights_equal[3])
rank_gold_only <- w26_medals %>%
  arrange(desc(Gold), desc(Silver), desc(Bronze)) %>%
  mutate(rank_gold_only = row_number())
rank_gold_heavy <- rank_countries(w26_medals, weights_gold_heavy[1], weights_gold_heavy[2], weights_gold_heavy[3])

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