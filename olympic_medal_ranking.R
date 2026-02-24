library(tidyverse)


url <- "https://en.wikipedia.org/wiki/2026_Winter_Olympics_medal_table"
page <- read_html(url)

tables <- html_table(page, fill = TRUE)
w26_medal_table <- tables[[3]]

# rename columns to text, wiki table has medal images
colnames(w26_medal_table) <- c(
  "Rank", # wiki table uses gold-priority / gold-only ranking
  "Team",
  "Gold",
  "Silver",
  "Bronze",
  "Total"
)

w26_medal_table <- w26_medal_table %>%
  slice(-n()) %>% # remove "Totals" row
  mutate(
    Team = str_remove_all(Team, "\\[.*?\\]"), # remove end/footnotes
    Team = str_remove_all(Team, "[^[:alnum:]\\s]"), # remove symbols
    Team = str_remove_all(Team, "\\.mw\\-parser\\-output.*"), # remove HTML/CSS
    Team = str_trim(Team)
  )

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