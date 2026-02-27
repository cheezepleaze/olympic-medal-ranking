library(tidyverse)
library(rvest)


# note: scraped tables depend on current wiki page structure (a/o 24 Feb 2026)
s24_url <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"
w26_url <- "https://en.wikipedia.org/wiki/2026_Winter_Olympics_medal_table"

scrape_wiki_medal_table <- function(url, games_label, table_index = 3) {
  read_html(url) %>%
    html_table(fill = TRUE) %>%
    .[[table_index]] %>%
    rename_with(~ c("Rank", "Team", "Gold", "Silver", "Bronze", "Total")) %>% # hard rename due to HTML/CSS artifacts
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

weights_equal      <- c(1, 1, 1)
weights_gold_heavy <- c(5, 2, 1)

ranking_totals <- rank_countries(w26_medals, weights_equal[1], weights_equal[2], weights_equal[3])
ranking_gold_prio <- rank_gold_priority(w26_medals)
ranking_gold_only <- rank_countries(w26_medals, 1, 0, 0)
ranking_gold_heavy <- rank_countries(w26_medals, weights_gold_heavy[1], weights_gold_heavy[2], weights_gold_heavy[3])

# compare rankings, noting delta from total counts to gold-only and gold-heavy priorities
comparison <- ranking_gold_prio %>%
  select(Team, rank_gold_priority = rank) %>%
  left_join(
    rank_count %>% select(Team, rank_totals = rank),
    by = "Team"
  ) %>%
  left_join(
    rank_gold_only %>% select(Team, rank_gold_only = rank),
    by = "Team"
  ) %>%
  left_join(
    rank_gold_heavy %>% select(Team, rank_gold_heavy = rank),
    by = "Team"
  ) %>%
  mutate(
    delta_total_count = rank_totals - rank_gold_priority,
    delta_gold_only = rank_gold_only - rank_gold_priority,
    delta_gold_heavy = rank_gold_heavy - rank_gold_priority
  )

# tidy and reshape data from wide to long
comparison_long <- comparison %>%
  select(Team, rank_gold_priority, rank_totals, rank_gold_heavy) %>%
  pivot_longer(cols = starts_with("rank"), names_to = "philosophy", values_to = "rank") %>%
  mutate(
    philosophy = factor(
      philosophy, 
      levels = c("rank_gold_priority", "rank_totals", "rank_gold_heavy")
    )
  )

# messy visualization with all teams
ggplot(comparison_long, aes(x = philosophy, y = rank, group = Team)) +
  geom_line(aes(color = Team)) +
  geom_point(aes(color = Team)) +
  geom_text_repel(aes(label = Team), size = 3, show.legend = FALSE) +
  scale_y_reverse() +  # rank 1 at top
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Rank Changes by Medal Weighting Philosophy",
       y = "Rank", x = "Weighting Philosophy")

# reduce team selection for cleaner visualization
top15 <- comparison |>
  arrange(rank_gold_heavy) |>
  slice_head(n = 15) |>
  pull(Team)

top15_long <- comparison_long |>
  filter(Team %in% top15)

# "cleaner" proof-of-concept visualization with fewer teams
ggplot(top15_long, aes(x = philosophy, y = rank, group = Team)) +
  geom_line(aes(color = Team)) +
  geom_point(aes(color = Team)) +
  geom_text_repel(
    data = top15_long |> filter(philosophy == "rank_gold_priority"),
    aes(label = Team),
    hjust = 1,
    nudge_x = -0.1,
    direction = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data = top15_long |> filter(philosophy == "rank_gold_heavy"),
    aes(label = Team),
    hjust = 0,
    nudge_x = 0.1,
    direction = "y",
    force = 0.01,
    segment.color = NA
  ) +
  scale_y_reverse() +
  theme_minimal() +
  theme(legend.position = "none")