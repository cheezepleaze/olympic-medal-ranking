# Olympic Medal Valuation and Ranking

Olympic Games (Winter or Summer) always spark conversation over how countries should be ranked in medal tables. 
The main point of contention is ranking by total medals or by gold medals. This gets deeper with silver and bronze medal counts and how they should be relatively valued.

This project builds a flexible ranking system that varies medal weights, changing country rankings accordingly.

---

## Methodology

Define a weighted medal score:
$$ Score = (w_g × Gold) + (w_s × Silver) + (w_b × Bronze) $$

The project evaluates:
- Fixed scoring systems (e.g., gold-priority, gold-heavy, total medals)
- Continuous weight variation
- Rank stability under simulated scoring systems

Data is sourced from publicly available Olympic medal datasets.

## Goals

- Build a reproducible medal ranking model
- Create an interactive visualization (Streamlit or Tableau)
- Analyze ranking sensitivity to medal weight assumptions
- Identify countries that dominate across most scoring systems
- Quantify thresholds where rankings change

## Current Status

### Data wrangling
- [x] 2026 Winter Olympics data
- [x] 2024 Summer Olympics data
- [ ] Exploratory Data Analysis