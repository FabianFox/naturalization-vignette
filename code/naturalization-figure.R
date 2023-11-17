# Naturalizations and citizenship reforms in Germany ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "hrbrthemes", "conflicted", "Cairo", "restatis")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

## Load data ----
nat.df <- gen_cube("12511BJ017") %>%
  select(gender = GES, year = JAHR, n = BEV008_WERT) %>%
  summarise(n = sum(n), .by = year) %>%
  mutate(souce = "GENESIS-API")

# Data from https://www.bamf.de/SharedDocs/Anlagen/DE/Forschung/Migrationsberichte/migrationsbericht-2004.pdf?__blob=publicationFile&v=12
archive.df <- tibble(
  year = seq(1995, 1999, 1),
  n = c(71981, 86356, 82913, 106790, 143267),
  source = "BAMF Migrationsbericht 2004")

# Merge
nat.df <- nat.df %>%
  bind_rows(archive.df)

# Plot
nat.fig <- nat.df %>%
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  # StAG
  geom_curve(aes(x = 2001, xend = 2000, y = 200000, yend = 188000), 
             arrow = arrow(type = "closed", length = unit(0.02, "npc")),
             size = .5, 
             curvature = .3) +
  # ZuwandG
  geom_curve(aes(x = 2008.5, xend = 2007, y = 150000, yend = 120000), 
             arrow = arrow(type = "closed", length = unit(0.02, "npc")),
             size = .5, 
             curvature = .3) +
  # RuStAG
  annotate("segment", x = 1993, xend = 1996, y = 150000, yend = 150000, 
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  # AuslG
  annotate("text", x = 1993, y = 170000, label = "RuStAG (1913)\nAuslG (1990)", 
           hjust = 0, family = "Roboto Condensed", size = 6) +
  # StAG
  annotate("text", x = 2001, y = 200000, label = "StAG (2000)", 
           hjust = 0, family = "Roboto Condensed", size = 6) +
  # ZuwandG
  annotate("text", x = 2008.5, y = 150000, label = "ZuwandG (2007)", 
           hjust = 0, family = "Roboto Condensed", size = 6) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_x_continuous(labels = seq(1995, 2022, 5), breaks = seq(1995, 2022, 5)) +
  labs(x = "", y = "") +
  theme_ipsum(ticks = TRUE, base_size = 14, base_family = "Roboto Condensed", strip_text_size = 10, 
              caption_size = 12, caption_face = "plain", 
              axis_title_size = 12, grid = "Yy") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0),
        text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"))

# Export
ggsave(here("figures", "naturalizations.png"), device = ragg::agg_png(), bg = "white", 
       width = 30, height = 18,
       dpi = 300, units = "cm")
