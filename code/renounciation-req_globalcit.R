# GLOBALCIT 
# Renunciation requirements

# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes",  
            "conflicted", "Cairo", "gt", "janitor", "countrycode")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

# Load data
# Download: 2024/01/15
cit.orig <- import(here("data", "data_v2.0_country-year.csv")) %>%
  tibble()

# Clean data
cit.df <- cit.orig %>%
  select(iso3, year, renounce_bin = A06b_bin, renounce_cat = A06b_cat) %>%
  mutate(eu = countrycode(iso3, "iso3c", "eu28"))

# Global trend
global_renounce.df <- cit.df %>%
  filter(!is.na(renounce_bin)) %>%
  count(year, renounce_bin) %>%
  group_by(year) %>%
  mutate(p = n / sum(n),
         renounce_bin = factor(renounce_bin, 
                               levels = c(0, 1, 9), 
                               labels = c("no requirement",
                                          "requirement",
                                          "no residence-based acquisition"))) %>%
  ungroup()

# Share of renounciation requirement at specific time points
global_renounce.df %>%
  filter(year %in% c(seq(1960, 2020, 10), 2022)) %>%
  select(-n) %>%
  mutate(p = p * 100) %>%
  pivot_wider(names_from = renounce_bin, values_from = p)

# Germany in 2022
# Renounciation requirements among states that naturalize (global): 55.6 no renounciation, 35.7 requirement in place
cit.df %>%
  filter(year == 2022, !is.na(renounce_bin)) %>%
  group_by(renounce_bin) %>%
  count() %>%
  ungroup() %>%
  mutate(p = n / sum(n) * 100)

# Among EU states
cit.df %>%
  filter(year == 2022, 
         !is.na(renounce_bin), 
         eu == "EU", 
         iso3 != "GBR") %>%
  group_by(renounce_bin) %>%
  count() %>%
  ungroup() %>%
  mutate(p = n / sum(n) * 100)


