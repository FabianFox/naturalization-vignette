# Check data quality of CAWI sample against ALLBUS 2021

## Setup ----
### Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "conflicted", "hrbrthemes", "patchwork")

### Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

# Functions
# Create aggregating function
freq_tbl <- function(data, var, wt = NULL){
  data %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}, wt = {{ wt }}) %>%
    mutate(p = n / sum(n))
}

### Load data ----
# Allbus
allbus21.df <- tibble(import(here("data", "ZA5280_v2-0-0.sav")))

# CAWI
cawi.orig <- tibble(import(here("data", "SVR_Cawi.sav"))) %>%
  janitor::clean_names() %>%
  rename_with(~str_c(., "_resp"), 
              .cols = c(
                starts_with(c("geb_", "pol", "sta_", "alter")),
                num_range("w", 1:10),
                "geschlecht", "bil_1", "bundesland"))

## Harmonize data ----
# Recode respondent characteristics
cawi.df <- cawi.orig %>%
  mutate(
    alter_kat_resp = sjlabelled::as_character(alter_kat_resp),
    alter_kat_resp = case_match(alter_kat_resp,
                                "18 - 29 Jahre" ~ "18–29 years",
                                "30 - 39 Jahre" ~ "30–39 years",
                                "40 - 49 Jahre" ~ "40–49 years",
                                "50 - 59 Jahre" ~ "50–59 years",
                                "60 + Jahre" ~ "Over 60 years"),
    alter_kat_resp = factor(alter_kat_resp, levels = c("18–29 years", 
                                                       "30–39 years", 
                                                       "40–49 years",
                                                       "50–59 years",
                                                       "Over 60 years")),
    bundesland_resp = as.factor(sjlabelled::as_character(bundesland_resp)),
    pass_resp = case_match(
      sta_1_resp,
      1 ~ "German",
      2 ~ "Other",
      .default = NULL),
    pass_resp = factor(pass_resp, levels = c("German", "Other"), exclude = NULL),
    staatsbürgerschaft_resp = case_when(
      sta_1_resp == 1 & sta_2_a_resp == 2 ~ "German",
      sta_1_resp == 1 & sta_2_a_resp == 1 ~ "Dual citizenship",
      sta_1_resp == 2 ~ "Third country",
      .default = NULL),
    staatsbürgerschaft_resp = factor(staatsbürgerschaft_resp, 
                                     levels = c("German",
                                                "Third country",
                                                "Dual citizenship"),
                                     exclude = NULL),
    geburtsland_resp = case_match(
      geb_1_resp,
      1 ~ "Germany",
      2 ~ "Foreign country",
      .default = NULL),
    geburtsland_resp = factor(geburtsland_resp, levels = c("Germany", "Foreign country"), 
                              exclude = NULL),
    geschlecht_resp = case_match(
      geschlecht_resp,
      1 ~ "Female",
      2 ~ "Male",
      .default = NULL),
    geschlecht_resp = factor(geschlecht_resp, levels = c("Female", "Male"),
                             exclude = NULL),
    bildung_resp = case_match(
      bil_1_resp,
      c(3, 4) ~ "Hauptschulabschluss, POS (8/9)",
      c(5, 6) ~ "Realschulabschluss, POS (10)",
      c(7, 8) ~ "Abitur, Fachhochschulreife",
      c(1, 2) ~ "No school completion/in schooling",
      .default = NULL),
    bildung_resp = factor(bildung_resp, levels = c("Other",
                                                   "No school completion/in schooling",
                                                   "Hauptschulabschluss, POS (8/9)",
                                                   "Realschulabschluss, POS (10)",
                                                   "Abitur, Fachhochschulreife"),
                          exclude = NULL),
    region_resp = case_match(
      bundesland_resp,
      c("Baden-Württemberg", "Bayern", "Berlin", "Bremen", "Hamburg", 
        "Hessen", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", 
        "Saarland", "Schleswig-Holstein") ~ "West Germany (incl. Berlin)",
      c("Brandenburg", "Mecklenburg-Vorpommern", 
        "Sachsen", "Sachsen-Anhalt", "Thüringen") ~ "East Germany",
      .default = NULL),
    region_resp = factor(region_resp, levels = c("West Germany (incl. Berlin)", 
                                                 "East Germany"),
                         exclude = NULL),
    pol_resp = factor(sjlabelled::as_character(pol_resp), 
                      levels = c("CDU/CSU", "Keine Partei", "Andere, und zwar:",
                                 "AfD", "Die Linke", "FDP", "Bündnis 90 / Die Grünen",
                                 "SPD"),
                      exclude = NULL),
    # Pseudo weight for aggregating function
    wghtpew = 1)

# Data wrangling 
allbus21.df <- allbus21.df %>%
  mutate(
    alter_kat_resp = case_when(
      between(age, 18, 29) ~ "18–29 years",
      between(age, 30, 39) ~ "30–39 years",
      between(age, 40, 49) ~ "40–49 years",
      between(age, 50, 59) ~ "50–59 years",
      age >= 60 ~ "Over 60 years",
      .default = NA_character_),
    alter_kat_resp = factor(alter_kat_resp, levels = c("18–29 years", 
                                                       "30–39 years", 
                                                       "40–49 years",
                                                       "50–59 years",
                                                       "Over 60 years")),
    geschlecht_resp = case_match(
      sex,
      1 ~ "Male",
      2 ~ "Female",
      .default = NA_character_),
    geschlecht_resp = factor(geschlecht, levels = c("Female", "Male")),
    bildung_resp = case_match(
      educ,
      c(1, 7) ~ "No school completion/in schooling",
      2 ~ "Hauptschulabschluss, POS (8/9)",
      3 ~ "Realschulabschluss, POS (10)",
      c(4, 5) ~ "Abitur, Fachhochschulreife",
      6 ~ "Other",
      .default = NULL),
    bildung_resp = factor(bildung_resp, levels = c(
      "Other",
      "No school completion/in schooling", 
      "Hauptschulabschluss, POS (8/9)", 
      "Realschulabschluss, POS (10)",
      "Abitur, Fachhochschulreife")))

# Combine data
df <- tibble(
  source = c("ALLBUS", "CAWI"),
  data = list(allbus21.df, cawi.df)
)

## Plot ----
### Age ----
age.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = alter_kat_resp, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))
  
age.fig <- age.tbl %>%
  ggplot(aes(x = alter_kat_resp, y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(p * 100, 1)), 
            position = position_dodge(width = .9), 
            colour = "white",
            hjust = 1.5,
            size = 5.5) +
  scale_y_percent() +
  scale_fill_manual(name = "", values = c("CAWI" = "#bdbdbd", "ALLBUS" = "#252525")) +
  coord_flip() +
  labs(title = "Age", x = "", y = "") +
  theme_ipsum(base_size = 14, base_family = "Roboto Condensed")

age.fig <- age.fig %>%
  lemon::reposition_legend(position = "right bottom")

### Gender ----
sex.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = geschlecht_resp, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))

sex.fig <- sex.tbl %>%
  ggplot(aes(x = geschlecht_resp, y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(p * 100, 1)), 
            position = position_dodge(width = .9), 
            colour = "white",
            hjust = 1.5,
            size = 5.5) +
  scale_y_percent() +
  scale_fill_manual(name = "", values = c("CAWI" = "#bdbdbd", "ALLBUS" = "#252525")) +
  coord_flip() +
  labs(title = "Gender", x = "", y = "") +
  theme_ipsum(base_size = 14, base_family = "Roboto Condensed") +
  theme(legend.position = "bottom")
  
### Education ----
educ.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = bildung_resp, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))

educ.fig <- educ.tbl %>%
  ggplot(aes(x = bildung_resp, y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = ifelse(p >= 0.05, 
                               as.character(round(p * 100, digit = 1)),
                               "")), 
            position = position_dodge(width = .9), 
            colour = "white",
            hjust = 1.25,
            size = 5.5) +
  scale_y_percent() +
  scale_fill_manual(name = "", values = c("CAWI" = "#bdbdbd", "ALLBUS" = "#252525")) +
  coord_flip() +
  labs(title = "Education", x = "", y = "") +
  theme_ipsum(base_size = 14, base_family = "Roboto Condensed")

educ.fig <- educ.fig %>%
  lemon::reposition_legend(position = "bottom right") 

### Age x Educ x Gender ----
cross.tbl <- df %>%
  mutate(data = map(data, ~.x %>%
                      filter(if_all(c(alter_kat_resp, geschlecht_resp, bildung_resp), ~ !is.na(.))) %>%
           count(alter_kat_resp, geschlecht_resp, bildung_resp, wt = wghtpew) %>%
           mutate(p = n / sum(n)))) %>%
  unnest(cols = data)

# Fig
cross.fig <- cross.tbl %>%
  ggplot(aes(x = bildung_resp, y = p, fill = geschlecht_resp)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(
    aes(label = ifelse(p > 0.01, as.character(round((p * 100), 1)), "")),
    position = position_dodge(width = 1),
    hjust = -0.5, 
    size = 2.5) +
  scale_y_percent(limits = c(0, 0.085)) +
  scale_fill_manual(name = "", values = c("Male" = "#bdbdbd", "Female" = "#252525")) +
  facet_grid(source ~ alter_kat_resp) +
  coord_flip() +
  labs(title = "Education by age and gender", x = "", y = "") +
  theme_ipsum(base_size = 12, base_family = "Roboto Condensed") +
  theme(legend.position = "bottom")

## Combine plots ----
# Layout
layout <- "
AABBBC
DDDDDD
"

# Plot
crossplot.fig <- wrap_plots(age.fig, educ.fig, sex.fig, cross.fig, design = layout) +
  plot_annotation(tag_levels = "A")

## Export ----
ggsave(plot = crossplot.fig, filename = here("figures", "sample_compare.png"), 
       device = ragg::agg_png(res = 300), bg = "white",
       width = 48, height = 24, units = "cm")
