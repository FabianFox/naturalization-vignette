# Check data quality of CAWI sample against ALLBUS 2021

# Functions
# Create aggregating function
freq_tbl <- function(data, var, wt = NULL){
  data %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}, wt = {{ wt }}) %>%
    mutate(p = n / sum(n))
}

# Load data
allbus21.df <- tibble(import(here("data", "ZA5280_v2-0-0.sav")))

## Load data ----
cawi.orig <- import(here("data", "SVR_Cawi.sav")) %>%
  tibble() %>%
  janitor::clean_names() 

# Recode respondent characteristics
cawi.df <- cawi.orig %>%
  mutate(
    alter_kat = sjlabelled::as_character(alter_kat),
    bundesland_resp = as.factor(sjlabelled::as_character(bundesland)),
    pass = case_match(
      sta_1,
      1 ~ "Deutsch",
      2 ~ "andere",
      .default = NULL),
    pass = factor(pass, levels = c("Deutsch", "andere")),
    staatsbürgerschaft = case_when(
      sta_1 == 1 & sta_2_a == 2 ~ "deutsche Staatsangehörigkeit",
      sta_1 == 1 & sta_2_a == 1 ~ "doppelte Staatsangehörigkeit",
      sta_1 == 2 ~ "keine deutsche Staatsangehörigkeit",
      .default = NA_character_),
    staatsbürgerschaft = factor(staatsbürgerschaft, 
                                     levels = c("deutsche Staatsangehörigkeit",
                                                "doppelte Staatsangehörigkeit",
                                                "keine deutsche Staatsangehörigkeit")),
    geburtsland = case_match(
      geb_1,
      1 ~ "Deutschland",
      2 ~ "Ausland",
      .default = NULL),
    geburtsland= factor(geburtsland, levels = c("Deutschland", "Ausland")),
    pol = fct_recode(sjlabelled::as_label(pol), "Sonstige" = "Andere, und zwar:"),
    geschlecht = case_match(
      geschlecht,
      1 ~ "weiblich",
      2 ~ "männlich",
      .default = NULL),
    geschlecht = factor(geschlecht, levels = c("weiblich", "männlich")),
    bildung = case_match(
      bil_1,
      2 ~ "kein Schulabschluss",
      1 ~ "in Beschulung",
      c(3, 4) ~ "Hauptschulabschluss, POS (8/9)",
      c(5, 6) ~ "Realschulabschluss, POS (10)",
      c(7, 8) ~ "Abitur, Fachhochschulreife",
      .default = NULL),
    bildung = factor(bildung, levels = c("kein Schulabschluss",
                                         "in Beschulung",
                                         "Hauptschulabschluss, POS (8/9)",
                                         "Realschulabschluss, POS (10)",
                                         "Abitur, Fachhochschulreife")),
    # Pseudo weight for aggregating function
    wghtpew = 1)

# Data wrangling 
allbus21.df <- allbus21.df %>%
  mutate(
    alter_kat = case_when(
      between(age, 18, 29) ~ "18 - 29 Jahre",
      between(age, 30, 39) ~ "30 - 39 Jahre",
      between(age, 40, 49) ~ "40 - 49 Jahre",
      between(age, 50, 59) ~ "50 - 59 Jahre",
      age >= 60 ~ "60 + Jahre",
      .default = NA_character_),
    geschlecht = case_match(
      sex,
      1 ~ "männlich",
      2 ~ "weiblich",
      .default = NA_character_),
    geschlecht = factor(geschlecht, levels = c("weiblich", "männlich")),
    bildung = case_match(
      educ,
      1 ~ "kein Schulabschluss",
      2 ~ "Hauptschulabschluss, POS (8/9)",
      3 ~ "Realschulabschluss, POS (10)",
      c(4, 5) ~ "Abitur, Fachhochschulreife",
      7 ~ "in Beschulung",
      .default = NA_character_),
    bildung = factor(bildung, levels = c("kein Schulabschluss", "in Beschulung", 
                                         "Hauptschulabschluss, POS (8/9)", 
                                         "Realschulabschluss, POS (10)",
                                         "Abitur, Fachhochschulreife")))

# Combine data
df <- tibble(
  source = c("ALLBUS", "CAWI"),
  data = list(allbus21.df, cawi.df)
)

# Age
age.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = alter_kat, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))
  
age.tbl %>%
  ggplot(aes(x = factor(alter_kat, ), y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_percent() +
  coord_flip() +
  theme_ipsum()

# Sex
sex.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = geschlecht, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))

sex.tbl %>%
  ggplot(aes(x = geschlecht, y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_percent() +
  coord_flip() +
  theme_ipsum()
  
# Educ
educ.tbl <- df$data %>%
  map_df(., ~freq_tbl(data = .x, var = bildung, wt = wghtpew), .id = "source") %>%
  mutate(source = ifelse(source == 1, "ALLBUS", "CAWI"))

educ.tbl %>%
  ggplot(aes(x = bildung, y = p, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_y_percent() +
  theme_ipsum()

# Table
cross.tbl <- df %>%
  mutate(data = map(data, ~.x %>%
                      filter(if_all(c(alter_kat, geschlecht, bildung), ~ !is.na(.))) %>%
           count(alter_kat, geschlecht, bildung, wt = wghtpew) %>%
           mutate(p = n / sum(n)))) %>%
  unnest(cols = data)

# Fig
cross.fig <- cross.tbl %>%
  ggplot(aes(x = bildung, y = p, fill = geschlecht)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(
    aes(label = ifelse(p > 0.01, as.character(round((p * 100), 1)), "")),
    position = position_dodge(width = 1),
    hjust = -.5) +
  scale_y_percent(limits = c(0, 0.085)) +
  facet_grid(source ~ alter_kat) +
  coord_flip() +
  theme_ipsum()
