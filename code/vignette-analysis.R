# CAWI: Attitudes toward requirements for naturalization ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes",  
            "conflicted", "Cairo", "lmerTest", "gt", "janitor", "ggtext",
            "patchwork", "modelsummary")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

## Renaming ----
### Vignettes ----
# Vignette names
{
vig_names <- c(
  "Gender" = "geschlecht_vig",
  "Country of origin" = "herkunft_vig",
  "Residence period" = "aufenthaltsdauer_vig",
  "Employment" = "erwerbstätigkeit_vig",
  "German proficiency" = "sprachkenntnisse_vig",
  "Dual citizenship" = "mehrstaatigkeit_vig")

# Vignette levels 
vig_levels = c(
  "Male" = "männlich",
  "Female" = "weiblich",
  "United Kingdom" = "Großbritannien",
  "India" = "Indien",
  "Turkey" = "Türkei",
  "3 years" = "3 Jahre",
  "5 years" = "5 Jahre",
  "10 years" = "10 Jahre",
  "Seeking employment" = "arbeitssuchend",
  "Employed" = "berufstätig",
  "Little" = "wenig",
  "Very good" = "sehr gut",
  "Retain" = "behalten",
  "Renounce" = "aufgeben")

### Respondents ----
# Respondent attribute names
resp_names <- c(
  "Gender" = "geschlecht_resp",
  "Education" = "bildung_resp",
  "Citizenship" = "staatsbürgerschaft_resp",
  "Age" = "alter_resp",
  "Age group" = "alter_kat_resp",
  "Party preference" = "pol_resp",
  "Federal state" = "bundesland_resp",
  "Region" = "region_resp")

# Respondent attributes levels
resp_levels <- c(
  "Male" = "männlich",
  "Female" = "weiblich",
  "No diploma/in school" = "kein Schulabschluss/in Beschulung",
  "Hauptschule (8/9)" = "Hauptschulabschluss, POS (8/9)",
  "Realschule (10)" = "Realschulabschluss, POS (10)",
  "Abitur, Fachhochschulreife" = "Abitur, Fachhochschulreife",
  "German" = "deutsche Staatsangehörigkeit",
  "Dual citizenship" = "doppelte Staatsangehörigkeit",
  "Third country" = "keine deutsche Staatsangehörigkeit",
  "18–29 years" =  "18 - 29 Jahre",
  "30–39 years" =  "30 - 39 Jahre",
  "40–49 years" =  "40 - 49 Jahre",
  "50–59 years" = "50 - 59 Jahre",
  "Over 60 years" = "60 + Jahre",
  "No preference" = "Keine Partei",
  "Other" = "Sonstige",
  "West Germany (incl. Berlin)" = "Westdeutschland (mit Berlin)", 
  "East Germany" = "Ostdeutschland")
}

## Load data ----
cawi.orig <- import(here("data", "SVR_Cawi.sav")) %>%
  tibble() %>%
  janitor::clean_names() %>%
  rename_with(~str_c(., "_resp"), 
              .cols = c(
                starts_with(c("geb_", "pol", "sta_", "alter")),
                num_range("w", 1:10),
                "geschlecht", "bil_1", "bundesland"))

# Recode respondent characteristics
cawi.df <- cawi.orig %>%
  mutate(
    alter_kat_resp = as.factor(sjlabelled::as_character(alter_kat_resp)),
    bundesland_resp = as.factor(sjlabelled::as_character(bundesland_resp)),
    pass_resp = case_match(
      sta_1_resp,
      1 ~ "Deutsch",
      2 ~ "andere",
      .default = NULL),
    pass_resp = factor(pass_resp, levels = c("Deutsch", "andere"), exclude = NULL),
    staatsbürgerschaft_resp = case_when(
      sta_1_resp == 1 & sta_2_a_resp == 2 ~ "deutsche Staatsangehörigkeit",
      sta_1_resp == 1 & sta_2_a_resp == 1 ~ "doppelte Staatsangehörigkeit",
      sta_1_resp == 2 ~ "keine deutsche Staatsangehörigkeit",
      .default = NULL),
    staatsbürgerschaft_resp = factor(staatsbürgerschaft_resp, 
                                     levels = c("deutsche Staatsangehörigkeit",
                                                "doppelte Staatsangehörigkeit",
                                                "keine deutsche Staatsangehörigkeit"),
                                     exclude = NULL),
    geburtsland_resp = case_match(
      geb_1_resp,
      1 ~ "Deutschland",
      2 ~ "Ausland",
      .default = NULL),
    geburtsland_resp = factor(geburtsland_resp, levels = c("Deutschland", "Ausland"), 
                              exclude = NULL),
    pol_resp = fct_recode(sjlabelled::as_label(pol_resp), "Sonstige" = "Andere, und zwar:"),
    geschlecht_resp = case_match(
      geschlecht_resp,
      1 ~ "weiblich",
      2 ~ "männlich",
      .default = NULL),
    geschlecht_resp = factor(geschlecht_resp, levels = c("weiblich", "männlich"),
                             exclude = NULL),
    bildung_resp = case_match(
      bil_1_resp,
      c(1, 2) ~ "kein Schulabschluss/in Beschulung",
      c(3, 4) ~ "Hauptschulabschluss, POS (8/9)",
      c(5, 6) ~ "Realschulabschluss, POS (10)",
      c(7, 8) ~ "Abitur, Fachhochschulreife",
      .default = NULL),
    bildung_resp = factor(bildung_resp, levels = c("kein Schulabschluss/in Beschulung",
                                                   "Hauptschulabschluss, POS (8/9)",
                                                   "Realschulabschluss, POS (10)",
                                                   "Abitur, Fachhochschulreife"),
                          exclude = NULL),
    region_resp = case_match(
      bundesland_resp,
      c("Baden-Württemberg", "Bayern", "Berlin", "Bremen", "Hamburg", 
        "Hessen", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", 
        "Saarland", "Schleswig-Holstein") ~ "Westdeutschland (mit Berlin)",
      c("Brandenburg", "Mecklenburg-Vorpommern", 
        "Sachsen", "Sachsen-Anhalt", "Thüringen") ~ "Ostdeutschland",
      .default = NULL),
    region_resp = factor(region_resp, levels = c("Westdeutschland (mit Berlin)", 
                                                 "Ostdeutschland"),
                         exclude = NULL))

# Vignette analysis ----
## Prepare data ----
# Make vignettes long
cawi_long.df <- cawi.df %>%
  pivot_longer(cols = num_range("vig_", 1:144), names_to = "vignette", values_to = "rating") %>%
  mutate(vignette = as.numeric(str_extract(vignette, "[:digit:]+")))

# Load vignette design to match
vig_design.df <- rio::import(here("data", "230906_Vignetten.xlsx")) %>%
  tibble() %>%
  rename_with(~str_c(., "_vig"), .cols = c("Geschlecht", "Herkunft", "Aufenthaltsdauer", "Erwerbstätigkeit",
                                  "Sprachkenntnisse", "Mehrstaatigkeit")) %>%
  rename_with(~tolower(.), .cols = everything())

# Join
cawi_long.df <- cawi_long.df %>%
  left_join(y = vig_design.df, by = c("vignette" = "vignette"))

# Turn vignette variables into factors
cawi_long.df <- cawi_long.df %>%
  mutate(geschlecht_vig = factor(geschlecht_vig, levels = c("weiblich", "männlich")),
         herkunft_vig = factor(herkunft_vig, levels = c("Großbritannien", "Indien", "Türkei")),
         aufenthaltsdauer_vig = factor(aufenthaltsdauer_vig, levels = c("3 Jahre", "10 Jahre", "5 Jahre")),
         erwerbstätigkeit_vig = factor(erwerbstätigkeit_vig, levels = c("arbeitssuchend", "berufstätig")),
         sprachkenntnisse_vig = factor(sprachkenntnisse_vig, levels = c("wenig", "sehr gut")),
         mehrstaatigkeit_vig = factor(mehrstaatigkeit_vig, levels = c("behalten", "aufgeben")))

# Remove missing ratings
cawi_long.df <- cawi_long.df %>% 
  filter(!is.na(rating))

# Description ----
# All ratings
cawi_long.df %>%
  mutate(mu = mean(rating, na.rm = TRUE)) %>%
  ggplot(aes(x = rating)) +
  geom_bar(fill = "#0098D4") +
  geom_vline(aes(xintercept = mu)) +
  theme_ipsum_rc()

# Mean by respondents
cawi_long.df %>%
  group_by(intnr) %>%
  summarise(mu = mean(rating, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(grand_mu = mean(mu)) %>%
  ggplot(aes(x = mu)) +
  geom_bar(fill = "#0098D4") +
  geom_vline(aes(xintercept = grand_mu)) +
  theme_ipsum_rc()

# Frequency plot
cawi_long.df %>%
  mutate(mu = mean(rating, na.rm = TRUE)) %>%
  group_by(rating) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(p = n / sum(n) * 100) %>%
  ggplot(aes(x = rating, y = p)) +
  geom_bar(fill = "#0098D4", stat = "identity") +
  scale_y_continuous(labels = function(x) str_c(x, "%"))

# Summary statistics
cawi_long.df %>%
  summarise(mu = mean(rating, na.rm = TRUE),
            median = median(rating, na.rm = TRUE),
            sigma = sd(rating, na.rm = TRUE),
            var = var(rating, na.rm = TRUE),
            n = n()) 

## Model ----
# Check block effects by including 'block' as a dummy variable (Ausprug/Hinz 2015: 91)
model.df <- tibble(
  model = c("base", "main", "int: employment", "int: language", "respondent", "cross-level interactions"),
  dv = "rating ~ ",
  iv = c(
    "1",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig",
    "mehrstaatigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + geschlecht_vig + herkunft_vig*erwerbstätigkeit_vig",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + aufenthaltsdauer_vig + geschlecht_vig + herkunft_vig*sprachkenntnisse_vig",
    "geschlecht_vig + mehrstaatigkeit_vig + aufenthaltsdauer_vig + herkunft_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + geschlecht_resp + alter_resp + bildung_resp + region_resp + pol_resp + staatsbürgerschaft_resp + w2_resp + w8_resp",
    "geschlecht_vig + aufenthaltsdauer_vig + herkunft_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + mehrstaatigkeit_vig*alter_resp + geschlecht_resp + bildung_resp + region_resp + pol_resp + staatsbürgerschaft_resp + w2_resp + w8_resp"),
  random = c(" + (1 | intnr) + (1 | vignette)")) %>%
  mutate(formula = str_c(dv, iv, random))

# Run models
model.df <- model.df %>%
  mutate(result = map(formula, ~lmerTest::lmer(formula = .x, data = cawi_long.df)))

# Tidy output
model.df <- model.df %>%
  mutate(tidy_result = map(result, ~.x %>%
                             broom.mixed::tidy(conf.int = TRUE) %>%
                             mutate(stars = gtools::stars.pval(p.value))))

# All interactions
model_int <- lmerTest::lmer(rating ~ (geschlecht_vig + herkunft_vig + aufenthaltsdauer_vig + 
                                        erwerbstätigkeit_vig + sprachkenntnisse_vig + 
                                        mehrstaatigkeit_vig)^2 + 
                              (1 | vignette) + (1 | intnr), 
                            data = cawi_long.df)

### Cross-level interactions ----
# All vignette variables
vig_vars <- cawi_long.df %>%
  names() %>%
  str_subset("_vig")

# All respondent variables
resp_vars <- cawi_long.df %>%
  names() %>%
  str_subset("geschlecht_resp|alter_resp|bundesland_resp|pol_resp|bildung_resp|staatsbürgerschaft_resp")

# Model formula (all combinations)
models <- tibble(
  vig_formula = str_c(vig_vars, collapse = " + "),
  resp_formula = str_c(resp_vars, collapse = " + "),
  tidyr::expand_grid(vig_vars, resp_vars) %>%
    mutate(interaction = str_c(vig_vars, ":", resp_vars)) %>%
    select(interaction)) %>%
  mutate(formula = str_c("rating ~ ", vig_formula, " + ", resp_formula, " + ", interaction, " + (1 | intnr) + (1 | vignette)")) %>%
  select(formula)

# Run models
models <- models %>%
  mutate(results = map(formula, ~.x %>%
                         lmerTest::lmer(data = cawi_long.df)))

# Tidy
models <- models %>%
  mutate(results = map(results, ~broom.mixed::tidy(.x))) %>%
  unnest(results) %>%
  mutate(stars = gtools::stars.pval(p.value)) 

# Significant interactions
models %>%
  filter(str_detect(term, ":") & stars %in% c("***", "**", "*"))

## Plot ----
# Geschlecht x Mehrstaatigkeit
marginaleffects::predictions(
  model_int2, 
  newdata = datagrid(geschlecht_vig = unique, 
                     mehrstaatigkeit_vig = unique),
  re.form = NA) %>%
  ggplot(aes(x = geschlecht_vig, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = mehrstaatigkeit_vig, fill = mehrstaatigkeit_vig)) + 
  geom_pointrange(position = position_dodge(width = .9)) +
  theme_ipsum(base_family = "Roboto Condensed")

# Sprachkenntnisse x Mehrstaatigkeit
marginaleffects::predictions(
  model_int2, 
  newdata = datagrid(sprachkenntnisse_vig = unique, 
                     mehrstaatigkeit_vig = unique),
  re.form = NA) %>%
  ggplot(aes(x = sprachkenntnisse_vig, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = mehrstaatigkeit_vig, fill = mehrstaatigkeit_vig)) + 
  geom_pointrange(position = position_dodge(width = .9)) +
  theme_ipsum(base_family = "Roboto Condensed")

# Staatsangehörigkeit x Erwerbstätigkeit
marginaleffects::predictions(
  model_full, 
  newdata = datagrid(staatsbürgerschaft_resp = unique, 
                     erwerbstätigkeit_vig = unique),
  re.form = NA) %>%
  ggplot(aes(x = staatsbürgerschaft_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = erwerbstätigkeit_vig, fill = erwerbstätigkeit_vig)) + 
  geom_pointrange(position = position_dodge(width = .9)) +
  theme_ipsum(base_family = "Roboto Condensed")

# Staatsangehörigkeit x Sprachkenntnisse
marginaleffects::predictions(
  model_full, 
  newdata = datagrid(staatsbürgerschaft_resp = unique, 
                     sprachkenntnisse_vig = unique),
  re.form = NA) %>%
  ggplot(aes(x = staatsbürgerschaft_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = sprachkenntnisse_vig, fill = sprachkenntnisse_vig)) + 
  geom_pointrange(position = position_dodge(width = .9)) +
  theme_ipsum(base_family = "Roboto Condensed")

# Alter x Mehrstaatigkeit
# Marginal effects
age_dualcit_interaction.df <- marginaleffects::predictions(
  model_resp, 
  newdata = datagrid(alter_resp = seq(18, 80, 1), 
                     mehrstaatigkeit_vig = c("behalten", "aufgeben")),
  re.form = NA) 

# Meaningful comparisons
age_dualcit_interaction.df %>%
  filter(alter_resp %in% c(20, 60, 80), mehrstaatigkeit_vig == "behalten") %>%
  select(estimate, mehrstaatigkeit_vig, alter_resp) %>% 
  pivot_wider(names_from = alter_resp, values_from = estimate, names_prefix = "alter_") %>% 
  mutate(diff20_60 = alter_20 - alter_60,
         diff20_80 = alter_20 - alter_80)

# Interaction plot
age_dualcit_interaction.fig <- age_dualcit_interaction.df %>%
  ggplot(aes(x = alter_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = mehrstaatigkeit_vig, fill = mehrstaatigkeit_vig)) + 
  geom_line() +
  geom_ribbon(alpha = .5) +
  scale_fill_manual(values = c("aufgeben" = "#0098D4", "behalten" = "#003c76")) +
  theme_ipsum(base_family = "Tahoma")

# Coefficient plot
main_model.fig <- modelsummary::modelplot(model.df[model.df$model == "main",]$result, 
                                         coef_omit = "Intercept|SD",
                                         colour = "black",
                                         size = .75,
                                         linewidth = 1,
                                         coef_rename = function(x)
                                           str_replace_all(x, 
                                                           c("mehrstaatigkeit_vig" = "",
                                                             "aufgeben" = "**Current citizenship:** Renounce<br>(ref.: Retain)</span>",
                                                             "erwerbstätigkeit_vig" = "",
                                                             "berufstätig" = "**Employment:** Employed<br>(ref.: Seeking employment)</span>",
                                                             "sprachkenntnisse_vig" = "",
                                                             "sehr gut" = "**Language proficiency:** Very good<br>(ref.: Little)</span>",
                                                             "aufenthaltsdauer_vig" = "",
                                                             "10 Jahre" = "10 years</span>",
                                                             "5 Jahre" = "**Duration of residence:** 5 years<br>(ref.: 3 years)</span>",
                                                             "herkunft_vig" = "",
                                                             "Türkei" = "**Country of origin:** Turkey<br>(ref.: Great Britain)</span>",
                                                             "Indien" = "India</span>",
                                                             "geschlecht_vig" = "",
                                                             "männlich" = "**Gender:** Male<br>(ref.: Female)</span>"
                                                             ))) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  geom_label(aes(x = estimate, y = term, label = round(estimate, 2)), 
             family = "Roboto Condensed", vjust = 1.5, size = 4.5) +
  annotate("text",  x = Inf, y = Inf, label = expression(atop(textstyle(paste("Pseudo-R"^2 ," (FE) " == 0.20)), atop(textstyle(paste("Pseudo-R"^2 ," (total) " == 0.52)), atop(scriptscriptstyle(""), textstyle(paste("ICC" == 0.40)))))), 
           vjust = 1, hjust = 1, parse = TRUE, family = "Roboto Condensed", size = 4.6)  +
  scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), 
                     limits = c(-1, 1.5),
                     labels = function(x) str_replace(x, "[.]", ",")) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", legend.text = element_text(size = 14), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0, size = 12),
        plot.caption.position = "plot",
        text = element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.y = element_line(colour = "black", linewidth = 0.5),
        axis.text.y = element_markdown(lineheight = 1.5, family = "Roboto Condensed", size = 14),
        axis.text.x = element_text(family = "Roboto Condensed", size = 14))

# Respondent characteristics
resp_model <- model.df %>% 
  filter(model == "respondent") %>% 
  pull(tidy_result) %>% 
  .[[1]] %>% 
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(level = ifelse(str_detect(term, "resp"), "Respondent", "Vignette")) %>%
  group_by(level) %>%
  nest() %>%
  mutate(plot = map(data, ~.x %>%
                      ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
                      geom_linerange() +
                      theme_ipsum())) 

wrap_plots(resp_model$plot)

# Description ----
#### Orthogonality ----
cawi_long.df %>%
  select(ends_with("_vig")) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  gt() %>%
  fmt_number(decimals = 3)

#### Balance ----
balance_tbl <- cawi_long.df %>%
  select(ends_with("_vig")) %>%
  rename(!!!vig_names) %>%
  # warning due to unused levels when splicing
  mutate(across(where(is.factor), ~fct_recode(., !!!vig_levels))) %>%
  imap_dfr(~ {
    counts <- table(.x)
    frequencies <- prop.table(counts)
    data.frame(
      Attribute = .y,
      Level = names(counts),
      N = as.numeric(counts),
      Percentage = as.numeric(frequencies))}) %>%
  gt(groupname_col = "Attribute",
     row_group_as_column = TRUE) %>%
  fmt_number(N, drop_trailing_zeros = TRUE) %>%
  fmt_number(Percentage, decimals = 1, scale_by = 100)

## Respondent attributes ----
resp_attr.tbl <- cawi.df %>%
  select(!!!resp_names) %>%  # w2_resp, w8_resp
  select(-c(Age, `Federal state`)) %>%
  mutate(across(where(is.factor), ~fct_recode(., !!!resp_levels)),
         Citizenship = fct_na_value_to_level(Citizenship, level = "Prefer not say"),
         ) %>%
  modelsummary::datasummary_balance(~1, data = ., output = "gt") %>%
  cols_label(
    " " = "Attribute",
    "  " = "Level",
    Pct. = "Percentage")

# Change formatting of `N`
resp_attr.tbl$`_data`$N <- as.numeric(resp_attr.tbl$`_data`$N)

# fmt_number
resp_attr.tbl <- resp_attr.tbl %>%
  fmt_number(N,  drop_trailing_zeros = TRUE)

# Export ----
ggsave(plot = main_model.fig, filename = here("figures", "main_mod_coefplot.png"), 
       device = ragg::agg_png(res = 300), bg = "white",
       width = 20, height = 16, units = "cm")

gtsave(balance_tbl, filename = here("output", "vignette-dimensions.rtf"))

gtsave(resp_attr.tbl, filename = here("output", "respondent_attributes.rtf"))
