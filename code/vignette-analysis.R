# CAWI: Attitudes toward requirements for naturalization ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes",  
            "conflicted", "Cairo", "lmerTest", "gt", "janitor", "ggtext",
            "patchwork", "modelsummary", "marginaleffects")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

## Renaming ----
# Replaces German variable names and levels with English ones
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
  "Region" = "region_resp",
  "Economic threat" = "w2_resp",
  "Cultural threat" = "w8_resp")

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

# Coefplot-theme
coefplot.theme <- theme(
  legend.position = "bottom", legend.text = element_text(size = 14), 
  legend.justification = "left",
  plot.caption = element_text(hjust = 0, size = 14, family = "Roboto Condensed"),
  plot.caption.position = "plot",
  text = element_text(colour = "black", size = 14, family = "Roboto Condensed"),
  axis.line = element_line(colour = "black"),
  axis.text = element_text(colour = "black"),
  axis.ticks.x = element_line(colour = "black", linewidth = 0.5),
  axis.ticks.y = element_line(colour = "black", linewidth = 0.5),
  axis.text.y = element_markdown(lineheight = 1.5, family = "Roboto Condensed", size = 20),
  axis.text.x = element_text(family = "Roboto Condensed", size = 20))
                             
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
                                                "keine deutsche Staatsangehörigkeit",
                                                "doppelte Staatsangehörigkeit"),
                                     exclude = NULL),
    geburtsland_resp = case_match(
      geb_1_resp,
      1 ~ "Deutschland",
      2 ~ "Ausland",
      .default = NULL),
    geburtsland_resp = factor(geburtsland_resp, levels = c("Deutschland", "Ausland"), 
                              exclude = NULL),
    geschlecht_resp = case_match(
      geschlecht_resp,
      1 ~ "weiblich",
      2 ~ "männlich",
      .default = NULL),
    geschlecht_resp = factor(geschlecht_resp, levels = c("weiblich", "männlich"),
                             exclude = NULL),
    bildung_resp = case_match(
      bil_1_resp,
      c(3, 4) ~ "Hauptschulabschluss, POS (8/9)",
      c(5, 6) ~ "Realschulabschluss, POS (10)",
      c(7, 8) ~ "Abitur, Fachhochschulreife",
      c(1, 2) ~ "kein Schulabschluss/in Beschulung",
      .default = NULL),
    bildung_resp = factor(bildung_resp, levels = c("Hauptschulabschluss, POS (8/9)",
                                                   "kein Schulabschluss/in Beschulung",
                                                   "Abitur, Fachhochschulreife",
                                                   "Realschulabschluss, POS (10)"),
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
                         exclude = NULL),
    pol_resp = factor(sjlabelled::as_character(pol_resp), 
                      levels = c("CDU/CSU", "Keine Partei", "Andere, und zwar:",
                                 "AfD", "Die Linke", "FDP", "Bündnis 90 / Die Grünen",
                                 "SPD"),
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
  geom_text(aes(label = round(p, 1), vjust = -1)) +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  theme_ipsum_rc()

# Summary statistics
cawi_long.df %>%
  summarise(mu = mean(rating, na.rm = TRUE),
            median = median(rating, na.rm = TRUE),
            sigma = sd(rating, na.rm = TRUE),
            var = var(rating, na.rm = TRUE),
            n = n())

# Model ----
# Check block effects by including 'block' as a dummy variable (Ausprug/Hinz 2015: 91)
model.df <- tibble(
  model = c("base", "main", "int: all", "int: sig", "respondent", "int: cross-level"),
  dv = "rating ~ ",
  iv = c(
    "1",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig",
    "(mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig)^2",
    "mehrstaatigkeit_vig*geschlecht_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + herkunft_vig",
    "erwerbstätigkeit_vig + sprachkenntnisse_vig + herkunft_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + pol_resp + w2_resp + w8_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + alter_resp + geschlecht_resp",
    "mehrstaatigkeit_vig*alter_resp + mehrstaatigkeit_vig*w8_resp + erwerbstätigkeit_vig*w8_resp + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + herkunft_vig*w8_resp + pol_resp + w2_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + geschlecht_resp"),
  random = c(" + (1 | intnr) + (1 | vignette)")) %>%
  mutate(formula = str_c(dv, iv, random))

# Run models
model.df <- model.df %>%
  mutate(result = map(formula, ~lmerTest::lmer(formula = .x, data = cawi_long.df)))

# Tidy output
model.df <- model.df %>%
  mutate(tidy_result = map(result, ~.x %>%
                             broom.mixed::tidy(conf.int = TRUE) %>%
                             mutate(stars = gtools::stars.pval(p.value))),
         rsq = map(result, ~.x %>%
                      jtools::summ() %>%
                      attr("rsqs") %>%
                      enframe() %>%
                      mutate(value = unlist(value))),
         icc = map(result, ~.x %>%
                     jtools::summ() %>%
                     attr("iccs") %>%
                     .[1:2] %>%
                     enframe() %>%
                     mutate(value = unlist(value))),
         aic = map(result, ~.x %>%
                     jtools::summ() %>%
                     attr("aic") %>%
                     enframe() %>%
                     mutate(value = unlist(value))),
         bic = map(result, ~.x %>%
                     jtools::summ() %>%
                     attr("bic") %>%
                     enframe() %>%
                     mutate(value = unlist(value))))

# Regression table
modelsummary(model.df[model.df$model %in% c("base", "main", "int: sig", 
                                            "respondent", "int: cross-level"),]$result)

# LR-Test for main and two-way interactions
lr_test.tbl <- model.df %>%
  filter(model %in% c("main", "int: all")) %>%
  select(model, result) %>%
  mutate(lrtest = map(result, ~.x %>%
                        drop1(., test = "Chisq") %>%
                        broom::tidy())) %>%
  select(-result) %>%
  unnest(lrtest) %>%
  mutate(stars = gtools::stars.pval(p.value)) %>%
  select(term, df = NumDF, LRT = statistic, p = p.value) %>%
  mutate(term = str_replace(string = 
           stringi::stri_replace_all_fixed(
             term, 
             pattern = term, 
             replacement = names(vig_names), 
             vectorise_all = F), 
           pattern = ":", 
           replacement = " × ")) %>%
  gt() %>%
  fmt_number(p, decimals = 4) %>%
  fmt_number(LRT, decimals = 2)

### All cross-level interactions ----
# All vignette variables
vig_vars <- cawi_long.df %>%
  names() %>%
  str_subset("_vig")

# All respondent variables
resp_vars <- cawi_long.df %>%
  names() %>%
  str_subset("geschlecht_resp|alter_resp|region_resp|pol_resp|bildung_resp|staatsbürgerschaft_resp|w2_resp|w8_resp")

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

# Model with significant cross-level interactions
clevel.mod <- lmerTest::lmer(rating ~ 
                               mehrstaatigkeit_vig*w8_resp + 
                               mehrstaatigkeit_vig*alter_resp + 
                               mehrstaatigkeit_vig*staatsbürgerschaft_resp +
                               erwerbstätigkeit_vig*w8_resp + 
                               herkunft_vig*w8_resp + 
                               aufenthaltsdauer_vig + geschlecht_vig + pol_resp + 
                               w2_resp + region_resp + bildung_resp + geschlecht_resp +
                               (1 | vignette) + (1 | intnr),
                             data = cawi_long.df)

## Plot ----
### Different quantities ----
# Geschlecht x Mehrstaatigkeit
gender_x_cit.fig <- marginaleffects::predictions(
  model.df[model.df$model == "int: sig",]$result[[1]], 
  newdata = datagrid(geschlecht_vig = unique, 
                     mehrstaatigkeit_vig = unique),
  re.form = NA) %>%
  mutate(geschlecht_vig = factor(geschlecht_vig, 
                                 levels = c("weiblich", "männlich"), 
                                 labels = c("female", "male")),
         mehrstaatigkeit_vig = factor(mehrstaatigkeit_vig, 
                                      levels = c("behalten", "aufgeben"),
                                      labels = c("Retain", "Renounce"))) %>%
  ggplot(aes(x = geschlecht_vig, y = estimate, ymin = conf.low, ymax = conf.high, 
             shape = mehrstaatigkeit_vig,
             colour = mehrstaatigkeit_vig)) + 
  geom_pointrange(position = position_dodge(width = .9), size = 1.5, linewidth = 1.5) +
  scale_y_continuous(limits = c(3, 4.9)) +
  scale_colour_manual(values = c("Renounce" = "#969696", "Retain" = "#252525")) +
  scale_shape_manual(values = c("Renounce" = 15, "Retain" = 16)) +
  labs(title = "Gender × Dual citizenship", x = "", y = "", colour = "", shape = "") +
  theme_ipsum(base_family = "Roboto Condensed") +
  coefplot.theme +
  theme(legend.text = element_text(size = 16, family = "Roboto Condensed"))

# Reposition legend
gender_x_cit.fig <- gender_x_cit.fig %>%
  lemon::reposition_legend(position = "top right")

# Geschlecht x Mehrstaatigkeit
gender_x_residence.fig <- marginaleffects::predictions(
  model.df[model.df$model == "int: sig",]$result[[1]], 
  newdata = datagrid(geschlecht_vig = unique, 
                     aufenthaltsdauer_vig = unique),
  re.form = NA) %>%
  mutate(geschlecht_vig = factor(geschlecht_vig, 
                                 levels = c("weiblich", "männlich"), 
                                 labels = c("female", "male")),
         aufenthaltsdauer_vig = factor(aufenthaltsdauer_vig,
                                       levels = c("3 Jahre", "5 Jahre", "10 Jahre"),
                                       labels = c("3 years", "5 years", "10 years"))) %>%
  ggplot(aes(x = geschlecht_vig, y = estimate, ymin = conf.low, ymax = conf.high, 
             colour = aufenthaltsdauer_vig, shape = aufenthaltsdauer_vig)) + 
  geom_pointrange(position = position_dodge(width = .9), size = 1.5, linewidth = 1.5) +
  scale_y_continuous(limits = c(3, 4.9)) +
  scale_colour_manual(values = c("3 years" = "#bdbdbd", "5 years" = "#737373", "10 years" = "#252525")) +
  labs(title = "Gender × Residence period", x = "", y = "", colour = "", shape = "") +
  theme_ipsum(base_family = "Roboto Condensed") +
  coefplot.theme +
  theme(legend.text = element_text(size = 16, family = "Roboto Condensed"))

# Reposition legend
gender_x_residence.fig <- gender_x_residence.fig %>%
  lemon::reposition_legend(position = "top right")

# Combine
twoway_vig.fig <- wrap_plots(
  gender_x_cit.fig, gender_x_residence.fig,
  ncol = 2) + 
  plot_annotation(tag_levels = "A")

### Coefficient plots ----
#### Main model ----
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
  coefplot.theme

#### Respondent characteristics ----
resp_model <- model.df %>% 
  filter(model == "respondent") %>% 
  expand_grid(level = c("Vignette", "Respondent")) %>%
  select(level, result)

# Add plots  
resp_model <- resp_model %>%
  mutate(plot = map_if(result, level %in% "Vignette", ~.x %>%
                         modelplot(., 
                                   coef_omit = "Intercept|SD|resp",
                                   colour = "black",
                                   size = .75,
                                   linewidth = 1,
                                   coef_map = rev(c(
                                     "geschlecht_vigmännlich" = "**Gender:** Male<br>(ref.: Female)</span>",
                                     "herkunft_vigTürkei" = "**Country of origin:** Turkey<br>(ref.: Great Britain)</span>",
                                     "herkunft_vigIndien" = "India</span>",
                                     "aufenthaltsdauer_vig5 Jahre" = "**Duration of residence:** 5 years<br>(ref.: 3 years)</span>",
                                     "aufenthaltsdauer_vig10 Jahre" = "10 years</span>",
                                     "sprachkenntnisse_vigsehr gut" = "**Language proficiency:** Very good<br>(ref.: Little)</span>",
                                     "erwerbstätigkeit_vigberufstätig" = "**Employment:** Employed<br>(ref.: Seeking employment)</span>",
                                     "mehrstaatigkeit_vigaufgeben" = "**Current citizenship:** Renounce<br>(ref.: Retain)</span>",
                                     "geschlecht_vigmännlich:mehrstaatigkeit_vigaufgeben" = "**Interaction:** Male × Renounce</spany>",
                                     "aufenthaltsdauer_vig5 Jahre:geschlecht_vigmännlich" = "**Interaction:** Male × 5 years</span>",
                                     "aufenthaltsdauer_vig10 Jahre:geschlecht_vigmännlich" = "**Interaction:** Male × 10 years</span>"
                                   ))) +
                         geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
                         geom_label(aes(x = estimate, y = term, label = round(estimate, 2)), 
                                    family = "Roboto Condensed", vjust = 1.5, size = 4.5) +
                         scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), 
                                            limits = c(-1, 1.5),
                                            labels = function(x) str_replace(x, "[.]", ",")) +
                         labs(title = "Vignette variables", x = "", y = "") +
                         coefplot.theme,
                       .else = ~.x %>%
                         modelplot(., 
                                   coef_omit = "Intercept|SD|vig",
                     colour = "black",
                     size = .75,
                     linewidth = 1,
                     coef_rename = function(x)
                       str_replace_all(x, 
                                       c("pol_resp" = "",
                                         "SPD" = "**Party preference:** SPD<br>(ref.: CDU/CSU)</span>",
                                         "Bündnis 90 / Die Grünen" = "Bündnis 90 / Die Grünen",
                                         "FDP" = "FDP",
                                         "Die Linke" = "Die Linke",
                                         "AFD" = "AFD",
                                         "Andere, und zwar:" = "Other",
                                         "Keine Partei" = "No preference",
                                         "staatsbürgerschaft_resp" = "",
                                         "doppelte Staatsangehörigkeit" = "**Citizenship:** Dual citizenship<br>(ref.: Only German)</span>",
                                         "keine deutsche Staatsangehörigkeit" = "Third country",
                                         "region_resp" = "",
                                         "Ostdeutschland" = "**Region:** East Germany<br>(ref.: West Germany)</span>",
                                         "w8_resp" = "**Immigration** concern",
                                         "w2_resp" = "**Economic** concern",
                                         "bildung_resp" = "",
                                         "Realschulabschluss, POS \\(10\\)" = "**Education:** Realschule (10)<br>(ref.: Hauptschule \\(8/9\\))</span>",
                                         "Abitur, Fachhochschulreife" = "Abitur, Fachhochschulreife</span>",
                                         "kein Schulabschluss/in Beschulung" = "No diploma/in school</span>",
                                         "alter_resp" = "**Age**",
                                         "geschlecht_resp" = "",
                                         "männlich" = "**Gender:** Male<br>(ref.: Female)</span>"
                                       ))) +
           geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
           geom_label(aes(x = estimate, y = term, label = round(estimate, 2)), 
                      family = "Roboto Condensed", vjust = 1.5, size = 4.5) +
           annotate("text",  x = Inf, y = Inf, label = expression(atop(textstyle(paste("Pseudo-R"^2 ," (FE) " == 0.29)), atop(textstyle(paste("Pseudo-R"^2 ," (total) " == 0.53)), atop(scriptscriptstyle(""), textstyle(paste("ICC" == 0.33)))))), 
                    vjust = 1, hjust = 1, parse = TRUE, family = "Roboto Condensed", size = 4.6)  +
           scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), 
                              limits = c(-1.5, 1.5),
                              labels = function(x) str_replace(x, "[.]", ",")) +
           labs(title = "Respondent characteristics", x = "", y = "") +
           coefplot.theme))

# Combine levels
respondent_model.fig <- wrap_plots(resp_model$plot)

#### Cross-level interactions ----
##### Age x Dual citizenship ----
# Marginal effects
age_dualcit_interaction.df <- marginaleffects::predictions(
  model.df[model.df$model == "int: cross-level",]$result[[1]], 
  newdata = datagrid(alter_resp = unique, 
                     mehrstaatigkeit_vig = c("behalten", "aufgeben")),
  re.form = NA) 

# Interaction plot
age_dualcit_interaction.fig <- age_dualcit_interaction.df %>%
  ggplot(aes(x = alter_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             fill = mehrstaatigkeit_vig)) + 
  geom_line(aes(linetype = mehrstaatigkeit_vig), size = 1.5) +
  geom_ribbon(alpha = .5) +
  annotate("text", x = 20, y = 4.65, label = "Renounce", size = 7, hjust = 0) +
  annotate("text", x = 70, y = 2.75, label = "Retain", size = 7, hjust = 0) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_colour_manual(values = rep("black", 2)) +
  scale_y_continuous(limits = c(2.5, 5.5)) +
  scale_fill_manual(values = c("aufgeben" = "#bdbdbd", "behalten" = "#252525")) +
  labs(x = "Age", y = "", title = "Dual citizenship (L1) × Age (L2)") +
  theme_ipsum(base_family = "Roboto Condensed") +
  coefplot.theme +
  theme(legend.position = "none")

# Meaningful comparisons
age_dualcit_compare.df <- age_dualcit_interaction.df %>%
  select(estimate, mehrstaatigkeit_vig, alter_resp) %>% 
  arrange(alter_resp) %>%
  pivot_wider(names_from = mehrstaatigkeit_vig, values_from = estimate) 

# Get comparison
age_dualcit_compare.df %>%
  select(-aufgeben) %>%
  filter(alter_resp %in% c(20, 40, 60, 80)) %>%
  pivot_wider(names_from = alter_resp, values_from = c(behalten), names_prefix = "alter_") %>%
  mutate(diff_20_40 = alter_20 - alter_40,
         diff_20_60 = alter_20 - alter_60,
         diff_20_80 = alter_20 - alter_80)

# Marginal effect
avg_slopes(
  model.df[model.df$model == "int: cross-level",]$result[[1]],
  variables = "alter_resp",
  by = "mehrstaatigkeit_vig", re.form = NA)

##### Concerns x Country of origin ----
migconcern_origin_interaction.df <- marginaleffects::predictions(
  model.df[model.df$model == "int: cross-level",]$result[[1]], 
  newdata = datagrid(w8_resp = unique, 
                     herkunft_vig = unique),
  re.form = NA)

# Interaction plot
migconcern_origin_interaction.fig <- migconcern_origin_interaction.df %>% 
  filter(herkunft_vig != "Indien") %>%
  ggplot(aes(x = w8_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             fill = herkunft_vig)) + 
  geom_line(aes(linetype = herkunft_vig), size = 1.5) +
  geom_ribbon(alpha = .5) +
  annotate("text", x = 5, y = 5.25, label = "United\nKingdom", size = 7, hjust = 0) +
  annotate("text", x = 5, y = 3.5, label = "Turkey", size = 7, hjust = 0) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(limits = c(2.5, 5.5)) +
  scale_fill_manual(values = c("Großbritannien" = "#bdbdbd", "Türkei" = "#252525")) +
  labs(x = "Migration concerns", y = "", title = "Country of origin (L1) × Migration concerns (L2)") +
  theme_ipsum(base_family = "Roboto Condensed") +
  coefplot.theme +
  theme(legend.position = "none") 

# Meaningful comparisons
migconcern_origin_compare.df <- migconcern_origin_interaction.df %>%
  select(estimate, herkunft_vig, w8_resp) %>% 
  arrange(w8_resp) %>%
  pivot_wider(names_from = w8_resp, values_from = estimate, names_prefix = "w8_") 

# Get comparison
migconcern_origin_compare.df %>%
  select(!num_range("w8_", 1:5)) %>%
  pivot_wider(names_from = herkunft_vig, values_from = c(w8_6, w8_7)) %>%
  mutate(diff_GB_Turkey_7 = w8_7_Großbritannien - w8_7_Türkei,
         diff_GB_India_7 = w8_7_Großbritannien - w8_7_Indien) %>%
  select(contains("7"))

# Marginal effect
avg_slopes(
  model.df[model.df$model == "int: cross-level",]$result[[1]],
  variables = "w8_resp",
  by = "herkunft_vig", re.form = NA)

##### Concerns x Employment ----
migconcern_employment_interaction.df <- marginaleffects::predictions(
  model.df[model.df$model == "int: cross-level",]$result[[1]], 
  newdata = datagrid(w8_resp = unique, 
                     erwerbstätigkeit_vig = unique),
  re.form = NA) 

# Interaction plot
migconcern_employment_interaction.fig <- migconcern_employment_interaction.df %>% 
  ggplot(aes(x = w8_resp, y = estimate, ymin = conf.low, ymax = conf.high, 
             fill = erwerbstätigkeit_vig)) + 
  geom_line(aes(linetype = erwerbstätigkeit_vig), size = 1.5) +
  geom_ribbon(alpha = .5) +
  annotate("text", x = 4.8, y = 4.8, label = "Employed", size = 7, hjust = 0) +
  annotate("text", x = 3, y = 3, label = "Seeking\nemployment", size = 7, hjust = 0) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(limits = c(2.5, 5.5)) +
  scale_fill_manual(values = c("arbeitssuchend" = "#bdbdbd", "berufstätig" = "#252525")) +
  labs(x = "Migration concerns", y = "", title = "Employment (L1) × Migration concerns (L2)") +
  theme_ipsum(base_family = "Roboto Condensed") +
  coefplot.theme +
  theme(legend.position = "none")   

# Meaningful comparisons
migconcern_employment_compare.df <- migconcern_employment_interaction.df %>%
  select(estimate, erwerbstätigkeit_vig, w8_resp) %>% 
  arrange(w8_resp) %>%
  pivot_wider(names_from = w8_resp, values_from = estimate, names_prefix = "w8_") 

# Get comparison
migconcern_employment_compare.df %>%
  filter(erwerbstätigkeit_vig == "arbeitssuchend") %>%
  select(erwerbstätigkeit_vig, num_range("w8_", c(4, 7))) %>%
  mutate(diff_w8_4_7 = w8_4 - w8_7)

# Marginal effect
avg_slopes(
  model.df[model.df$model == "int: cross-level",]$result[[1]],
  variables = "w8_resp",
  by = "herkunft_vig", re.form = NA)

# Combine
cross_lvl.fig <- wrap_plots(
  age_dualcit_interaction.fig,
  migconcern_origin_interaction.fig, 
  migconcern_employment_interaction.fig,
  ncol = 2) + 
  plot_annotation(tag_levels = "A") 

# Description ----
#### Orthogonality ----
orthogonality.tbl <- cawi_long.df %>%
  select(ends_with("_vig")) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  rename(!!!vig_names) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  gt() %>%
  fmt_number(decimals = 3)

#### Balance ----
balance.tbl <- cawi_long.df %>%
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
  select(!!!resp_names) %>% 
  select(-c(Age, `Federal state`)) %>%
  mutate(across(where(is.factor), ~fct_recode(., !!!resp_levels)),
         Citizenship = fct_na_value_to_level(Citizenship, level = "Prefer not say")) %>%
  modelsummary::datasummary_balance(~1, data = ., output = "gt") 

# Robustness ----
## Nondifferentation ----
# Variance in individual responses 
cawi_long_nondiff.df <- cawi_long.df %>%
  group_by(intnr) %>%
  mutate(var_resp = var(rating)) %>%
  ungroup() %>%
  filter(!var_resp == 0)

# Share of nondifferent responses
100 - (nrow(cawi_long_nondiff.df) / nrow(cawi_long.df) * 100) # 11.6%

# Model
model_nondiff.df <- tibble(
  model = c("base", "main", "int: all", "int: sig", "respondent", "int: cross-level"),
  dv = "rating ~ ",
  iv = c(
    "1",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig",
    "(mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig)^2",
    "mehrstaatigkeit_vig*geschlecht_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + herkunft_vig",
    "erwerbstätigkeit_vig + sprachkenntnisse_vig + herkunft_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + pol_resp + w2_resp + w8_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + alter_resp + geschlecht_resp",
    "mehrstaatigkeit_vig*alter_resp + mehrstaatigkeit_vig*w8_resp + erwerbstätigkeit_vig*w8_resp + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + herkunft_vig*w8_resp + pol_resp + w2_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + geschlecht_resp"),
  random = c(" + (1 | intnr) + (1 | vignette)")) %>%
  mutate(formula = str_c(dv, iv, random))

# Run models
model_nondiff.df <- model_nondiff.df %>%
  mutate(result = map(formula, ~lmerTest::lmer(formula = .x, data = cawi_long_nondiff.df)))

# Summary
modelsummary(model_nondiff.df$result, stars = TRUE)

## Block effects ----
# Check block effects by including 'block' as a dummy variable (Ausprug/Hinz 2015: 91)
model_block.df <- tibble(
  model = c("base", "main", "int: all", "int: sig", "respondent", "int: cross-level"),
  dv = "rating ~ ",
  iv = c(
    "1",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig",
    "(mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig)^2",
    "mehrstaatigkeit_vig*geschlecht_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + herkunft_vig",
    "erwerbstätigkeit_vig + sprachkenntnisse_vig + herkunft_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + pol_resp + w2_resp + w8_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + alter_resp + geschlecht_resp",
    "mehrstaatigkeit_vig*alter_resp + mehrstaatigkeit_vig*w8_resp + erwerbstätigkeit_vig*w8_resp + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + herkunft_vig*w8_resp + pol_resp + w2_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + geschlecht_resp"),
  block = c(" + block"),
  random = c(" + (1 | intnr) + (1 | vignette)")) %>%
  mutate(formula = str_c(dv, iv, block, random))

# Run models
model_block.df <- model_block.df %>%
  mutate(result = map(formula, ~lmerTest::lmer(formula = .x, data = cawi_long.df)))

# Extract coefs
model_block.df <- model_block.df %>%
  mutate(tidy_result = map(result, ~.x %>%
                             broom.mixed::tidy(conf.int = TRUE) %>%
                             mutate(stars = gtools::stars.pval(p.value))))

# Significant interactions
model_block.df %>%
  select(model, tidy_result) %>%
  unnest(tidy_result) %>%
  filter(str_detect(term, "block") & stars %in% c("***", "**", "*"))

## Tobit model ----
require(GLMMadaptive)

# Information on censoring
cawi_long.df <- cawi_long.df %>%
  mutate(ind = case_when(
    between(rating, 2, 6) ~ 0, # no censoring
    rating == 1 ~ 1, # left censoring
    rating == 7 ~ 2,  # right censoring
    .default = NA_real_))

# Model df
tobit.df <- tibble(
  model = c("base", "main", "int: all", "int: sig", "respondent", "int: cross-level"),
  dv = "cbind(rating, ind) ~ ",
  iv = c(
    "1",
    "mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig",
    "(mehrstaatigkeit_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig)^2",
    "mehrstaatigkeit_vig*geschlecht_vig + erwerbstätigkeit_vig + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + herkunft_vig",
    "erwerbstätigkeit_vig + sprachkenntnisse_vig + herkunft_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + pol_resp + w2_resp + w8_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + alter_resp + geschlecht_resp",
    "mehrstaatigkeit_vig*alter_resp + mehrstaatigkeit_vig*w8_resp + erwerbstätigkeit_vig*w8_resp + sprachkenntnisse_vig + aufenthaltsdauer_vig*geschlecht_vig + mehrstaatigkeit_vig*geschlecht_vig + herkunft_vig*w8_resp + pol_resp + w2_resp + region_resp + staatsbürgerschaft_resp + bildung_resp + geschlecht_resp")) %>%
  mutate(formula = str_c(dv, iv))

# Run models
tobit.df <- tobit.df %>%
  mutate(result = map(formula, ~GLMMadaptive::mixed_model(fixed = as.formula(.x), 
                                                          random = ~ 1 | intnr,
                                                          data = cawi_long.df,
                                                          family = censored.normal())))

# Export ----
ggsave(plot = main_model.fig, filename = here("figures", "main_mod_coefplot.png"), 
       device = ragg::agg_png(res = 300), bg = "white",
       width = 20, height = 16, units = "cm")

ggsave(plot = twoway_vig.fig, filename = here("figures", "vig_twoway.png"),
       device = ragg::agg_png(res = 300), bg = "white",
       width = 28, height = 18, units = "cm")

ggsave(plot = respondent_model.fig, filename = here("figures", "resp_mod_coefplot.png"), 
       device = ragg::agg_png(res = 300), bg = "white",
       width = 34, height = 28, units = "cm")

ggsave(plot = resp_model$plot[[2]] + labs(title = ""),
       filename = here("figures", "resp_mod_coefplot_L2.png"), 
       device = ragg::agg_png(res = 300), bg = "white",
       width = 20, height = 26, units = "cm")

ggsave(plot = cross_lvl.fig, filename = here("figures", "cross_lvl_interactions.png"),
       device = ragg::agg_png(res = 300), bg = "white",
       width = 30, height = 24, units = "cm")

# Tables
gtsave(balance.tbl, filename = here("output", "vignette-dimensions.rtf"))

gtsave(orthogonality.tbl, filename = here("output", "orthogonality.rtf"))

gtsave(lr_test.tbl, filename = here("output", "lrtest.rtf"))

gtsave(resp_attr.tbl, filename = here("output", "respondent_attributes.rtf"))
