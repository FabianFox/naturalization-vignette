# CAWI: Attitudes toward requirements for naturalization ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes", "gtable", "gridExtra", 
            "ggridges", "conflicted", "Cairo", "lmerTest", "gt", "janitor")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

# English renaming of vignette levels
vig_names <- c(
  "Gender" = "geschlecht_vig",
  "Country of origin" = "herkunft_vig",
  "Residence period" = "aufenthaltsdauer_vig",
  "Employment" = "erwerbstätigkeit_vig",
  "German proficiency" = "sprachkenntnisse_vig",
  "Dual citizenship" = "mehrstaatigkeit_vig")

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

## Load data ----
cawi.orig <- import(here("data", "SVR_Cawi.sav")) %>%
  tibble() %>%
  janitor::clean_names() %>%
  rename_with(~str_c(., "_resp"), 
              .cols = c(
                starts_with(c("geb_", "pol", "sta_", "alter")),
                "geschlecht", "bil_1", "bundesland"))

# Recode respondent characteristics
cawi.df <- cawi.orig %>%
  mutate(
    bundesland_resp = as.factor(sjlabelled::as_character(bundesland_resp)),
    pass_resp = case_match(
      sta_1_resp,
      1 ~ "Deutsch",
      2 ~ "andere",
      .default = NULL),
    pass_resp = factor(pass_resp, levels = c("Deutsch", "andere")),
    staatsbürgerschaft_resp = case_when(
      sta_1_resp == 1 & sta_2_a_resp == 2 ~ "deutsche Staatsangehörigkeit",
      sta_1_resp == 1 & sta_2_a_resp == 1 ~ "doppelte Staatsangehörigkeit",
      sta_1_resp == 2 ~ "keine deutsche Staatsangehörigkeit",
      .default = NA_character_),
    staatsbürgerschaft_resp = factor(staatsbürgerschaft_resp, 
                                     levels = c("deutsche Staatsangehörigkeit",
                                                "doppelte Staatsangehörigkeit",
                                                "keine deutsche Staatsangehörigkeit")),
    geburtsland_resp = case_match(
      geb_1_resp,
      1 ~ "Deutschland",
      2 ~ "Ausland",
      .default = NULL),
    geburtsland_resp = factor(geburtsland_resp, levels = c("Deutschland", "Ausland")),
    pol_resp = fct_recode(sjlabelled::as_label(pol_resp), "Sonstige" = "Andere, und zwar:"),
    geschlecht_resp = case_match(
      geschlecht_resp,
      1 ~ "weiblich",
      2 ~ "männlich",
      .default = NULL),
    geschlecht_resp = factor(geschlecht_resp, levels = c("weiblich", "männlich")),
    bildung_resp = case_match(
      bil_1_resp,
      2 ~ "kein Schulabschluss",
      1 ~ "in Beschulung",
      c(3, 4) ~ "Hauptschulabschluss, POS (8/9)",
      c(5, 6) ~ "Realschulabschluss, POS (10)",
      c(7, 8) ~ "Abitur, Fachhochschulreife",
      .default = NULL),
    bildung_resp = factor(bildung_resp, levels = c("kein Schulabschluss",
                                                   "in Beschulung",
                                                   "Hauptschulabschluss, POS (8/9)",
                                                   "Realschulabschluss, POS (10)",
                                                   "Abitur, Fachhochschulreife")))

# Analysis of ALLBUS replication ----
# A1-9 + 10
# Frequency 
allbus_p.df <- cawi.orig %>%
  select("intnr", num_range("a", 1:10)) %>%
  pivot_longer(cols = num_range("a", 1:10), names_to = "variable", values_to = "value") %>%
  mutate(value = if_else(value %in% c(97, 98), NA_real_, value),
         variable = factor(variable, 
                           levels = str_c("a", 1:10),
                           labels = c("Ob die Person in Deutsch-\n   land geboren ist (jus soli)",
                                      "...deutscher Abstammung ist\n(jus sanguinis)",
                                      "...die deutsche Sprache\n   beherrscht",
                                      "...lange Zeit in Deutsch-\n   land gelebt hat",
                                      "...sich an den Lebensstil\n   der Deutschen anzu\n   passen",
                                      "...einer christlichen Kirche\n   angehört",
                                      "...Straftaten begangen hat",
                                      "...für ihren Lebensunterhalt\n   selbst aufkommen kann",
                                      "...zur freiheitlich demo-\n   kratischen Grundordnung\n   bekennt",
                                      "für Politik in Deutschland\n   interessiert"))) %>%
  filter(!is.na(value)) %>%
  group_by(value, variable) %>%
  count() %>%
  group_by(variable) %>%
  mutate(p = n / sum(n))

# Plot frequency
allbus_p.fig <- ggplot(allbus_p.df) +
  geom_bar(aes(x = value, y = p, group = variable),
           width = 1,
           show.legend = FALSE,
           stat = "identity",
           fill = "#0098D4") +
  scale_x_continuous(breaks = c(1:7), labels = c("1\nÜberhaupt\nnicht\nwichtig", 
                                                 seq(2, 6, 1), 
                                                 "7\nSehr\nwichtig"), 
                     minor_breaks = 1:7) +
  scale_y_percent() +
  labs(title = "Einstellungen zu Einbürgerungsvoraussetzungen",
       x = "", 
       y = "",
       caption = 'Frage: "Bitte geben Sie anhand der Skala an, wie wichtig ihrer Meinung nach diese Dinge jeweils sein sollten. Ob die Person..."\n\nQuelle: SVR-CAWI-Umfrage: Einstellung zur Einbürgerung') +
  lemon::facet_rep_grid(~ variable, scales = "free",
                        switch = "y") +
  theme_ipsum(ticks = TRUE, base_size = 14, base_family = "Roboto Condensed", strip_text_size = 10, 
              caption_size = 12, caption_face = "plain", 
              axis_title_size = 12, grid = "Yy") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0),
        text = element_text(colour = "black"))

# Concerns ----
concerns.df <- cawi.orig %>%
  select("intnr", num_range("w", 1:9)) %>%
  pivot_longer(cols = num_range("w", 1:9), names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, 
                           levels = str_c("w", 1:9),
                           labels = c("allgemeine wirtschaftliche Entwicklung", 
                                      "eigene wirtschaftliche Situation",
                                      "Folgen des Klimawandels",
                                      "Erhaltung des Friedens",
                                      "Gesundheit",
                                      "Kriminalität",
                                      "sozialen Zusammenhalt",
                                      "Zuwanderung",
                                      "Ausländerfeindlichkeit und Fremdenhass")),
         value = factor(value, levels = c(1:7), 
                        labels = c("Keine Sorgen", 
                                   2:6, 
                                   "Große Sorgen"))) %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(variable, p, max), y = p, fill = value)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = if_else(p <= .05, 
                                "", 
                                str_replace(as.character(round(p, 2) * 100), "[.]", ","))),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 4,
            colour = "#000000") +
  scale_fill_brewer(type = "diverging", palette = "BrBG") +
  scale_y_percent() +
  labs(x = "", y = "", title = "Sorgen in verschiedenen Lebensbereichen",
       caption = 'Frage: "Wie ist es mit folgenden Bereichen - machen Sie sich da Sorgen?"\n\nQuelle: SVR-CAWI-Umfrage: Einstellung zur Einbürgerung',
       fill = "") +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_ipsum(ticks = TRUE, base_size = 14, base_family = "Roboto Condensed", strip_text_size = 10, 
              caption_size = 12, caption_face = "plain", 
              axis_title_size = 12, grid = "Yy") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0),
        text = element_text(colour = "black"))

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
         aufenthaltsdauer_vig = factor(aufenthaltsdauer_vig, levels = c("3 Jahre", "5 Jahre", "10 Jahre")),
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
# Base: Only vignette effects
model <- lme4::lmer(rating ~ mehrstaatigkeit_vig + sprachkenntnisse_vig + erwerbstätigkeit_vig +
                           aufenthaltsdauer_vig + herkunft_vig + geschlecht_vig +
                          (1 | intnr) +  (1 | vignette), 
                        data = cawi_long.df)

# With interactions
model_int <- lmerTest::lmer(rating ~ (geschlecht_vig + herkunft_vig + aufenthaltsdauer_vig + 
                                        erwerbstätigkeit_vig + sprachkenntnisse_vig + 
                                        mehrstaatigkeit_vig)^2 + 
                              (1 | vignette) + (1 | intnr), 
                            data = cawi_long.df)

# With significant interactions
model_int2 <- lmerTest::lmer(rating ~ geschlecht_vig*mehrstaatigkeit_vig + herkunft_vig + 
                               aufenthaltsdauer_vig + erwerbstätigkeit_vig +
                               sprachkenntnisse_vig*mehrstaatigkeit_vig +
                               (1 | intnr) + (1 | vignette), 
                        data = cawi_long.df)

# With person covariates (but not vignette interactions)
model_resp <- lmerTest::lmer(rating ~ 
                               # Vignette dimensions
                               geschlecht_vig + herkunft_vig + aufenthaltsdauer_vig + 
                               sprachkenntnisse_vig + erwerbstätigkeit_vig +
                               sprachkenntnisse_vig +
                               # Respondent characteristics
                               geschlecht_resp + bildung_resp + bundesland_resp +
                               pol_resp + staatsbürgerschaft_resp +
                               # Interactions
                               alter_resp*mehrstaatigkeit_vig +
                               (1 | intnr) + (1 | vignette),
                             data = cawi_long.df)

model_full <- lmerTest::lmer(rating ~ 
                               # Vignette dimensions
                               geschlecht_vig + herkunft_vig + aufenthaltsdauer_vig + 
                               sprachkenntnisse_vig + 
                               # Respondent characteristics
                               geschlecht_resp + bildung_resp + bundesland_resp +
                               pol_resp + 
                               # Interactions
                               alter_resp*mehrstaatigkeit_vig +
                               staatsbürgerschaft_resp*erwerbstätigkeit_vig +
                               staatsbürgerschaft_resp*sprachkenntnisse_vig +
                               (1 | intnr) + (1 | vignette),
                             data = cawi_long.df)

# Tidy output
model_resp %>%
  broom.mixed::tidy() %>%
  mutate(stars = gtools::stars.pval(p.value))

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
modelplot.fig <- modelsummary::modelplot(model, 
                                         coef_omit = "Intercept|SD",
                                         colour = "#0098D4",
                                         size = .75,
                                         linewidth = 1,
                                         coef_rename = function(x)
                                           str_c("<span style = 'font-size:11pt; font-family:Tahoma;'>", 
                                                 str_replace_all(x, 
                                                                 c("geschlecht_vig" = "",
                                                                   "männlich" = "**Geschlecht:** männlich<br>[Vergleich: weiblich]</span>",
                                                                   "herkunft_vig" = "",
                                                                   "Türkei" = "**Herkunft:** Türkei<br>[Vergleich: Großbritannien]</span>",
                                                                   "Indien" = "Indien</span>",
                                                                   "aufenthaltsdauer_vig" = "",
                                                                   "10 Jahre" = "**Aufenthaltsdauer:** 10 Jahre<br>[Vergleich: 3 Jahre]</span>",
                                                                   "5 Jahre" = "5 Jahre</span>",
                                                                   "erwerbstätigkeit_vig" = "",
                                                                   "berufstätig" = "**Erwerbstätigkeit:** berufstätig<br>[Vergleich: arbeitssuchend]</span>",
                                                                   "sprachkenntnisse_vig" = "",
                                                                   "sehr gut" = "**Sprachkenntnisse:** sehr gut<br>[Vergleich: wenig]</span>",
                                                                   "mehrstaatigkeit_vig" = "",
                                                                   "aufgeben" = "**Bestehende\nStaatsbürgerschaft:** aufgeben<br>[Vergleich: behalten]</span>"
                                                                  )),
                                                 "</span>")) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_label(aes(x = estimate, y = term, label = str_replace(round(estimate, 2), "[.]", ",")), 
             family = "Tahoma", vjust = 1.5) +
  annotate("text",  x = Inf, y = Inf, label = expression(atop(textstyle(paste("Pseudo-R"^2 ," (FE) " == 0.20)), atop(textstyle(paste("Pseudo-R"^2 ," (total) " == 0.52)), atop(scriptscriptstyle(""), textstyle(paste("ICC" == 0.40)))))), 
           vjust = 1, hjust = 1, parse = TRUE, family = "Tahoma")  +
  scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), 
                     limits = c(-1, 1.5),
                     labels = function(x) str_replace(x, "[.]", ",")) +
  labs(#title = "Einbürgerungspräferenzen nach Vignettendimensionen",
       #subtitle = "Ergebnisse eines linearen Mehrebenenmodells",
       x = "", y = "",
       #caption = "Quelle: SVR-CAWI-Umfrage: Einstellung zur Einbürgerung\nDarstellung und Berechnung: SVR\nLesehilfe: Negative Werte bedeuten eine geringere Wahrscheinlichkeit sich Deutschland zugehörig zu fühlen,\nhöhere Werte eine größere Wahrscheinlichkeit. Die Nulllinie ist die Vergleichsgruppe: Dies sind zum Beispiel\nPersonen mit wenig interkulturellen Kontakten und solche mit regelmäßigen Kontakten. Im Mittel haben Per-\nsonen mit mehr interkulturellen Kontakten eine um 8 Prozentpunkte höhere Wahrscheinlichkeit sich Deutsch-\nland zugehörig zu fühlen als Personen mit wenig Kontakten."
       ) +
  theme(text = element_text(family = "Tahoma"),
        plot.title = element_text(face = "bold", family = "Tahoma"),
        legend.position = "bottom", legend.text = element_text(size = 12), 
        legend.justification = "left",
        plot.caption = element_text(hjust = 0, size = 12),
        plot.caption.position = "plot",
        #text = element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"),
        #axis.text = element_text(colour = "black"),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.y = element_line(colour = "black", linewidth = 0.5),
        axis.text = element_text(family = "Tahoma", size = 11, colour = "black"),
        axis.text.y = ggtext::element_markdown(),
        axis.text.x = element_text(family = "Tahoma", size = 11, color = "black"))

# Checks ----
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
  fmt_number(N, drop_trailing_zeros = T) %>%
  fmt_number(Percentage, decimals = 1, scale_by = 100)
    
# Export ----
gtsave(balance_tbl, filename = here("output", "vignette-dimensions.rtf"))
