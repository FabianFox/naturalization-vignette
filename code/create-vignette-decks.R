# Vignette Design ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "here", "conflicted", "Cairo", "AlgDesign")

# Vignette universe
vignette.df <- tribble(
  ~dimension, ~value, ~level,
  "Geschlecht", 1, "weiblich",
  "Geschlecht", 2, "männlich",
  "Herkunft", 1, "Indien",
  "Herkunft", 2, "Türkei",
  "Herkunft", 3, "Großbritannien",
  "Erwerbstätigkeit", 1, "arbeitssuchend", 
  "Erwerbstätigkeit", 2, "berufstätig",
  "Aufenthaltsdauer", 1, "3 Jahre",
  "Aufenthaltsdauer", 2, "5 Jahre",
  "Aufenthaltsdauer", 3, "10 Jahre",
  "Sprachkenntnisse", 1, "wenig",
  "Sprachkenntnisse", 2, "sehr gut",
  "Mehrstaatigkeit", 1, "aufgeben", 
  "Mehrstaatigkeit", 2, "behalten")

# D-efficient design using AlgDesign
v <- gen.factorial(
  levels = c(2, 3, 3, 2, 2, 2),
  nVars = 6, 
  factors = "all",
  varNames = c("Geschlecht", "Herkunft", "Aufenthaltsdauer", "Erwerbstätigkeit", "Sprachkenntnisse", 
               "Mehrstaatigkeit"))

# Find D-efficient subsample and then block
# Optimal design 
design <- optFederov(~.^2, v, crit = "D", nRepeats = 1000, nTrials = 144)
# Check (Ge = D-Efficiency)
design[1:4]
eval.design(~.^2, design = design$design)
# Block optimized design
block.design <- optBlock(frml = ~.^2, withinData = design$design, blocksize = rep(4, 36),
                         criterion = "D", nRepeats = 1000)
# Check
block.design[1:2]
e.block <- eval.blockdesign(~.^2, design = block.design$design, blocksizes = rep(4, 36),
                            confounding = TRUE)

# Confounding matrix
conf.mat <- e.block$confounding
colnames(conf.mat) <- rownames(e.block$confounding)

## Sample size calculation
# 
1000 * 3 / (1 * 4)
# 
144 * 3 / 4

# Orthogonality
block <- data.matrix(block.design$Blocks$B2)
t(block)%*%block

# Create design for survey institute
output.design <- block.design$Blocks %>%
  bind_rows(.id = "Block") %>%
  rowid_to_column(var = "Vignette")

# Replace output.design with factor levels in vignette.df
for (i in 1:nrow(vignette.df)) {
  col_name <- vignette.df$dimension[i]
  original_value <- vignette.df$value[i]
  replacement_value <- vignette.df$level[i]
  
  output.design <- output.design %>%
    mutate(
      !!col_name := ifelse(!!sym(col_name) == original_value, replacement_value, !!sym(col_name))
    )
}

# Add text
output.design <- output.design %>%
  mutate(v_text = case_when(
    Herkunft == "Türkei" ~  str_c(
    "Eine ", Geschlecht, "e Person ",
    "aus der ", Herkunft, 
    ", die seit ", Aufenthaltsdauer, "n in Deutschland lebt,", 
    " die ", Erwerbstätigkeit, " ist,",
    " die ", Sprachkenntnisse, " deutsch spricht,", 
    " die ihre bisherige Staatsbürgerschaft ", Mehrstaatigkeit, " möchte."),
    Herkunft %in% c("Indien", "Großbritannien") ~str_c(
      "Eine ", Geschlecht, "e Person ",
      "aus ", Herkunft, 
      ", die seit ", Aufenthaltsdauer, "n in Deutschland lebt,", 
      " die ", Erwerbstätigkeit, " ist,",
      " die ", Sprachkenntnisse, " deutsch spricht,", 
      " die ihre bisherige Staatsbürgerschaft ", Mehrstaatigkeit, " möchte."),
    TRUE ~ NA_character_))

# Simulate responses
df <- tibble(
  sets = des$Blocks) %>%
  mutate(sets = map(sets, ~rownames_to_column(.x, "vignette")),
         set = 1:8) %>%
  unnest(sets) %>%
  mutate(responses = map(set, ~rLikert(size = 125, items = 1, levels = 7) %>%
                           mutate(id = row_number()))) %>%
  unnest(responses) %>%
  mutate(id = str_c(id, set))
         
# Model
blme::blmer(X ~ Geschlecht + Herkunft + Beruf + Aufenthaltsdauer + Sprachkenntnisse + 
             Mehrstaatigkeit + (1 | set/id), data = df) %>%
  jtools::summ()

# Export
rio::export(output.design, here("data", "questionnaire", "230906_Vignetten.xlsx"))
