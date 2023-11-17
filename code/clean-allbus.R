# Attitudes to naturalization ----
# Clean data ----

## Setup ----
### Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "conflicted")

### Load data ----
# Cumulative ALLBUS
allbus <- import(here("data", "ZA5274_v1-1-0.sav")) %>%
  tibble()

# Codebook
# Overview of variables
allbus.vars <- allbus %>%
  colnames() %>%
  enframe(name = NULL,
          value = "colname") %>%
  bind_cols(description = get_label(allbus),
            label = get_labels(allbus) %>% map_chr(., ~paste0(., collapse = "; ")))

## Data wrangling ----
### Select waves ----
# Select waves with dependent variables
allbus.df <- allbus %>%
  filter(year %in% c(1996, 2006, 2016)) 

## Export ----
export(allbus.df, here("data", "allbus_1996-2016.rds"))
