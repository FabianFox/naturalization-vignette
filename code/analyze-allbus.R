# Attitudes toward requirements for naturalization ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes", "gtable", "gridExtra", 
            "conflicted", "Cairo")

## Register fonts ----
extrafont::loadfonts()

# Conflicts
conflicts_prefer(
  dplyr::filter,
  dplyr::select)

## Load data ----
# Cumulative ALLBUS
allbus <- import(here("data", "allbus_1996-2016.rds"))

# Data wrangling ----
allbus_long.df <- allbus %>%
  filter(german != 3) %>% # Exclude persons without German citizenship
  group_by(year) %>%
  nest() %>%
  mutate(data = map_if(
    data, year == 2016, ~.x %>%
      filter(splt16_4 != 2))) %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  select("respid", num_range("mn0", c(1:4, 8)), "ma05", "wghtpew", "year") %>%
  pivot_longer(cols = c(num_range("mn0", c(1:4, 8)), "ma05"), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, 
                           levels = c(str_c("mn0", 1:9), "ma05"), 
                           labels = c("Person should be\nborn in Germany\n(jus soli)",
                                      "...German descent\n   (jus sanguinis)",
                                      "...speak German",
                                      "...lived in Germany\n   for a long time",
                                      "...adapt to German life-\n   style",
                                      "...belongs to Christian\n   church",
                                      "...committed criminal\n   offences",
                                      "...support oneself\n   financially",
                                      "...commit to Basic Law",
                                      "Dual citizenship")))

## Percentages ----
citreq_p.df <- allbus_long.df %>%
  filter(!is.na(value)) %>%
  group_by(year, variable) %>%
  count(value, wt = wghtpew) %>%
  mutate(total = sum(n),
         p = n / total * 100) 

# Plot ----
## Distribution ----
### Base plot ----
citreq.fig <- ggplot(allbus_long.df) +
  geom_bar(aes(x = value, fill = year, weight = wghtpew, group = variable),
           width = 1,
           show.legend = FALSE) +
  scale_x_continuous(breaks = c(1:7), labels = c(1:7), 
                     minor_breaks = 1:7) +
  labs(title = "",
       x = "", 
       y = "Survey wave",
       caption = "Source: ALLBUS 1996, 2006, 2016; weighted") +
  lemon::facet_rep_grid(fct_rev(factor(year)) ~ variable, scales = "free",
                        switch = "y", repeat.tick.labels = TRUE) +
  theme_ipsum(ticks = TRUE, base_family = "Roboto Condensed", base_size = 14, 
              strip_text_size = 14, 
              caption_size = 12, caption_face = "plain", grid = FALSE,
              axis_title_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(angle = 180, size = 14, 
                                    family = "Roboto Condensed"), 
        strip.placement = "outside", 
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(-.2, "cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black", family = "Roboto Condensed"),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0, family = "Roboto Condensed"))

### Add mean lines ----
mean.df <- allbus_long.df %>%
  group_by(year, variable) %>%
  summarise(mean = Hmisc::wtd.mean(value, weights = wghtpew)) %>%
  ungroup() %>%
  mutate(
    fill = case_when(
      year == 2016 ~ "#56B1F7",
      year == 2006 ~ "#336A98",
      year == 1996 ~ "#132B43")) %>%
  group_by(year) %>%
  mutate(group = row_number()) %>%
  ungroup() %>%
  right_join(ggplot_build(citreq.fig)
             %>% pluck("data", 1)) %>%
  group_by(fill, group) %>%
  filter(between(mean, left = xmin, right = xmax)) %>%
  ungroup() %>%
  mutate(colour = if_else(year < 2016, "white", "black"))

# Add mean (dashed) line
citreq.fig <- citreq.fig + 
  geom_segment(aes(x = mean, xend = mean, y = ymin, yend = ymax, colour = colour),
               linewidth = .75,
               data = mean.df) +
  scale_colour_identity()

### Add annotation ----
# Arrow to highlight mean
arrow_mean.df <- mean.df %>%
  filter(year == 2016, variable == "Person should be\nborn in Germany\n(jus soli)") %>%
  mutate(
    x = 4.5,
    xend = mean,
    y = 400, 
    yend = count + 50)

# Annotation to highlight mean (text)
annotate_mean.df <- mean.df %>%
  filter(year == 2016, variable == "Person should be\nborn in Germany\n(jus soli)") %>%
  mutate(x = 5.5, y = 490, label = "Mean")

# Add annotations
citreq.fig <- citreq.fig +
  geom_curve(data = arrow_mean.df, aes(x = x, xend = xend, y = y, yend = yend),
             arrow = arrow(
               length = unit(0.03, "npc"), 
               type = "closed"),
             angle = 90,
             curvature = .3) +
  geom_text(data = annotate_mean.df, aes(x = x, y = y, label = label, 
                                         family = "Roboto Condensed"), 
            size = 5)

## Export ----
# PNG
ggsave(here("figures", "Attitudes-naturalization-reqs_Allbus-EN.tiff"),
       plot = citreq.fig,
       height = 20, width = 34, units = "cm", device = ragg::agg_tiff, dpi = 300,
       bg = "white")

# TIFF
ggsave(here("figures", "Attitudes-naturalization-reqs_Allbus-EN.tiff"),
       plot = citreq.fig,
       height = 20, width = 34, units = "cm", device = ragg::agg_tiff, dpi = 330,
       bg = "white")

# Close device
dev.off()
