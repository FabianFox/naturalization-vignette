# Attitudes toward requirements for naturalization ----
# 
# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "hrbrthemes", "gtable", "gridExtra", 
            "ggridges", "conflicted", "Cairo")

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
allbus.ridges <- allbus %>%
  filter(german != 3) %>% # Exclude persons without German citizenship
  group_by(year) %>%
  nest() %>%
  mutate(data = map_if(
    data, year == 2016, ~.x %>%
      filter(splt16_4 != 2))) %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  select("respid", num_range("mn0", 1:9), "wghtpew", "year") %>%
  pivot_longer(cols = num_range("mn0", 1:9), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, 
                           levels = c(str_c("mn0", 1:9)), 
                           labels = c("Person should be born\nin Germany (jus soli)",
                                      "...German descent\n   (jus sanguinis)",
                                      "...speak German",
                                      "...lived in Germany for\n   a long time",
                                      "...adapt to German life-\n   style",
                                      "...belongs to Christian\n   church",
                                      "...committed criminal\n   offences",
                                      "...support oneself finan-\n   cially",
                                      "...commit to Basic Law")))

## Percentages ----
citreq_p.df <- allbus.ridges %>%
  filter(!is.na(value)) %>%
  group_by(year, variable) %>%
  count(value, wt = wghtpew) %>%
  mutate(total = sum(n),
         p = n / total * 100) 

# Plot ----
## Distribution ----
### Base plot ----
citreq.fig <- ggplot(allbus.ridges) +
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
                        switch = "y") +
  theme_ipsum(ticks = TRUE, base_size = 14, strip_text_size = 12, 
              caption_size = 12, caption_face = "plain", grid = FALSE,
              axis_title_size = 12) +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(angle = 180, size = 14), 
        strip.placement = "outside", 
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(-.5, "cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0))

### Add mean lines ----
mean.ridges <- allbus.ridges %>%
  group_by(year, variable) %>%
  summarise(mean = Hmisc::wtd.mean(value, weights = wghtpew)) %>%
  ungroup() %>%
  mutate(
    fill = case_when(
      year == 2016 ~ "#56B1F7",
      year == 2006 ~ "#336A98",
      year == 1996 ~ "#132B43"),
    group = as.integer(variable)) %>%
  right_join(ggplot_build(citreq.fig)
             %>% pluck("data", 1)) %>%
  group_by(fill, group) %>%
  filter(between(mean, left = xmin, right = xmax)) %>%
  ungroup() %>%
  mutate(colour = if_else(year < 2016, "white", "black"))

# Add mean (dashed) line
citreq.fig <- citreq.fig + 
  geom_segment(aes(x = mean, xend = mean, y = ymin, yend = ymax, colour = colour),  linewidth = .75,
               data = mean.ridges) +
  scale_colour_identity()

### Add annotation ----
# Arrow to highlight mean
arrow_mean.df <- mean.ridges %>%
  filter(year == 2016, variable == "Person should be born\nin Germany (jus soli)") %>%
  mutate(
    x = 4.5,
    xend = mean,
    y = 400, 
    yend = count + 50)

# Annotation to highlight mean (text)
annotate_mean.df <- mean.ridges %>%
  filter(year == 2016, variable == "Person should be born\nin Germany (jus soli)") %>%
  mutate(x = 5.5, y = 490, label = "Mean")

# Add annotations
citreq.fig <- citreq.fig +
  geom_curve(data = arrow_mean.df, aes(x = x, xend = xend, y = y, yend = yend),
             arrow = arrow(
               length = unit(0.03, "npc"), 
               type = "closed"),
             angle = 90,
             curvature = .3) +
  geom_text(data = annotate_mean.df, aes(x = x, y = y, label = label), 
            size = 4)
  
### Insert survey question ----
# See: https://stackoverflow.com/questions/28496764/add-textbox-to-facet-wrapped-layout-in-ggplot2
citreq.fig <- ggplotGrob(citreq.fig)

# Inspect layout
# gtable_show_layout(citreq.fig)

# Remove empty panel
idx <- which(citreq.fig$layout$name %in% c("panel-3-9", "axis-b-9", "xlab-b"));
for (i in idx) citreq.fig$grobs[[i]] <- grid::nullGrob();

# Add text to empty panel
citreq.fig <- gtable_add_grob(x = citreq.fig,
                                            grobs = tableGrob('Question:\n"I will now tell you various things that\nmay play a role in the decision\nabout granting German citizenship."',
                                                              theme = ttheme_minimal(base_family = "Arial Narrow")),
                                            t = 14, 
                                            l = 39, 
                                            b = 14, 
                                            r = 39, 
                                            name = "textbox")

# Plot
# citreq.fig <- grid::grid.draw(citreq.fig)

## Percentage distribution ----
### Base plot ----
citreq_p.fig <- ggplot(citreq_p.df) +
  geom_bar(aes(x = value, y = p, fill = year, group = variable),
           width = 1,
           show.legend = FALSE,
           stat = "identity") +
  scale_x_continuous(breaks = c(1:7), labels = c("1\nNot at all\nimportant", 
                                                 seq(2, 6, 1), 
                                                 "7\nVery\nimportant"), 
                     minor_breaks = 1:7) +
  scale_y_continuous(labels = function(x) str_c(x, "%")) +
  labs(title = "Attitudes toward naturalization requirements",
       x = "", 
       y = "Survey wave",
       caption = "Source: ALLBUS 1996, 2006, 2016; weighted") +
  lemon::facet_rep_grid(fct_rev(factor(year)) ~ variable, 
                        switch = "y") +
  theme_ipsum(ticks = TRUE, base_size = 14, strip_text_size = 12, 
              caption_size = 12, caption_face = "plain", grid = "y",
              axis_title_size = 12) +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(angle = 180, size = 14), 
        strip.placement = "outside", 
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(-1, "cm"),
       # axis.text.y = element_blank(),
       # axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0))

### Add mean lines ----
mean.ridges <- allbus.ridges %>%
  group_by(year, variable) %>%
  summarise(mean = Hmisc::wtd.mean(value, weights = wghtpew)) %>%
  ungroup() %>%
  mutate(
    fill = case_when(
      year == 2016 ~ "#56B1F7",
      year == 2006 ~ "#336A98",
      year == 1996 ~ "#132B43"),
    group = as.integer(variable)) %>%
  right_join(ggplot_build(citreq.fig)
             %>% pluck("data", 1)) %>%
  group_by(fill, group) %>%
  filter(between(mean, left = xmin, right = xmax)) %>%
  ungroup() %>%
  mutate(colour = if_else(year < 2016, "white", "black"))

# Add mean (dashed) line
citreq_p.fig <- citreq_p.fig + 
  geom_segment(aes(x = mean, xend = mean, y = ymin, yend = ymax, colour = colour),  linewidth = .75,
               data = mean.ridges) +
  scale_colour_identity()

### Add annotation ----
# Arrow to highlight mean
arrow_mean.df <- mean.ridges %>%
  filter(year == 2016, variable == "Person should be born\nin Germany (jus soli)") %>%
  mutate(
    x = 4.5,
    xend = mean,
    y = 400, 
    yend = count + 50)

# Annotation to highlight mean (text)
annotate_mean.df <- mean.ridges %>%
  filter(year == 2016, variable == "Person should be born\nin Germany (jus soli)") %>%
  mutate(x = 5, y = 450, label = "Mittelwert")

# Add annotations
citreq_p.fig <- citreq_p.fig +
  geom_curve(data = arrow_mean.df, aes(x = x, xend = xend, y = y, yend = yend),
             arrow = arrow(
               length = unit(0.03, "npc"), 
               type = "closed"),
             angle = 90,
             curvature = .3) +
  geom_text(data = annotate_mean.df, aes(x = x, y = y, label = label), 
            size = 4)

### Insert survey question ----
# See: https://stackoverflow.com/questions/28496764/add-textbox-to-facet-wrapped-layout-in-ggplot2
citreq_p.fig <- ggplotGrob(citreq_p.fig)

# Inspect layout
# gtable_show_layout(citreq.fig)

# Remove empty panel
idx <- which(citreq_p.fig$layout$name %in% c("panel-3-9", "axis-b-9", "xlab-b"));
for (i in idx) citreq_p.fig$grobs[[i]] <- grid::nullGrob();

# Add text to empty panel
citreq_p.fig <- gtable_add_grob(x = citreq_p.fig,
                              grobs = tableGrob('Question:\n"I will now tell you various things that\nmay play a role in the decision\nabout granting German citizenship."',
                                                theme = ttheme_minimal(base_family = "Arial Narrow")),
                              t = 14, 
                              l = 39, 
                              b = 14, 
                              r = 39, 
                              name = "textbox")

# Plot
citreq_p.fig <- grid::grid.draw(citreq_p.fig)

## ggridges ----
### Base density plot ----
citreq.fig <- ggplot(allbus.ridges) + 
  geom_density_ridges(aes(height = after_stat(density), weight = wghtpew, 
                          x = value, 
                          y = factor(year)),
                      alpha = .75, 
                      stat = "density") +
  facet_wrap(~variable) +
  scale_x_continuous(breaks = c(1:7), labels = c("1\nNot at all\nimportant", 
                                                 seq(2, 6, 1), 
                                                 "7\nVery\nimportant"), 
                     minor_breaks = 1:7) 

### Add mean lines ----
mean.ridges <- allbus.ridges %>%
  group_by(year, variable) %>%
  summarise(mean = Hmisc::wtd.mean(value, weights = wghtpew)) %>%
  ungroup() %>%
  mutate(group = as.integer(factor(year)),
         PANEL = factor(as.integer(variable))) %>%
  right_join(ggplot_build(citreq.fig)
             %>% pluck("data", 1)) %>%
  group_by(group, PANEL) %>%
  summarise(mean = first(mean), 
            density = approx(x, density, first(mean))$y, 
            scale = first(scale), 
            iscale = first(iscale)) %>%
  ungroup() %>%
  mutate(year = case_when(group == 1 ~ 1996, 
                          group == 2 ~ 2006, 
                          group == 3 ~ 2016),
         year = factor(year),
         variable = factor(str_c("mn0", PANEL),
                           labels = c("Person should be born in\nGermany (jus soli)",
                                      "...German descent\n   (jus sanguinis)",
                                      "...speak German",
                                      "...lived in Germany for\n   a long time",
                                      "...adapt to German life-\n   style",
                                      "...belongs to Christian\n   church",
                                      "...committed criminal\n   offences",
                                      "...support oneself finan-\n   cially",
                                      "...commit to Basic Law")))

### Add mean lines to base plot ----
citreq.fig <- citreq.fig +
  geom_segment(aes(x = mean, y = year, xend = mean, 
                   yend = as.integer(year) + ifelse(
                     density < 0.04,
                     density + 0.1, density) * scale * iscale),
               data = mean.ridges) +
  labs(title = "Attitudes toward naturalization requirements",
       subtitle = '"I will now tell you various things that may play a role in the decision about granting German citizenship."',
       x = "", 
       y = "Density",
       caption = "Source: ALLBUS 1996, 2006, 2016; weighted") +
  theme_ipsum(ticks = TRUE, base_size = 14, strip_text_size = 14)

## Export ----
ggsave(here("figures", "Attitudes-naturalization-reqs_Allbus-EN.png"),
       plot = citreq.fig,
       height = 20, width = 48, units = "cm", device = ragg::agg_png, dpi = 300)

# Close device
dev.off()
