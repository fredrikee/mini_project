# Why is H&M clothes lower quality 
# and how older looms make better clothes

library(tidyverse)

dir.create("figures", showWarnings = FALSE)

# Data ----------------------------------------------------------------------
# warpbreaks: 6 groups (wool A/B x tension L/M/H), 9 looms in each group.
# Give each loom a number (1-9) within its group.
looms <- as_tibble(warpbreaks) |>
  mutate(tension = fct_recode(tension, Low = "L", Medium = "M", High = "H"),
         wool    = fct_recode(wool, "Wool A" = "A", "Wool B" = "B")) |>
  group_by(wool, tension) |>
  mutate(loom = row_number()) |>
  ungroup()

# Mean breaks per group, for the reference line and label in each panel
group_means <- looms |>
  group_by(wool, tension) |>
  summarise(mean_breaks = mean(breaks), .groups = "drop") |>
  mutate(label = str_glue("mean\n{round(mean_breaks, 1)}"))

overall_mean <- mean(looms$breaks)

# Infographic ---------------------------------------------------------------
p_looms <- ggplot(looms, aes(x = factor(loom), y = breaks, fill = wool)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = breaks), vjust = -0.4, size = 2.8, colour = "grey30") +
  geom_hline(data = group_means, aes(yintercept = mean_breaks),
             linetype = "dashed", colour = "grey20") +
  geom_label(data = group_means,
             aes(x = 9.55, y = mean_breaks, label = label),
             inherit.aes = FALSE, hjust = 0, size = 3, lineheight = 0.9,
             linewidth = 0, fill = "white") +
  facet_grid(wool ~ tension) +
  scale_fill_manual(values = c("Wool A" = "#C8553D", "Wool B" = "#2A7F8F"),
                    guide = "none") +
  scale_x_discrete(expand = expansion(add = c(0.6, 2.1))) +  # room for mean label
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Warp breaks on 54 looms: 9 looms per wool and tension",
    subtitle = str_glue(
      "Each bar is one loom. Dashed line = group mean ",
      "(overall mean {round(overall_mean, 1)} breaks).\n",
      "Wool A at low tension breaks far more than any other group; wool B only improves at high tension."
    ),
    x        = "Loom",
    y        = "Number of warp breaks",
    caption  = "Data: warpbreaks (built into R)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(colour = "grey35", margin = margin(b = 10)),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(1, "lines"),
    plot.caption       = element_text(colour = "grey50")
  )

p_looms

ggsave("figures/loom_infographic.png", p_looms, width = 11, height = 6.5, dpi = 150)
