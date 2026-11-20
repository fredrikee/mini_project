# Exploratory analysis of the warpbreaks dataset ---------------------------
#
# warpbreaks (built into R): number of warp breaks per loom, 54 looms.
#   breaks  - number of breaks (count)
#   wool    - type of wool: A or B
#   tension - tension on the yarn: L (low), M (medium), H (high)
# Each wool x tension combination was run on 9 looms.

library(tidyverse)

dir.create("figures", showWarnings = FALSE)

# 1. Load and inspect -------------------------------------------------------
warp <- as_tibble(warpbreaks)

glimpse(warp)

# Check the design: is it balanced?
warp |>
  count(wool, tension)

# Any missing values?
warp |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# 2. Summary statistics -----------------------------------------------------

# Overall
warp |>
  summarise(
    n      = n(),
    mean   = mean(breaks),
    median = median(breaks),
    sd     = sd(breaks),
    min    = min(breaks),
    max    = max(breaks)
  )

# By wool
warp |>
  group_by(wool) |>
  summarise(n = n(), mean = mean(breaks), median = median(breaks),
            sd = sd(breaks), .groups = "drop")

# By tension
warp |>
  group_by(tension) |>
  summarise(n = n(), mean = mean(breaks), median = median(breaks),
            sd = sd(breaks), .groups = "drop")

# By wool and tension (the cell means)
cell_summary <- warp |>
  group_by(wool, tension) |>
  summarise(
    n        = n(),
    mean     = mean(breaks),
    median   = median(breaks),
    sd       = sd(breaks),
    variance = var(breaks),
    se       = sd / sqrt(n),
    .groups  = "drop"
  ) |>
  # breaks are counts: if variance >> mean the data are overdispersed
  mutate(var_to_mean = variance / mean)

cell_summary

# Wide table of cell means, easier to read
cell_summary |>
  select(wool, tension, mean) |>
  pivot_wider(names_from = tension, values_from = mean)

# 3. Plots ------------------------------------------------------------------

# Distribution of breaks
p_hist <- ggplot(warp, aes(x = breaks)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "grey60", colour = "white") +
  labs(title = "Distribution of warp breaks",
       x = "Breaks per loom", y = "Number of looms") +
  theme_minimal()

ggsave("figures/breaks_histogram.png", p_hist, width = 6, height = 4, dpi = 150)

# Breaks by tension and wool
p_box <- ggplot(warp, aes(x = tension, y = breaks, fill = wool)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, seed = 1),
             alpha = 0.7) +
  labs(title = "Warp breaks by tension and wool type",
       x = "Tension", y = "Breaks per loom", fill = "Wool") +
  theme_minimal()

ggsave("figures/breaks_boxplot.png", p_box, width = 6, height = 4, dpi = 150)

# Interaction plot: mean +/- SE for each wool across tension
p_interaction <- ggplot(cell_summary,
                        aes(x = tension, y = mean, colour = wool, group = wool)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(title = "Mean warp breaks (± SE) by tension",
       x = "Tension", y = "Mean breaks per loom", colour = "Wool") +
  theme_minimal()

ggsave("figures/breaks_interaction.png", p_interaction, width = 6, height = 4, dpi = 150)
