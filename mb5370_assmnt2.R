#################################################
# PART 2 – Data import, cleaning, wrangling, plot
#################################################

# 1. Load packages (as in workbook)
library(tidyverse)
library(janitor)
library(here)

# 2. Import data
qfish_raw <- read_csv(here("data", "qfish_shark_control.csv"))

# 3. Inspect structure
glimpse(qfish_raw)
names(qfish_raw)
head(qfish_raw)

# 4. Clean column names
qfish <- qfish_raw %>%
  clean_names()

# 5. Rename important variables (adjust if needed)
qfish <- qfish %>%
  rename(
    year = calendar_year,
    species = species_group,
    count = number_caught
  )

# 6. Remove missing years
qfish <- qfish %>%
  filter(!is.na(year))

# 7. Summarise catches by year and species
qfish_summary <- qfish %>%
  group_by(year, species) %>%
  summarise(total_caught = sum(count, na.rm = TRUE), .groups = "drop")

# 8. Identify the top 6 species overall
top_species <- qfish_summary %>%
  group_by(species) %>%
  summarise(total = sum(total_caught, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 6) %>%
  pull(species)

# 9. Filter dataset to only those species
plot_data <- qfish_summary %>%
  filter(species %in% top_species)

# 10. Build ggplot (workbook step-by-step: data → aes → geom → labels → theme)
catch_plot <- ggplot(plot_data, aes(x = year, y = total_caught, colour = species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Catch trends of top 6 species groups",
    x = "Year",
    y = "Number caught",
    colour = "Species group"
  ) +
  theme_minimal()

# 11. Print plot
catch_plot

# 12. Save plot to output folder
ggsave(
  filename = here("output", "figures", "qfish_shark_trends.png"),
  plot = catch_plot,
  width = 10,
  height = 6,
  dpi = 300
)
