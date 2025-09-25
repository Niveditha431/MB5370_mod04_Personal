#################################################
# PART 2 – Data import, cleaning, wrangling, plot
#################################################

# 1. Load packages (as in workbook)
library(tidyverse)
library(janitor)
library(here)

# 2. Import data (skip the first 2 header rows)
qfish_raw <- read_csv(here("data", "qfish_shark_control.csv"), skip = 2)

# 3. Inspect structure
glimpse(qfish_raw)
cat("Raw data dimensions:", dim(qfish_raw), "\n")

# 4. Clean and prepare the pivot table data
qfish_clean <- qfish_raw %>%
  rename(area = Area) %>%  # use the actual column name
  filter(!is.na(area), 
         area != "Area", 
         area != "Grand Total") %>%  # Remove header and total rows
  select(-starts_with("...")) %>%    # drop all those ...4, ...7, etc.
  clean_names()


# 5. Create proper column names with year_species pattern
# The data has years 2001-2025, each with 4 species: Mammal, Other, Shark, Turtle
years <- rep(2001:2025, each = 4)
species <- rep(c("Mammal", "Other", "Shark", "Turtle"), times = 25)
col_names <- c("area", paste(years, species, sep = "_"))

# Apply the column names (only for columns that exist)
names(qfish_clean) <- col_names[1:ncol(qfish_clean)]

cat("Cleaned data dimensions:", dim(qfish_clean), "\n")


# 6. Pivot to long format and filter only sharks
qfish <- qfish_clean %>%
  pivot_longer(cols = -area,
               names_to = "year_species", 
               values_to = "count",
               values_transform = as.numeric,
               values_drop_na = TRUE) %>%
  separate(year_species, into = c("year", "species"), sep = "_") %>%
  mutate(year = as.numeric(year)) %>%
  filter(count > 0, species == "Shark")   # ✅ keep only sharks

cat("Final shark-only data dimensions:", dim(qfish), "\n")

# 7. Summarise catches by year
qfish_summary <- qfish %>%
  group_by(year) %>%
  summarise(total_sharks = sum(count, na.rm = TRUE), .groups = "drop")

# 8. Build shark-only ggplot
shark_plot <- ggplot(qfish_summary, aes(x = year, y = total_sharks)) +
  geom_line(linewidth = 1.2, colour = "darkblue") +
  geom_point(size = 2, colour = "darkblue") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Shark catch trends in Queensland shark control program",
    subtitle = paste("Data from", min(qfish_summary$year), "to", max(qfish_summary$year)),
    x = "Year",
    y = "Number of sharks caught"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# 9. Print the shark plot to screen
print(shark_plot)

# 10. Create output directory if it doesn't exist
if (!dir.exists(here("output", "figures"))) {
  dir.create(here("output", "figures"), recursive = TRUE)
}

# 11. Save shark plot
ggsave(
  filename = here("output", "figures", "qfish_shark_trend.png"),
  plot = shark_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("Shark-only plot saved to:", here("output", "figures", "qfish_shark_trend.png"), "\n")

# 14. Display summary statistics
cat("\nSummary of catch data:\n")
qfish_summary %>%
  group_by(species) %>%
  summarise(
    total_caught = sum(total_caught),
    avg_per_year = round(mean(total_caught), 1),
    min_year = min(year),
    max_year = max(year),
    .groups = "drop"
  ) %>%
  arrange(desc(total_caught)) %>%
  print()