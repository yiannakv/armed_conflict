# Recreate the country-year analysis data set from the raw CSV files.
# The script can be run either from the project root or from scripts/.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(janitor)

data_dir <- if (file.exists("data/raw/maternal_mortality.csv")) {
  "data/raw"
} else if (file.exists("../data/raw/maternal_mortality.csv")) {
  "../data/raw"
} else {
  stop("Cannot find data/raw/maternal_mortality.csv. Run from the project root or scripts/.")
}

processed_dir <- if (dir.exists("data/processed")) "data/processed" else "../data/processed"
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

# Convert the mortality files from one row per country to one row per country-year.
wide_to_long_mortality <- function(data, value_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = value_name
    ) |>
    mutate(year = as.integer(year)) |>
    select(iso, year, all_of(value_name))
}

maternal_mortality <- read_csv(
  file.path(data_dir, "maternal_mortality.csv"), show_col_types = FALSE
) |>
  wide_to_long_mortality("maternal_mortality")

neonatal_mortality <- read_csv(
  file.path(data_dir, "neonatal_mortality.csv"), show_col_types = FALSE
) |>
  wide_to_long_mortality("neonatal_mortality")

infant_mortality <- read_csv(
  file.path(data_dir, "infant_mortality.csv"), show_col_types = FALSE
) |>
  wide_to_long_mortality("infant_mortality")

under5_mortality <- read_csv(
  file.path(data_dir, "under5_mortality.csv"), show_col_types = FALSE
) |>
  wide_to_long_mortality("under5_mortality")

# Keep droughts and earthquakes, then reduce potentially multiple disaster
# records to one binary indicator per country-year.
disaster_indicators <- read_csv(
  file.path(data_dir, "disaster.csv"), show_col_types = FALSE
) |>
  clean_names() |>
  filter(
    between(year, 2000L, 2019L),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  transmute(
    iso,
    year = as.integer(year),
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(iso, year) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

# A country has armed conflict in a year if any conflict in that country-year
# has at least 25 battle-related deaths. Lag the exposure one year so conflict
# in year t is matched to outcomes in year t + 1.
armed_conflict_lagged <- read_csv(
  file.path(data_dir, "conflict.csv"), show_col_types = FALSE
) |>
  group_by(conflict_id, iso, year) |>
  summarise(battle_related_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(armed_conflict = as.integer(battle_related_deaths >= 25)) |>
  group_by(iso, year) |>
  summarise(armed_conflict = max(armed_conflict), .groups = "drop") |>
  transmute(iso, year = as.integer(year + 1L), armed_conflict)

covariates <- read_csv(
  file.path(data_dir, "covariates.csv"), show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

# Mortality data define the analysis rows. Missing disaster/conflict records
# denote no event; covariate and mortality missingness is retained as NA.
merged_data <- list(
  maternal_mortality,
  neonatal_mortality,
  infant_mortality,
  under5_mortality
) |>
  reduce(full_join, by = c("iso", "year")) |>
  left_join(disaster_indicators, by = c("iso", "year")) |>
  left_join(armed_conflict_lagged, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  mutate(across(c(earthquake, drought, armed_conflict), ~ coalesce(.x, 0L))) |>
  arrange(iso, year)

stopifnot(!anyDuplicated(merged_data[c("iso", "year")]))

write_csv(
  merged_data,
  file.path(processed_dir, "Codex_Script_MergedData.csv"),
  na = ""
)
