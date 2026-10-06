# Mortality data: reshape wide annual values to long format
library(tidyverse)
library(janitor)

# This works whether the script is run from the project root or reports/.
data_dir <- if (file.exists("data/raw/maternal_mortality.csv")) {
  "data/raw"
} else {
  "../data/raw"
}

# 1a. Read the maternal-mortality data and convert it from wide to long form.
maternal_mortality <- read_csv(file.path(data_dir, "maternal_mortality.csv"),
                               show_col_types = FALSE)

maternal_mortality_long_direct <- maternal_mortality |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "maternal_mortality"
  ) |>
  mutate(year = as.numeric(year)) |>
  select(iso, year, maternal_mortality)

# 1b. Reusable function for every mortality CSV with the same wide layout.
wide_to_long_mortality <- function(data, value_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = value_name
    ) |>
    mutate(year = as.numeric(year)) |>
    select(iso, year, all_of(value_name))
}

# Apply the function to each mortality data set.
maternal_mortality_long <- wide_to_long_mortality(
  maternal_mortality, "maternal_mortality"
)

infant_mortality <- read_csv(file.path(data_dir, "infant_mortality.csv"),
                             show_col_types = FALSE)
infant_mortality_long <- wide_to_long_mortality(
  infant_mortality, "infant_mortality"
)

neonatal_mortality <- read_csv(file.path(data_dir, "neonatal_mortality.csv"),
                               show_col_types = FALSE)
neonatal_mortality_long <- wide_to_long_mortality(
  neonatal_mortality, "neonatal_mortality"
)

under5_mortality <- read_csv(file.path(data_dir, "under5_mortality.csv"),
                             show_col_types = FALSE)
under5_mortality_long <- wide_to_long_mortality(
  under5_mortality, "under5_mortality"
)

# Confirm the function reproduces the result from part 1a exactly.
stopifnot(identical(maternal_mortality_long, maternal_mortality_long_direct))

# 2. Disaster data
# Clean the original variable names (e.g., "Disaster Type" becomes
# "disaster_type") before selecting and transforming variables.
disaster <- read_csv(file.path(data_dir, "disaster.csv"), show_col_types = FALSE) |>
  clean_names()

# Retain records from 2000--2019 for droughts and earthquakes only.
disaster_filtered <- disaster |>
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type)

# Create country-year indicators.  max() means a country receives a 1 when it
# has one or more records of that disaster type in a particular year.
disaster_indicators <- disaster_filtered |>
  mutate(
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(year, iso) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  ) |>
  select(year, iso, earthquake, drought)

# 3. Armed-conflict exposure
conflict <- read_csv(file.path(data_dir, "conflict.csv"), show_col_types = FALSE)

# The paper defines armed conflict as >= 25 battle-related deaths in a
# country-conflict-year.  First aggregate event records within each conflict,
# then retain one binary indicator for every country-year.
conflict_by_id_year <- conflict |>
  group_by(conflict_id, iso, year) |>
  summarise(battle_related_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(conflict_present = as.integer(battle_related_deaths >= 25))

conflict_country_year <- conflict_by_id_year |>
  group_by(iso, year) |>
  summarise(armed_conflict = max(conflict_present), .groups = "drop")

# Exposure is lagged one year: conflict in year t is assigned to health outcomes
# in year t + 1.  For example, a 2000 conflict contributes to the 2001 value.
armed_conflict_lagged <- conflict_country_year |>
  transmute(
    iso,
    year = year + 1,
    armed_conflict
  )

# 4. Merge all analysis data by country and year.  The mortality data provide
# the country-year analysis records; a missing disaster or conflict record means
# that the corresponding binary indicator is zero.
merged_data <- list(
  maternal_mortality_long,
  infant_mortality_long,
  neonatal_mortality_long,
  under5_mortality_long
) |>
  reduce(full_join, by = c("iso", "year")) |>
  left_join(disaster_indicators, by = c("iso", "year")) |>
  left_join(armed_conflict_lagged, by = c("iso", "year")) |>
  mutate(
    across(c(earthquake, drought, armed_conflict), ~ replace_na(.x, 0L))
  ) |>
  arrange(iso, year)

# Save the final analytic data set in the processed-data folder.
processed_dir <- if (dir.exists("data/processed")) {
  "data/processed"
} else {
  "../data/processed"
}

write_csv(
  merged_data,
  file.path(processed_dir, "CodexTrial_MergedData.csv"),
  na = ""
)

#OVERVIEW OF CODEX CODING:
#For portion a) the function generated worked as intended to
# format the data frames into long format. The function generated 
# matches the exact function coded myself 

#For portion b) the dataset was formatted exactly as intended with the 
# selected variables. 

#For part c) Again this correctly identified that conflict was 25 events or more 
# something I missed (and have since changed) in my own code. 
# correct formatting of table! However year has decimals 

