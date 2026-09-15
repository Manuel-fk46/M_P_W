library(tidyverse)
library(readxl)
library(lubridate)

# 1. List your files
files <- list.files(path = "your/folder/path", pattern = "*.xlsx", full.names = TRUE)

# 2. Read each file, keep only the 3 columns, rename them
read_one <- function(f) {
  read_excel(f) %>%
    select(date = datetimeUtc, site = location_name, value) %>%
    mutate(
      date = ymd_hms(date, tz = "UTC"),   # parse ISO datetime
      site = str_trim(site)              # remove stray spaces e.g. "Kaneshie Market "
    )
}

combined <- files %>% map_dfr(read_one)

# 3. Round to a common time interval (choose what fits your analysis: hour, 15 min, day)
combined <- combined %>%
  mutate(date_rounded = floor_date(date, unit = "hour"))   # <- change "hour" if you want finer/coarser

# 4. Aggregate to one value per site per time bin (averages duplicates + near-matches)
agg <- combined %>%
  group_by(date_rounded, site) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# 5. Pivot to wide format
wide_data <- agg %>%
  pivot_wider(
    id_cols = date_rounded,
    names_from = site,
    values_from = value
  ) %>%
  rename(date = date_rounded) %>%
  arrange(date)

# 6. Save
write_csv(wide_data, "merged_pm25_wide.csv")