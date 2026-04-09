install.packages(c("readxl", "dplyr", "purrr"))
library(readxl)
library(dplyr)
library(purrr)

# Set the path to your folder containing the xlsx files
folder_path <- "SMD houry-March2026-2017"  # <-- update this

# Get all xlsx file paths
xlsx_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read "ISO NE CA" tab from each file and combine
df_combined <- xlsx_files |>
  set_names() |>                          # keeps file name for tracking
  map(function(file) {
    sheets <- excel_sheets(file)
    if ("ISO NE CA" %in% sheets) {
      read_excel(file, sheet = "ISO NE CA") |>
        mutate(source_file = basename(file))  # optional: track origin
    } else {
      message("Skipping (tab not found): ", basename(file))
      NULL
    }
  }) |>
  compact() |>                            # drop NULLs (files missing the tab)
  list_rbind() 


# ISO NE convention for the extra hour during the daylight saving time fallback (the clock "falls back" and hour 2 occurs twice).
df_combined |>
  pull(Hr_End) |>
  unique() |>
  sort()


library(ggplot2)
library(dplyr)
library(lubridate)

# Make sure datetime is parsed correctly
df_combined <- df_combined |>
  mutate(Date = as.POSIXct(Date))

# --- 1. Full time series ---
year_starts <- df_combined |>
  mutate(Date = as.POSIXct(Date)) |>
  group_by(year = year(Date)) |>
  summarise(start = min(Date))

# full time series with year boundary lines
ggplot(df_combined, aes(x = Date, y = RT_Demand)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  geom_vline(data = year_starts, aes(xintercept = as.numeric(start)),
             color = "red", linetype = "dashed", linewidth = 0.6) +
  labs(title = "RT Demand – Full Series", x = NULL, y = "RT Demand") +
  theme_minimal()

# --- 2. Yearly seasonality (avg demand by day-of-year) ---
df_combined |>
  mutate(day_of_year = yday(Date)) |>
  group_by(day_of_year) |>
  summarise(avg_demand = mean(RT_Demand, na.rm = TRUE)) |>
  ggplot(aes(x = day_of_year, y = avg_demand)) +
  geom_line(color = "darkorange", linewidth = 0.7) +
  labs(title = "Yearly Seasonality (Avg by Day of Year)",
       x = "Day of Year", y = "Avg RT Demand") +
  theme_minimal()

# --- 3. Weekly seasonality (avg demand by day-of-week) ---
df_combined |>
  mutate(day_of_week = wday(Date, label = TRUE)) |>
  group_by(day_of_week) |>
  summarise(avg_demand = mean(RT_Demand, na.rm = TRUE)) |>
  ggplot(aes(x = day_of_week, y = avg_demand)) +
  geom_col(fill = "steelblue") +
  labs(title = "Weekly Seasonality (Avg by Day of Week)",
       x = "Day of Week", y = "Avg RT Demand") +
  theme_minimal()

# --- 4. Daily seasonality (avg demand by hour of day) ---
df_combined |>
  mutate(Hr_End = case_when(
    Hr_End == "02X" ~ "2",   # treat as a second hour 2 (duplicate)
    .default = Hr_End
  ),
  Hr_End = as.numeric(Hr_End)) |>
  filter(!is.na(Hr_End)) |>
  group_by(Hr_End) |>
  summarise(avg_demand = mean(RT_Demand, na.rm = TRUE)) |>
  ggplot(aes(x = Hr_End, y = avg_demand)) +
  geom_line(color = "seagreen", linewidth = 0.8) +
  geom_point(color = "seagreen", size = 2) +
  scale_x_continuous(breaks = 1:24) +
  labs(title = "Daily Seasonality (Avg by Hour of Day)",
       x = "Hour Ending", y = "Avg RT Demand") +
  theme_minimal()

# --- 5. Monthly seasonality (avg demand by month) ---
df_combined |>
  mutate(month = month(Date, label = TRUE)) |>
  ggplot(aes(x = month, y = RT_Demand, fill = month)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(title = "Monthly Seasonality – RT Demand Distribution",
       x = "Month", y = "RT Demand") +
  theme_minimal() +
  theme(legend.position = "none")



