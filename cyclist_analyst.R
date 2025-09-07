options(scipen = 999)

library(tidyverse)
library(lubridate)
library(scales)

path <- "C:/Users/Admin/Desktop/working.csv"

if (dir.exists(path)) {
  files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("!include csv: ", path)
  df <- purrr::map_dfr(files, readr::read_csv, show_col_types = FALSE)
} else {
  df <- readr::read_csv(path, show_col_types = FALSE)
}

req_cols <- c("ride_id","rideable_type","started_at","ended_at",
              "start_station_name","start_station_id",
              "end_station_name","end_station_id","member_casual")
missing <- setdiff(req_cols, names(df))
if (length(missing) > 0) {
  warning("missing value: ", paste(missing, collapse = ", "))
}

df <- df %>%
  mutate(
    started_at  = ymd_hms(started_at, quiet = TRUE, tz = "UTC"),
    ended_at    = ymd_hms(ended_at,   quiet = TRUE, tz = "UTC"),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )

df_clean <- df %>%
  filter(!(end_station_name %in% c("Base - 2132 W Hubbard Warehouse"))) %>%
  filter(!is.na(start_station_name) & start_station_name != "") %>%
  filter(!is.na(start_station_id)   & start_station_id   != "") %>%
  filter(!is.na(end_station_name)   & end_station_name   != "") %>%
  filter(!is.na(end_station_id)     & end_station_id     != "") %>%
  filter(!is.na(ride_length)) %>%
  filter(ride_length >= 0, ride_length <= 180)   

df_clean <- df_clean %>%
  mutate(
    start_hour   = hour(started_at),
    day_of_week  = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1),
    month_of_day = month(started_at, label = TRUE, abbr = TRUE),
    time_of_day  = case_when(
      between(start_hour,  4,  9) ~ "Morning",
      between(start_hour, 10, 15) ~ "Noon",
      between(start_hour, 16, 21) ~ "Evening",
      TRUE                        ~ "Dusk"
    ),
    rideable_type = factor(rideable_type,
                           levels = c("classic_bike","docked_bike","electric_bike"),
                           labels = c("Clas","Dock","Elec")),
    time_of_day = factor(time_of_day,
                         levels = c("Morning","Noon","Evening","Dusk"),
                         labels = c("Morn","After","Even","Dusk"))
  )

basic_data <- df_clean %>%
  filter(rideable_type != "Dock") %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    users       = dplyr::n(),
    mean_val    = mean(ride_length),
    median_ride = median(ride_length),
    qua_25      = quantile(ride_length, 0.25),
    qua_75      = quantile(ride_length, 0.75),
    sd_val      = sd(ride_length),
    left_95     = mean_val - qt(0.975, df = users - 1) * sd_val / sqrt(users),
    right_95    = mean_val + qt(0.975, df = users - 1) * sd_val / sqrt(users),
    .groups = "drop"
  ) %>%
  mutate(
    mean_ride = round(mean_val, 2),
    st_de     = round(sd_val, 2),
    left_95   = round(left_95, 2),
    right_95  = round(right_95, 2)
  ) %>%
  select(member_casual, rideable_type, users, mean_ride, median_ride,
         qua_25, qua_75, st_de, left_95, right_95)

agg_data <- df_clean %>%
  filter(rideable_type != "Dock") %>%
  group_by(month_of_day, day_of_week, time_of_day, member_casual, rideable_type) %>%
  summarise(
    mean_ride   = round(mean(ride_length), 1),
    median_ride = median(ride_length),
    users       = dplyr::n(),
    .groups     = "drop"
  )

df %>% select(started_at, ended_at) %>% head()
str(df_clean$started_at)
names(df_clean)[names(df_clean) %in% c("start_hour","day_of_week","month_of_day","time_of_day")]


p_users_per_month <-
  df_clean %>%
  mutate(month_lab = month(started_at, label = TRUE, abbr = TRUE)) %>%
  count(month_lab, member_casual, name = "users") %>%
  ggplot(aes(month_lab, users, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(title = "# of Rides per Month", x = "Month", y = "Rides")

