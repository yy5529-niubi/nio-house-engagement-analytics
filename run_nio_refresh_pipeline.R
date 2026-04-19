# ==========================================
# NIO House Refresh Pipeline
# Author: Yizhou Yang
#
# Purpose:
# This script is designed for future data updates.
# By placing new CSV files into the /data folder,
# users can run this script to automatically
# regenerate updated KPIs, segment profiles,
# and activity performance outputs.
# ==========================================
# ==========================================
# HOW TO USE:
# 1. Replace CSV files in /data folder
# 2. Click "Source"
# 3. Check updated outputs in /outputs
# ==========================================

library(tidyverse)
library(lubridate)

# -------------------------------
# 1. Set folders
# -------------------------------
data_dir <- "data"
output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -------------------------------
# 2. Check required files
# -------------------------------
required_files <- c(
  "nio_house_users.csv",
  "nio_house_visits.csv",
  "nio_house_event_participation.csv",
  "nio_house_transactions.csv",
  "nio_house_events_calendar.csv"
)

missing_files <- required_files[!file.exists(file.path(data_dir, required_files))]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing required files in data folder:",
      paste(missing_files, collapse = ", ")
    )
  )
}

# -------------------------------
# 3. Read data
# -------------------------------
users <- read_csv(file.path(data_dir, "nio_house_users.csv"), show_col_types = FALSE)

visits <- read_csv(file.path(data_dir, "nio_house_visits.csv"), show_col_types = FALSE) %>%
  mutate(
    visit_datetime = ymd_hms(visit_datetime),
    visit_date = as_date(visit_datetime)
  ) %>%
  distinct(user_id, visit_datetime, .keep_all = TRUE)

event_part <- read_csv(file.path(data_dir, "nio_house_event_participation.csv"), show_col_types = FALSE) %>%
  mutate(event_date = ymd(event_date))

txns <- read_csv(file.path(data_dir, "nio_house_transactions.csv"), show_col_types = FALSE) %>%
  mutate(
    txn_datetime = ymd_hms(txn_datetime),
    txn_date = as_date(txn_datetime)
  )

events_calendar <- read_csv(file.path(data_dir, "nio_house_events_calendar.csv"), show_col_types = FALSE) %>%
  mutate(event_date = ymd(event_date))

# -------------------------------
# 4. Basic column validation
# -------------------------------
check_columns <- function(df, required_cols, df_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns in", df_name, ":",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

check_columns(users, c("user_id"), "users")
check_columns(visits, c("user_id", "visit_datetime", "visit_date"), "visits")
check_columns(event_part, c("user_id", "event_id", "event_date", "attended_flag"), "event_part")
check_columns(txns, c("user_id", "txn_datetime", "txn_date", "spend_amount"), "txns")
check_columns(events_calendar, c("event_id", "event_date", "activity_type", "event_theme"), "events_calendar")

# -------------------------------
# 5. Set dynamic analysis window
# -------------------------------
as_of <- max(visits$visit_date, na.rm = TRUE)

win_30  <- as_of - days(30)
win_60  <- as_of - days(60)
win_120 <- as_of - days(120)

# -------------------------------
# 6. Build KPI tables
# -------------------------------
visit_kpis <- visits %>%
  filter(!is.na(visit_date)) %>%
  group_by(user_id) %>%
  summarise(
    visits_last_30d  = sum(visit_date >= win_30, na.rm = TRUE),
    visits_last_60d  = sum(visit_date >= win_60, na.rm = TRUE),
    visits_last_120d = sum(visit_date >= win_120, na.rm = TRUE),
    last_visit_date  = max(visit_date, na.rm = TRUE),
    days_since_last_visit = as.integer(as_of - last_visit_date),
    .groups = "drop"
  ) %>%
  mutate(
    visits_per_month = visits_last_120d / 4
  )

retention_kpis <- visits %>%
  filter(!is.na(visit_date)) %>%
  distinct(user_id, visit_date) %>%
  arrange(user_id, visit_date) %>%
  group_by(user_id) %>%
  mutate(
    next_visit_date = lead(visit_date),
    gap_to_next = as.integer(next_visit_date - visit_date)
  ) %>%
  summarise(
    min_gap_to_next = suppressWarnings(min(gap_to_next, na.rm = TRUE)),
    retention_30d = as.integer(is.finite(min_gap_to_next) & min_gap_to_next <= 30),
    retention_60d = as.integer(is.finite(min_gap_to_next) & min_gap_to_next <= 60),
    .groups = "drop"
  )

event_kpis <- event_part %>%
  filter(attended_flag == 1, !is.na(event_date)) %>%
  group_by(user_id) %>%
  summarise(
    events_attended_last_30d = sum(event_date >= win_30, na.rm = TRUE),
    events_attended_last_60d = sum(event_date >= win_60, na.rm = TRUE),
    .groups = "drop"
  )

txn_kpis <- txns %>%
  filter(!is.na(txn_date)) %>%
  group_by(user_id) %>%
  summarise(
    spend_last_30d = sum(spend_amount[txn_date >= win_30], na.rm = TRUE),
    txn_count_last_30d = sum(txn_date >= win_30, na.rm = TRUE),
    .groups = "drop"
  )

user_kpis <- users %>%
  select(user_id) %>%
  left_join(visit_kpis, by = "user_id") %>%
  left_join(retention_kpis, by = "user_id") %>%
  left_join(event_kpis, by = "user_id") %>%
  left_join(txn_kpis, by = "user_id") %>%
  mutate(
    visits_last_30d  = replace_na(visits_last_30d, 0),
    visits_last_60d  = replace_na(visits_last_60d, 0),
    visits_last_120d = replace_na(visits_last_120d, 0),
    visits_per_month = replace_na(visits_per_month, 0),
    days_since_last_visit = replace_na(days_since_last_visit, as.integer(as_of - win_120) + 1),
    events_attended_last_30d = replace_na(events_attended_last_30d, 0),
    events_attended_last_60d = replace_na(events_attended_last_60d, 0),
    spend_last_30d = replace_na(spend_last_30d, 0),
    txn_count_last_30d = replace_na(txn_count_last_30d, 0),
    retention_30d = replace_na(retention_30d, 0),
    retention_60d = replace_na(retention_60d, 0),
    is_active_120d = as.integer(visits_last_120d > 0)
  )

# -------------------------------
# 7. Segment assignment
# -------------------------------
user_kpis <- user_kpis %>%
  mutate(
    seg_high_value  = visits_per_month >= 4 & spend_last_30d > 200,
    seg_core        = visits_per_month >= 6 & retention_30d == 1,
    seg_event_lover = visits_per_month >= 4 & events_attended_last_30d >= 2,
    seg_new_occ     = visits_per_month < 2 & days_since_last_visit <= 30,
    seg_at_risk     = visits_per_month < 2 & days_since_last_visit > 60
  ) %>%
  mutate(
    segment = case_when(
      is_active_120d == 0 ~ "Inactive / No Recent Visits",
      seg_core ~ "Core Community",
      seg_high_value ~ "High Value Frequent Visitor",
      seg_event_lover ~ "Highly Engaged Event Lover",
      seg_at_risk ~ "At Risk / Dormant",
      seg_new_occ ~ "New or Occasional Visitor",
      TRUE ~ "Regular Visitor"
    )
  )

# -------------------------------
# 8. Sponsor-facing outputs
# -------------------------------
segment_profile_active <- user_kpis %>%
  filter(is_active_120d == 1) %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    avg_visits_per_month = mean(visits_per_month, na.rm = TRUE),
    median_days_since_last = median(days_since_last_visit, na.rm = TRUE),
    avg_events_30d = mean(events_attended_last_30d, na.rm = TRUE),
    avg_spend_30d = mean(spend_last_30d, na.rm = TRUE),
    retention30_rate = mean(retention_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n / sum(n)
  ) %>%
  arrange(desc(n))

activity_scorecard <- event_part %>%
  filter(attended_flag == 1) %>%
  left_join(
    events_calendar %>%
      select(event_id, activity_type, event_theme) %>%
      rename(
        calendar_activity_type = activity_type,
        calendar_event_theme = event_theme
      ),
    by = "event_id"
  ) %>%
  left_join(
    user_kpis %>% select(user_id, segment, visits_per_month, spend_last_30d),
    by = "user_id"
  ) %>%
  group_by(calendar_activity_type) %>%
  summarise(
    n_attendances = n(),
    n_users = n_distinct(user_id),
    avg_user_visits_per_month = mean(visits_per_month, na.rm = TRUE),
    avg_user_spend_30d = mean(spend_last_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users))

summary_table <- tibble(
  metric = c(
    "as_of_date",
    "total_users",
    "active_users_120d",
    "active_user_rate"
  ),
  value = c(
    as.character(as_of),
    nrow(users),
    sum(user_kpis$is_active_120d, na.rm = TRUE),
    mean(user_kpis$is_active_120d, na.rm = TRUE)
  )
)

# -------------------------------
# 9. Export outputs
# -------------------------------
write_csv(user_kpis, file.path(output_dir, "user_kpis_refresh.csv"))
write_csv(segment_profile_active, file.path(output_dir, "segment_profile_active_refresh.csv"))
write_csv(activity_scorecard, file.path(output_dir, "activity_scorecard_refresh.csv"))
write_csv(summary_table, file.path(output_dir, "summary_table_refresh.csv"))

# -------------------------------
# 10. Basic uplift (treated vs control proxy)
# -------------------------------

user_kpis <- user_kpis %>%
  mutate(
    is_treated = as.integer(events_attended_last_30d > 0)
  )

uplift_summary <- user_kpis %>%
  group_by(is_treated) %>%
  summarise(
    n = n(),
    avg_visits = mean(visits_last_30d, na.rm = TRUE),
    avg_spend  = mean(spend_last_30d, na.rm = TRUE),
    retention30 = mean(retention_30d, na.rm = TRUE),
    .groups = "drop"
  )

net_uplift <- uplift_summary %>%
  summarise(
    net_visit_uplift = avg_visits[is_treated == 1] - avg_visits[is_treated == 0],
    net_spend_uplift = avg_spend[is_treated == 1] - avg_spend[is_treated == 0],
    net_retention30  = retention30[is_treated == 1] - retention30[is_treated == 0]
  )

# -------------------------------
# 11. Activity ROI (proxy version)
# -------------------------------

activity_roi <- event_part %>%
  filter(attended_flag == 1) %>%
  left_join(
    events_calendar %>%
      select(event_id, activity_type, estimated_cost) %>%
      rename(calendar_activity_type = activity_type),
    by = "event_id"
  ) %>%
  left_join(
    user_kpis %>%
      select(user_id, spend_last_30d, visits_last_30d),
    by = "user_id"
  ) %>%
  group_by(calendar_activity_type) %>%
  summarise(
    n_users = n_distinct(user_id),
    total_spend = sum(spend_last_30d, na.rm = TRUE),
    total_visits = sum(visits_last_30d, na.rm = TRUE),
    total_cost = sum(estimated_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    spend_per_user = total_spend / n_users,
    visits_per_user = total_visits / n_users,
    roi = ifelse(total_cost > 0, total_spend / total_cost, NA_real_)
  ) %>%
  arrange(desc(roi))

activity_roi <- activity_roi %>%
  rename(
    activity_type = calendar_activity_type,
    proxy_roi = roi
  )

# -------------------------------
# 12. Segment uplift
# -------------------------------

segment_uplift <- user_kpis %>%
  group_by(segment, is_treated) %>%
  summarise(
    avg_visits = mean(visits_last_30d, na.rm = TRUE),
    avg_spend  = mean(spend_last_30d, na.rm = TRUE),
    retention30 = mean(retention_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = is_treated,
    values_from = c(avg_visits, avg_spend, retention30),
    names_prefix = "treated_"
  ) %>%
  mutate(
    net_visit_uplift = avg_visits_treated_1 - avg_visits_treated_0,
    net_spend_uplift = avg_spend_treated_1 - avg_spend_treated_0,
    net_retention30  = retention30_treated_1 - retention30_treated_0
  ) %>%
  arrange(desc(net_visit_uplift))

# -------------------------------
# 13. Executive summary
# -------------------------------

exec_summary <- tibble(
  metric = c(
    "Net visit uplift",
    "Net spend uplift",
    "Net 30d retention uplift",
    "Active user rate"
  ),
  value = c(
    net_uplift$net_visit_uplift,
    net_uplift$net_spend_uplift,
    net_uplift$net_retention30,
    mean(user_kpis$is_active_120d, na.rm = TRUE)
  )
)

write_csv(net_uplift, file.path(output_dir, "net_uplift.csv"))
write_csv(activity_roi, file.path(output_dir, "activity_roi.csv"))
write_csv(segment_uplift, file.path(output_dir, "segment_uplift.csv"))
write_csv(exec_summary, file.path(output_dir, "exec_summary.csv"))


cat("Refresh pipeline completed successfully.\n")
cat("Latest data date:", as.character(as_of), "\n")
cat("Outputs saved in:", output_dir, "\n")

