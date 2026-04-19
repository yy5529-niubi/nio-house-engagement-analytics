# ==========================================
# NIO House Project -- Full Analysis
# Author: Yizhou Yang
#
# Purpose:
# This script performs the full analytical workflow,
# including KPI construction, user segmentation,
# causal analysis (DID), and robustness checks.
#
# This version is used for:
# - Final report
# - Methodology validation
# - Detailed analytical outputs
# ==========================================

library(tidyverse)
library(lubridate)

data_dir <- "data"
output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

users <- read_csv(file.path(data_dir, "nio_house_users.csv"), show_col_types = FALSE)

visits <- read_csv(file.path(data_dir, "nio_house_visits.csv"), show_col_types = FALSE) %>%
  mutate(
    visit_datetime = ymd_hms(visit_datetime),
    visit_date = as_date(visit_datetime)
  )

visits <- visits %>%
  distinct(user_id, visit_datetime, .keep_all = TRUE)

event_part <- read_csv(file.path(data_dir, "nio_house_event_participation.csv"), show_col_types = FALSE) %>%
  mutate(event_date = ymd(event_date))

txns <- read_csv(file.path(data_dir, "nio_house_transactions.csv"), show_col_types = FALSE) %>%
  mutate(
    txn_datetime = ymd_hms(txn_datetime),
    txn_date = as_date(txn_datetime)
  )

glimpse(users)

glimpse(visits)

glimpse(event_part)

glimpse(txns)

as_of <- max(visits$visit_date, na.rm = TRUE)
as_of

win_30  <- as_of - days(30)
win_60  <- as_of - days(60)
win_120 <- as_of - days(120)

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
  mutate(next_visit_date = lead(visit_date),
         gap_to_next = as.integer(next_visit_date - visit_date)) %>%
  summarise(
    min_gap_to_next = suppressWarnings(min(gap_to_next, na.rm = TRUE)),
    retention_30d = as.integer(is.finite(min_gap_to_next) & min_gap_to_next <= 30),
    retention_60d = as.integer(is.finite(min_gap_to_next) & min_gap_to_next <= 60),
    .groups = "drop"
  )

table(retention_kpis$retention_30d, useNA = "ifany")

mean(retention_kpis$retention_30d, na.rm = TRUE)

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

summary(user_kpis$visits_last_120d)
quantile(user_kpis$visits_last_120d,
         probs = c(.5, .75, .9, .95),
         na.rm = TRUE)

stopifnot(nrow(users) == dplyr::n_distinct(users$user_id))
stopifnot(nrow(user_kpis) == dplyr::n_distinct(user_kpis$user_id))

colSums(is.na(user_kpis))

glimpse(user_kpis)

sum(user_kpis$is_active_120d)

mean(user_kpis$is_active_120d)

summary(user_kpis$visits_per_month)

summary(user_kpis$days_since_last_visit)

summary(user_kpis$events_attended_last_30d)

summary(user_kpis$spend_last_30d)

user_kpis <- user_kpis %>%
  mutate(
    seg_high_value = visits_per_month >= 4 & spend_last_30d > 200,
    seg_core       = visits_per_month >= 6 & retention_30d == 1,
    seg_event_lover = visits_per_month >= 4 & events_attended_last_30d >= 2,
    seg_new_occ     = visits_per_month < 2 & days_since_last_visit <= 30,
    seg_at_risk     = visits_per_month < 2 & days_since_last_visit > 60
  ) %>%
  mutate(
    segment = case_when(
      is_active_120d == 0 ~ "Inactive / No Recent Visits",
      seg_core       ~ "Core Community",
      seg_high_value ~ "High Value Frequent Visitor",
      seg_event_lover ~ "Highly Engaged Event Lover",
      seg_at_risk    ~ "At Risk / Dormant",
      seg_new_occ    ~ "New or Occasional Visitor",
      TRUE           ~ "Regular Visitor"
    )
  )

user_kpis <- user_kpis %>%
  mutate(
    n_rules = rowSums(
      cbind(
        replace_na(seg_high_value, FALSE),
        replace_na(seg_core, FALSE),
        replace_na(seg_event_lover, FALSE),
        replace_na(seg_new_occ, FALSE),
        replace_na(seg_at_risk, FALSE)
      )
    )
  )
count(user_kpis, n_rules)

user_kpis %>%
  filter(is.na(visits_per_month) | is.na(days_since_last_visit))

table(user_kpis$segment)

prop.table(table(user_kpis$segment))

active_kpis <- user_kpis %>%
  filter(is_active_120d == 1)

mean(!is.na(active_kpis$segment))

dplyr::n_distinct(active_kpis$segment)

active_segment_dist <- active_kpis %>%
  count(segment) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(n))

active_segment_dist

segment_profile <- user_kpis %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    pct = n()/nrow(user_kpis),
    avg_visits_per_month = mean(visits_per_month, na.rm = TRUE),
    median_days_since_last = median(days_since_last_visit, na.rm = TRUE),
    avg_events_30d = mean(events_attended_last_30d, na.rm = TRUE),
    avg_spend_30d = mean(spend_last_30d, na.rm = TRUE),
    retention30_rate = mean(retention_30d, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

segment_profile

segment_profile_active <- active_kpis %>%
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

segment_profile_active




### Objective 2 ###
events_calendar <- read_csv(file.path(data_dir, "nio_house_events_calendar.csv"), show_col_types = FALSE) %>%
  mutate(event_date = ymd(event_date))

first_event <- event_part %>%
  filter(attended_flag == 1) %>%
  left_join(
    events_calendar %>% 
      select(event_id, event_date, activity_type, event_theme),
    by = "event_id",
    suffix = c("", "_cal")
  ) %>%
  filter(!is.na(event_date_cal)) %>%
  arrange(user_id, event_date_cal) %>%
  group_by(user_id) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    user_id,
    anchor_date = event_date_cal,
    anchor_event_id = event_id,
    anchor_activity_type = activity_type_cal,
    anchor_theme = event_theme
  )

glimpse(first_event)

set.seed(42)

all_users <- users %>% distinct(user_id)

treated_users <- first_event %>% distinct(user_id) %>% mutate(is_treated = 1)


control_users <- all_users %>%
  anti_join(treated_users, by="user_id") %>%
  left_join(user_kpis %>% select(user_id, segment), by="user_id") %>%
  mutate(segment = replace_na(segment, "Unknown"),
         is_treated = 0)

event_date_pool <- first_event %>%
  distinct(anchor_date) %>%
  pull(anchor_date)

control_anchor_base <- control_users %>%
  group_by(segment) %>%
  mutate(anchor_date = sample(event_date_pool, size = n(), replace = TRUE)) %>%
  ungroup() %>%
  select(user_id, anchor_date)

control_anchor <- control_anchor_base %>%
  left_join(
    events_calendar %>%
      filter(!is.na(event_date)) %>%
      select(event_date, event_id, activity_type, event_theme),
    by = c("anchor_date" = "event_date")
  ) %>%
  filter(!is.na(event_id), !is.na(activity_type), !is.na(event_theme)) %>%  
  group_by(user_id, anchor_date) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  transmute(
    user_id,
    anchor_date,
    anchor_event_id = event_id,
    anchor_activity_type = activity_type,
    anchor_theme = event_theme
  )

control_anchor <- control_anchor %>%
  filter(!is.na(anchor_event_id), !is.na(anchor_activity_type), !is.na(anchor_theme))

target_n <- nrow(control_users)
attempt <- 1

while(nrow(control_anchor) < target_n && attempt <= 20){
  attempt <- attempt + 1
  
  control_anchor_base <- control_users %>%
    group_by(segment) %>%
    mutate(anchor_date = sample(event_date_pool, size = n(), replace = TRUE)) %>%
    ungroup() %>%
    select(user_id, anchor_date)
  
  control_anchor <- control_anchor_base %>%
    left_join(
      events_calendar %>%
        filter(!is.na(event_date)) %>%
        select(event_date, event_id, activity_type, event_theme),
      by = c("anchor_date" = "event_date")
    ) %>%
    filter(!is.na(event_id), !is.na(activity_type), !is.na(event_theme)) %>%
    group_by(user_id, anchor_date) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    transmute(
      user_id,
      anchor_date,
      anchor_event_id = event_id,
      anchor_activity_type = activity_type,
      anchor_theme = event_theme
    )
}

nrow(control_users); nrow(control_anchor)

anchor_table <- bind_rows(
  first_event %>% mutate(is_treated = 1),
  control_anchor %>% mutate(is_treated = 0)
)

glimpse(anchor_table)
table(anchor_table$is_treated)

visit_win <- visits %>%
  filter(!is.na(visit_date)) %>%
  inner_join(
    anchor_table %>% 
      select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
    by = "user_id"
  ) %>%
  mutate(
    diff_days = as.integer(visit_date - anchor_date),
    period = case_when(
      diff_days >= -30 & diff_days < 0  ~ "before",
      diff_days >= 0  & diff_days <= 30 ~ "after",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

visits_kpi <- visit_win %>%
  group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
  summarise(visits = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = period, values_from = visits, values_fill = 0) %>%
  mutate(visit_uplift = after - before)

visits_kpi <- visits_kpi %>%
  mutate(
    visit_uplift_pct = ifelse(before == 0, NA_real_, (after - before) / before)
  )

txn_win <- txns %>%
  filter(!is.na(txn_date)) %>%
  inner_join(
    anchor_table %>% 
      select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
    by = "user_id"
  ) %>%
  mutate(
    diff_days = as.integer(txn_date - anchor_date),
    period = case_when(
      diff_days >= -30 & diff_days < 0  ~ "before",
      diff_days >= 0  & diff_days <= 30 ~ "after",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

txn_kpi <- txn_win %>%
  group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
  summarise(
    spend = sum(spend_amount, na.rm = TRUE),
    txn_count = n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = c(spend, txn_count),
    values_fill = 0
  ) %>%
  mutate(
    spend_uplift = spend_after - spend_before,
    txn_uplift   = txn_count_after - txn_count_before
  )

txn_kpi <- txn_kpi %>%
  mutate(
    spend_uplift_pct = ifelse(spend_before == 0, NA_real_, (spend_after - spend_before) / spend_before)
  )

return_30 <- visits %>%
  filter(!is.na(visit_date)) %>%
  inner_join(
    anchor_table %>% 
      select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
    by = "user_id"
  ) %>%
  mutate(diff_days = as.integer(visit_date - anchor_date)) %>%
  group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme) %>%
  summarise(
    return_30d = as.integer(any(diff_days > 0 & diff_days <= 30)),
    .groups = "drop"
  )

obj2 <- visits_kpi %>%
  left_join(txn_kpi, by = c("user_id","anchor_event_id","is_treated","anchor_activity_type","anchor_theme")) %>%
  left_join(return_30, by = c("user_id","anchor_event_id","is_treated","anchor_activity_type","anchor_theme")) %>%
  left_join(user_kpis %>% select(user_id, segment, is_active_120d), by = "user_id") %>%
  mutate(segment = replace_na(segment, "Unknown"))

glimpse(obj2)

obj2 <- obj2 %>%
  mutate(
    spend_before = replace_na(spend_before, 0),
    spend_after  = replace_na(spend_after, 0),
    txn_count_before = replace_na(txn_count_before, 0),
    txn_count_after  = replace_na(txn_count_after, 0),
    return_30d = replace_na(return_30d, 0),
    spend_uplift = spend_after - spend_before,
    txn_uplift   = txn_count_after - txn_count_before
  )

obj2_main <- obj2 %>%
  filter(is_active_120d == 1)

baseline_check <- obj2_main %>%
  group_by(is_treated) %>%
  summarise(
    avg_before = mean(before, na.rm = TRUE),
    avg_after  = mean(after, na.rm = TRUE),
    .groups = "drop"
  )

baseline_check

did_overall <- obj2_main %>%
  group_by(is_treated) %>%
  summarise(
    n = n(),
    avg_visit_uplift  = mean(visit_uplift, na.rm = TRUE),
    avg_spend_uplift  = mean(spend_uplift, na.rm = TRUE),
    avg_txn_uplift    = mean(txn_uplift, na.rm = TRUE),
    return_30d_rate   = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  )

did_overall

did_net <- did_overall %>%
  summarise(
    net_visit_uplift = avg_visit_uplift[is_treated == 1] - avg_visit_uplift[is_treated == 0],
    net_spend_uplift = avg_spend_uplift[is_treated == 1] - avg_spend_uplift[is_treated == 0],
    net_txn_uplift   = avg_txn_uplift[is_treated == 1] - avg_txn_uplift[is_treated == 0],
    net_return_30d   = return_30d_rate[is_treated == 1] - return_30d_rate[is_treated == 0]
  )

did_net

placebo <- visits %>%
  filter(!is.na(visit_date)) %>%
  inner_join(anchor_table %>% select(user_id, anchor_date, is_treated), by="user_id") %>%
  mutate(
    diff_days = as.integer(visit_date - anchor_date),
    period = case_when(
      diff_days >= -60 & diff_days < -30 ~ "before",
      diff_days >= -30 & diff_days < 0   ~ "after",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  count(user_id, is_treated, period) %>%
  tidyr::pivot_wider(names_from=period, values_from=n, values_fill=0) %>%
  mutate(uplift_placebo = after - before) %>%
  left_join(user_kpis %>% select(user_id, is_active_120d), by="user_id") %>%
  filter(is_active_120d == 1)

placebo_did <- placebo %>%
  group_by(is_treated) %>%
  summarise(avg_uplift_placebo = mean(uplift_placebo), .groups="drop") %>%
  summarise(net_placebo = avg_uplift_placebo[is_treated==1] - avg_uplift_placebo[is_treated==0])

placebo_did

t.test(uplift_placebo ~ is_treated, data = placebo)
wilcox.test(uplift_placebo ~ is_treated, data = placebo)

placebo_ratio <- placebo_did$net_placebo / did_net$net_visit_uplift
placebo_ratio

pretrend <- visits %>%
  filter(!is.na(visit_date)) %>%
  inner_join(anchor_table %>% select(user_id, anchor_date, is_treated), by="user_id") %>%
  mutate(
    diff_days = as.integer(visit_date - anchor_date),
    period = case_when(
      diff_days >= -90 & diff_days < -60 ~ "w1",
      diff_days >= -60 & diff_days < -30 ~ "w2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  count(user_id, is_treated, period) %>%
  tidyr::pivot_wider(names_from=period, values_from=n, values_fill=0) %>%
  mutate(uplift_pretrend = w2 - w1) %>%
  left_join(user_kpis %>% select(user_id, is_active_120d), by="user_id") %>%
  filter(is_active_120d == 1)

pretrend_did <- pretrend %>%
  group_by(is_treated) %>%
  summarise(avg_uplift_pretrend = mean(uplift_pretrend), .groups="drop") %>%
  summarise(net_pretrend = avg_uplift_pretrend[is_treated==1] - avg_uplift_pretrend[is_treated==0])

pretrend_did
t.test(uplift_pretrend ~ is_treated, data = pretrend)

run_obj2_window <- function(win = 30){
  
  visit_win2 <- visits %>%
    filter(!is.na(visit_date)) %>%
    inner_join(anchor_table %>% select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
               by = "user_id") %>%
    mutate(
      diff_days = as.integer(visit_date - anchor_date),
      period = case_when(
        diff_days >= -win & diff_days < 0  ~ "before",
        diff_days >= 0    & diff_days <= win ~ "after",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))
  
  visits_kpi2 <- visit_win2 %>%
    group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
    summarise(visits = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = visits, values_fill = 0) %>%
    mutate(visit_uplift = after - before)
  
  txn_win2 <- txns %>%
    filter(!is.na(txn_date)) %>%
    inner_join(anchor_table %>% select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
               by = "user_id") %>%
    mutate(
      diff_days = as.integer(txn_date - anchor_date),
      period = case_when(
        diff_days >= -win & diff_days < 0  ~ "before",
        diff_days >= 0    & diff_days <= win ~ "after",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))
  
  txn_kpi2 <- txn_win2 %>%
    group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
    summarise(spend = sum(spend_amount, na.rm = TRUE), txn_count = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = c(spend, txn_count), values_fill = 0) %>%
    mutate(spend_uplift = spend_after - spend_before,
           txn_uplift   = txn_count_after - txn_count_before)
  
  return_30_2 <- visits %>%
    filter(!is.na(visit_date)) %>%
    inner_join(anchor_table %>% select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
               by = "user_id") %>%
    mutate(diff_days = as.integer(visit_date - anchor_date)) %>%
    group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme) %>%
    summarise(return_rate = as.integer(any(diff_days > 0 & diff_days <= win)), .groups = "drop")
  
  obj2_2 <- visits_kpi2 %>%
    left_join(txn_kpi2, by = c("user_id","anchor_event_id","is_treated","anchor_activity_type","anchor_theme")) %>%
    left_join(return_30_2, by = c("user_id","anchor_event_id","is_treated","anchor_activity_type","anchor_theme")) %>%
    left_join(user_kpis %>% select(user_id, is_active_120d), by = "user_id") %>%
    mutate(
      spend_before = replace_na(spend_before, 0),
      spend_after  = replace_na(spend_after, 0),
      txn_count_before = replace_na(txn_count_before, 0),
      txn_count_after  = replace_na(txn_count_after, 0),
      return_rate = replace_na(return_rate, 0),
      spend_uplift = spend_after - spend_before,
      txn_uplift   = txn_count_after - txn_count_before
    ) %>%
    filter(is_active_120d == 1)
  
  did <- obj2_2 %>%
    group_by(is_treated) %>%
    summarise(
      avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
      avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
      return_rate = mean(return_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    summarise(
      win = win,
      net_visit_uplift = avg_visit_uplift[is_treated==1] - avg_visit_uplift[is_treated==0],
      net_spend_uplift = avg_spend_uplift[is_treated==1] - avg_spend_uplift[is_treated==0],
      net_return_rate  = return_rate[is_treated==1] - return_rate[is_treated==0]
    )
  
  did
}

sensitivity_windows <- bind_rows(
  run_obj2_window(14),
  run_obj2_window(30),
  run_obj2_window(60)
)

sensitivity_windows

t_test_visit  <- t.test(visit_uplift ~ is_treated, data = obj2_main)
t_test_spend  <- t.test(spend_uplift ~ is_treated, data = obj2_main)
t_test_txn    <- t.test(txn_uplift ~ is_treated, data = obj2_main)

t_test_visit
t_test_spend
t_test_txn

wilcox.test(visit_uplift ~ is_treated, data = obj2_main)
wilcox.test(spend_uplift ~ is_treated, data = obj2_main)
wilcox.test(txn_uplift ~ is_treated, data = obj2_main)


run_with_seed <- function(seed = 1){
  
  set.seed(seed)
  
  control_anchor_base2 <- control_users %>%
    group_by(segment) %>%
    mutate(anchor_date = sample(event_date_pool, size = n(), replace = TRUE)) %>%
    ungroup() %>%
    select(user_id, anchor_date)
  
  control_anchor2 <- control_anchor_base2 %>%
    left_join(
      events_calendar %>%
        filter(!is.na(event_date)) %>%
        select(event_date, event_id, activity_type, event_theme),
      by = c("anchor_date" = "event_date")
    ) %>%
    group_by(user_id, anchor_date) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    transmute(
      user_id,
      anchor_date,
      anchor_event_id = event_id,
      anchor_activity_type = activity_type,
      anchor_theme = event_theme
    ) %>%
    filter(!is.na(anchor_event_id), !is.na(anchor_activity_type), !is.na(anchor_theme))
  
  anchor_table2 <- bind_rows(
    first_event %>% mutate(is_treated = 1),
    control_anchor2 %>% mutate(is_treated = 0)
  )
  
  visit_win2 <- visits %>%
    filter(!is.na(visit_date)) %>%
    inner_join(
      anchor_table2 %>% select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
      by = "user_id"
    ) %>%
    mutate(
      diff_days = as.integer(visit_date - anchor_date),
      period = case_when(
        diff_days >= -30 & diff_days < 0  ~ "before",
        diff_days >= 0  & diff_days <= 30 ~ "after",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period)) %>%
    count(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
    tidyr::pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
    mutate(visit_uplift = after - before)
  
  txn_win2 <- txns %>%
    filter(!is.na(txn_date)) %>%
    inner_join(
      anchor_table2 %>% select(user_id, anchor_date, anchor_event_id, is_treated, anchor_activity_type, anchor_theme),
      by = "user_id"
    ) %>%
    mutate(
      diff_days = as.integer(txn_date - anchor_date),
      period = case_when(
        diff_days >= -30 & diff_days < 0  ~ "before",
        diff_days >= 0  & diff_days <= 30 ~ "after",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period)) %>%
    group_by(user_id, anchor_event_id, is_treated, anchor_activity_type, anchor_theme, period) %>%
    summarise(spend = sum(spend_amount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = spend, values_fill = 0) %>%
    mutate(spend_uplift = after - before)
  
  obj2_seed <- visit_win2 %>%
    left_join(txn_win2, by = c("user_id","anchor_event_id","is_treated","anchor_activity_type","anchor_theme")) %>%
    left_join(user_kpis %>% select(user_id, is_active_120d), by = "user_id") %>%
    mutate(spend_uplift = replace_na(spend_uplift, 0)) %>%
    filter(is_active_120d == 1)
  
  did_seed <- obj2_seed %>%
    group_by(is_treated) %>%
    summarise(
      avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
      avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
      .groups = "drop"
    )
  
  tibble(
    seed = seed,
    net_visit_uplift = did_seed$avg_visit_uplift[did_seed$is_treated == 1] - did_seed$avg_visit_uplift[did_seed$is_treated == 0],
    net_spend_uplift = did_seed$avg_spend_uplift[did_seed$is_treated == 1] - did_seed$avg_spend_uplift[did_seed$is_treated == 0]
  )
}

mc_seed_results <- bind_rows(lapply(1:30, run_with_seed))

mc_seed_results
summary(mc_seed_results$net_spend_uplift)
summary(mc_seed_results$net_visit_uplift)

mc_summary <- mc_seed_results %>%
  summarise(
    n_runs = n(),
    visit_mean = mean(net_visit_uplift),
    visit_p05  = quantile(net_visit_uplift, 0.05),
    visit_p95  = quantile(net_visit_uplift, 0.95),
    spend_mean = mean(net_spend_uplift),
    spend_p05  = quantile(net_spend_uplift, 0.05),
    spend_p95  = quantile(net_spend_uplift, 0.95)
  )

mc_summary

impact_by_type <- obj2_main %>%
  filter(is_treated == 1) %>%
  group_by(anchor_activity_type) %>%
  summarise(
    n_users = n(),
    avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
    avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
    return_30d_rate  = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users))

impact_by_type

event_level <- obj2_main %>%
  filter(is_treated == 1) %>%
  left_join(
    events_calendar %>% select(event_id, estimated_cost, activity_type, event_theme),
    by = c("anchor_event_id" = "event_id")
  ) %>%
  group_by(anchor_event_id, anchor_activity_type, anchor_theme) %>%
  summarise(
    event_cost = first(estimated_cost),
    n_users = n(),
    total_spend_uplift = sum(spend_uplift, na.rm = TRUE),
    total_visit_uplift = sum(visit_uplift, na.rm = TRUE),
    avg_return_30d = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  )

event_level %>% summarise(
  n_events = n(),
  missing_cost = sum(is.na(event_cost))
)

event_level_costed <- event_level %>%
  filter(!is.na(event_cost) & event_cost > 0)

roi_by_type <- event_level_costed %>%
  group_by(anchor_activity_type) %>%
  summarise(
    n_events = n(),
    n_users = sum(n_users),
    total_estimated_cost = sum(event_cost, na.rm = TRUE),
    total_spend_uplift = sum(total_spend_uplift, na.rm = TRUE),
    total_visit_uplift = sum(total_visit_uplift, na.rm = TRUE),
    avg_return_30d = mean(avg_return_30d, na.rm = TRUE),
    uplift_per_dollar = total_spend_uplift / total_estimated_cost,
    .groups = "drop"
  ) %>%
  arrange(desc(uplift_per_dollar))

roi_by_type

deliverable_roi_by_type <- roi_by_type
deliverable_roi_by_type

n_distinct(obj2_main %>% filter(is_treated == 1) %>% pull(anchor_activity_type))

impact_by_theme <- obj2_main %>%
  filter(is_treated == 1) %>%
  group_by(anchor_theme) %>%
  summarise(
    n_users = n(),
    avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
    avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
    return_30d_rate  = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users))

impact_by_theme

impact_seg_type <- obj2_main %>%
  filter(is_treated == 1) %>% 
  group_by(segment, anchor_activity_type) %>%
  summarise(
    n_users = n(),
    avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
    avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
    return_30d_rate  = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users))

impact_seg_type

obj2_main %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  ggplot(aes(x = visit_uplift, fill = group)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Visit uplift distribution: Treated vs Control",
       x = "Visit uplift (after - before)",
       y = "Count")

obj2_main %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  ggplot(aes(x = group, y = spend_uplift)) +
  geom_boxplot() +
  labs(title = "Spend uplift: Treated vs Control",
       x = "",
       y = "Spend uplift (after - before)")

deliverable_overall <- did_overall %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  select(group, n, avg_visit_uplift, avg_spend_uplift, avg_txn_uplift, return_30d_rate)

deliverable_net <- did_net

deliverable_activity_rank <- impact_by_type %>%
  arrange(desc(avg_visit_uplift))

deliverable_theme_rank <- impact_by_theme %>%
  arrange(desc(avg_visit_uplift))

deliverable_overall
deliverable_net
deliverable_activity_rank
deliverable_theme_rank

adjusted_net_visit_uplift <- did_net$net_visit_uplift - placebo_did$net_placebo

deliverable_exec <- tibble(
  Metric = c(
    "Net visit uplift",
    "Adjusted net visit uplift (net - placebo)",
    "Net spend uplift ($)",
    "Net txn uplift",
    "Net 30d return uplift",
    "Pre-trend (visits)",
    "Placebo uplift (visits)",
    "Placebo ratio"
  ),
  Value = c(
    did_net$net_visit_uplift,
    adjusted_net_visit_uplift,
    did_net$net_spend_uplift,
    did_net$net_txn_uplift,
    did_net$net_return_30d,
    pretrend_did$net_pretrend,
    placebo_did$net_placebo,
    placebo_ratio
  )
)

deliverable_exec


segment_did_summary <- obj2_main %>%
  group_by(segment, is_treated) %>%
  summarise(
    n_users = n(),
    avg_visit_uplift = mean(visit_uplift, na.rm = TRUE),
    avg_spend_uplift = mean(spend_uplift, na.rm = TRUE),
    avg_txn_uplift = mean(txn_uplift, na.rm = TRUE),
    return_30d_rate = mean(return_30d, na.rm = TRUE),
    .groups = "drop"
  )

segment_did_summary

segment_net_uplift <- segment_did_summary %>%
  select(segment, is_treated, avg_visit_uplift, avg_spend_uplift, avg_txn_uplift, return_30d_rate) %>%
  pivot_wider(
    names_from = is_treated,
    values_from = c(avg_visit_uplift, avg_spend_uplift, avg_txn_uplift, return_30d_rate),
    names_prefix = "treated_"
  ) %>%
  mutate(
    net_visit_uplift = avg_visit_uplift_treated_1 - avg_visit_uplift_treated_0,
    net_spend_uplift = avg_spend_uplift_treated_1 - avg_spend_uplift_treated_0,
    net_txn_uplift   = avg_txn_uplift_treated_1 - avg_txn_uplift_treated_0,
    net_return_30d   = return_30d_rate_treated_1 - return_30d_rate_treated_0
  ) %>%
  arrange(desc(net_visit_uplift))

segment_net_uplift

deliverable_segment_net <- segment_net_uplift %>%
  select(
    segment,
    net_visit_uplift,
    net_spend_uplift,
    net_txn_uplift,
    net_return_30d
  ) %>%
  arrange(desc(net_visit_uplift))

deliverable_segment_net

deliverable_segment_net %>%
  ggplot(aes(x = reorder(segment, net_visit_uplift), y = net_visit_uplift)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Net visit uplift by segment",
    x = "Segment",
    y = "Net visit uplift (treated - control)"
  )

deliverable_segment_net %>%
  ggplot(aes(x = reorder(segment, net_spend_uplift), y = net_spend_uplift)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Net spend uplift by segment",
    x = "Segment",
    y = "Net spend uplift (treated - control)"
  )


did_visit_model <- lm(
  visit_uplift ~ is_treated + segment,
  data = obj2_main
)

summary(did_visit_model)

did_spend_model <- lm(
  spend_uplift ~ is_treated + segment,
  data = obj2_main
)

summary(did_spend_model)

did_txn_model <- lm(
  txn_uplift ~ is_treated + segment,
  data = obj2_main
)

summary(did_txn_model)


next_visit_after_anchor <- visits %>%
  filter(!is.na(visit_date)) %>%
  inner_join(
    anchor_table %>% select(user_id, anchor_date, is_treated),
    by = "user_id"
  ) %>%
  mutate(diff_days = as.integer(visit_date - anchor_date)) %>%
  filter(diff_days > 0) %>%
  group_by(user_id, is_treated) %>%
  summarise(
    days_to_return = min(diff_days),
    .groups = "drop"
  )

summary(next_visit_after_anchor$days_to_return)

return_summary <- next_visit_after_anchor %>%
  group_by(is_treated) %>%
  summarise(
    n_users = n(),
    mean_days = mean(days_to_return),
    median_days = median(days_to_return),
    .groups = "drop"
  )

return_summary

next_visit_after_anchor %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  ggplot(aes(x = days_to_return, fill = group)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Time to next visit after event participation",
    x = "Days until next visit",
    y = "Density"
  )


baseline_balance <- obj2_main %>%
  group_by(is_treated) %>%
  summarise(
    n_users = n(),
    avg_before_visits = mean(before, na.rm = TRUE),
    median_before_visits = median(before, na.rm = TRUE),
    avg_before_spend = mean(spend_before, na.rm = TRUE),
    median_before_spend = median(spend_before, na.rm = TRUE),
    avg_before_txn = mean(txn_count_before, na.rm = TRUE),
    median_before_txn = median(txn_count_before, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  select(group, everything(), -is_treated)

baseline_balance

baseline_segment_mix <- obj2_main %>%
  count(is_treated, segment) %>%
  group_by(is_treated) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(group = ifelse(is_treated == 1, "Treated", "Control")) %>%
  select(group, segment, n, pct)

baseline_segment_mix

deliverable_baseline_mix <- baseline_segment_mix %>%
  select(group, segment, pct) %>%
  pivot_wider(
    names_from = group,
    values_from = pct
  ) %>%
  mutate(diff = Treated - Control) %>%
  arrange(desc(Treated))

deliverable_baseline_mix

final_activity_scorecard <- impact_by_type %>%
  left_join(
    roi_by_type %>%
      select(anchor_activity_type, n_events, total_estimated_cost, uplift_per_dollar),
    by = "anchor_activity_type"
  ) %>%
  rename(
    activity_type = anchor_activity_type
  ) %>%
  arrange(desc(avg_visit_uplift))

final_activity_scorecard

deliverable_activity_scorecard <- final_activity_scorecard %>%
  select(
    activity_type,
    n_users,
    avg_visit_uplift,
    avg_spend_uplift,
    return_30d_rate,
    n_events,
    total_estimated_cost,
    uplift_per_dollar
  ) %>%
  arrange(desc(uplift_per_dollar))

deliverable_activity_scorecard

deliverable_activity_scorecard %>%
  ggplot(aes(x = reorder(activity_type, uplift_per_dollar), y = uplift_per_dollar)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Activity ROI by type",
    x = "Activity type",
    y = "Spend uplift per dollar"
  )

deliverable_activity_scorecard %>%
  ggplot(aes(x = reorder(activity_type, avg_visit_uplift), y = avg_visit_uplift)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average visit uplift by activity type",
    x = "Activity type",
    y = "Average visit uplift"
  )

write_csv(segment_profile_active, file.path(output_dir, "segment_profile_active.csv"))
write_csv(deliverable_net, file.path(output_dir, "deliverable_net.csv"))
write_csv(deliverable_segment_net, file.path(output_dir, "deliverable_segment_net.csv"))
write_csv(deliverable_activity_scorecard, file.path(output_dir, "deliverable_activity_scorecard.csv"))
write_csv(return_summary, file.path(output_dir, "return_summary.csv"))
write_csv(deliverable_baseline_mix, file.path(output_dir, "deliverable_baseline_mix.csv"))
write_csv(deliverable_exec, file.path(output_dir, "deliverable_exec.csv"))
write_csv(mc_summary, file.path(output_dir, "mc_summary.csv"))
write_csv(sensitivity_windows, file.path(output_dir, "sensitivity_windows.csv"))




cat("Full analysis completed successfully\n")

