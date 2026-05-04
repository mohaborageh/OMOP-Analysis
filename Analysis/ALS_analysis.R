setwd("~/descriptive_anaysis")

library(dplyr)
library(stringr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(RPostgres)
library(DBI)
library(readr)

# Connect to database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "database",
                 host = "localhost",         
                 port = 5432,                
                 user = "postgres",
                 sslmode = "disable")


source("~/etl_pipeline/src/db_functions.R")
source("~/etl_pipeline/src/analysis_definitions.R")

first_als_diag <- tbl(con, "condition_occurrence") %>%
  filter(
    condition_source_value %in% c("DG122E", "DG122F", "DG122G"),
    condition_start_date >= as.Date("2000-01-01"),
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  group_by(person_id) %>%
  summarise(first_als_date = min(condition_start_date)) %>%
  ungroup() %>%
  collect()

exclusion_by_status <- tbl(con, "observation") %>%
  inner_join(first_als_diag, by = "person_id", copy = TRUE) %>%
  filter(
    value_as_number %in% c(20, 30, 50, 60, 70, 80),
    observation_date > first_als_date - years(5),
    observation_date <= first_als_date,
  ) %>%
  select(person_id) %>%
  distinct()

exlusion_by_missing_birthdate <- tbl(con, "person") %>%
  filter(
    is.na(year_of_birth) |
      is.na(month_of_birth) |
      is.na(day_of_birth)
  ) %>%
  select(person_id) %>%
  distinct()


diagnosis_prior_to_als <- tbl(con, "condition_occurrence") %>%
  inner_join(first_als_diag, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date < first_als_date,
    condition_source_value %in% c("DG122E", "DG122F", "DG122G")
  ) %>%
  select(person_id) %>%
  distinct()

qualifying_people <- tbl(con, "person") %>%
  anti_join(exclusion_by_status, by = "person_id") %>%
  anti_join(diagnosis_prior_to_als, by = "person_id") %>%
  anti_join(exlusion_by_missing_birthdate, by = "person_id")                    


first_als_diag <- first_als_diag %>%
  semi_join(qualifying_people, by = "person_id", copy = TRUE) %>%
  collect()


gender_stats <- qualifying_people %>%
  filter(person_id %in% first_als_diag$person_id,
         gender_concept_id %in% c(8507, 8532)) %>%
  group_by(gender_concept_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(
    total = sum(count),
    percent = round(100 * count / total, 2),
    gender = case_when(
      gender_concept_id == 8532 ~ "Female",
      gender_concept_id == 8507 ~ "Male"
    )
  ) %>%
  select(gender, count, percent) %>%
  arrange(gender) %>%
  collect()

age_data <- first_als_diag %>%
  inner_join(tbl(con, "person"), by = "person_id", copy = TRUE) %>%
  select(person_id, first_als_date, year_of_birth, month_of_birth, day_of_birth) %>%
  filter(!is.na(year_of_birth), !is.na(month_of_birth), !is.na(day_of_birth)) %>%
  collect()

age_data <- age_data %>%
  mutate(
    birth_date = make_date(year_of_birth, month_of_birth, day_of_birth),
    age_at_diagnosis = as.numeric(floor(interval(birth_date, first_als_date) / years(1))),
    age_group = case_when(
      age_at_diagnosis >= 18 & age_at_diagnosis <= 34 ~ "18–34",
      age_at_diagnosis >= 35 & age_at_diagnosis <= 44 ~ "35–44",
      age_at_diagnosis >= 45 & age_at_diagnosis <= 54 ~ "45–54",
      age_at_diagnosis >= 55 & age_at_diagnosis <= 64 ~ "55–64",
      age_at_diagnosis >= 65 & age_at_diagnosis <= 74 ~ "65–74",
      age_at_diagnosis >= 75                         ~ "75+",
      TRUE ~ NA_character_
    )
  )

age_group_counts <- age_data %>%
  count(age_group, name = "count")

age_stats <- age_data %>%
  summarise(
    median_age = median(age_at_diagnosis, na.rm = TRUE),
    Q1_age     = quantile(age_at_diagnosis, 0.25, na.rm = TRUE),
    Q3_age     = quantile(age_at_diagnosis, 0.75, na.rm = TRUE)
  )

age_results <- age_group_counts %>%
  bind_rows(
    tibble(
      age_group = c("Median", "Q1", "Q3"),
      count = c(age_stats$median_age, age_stats$Q1_age, age_stats$Q3_age)
    )
  )
write_tsv(age_results, "age_bins_als.txt", quote = "none")

diagnosis_by_period <- first_als_diag %>%
  mutate(
    diagnosis_period = case_when(
      year(first_als_date) >= 2000 & year(first_als_date) <= 2010 ~ "2000–2010",
      year(first_als_date) >= 2011 & year(first_als_date) <= 2021 ~ "2011–2021",
      year(first_als_date) >= 2005 & year(first_als_date) <= 2013 ~ "2005–2013",
      year(first_als_date) >= 2014 & year(first_als_date) <= 2022 ~ "2014–2022",
      TRUE ~ "Other"
    )
  )

period_summary <- diagnosis_by_period %>%
  count(diagnosis_period, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1)) %>% collect()

write_tsv(period_summary, "year_periods_als.txt", quote = "none")

##### Comorbidities #####

preexisting_diag_cardio <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_als_date,
    condition_start_date >= first_als_date - years(5),
    str_detect(condition_source_value, "^DI2[0-5]") |
      str_detect(condition_source_value, "^DI50")     |
      str_detect(condition_source_value, "^DI74")     |
      str_detect(condition_source_value, "^DI6[0-4]") |
      str_detect(condition_source_value, "^DI69")     |
      str_detect(condition_source_value, "^DE78[0-5]"),
    condition_status_concept_id %in% c(32903, 32909)
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()

excluded_people_cardio <- preexisting_diag_cardio %>%
  inner_join(tbl(con, "visit_occurrence"), by = c("visit_occurrence_id", "person_id"), copy = TRUE) %>%
  filter(visit_concept_id == 9203) %>%
  select(person_id) %>%
  distinct()

included_diag_cardio <- preexisting_diag_cardio %>%
  anti_join(excluded_people_cardio, by = "person_id") %>%
  distinct(person_id)

cardio_diag_count <- included_diag_cardio %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_cardio <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_cardio <- tibble(
  n_comorb = cardio_diag_count$n,
  n_total = total_eligible_count_cardio$n_total,
  percentage = round(100 * cardio_diag_count$n / total_eligible_count_cardio$n_total, 1),
  category = "Cardiovascular"
)


resp_diag_all <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_als_date,
    condition_start_date >= first_als_date - years(5),
    str_detect(condition_source_value, "^DJ4[0-7]") |
      str_detect(condition_source_value, "^DJ6[0-7]") |
      str_detect(condition_source_value, "^DJ684")    |
      str_detect(condition_source_value, "^DJ70")     |
      str_detect(condition_source_value, "^DJ84")     |
      str_detect(condition_source_value, "^DJ982")    |
      str_detect(condition_source_value, "^DJ983"),
    condition_status_concept_id %in% c(32903, 32909)
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()

excluded_people_resp <- resp_diag_all %>%
  inner_join(tbl(con, "visit_occurrence"), by = c("visit_occurrence_id", "person_id"), copy = TRUE) %>%
  filter(visit_concept_id == 9203) %>%
  distinct(person_id)

included_diag_resp <- resp_diag_all %>%
  anti_join(excluded_people_resp, by = "person_id") %>%
  distinct(person_id)

included_resp_count <- included_diag_resp %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_resp <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_respiratory <- tibble(
  n_comorb = included_resp_count$n,
  n_total = total_eligible_count_resp$n_total,
  percentage = round(100 * included_resp_count$n / total_eligible_count_resp$n_total, 1),
  category = "Respiratory diseases"
)


dementia_diag_all <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_als_date,
    condition_start_date >= first_als_date - years(5),
    str_detect(condition_source_value, "^DF0[0-3]") |
      str_detect(condition_source_value, "^DG3[0-1]"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()

included_dementia_count <- dementia_diag_all %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()


total_eligible_count_dementia <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_dementia <- tibble(
  n_comorb = included_dementia_count$n,
  n_total = total_eligible_count_dementia$n_total,
  percentage = round(100 * included_dementia_count$n / total_eligible_count_dementia$n_total, 1),
  category = "Dementia"
)


included_psych_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    str_detect(condition_source_value, "^DF2[0-9]") |
      str_detect(condition_source_value, "^DF323")   |
      str_detect(condition_source_value, "^DF333"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_psych_diag_count <- included_psych_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_psych <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_psych <- tibble(
  n_comorb = included_psych_diag_count$n,
  n_total = total_eligible_count_psych$n_total,
  percentage = round(100 * included_psych_diag_count$n / total_eligible_count_psych$n_total, 1),
  category = "Psychotic Disorders"
)

write_tsv(result_psych, "comorb_psych_als.txt", quote = "none")


included_musc_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    str_detect(condition_source_value, "^DR25[0-9]"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_musc_diag_count <- included_musc_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_musc <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_musc <- tibble(
  n_comorb = included_musc_diag_count$n,
  n_total = total_eligible_count_musc$n_total,
  percentage = round(100 * included_musc_diag_count$n / total_eligible_count_musc$n_total, 1),
  category = "Fasiculations, cramps & muscle twitching"
)


included_muscle_atrophy_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    condition_source_value %in% c("DG122A", "DG122B", "DG122C", "DG122D"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_muscle_atrophy_diag_count <- included_muscle_atrophy_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_muscle_atrophy <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_muscle_atrophy <- tibble(
  n_comorb = included_muscle_atrophy_diag_count$n,
  n_total = total_eligible_count_muscle_atrophy$n_total,
  percentage = round(100 * included_muscle_atrophy_diag_count$n / total_eligible_count_muscle_atrophy$n_total, 1),
  category = "Muscle Atrophy"
)


included_disar_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    condition_source_value %in% c("DR471A"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_disar_diag_count <- included_disar_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_disar <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_disar <- tibble(
  n_comorb = included_disar_diag_count$n,
  n_total = total_eligible_count_disar$n_total,
  percentage = round(100 * included_disar_diag_count$n / total_eligible_count_disar$n_total, 1),
  category = "Dysarthris"
)


included_ftd_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    condition_source_value %in% c("DG310"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_ftd_diag_count <- included_ftd_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_ftd <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_ftd <- tibble(
  n_comorb = included_ftd_diag_count$n,
  n_total = total_eligible_count_ftd$n_total,
  percentage = round(100 * included_ftd_diag_count$n / total_eligible_count_ftd$n_total, 1),
  category = "Frontotemporal Dementia"
)


included_mood_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    str_detect(condition_source_value, "^DF3[0-9]"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_mood_diag_count <- included_mood_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_mood <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_mood <- tibble(
  n_comorb = included_mood_diag_count$n,
  n_total = total_eligible_count_mood$n_total,
  percentage = round(100 * included_mood_diag_count$n / total_eligible_count_mood$n_total, 1),
  category = "Mood Disorders"
)


included_other_motor_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    str_detect(condition_source_value, "^DR26[0-9]") |
      str_detect(condition_source_value, "^DR27[0-9]") |
      str_detect(condition_source_value, "^DR29[0-9]"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_other_motor_diag_count <- included_other_motor_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_other_motor <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_other_motor <- tibble(
  n_comorb = included_other_motor_diag_count$n,
  n_total = total_eligible_count_other_motor$n_total,
  percentage = round(100 * included_other_motor_diag_count$n / total_eligible_count_other_motor$n_total, 1),
  category = "Other Motor Impairments"
)


included_anxiety_diag <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    str_detect(condition_source_value, "^DF40[0-9]") |
      str_detect(condition_source_value, "^DF41[0-9]"),
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_anxiety_diag_count <- included_anxiety_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_anxiety <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_anxiety <- tibble(
  n_comorb = included_anxiety_diag_count$n,
  n_total = total_eligible_count_anxiety$n_total,
  percentage = round(100 * included_anxiety_diag_count$n / total_eligible_count_anxiety$n_total, 1),
  category = "Anxiety"
)


included_brisk_reflexes <- first_als_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date - years(5),
    condition_start_date <= first_als_date,
    condition_source_value == "DR292"
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_brisk_reflexes_count <- included_brisk_reflexes %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_brisk_reflexes <- first_als_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_brisk_reflexes <- tibble(
  n_comorb = included_brisk_reflexes_count$n,
  n_total = total_eligible_count_anxiety$n_total,
  percentage = round(100 * included_brisk_reflexes_count$n / total_eligible_count_anxiety$n_total, 1),
  category = "Absent or Pathologically Brisk Reflexes"
)


disease_lookback_5y <- bind_rows(result_cardio, result_respiratory, result_dementia, result_disar, result_ftd, result_mood,
                                 result_anxiety, result_musc, result_other_motor, result_psych, result_brisk_reflexes, result_muscle_atrophy)

write_tsv(disease_lookback_5y, "comorbidities_5y_lookback_als.txt", quote = "none")


##### Drug administration 5y Lookback#####


antidepressants_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N06A%")

antidepressant_with_als <- first_als_diag %>%
  inner_join(antidepressants_5y, by = "person_id", copy = TRUE)

pre_als_antidepressants <- antidepressant_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_antidepressants <- pre_als_antidepressants %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Anti-depressants / Mood disorder medication"
  )

post_als_antidepressants <- antidepressant_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_antidepressants <- post_als_antidepressants %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Anti-depressants / Mood disorder medication"
  )

antidepressants_usage <- full_join(
  pre_als_antidepressants %>% select(category, n_pre, percentage),
  post_als_antidepressants %>% select(category, n_post),
  by = "category"
)


benzodiazepines_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N05B%" |
           drug_source_value %LIKE% "N05C%" )

benzodiazepines_with_als <- first_als_diag %>%
  inner_join(benzodiazepines_5y, by = "person_id", copy = TRUE)

pre_als_benzodiazepines <- benzodiazepines_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_benzodiazepines <- pre_als_benzodiazepines %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Benzodiazepines & Related Drugs"
  )

post_als_benzodiazepines <- benzodiazepines_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_benzodiazepines <- post_als_benzodiazepines %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Benzodiazepines & Related Drugs"
  )

benzodiazepines_usage <- full_join(
  pre_als_benzodiazepines %>% select(category, n_pre, percentage),
  post_als_benzodiazepines %>% select(category, n_post),
  by = "category"
)


antiepileptics_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("N03AX12", "N03AX16", "N03AX14", "NO2BF01", "N02BF02"))

antiepileptics_with_als <- first_als_diag %>%
  inner_join(antiepileptics_5y, by = "person_id", copy = TRUE)

pre_als_antiepileptics <- antiepileptics_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_antiepileptics <- pre_als_antiepileptics %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Antiepileptics"
  )

post_als_antiepileptics <- antiepileptics_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_antiepileptics <- post_als_antiepileptics %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Antiepileptics"
  )

antiepileptics_usage <- full_join(
  pre_als_antiepileptics %>% select(category, n_pre, percentage),
  post_als_antiepileptics %>% select(category, n_post),
  by = "category"
)

smrelaxants_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("M03CA01") |
           drug_source_value %LIKE% "M03B%" )

smrelaxants_with_als <- first_als_diag %>%
  inner_join(smrelaxants_5y, by = "person_id", copy = TRUE)

pre_als_smrelaxants <- smrelaxants_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_smrelaxants <- pre_als_smrelaxants %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Skeletal Muscle Relaxants"
  )

post_als_smrelaxants <- smrelaxants_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_smrelaxants <- post_als_smrelaxants %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Skeletal Muscle Relaxants"
  )

smrelaxants_usage <- full_join(
  pre_als_smrelaxants %>% select(category, n_pre, percentage),
  post_als_smrelaxants %>% select(category, n_post),
  by = "category"
)

antibiotics_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "J01%" )

antibiotics_with_als <- first_als_diag %>%
  inner_join(antibiotics_5y, by = "person_id", copy = TRUE)

pre_als_antibiotics <- antibiotics_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_antibiotics <- pre_als_antibiotics %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Antibiotics"
  )

post_als_antibiotics <- antibiotics_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_antibiotics <- post_als_antibiotics %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Antibiotics"
  )

antibiotics_usage <- full_join(
  pre_als_antibiotics %>% select(category, n_pre, percentage),
  post_als_antibiotics %>% select(category, n_post),
  by = "category"
)

triamlido_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "D07XB02%")

triamlido_with_als <- first_als_diag %>%
  inner_join(triamlido_5y, by = "person_id", copy = TRUE)

pre_als_triamlido <- triamlido_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_triamlido <- pre_als_triamlido %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Triamcinolone +- lidocaine"
  )

post_als_triamlido <- triamlido_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_triamlido <- post_als_triamlido %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Triamcinolone +- lidocaine"
  )

triamlido_usage <- full_join(
  pre_als_triamlido %>% select(category, n_pre, percentage),
  post_als_triamlido %>% select(category, n_post),
  by = "category"
)

hsb_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "A03BB%")

hsb_with_als <- first_als_diag %>%
  inner_join(hsb_5y, by = "person_id", copy = TRUE)

pre_als_hsb <- hsb_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_hsb <- pre_als_hsb %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Hyoscine, Scopolamine, Buscopan"
  )

post_als_hsb <- hsb_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_hsb <- post_als_hsb %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Hyoscine, Scopolamine, Buscopan"
  )

hsb_usage <- full_join(
  pre_als_hsb %>% select(category, n_pre, percentage),
  post_als_hsb %>% select(category, n_post),
  by = "category"
)

cannabis_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N03AX24%")

cannabis_with_als <- first_als_diag %>%
  inner_join(cannabis_5y, by = "person_id", copy = TRUE)

pre_als_cannabis <- cannabis_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_cannabis <- pre_als_cannabis %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Cannabis"
  )

post_als_cannabis <- cannabis_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_cannabis <- post_als_cannabis %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Cannabis"
  )

cannabis_usage <- full_join(
  pre_als_cannabis %>% select(category, n_pre, percentage),
  post_als_cannabis %>% select(category, n_post),
  by = "category"
)

riluzole_5y <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("N07XX02"))

riluzole_with_als <- first_als_diag %>%
  inner_join(riluzole_5y, by = "person_id", copy = TRUE)

pre_als_riluzole <- riluzole_with_als %>%
  filter(
    drug_exposure_start_date <= first_als_date,
    drug_exposure_start_date >= first_als_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre = n()) %>%
  collect()

total_als <- first_als_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_als_riluzole <- pre_als_riluzole %>%
  mutate(
    percentage = round(100 * n_pre / total_als$n_total, 2),
    category = "Riluzole"
  )

post_als_riluzole <- riluzole_with_als %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post = n()) %>%
  collect()

post_als_riluzole <- post_als_riluzole %>%
  mutate(
    percentage = round(100 * n_post / total_als$n_total, 2),
    category = "Riluzole"
  )

riluzole_usage <- full_join(
  pre_als_riluzole %>% select(category, n_pre, percentage),
  post_als_riluzole %>% select(category, n_post),
  by = "category"
)

drug_lookback_5y <- bind_rows(antidepressants_usage, benzodiazepines_usage, antiepileptics_usage, smrelaxants_usage, antibiotics_usage,
                              triamlido_usage, hsb_usage, cannabis_usage, riluzole_usage)

write_tsv(drug_lookback_5y, "drug_use_5y_lookback_als.txt", quote = "none")

riluzole <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("N07XX02"))

riluzole_post365 <- first_als_diag %>%
  inner_join(riluzole, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

riluzole_summary <- riluzole_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Riluzole"
  )

qsulphate <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("M09AA01", "P01BC01"))

qsulphate_post365 <- first_als_diag %>%
  inner_join(qsulphate, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

qsulphate_summary <- qsulphate_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Quinine Sulphate"
  )

antiepileptics <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("N03AX12", "N03AX16", "N03AX14", "NO2BF01", "N02BF02"))

antiepileptics_post365 <- first_als_diag %>%
  inner_join(antiepileptics, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

antiepileptics_summary <- antiepileptics_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Antiepileptics"
  )


smrelaxants <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% c("M03CA01") |
           drug_source_value %LIKE% "M03B%" )

smrelaxants_post365 <- first_als_diag %>%
  inner_join(smrelaxants, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

smrelaxants_summary <- smrelaxants_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Skeletal muscle relaxants"
  )


antibiotics <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "J01%" )

antibiotics_post365 <- first_als_diag %>%
  inner_join(antibiotics, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

antibiotics_summary <- antibiotics_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Antibiotics"
  )


benzodiazepines <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N05B%" |
           drug_source_value %LIKE% "N05C%" )

benzodiazepines_post365 <- first_als_diag %>%
  inner_join(benzodiazepines, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

benzodiazepines_summary <- benzodiazepines_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Benzodiazepines"
  )


hsb <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "A03BB%")

hsb_post365 <- first_als_diag %>%
  inner_join(hsb, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

hsb_summary <- hsb_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Hyoscine, Scopolamine, Buscopan"
  )


trilido <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "D07XB02%")

trilido_post365 <- first_als_diag %>%
  inner_join(trilido, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

trilido_summary <- trilido_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Triamcinolone +- lidocaine"
  )


antidepress <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N06A%")

antidepress_post365 <- first_als_diag %>%
  inner_join(antidepress, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

antidepress_summary <- antidepress_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Antidepressants/mood disorder medication"
  )


cannabis <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "N03AX24%")

cannabis_post365 <- first_als_diag %>%
  inner_join(cannabis, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_als_date,
    drug_exposure_start_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

cannabis_summary <- cannabis_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Cannabis"
  )


icu_admission_1y <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("NABE", "NABB", "BGDA0"))

icu_admission_1y_post_als <- first_als_diag %>%
  inner_join(icu_admission_1y, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_als_date,
    procedure_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

icu_admission_1y_summary <- icu_admission_1y_post_als %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "ICU Admission - 1 year follow-up"
  )


non_inv_vent_1y <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("BGDA1"))

non_inv_vent_1y_post_als <- first_als_diag %>%
  inner_join(non_inv_vent_1y, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_als_date,
    procedure_date <= first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

non_inv_vent_1y_summary <- non_inv_vent_1y_post_als %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "Non-invasive Ventilation - 1 year follow-up"
  )


summary_signals_of_treatment = bind_rows(riluzole_summary, qsulphate_summary, antiepileptics_summary, smrelaxants_summary,  antibiotics_summary, benzodiazepines_summary,
                                         hsb_summary, trilido_summary, antidepress_summary, cannabis_summary, icu_admission_1y_summary, non_inv_vent_1y_summary)

write_tsv(summary_signals_of_treatment, "signals_of_treatment_summary_als.txt", quote = "none")


resp_infec <- tbl(con, "condition_occurrence") %>%
  filter(str_detect(condition_source_value, "^DJ12")     |
          str_detect(condition_source_value, "^DJ13")     |
          str_detect(condition_source_value, "^DJ14")     |
          str_detect(condition_source_value, "^DJ15")     |
          str_detect(condition_source_value, "^DJ16")     |
          str_detect(condition_source_value, "^DJ17")     |
          str_detect(condition_source_value, "^DJ18")     )

resp_infec_post_als <- first_als_diag %>%
  inner_join(resp_infec, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date > first_als_date,
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_als = n()) %>%
  collect()

resp_infec_summary <- resp_infec_post_als %>%
  mutate(
    percentage = round(100 * n_post_als / total_als$n_total, 2),
    Signal = "Respiratory Infection"
  )


acute_resp_infec <- tbl(con, "condition_occurrence") %>%
  filter(condition_source_value == "DJ960")

acute_resp_infec_post_als <- first_als_diag %>%
  inner_join(acute_resp_infec, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date > first_als_date,
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_als = n()) %>%
  collect()

acute_resp_infec_summary <- acute_resp_infec_post_als %>%
  mutate(
    percentage = round(100 * n_post_als / total_als$n_total, 2),
    Signal = "Acute Respiratory Failure"
  )


icu_admission <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("NABE", "NABB", "BGDA0"))

icu_admission_post_als <- first_als_diag %>%
  inner_join(icu_admission, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_als_date,
    procedure_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_als = n()) %>%
  collect()

icu_admission_summary <- icu_admission_post_als %>%
  mutate(
    percentage = round(100 * n_post_als / total_als$n_total, 2),
    Signal = "ICU Admission"
  )

peg_con_codes <- c("DZ931")
peg_proc_codes <- c("KJBD10")


peg_proc <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% peg_proc_codes)

peg_con <- tbl(con, "condition_occurrence") %>%
  filter(condition_source_value %in% peg_con_codes)

peg_proc_post365 <- first_als_diag %>%
  inner_join(peg_proc, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date >= first_als_date,
    procedure_date < first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

peg_con_post365 <- first_als_diag %>%
  inner_join(peg_con, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_als_date,
    condition_start_date < first_als_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

peg_summary <- bind_rows(peg_proc_post365, peg_con_post365) %>%
  summarise(across(everything(),  ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentage = round(100 * n_post365 / total_als$n_total, 2),
    treatment = "PEGs"
  )

cahexia <- tbl(con, "condition_occurrence") %>%
  filter(str_detect(condition_source_value, "^DR64"))

cahexia_post_als <- first_als_diag %>%
  inner_join(cahexia, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date > first_als_date,
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_als = n()) %>%
  collect()

cahexia_summary <- cahexia_post_als %>%
  mutate(
    percentage = round(100 * n_post_als / total_als$n_total, 2),
    Signal = "Cahexia"
  )


non_inv_vent <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("BGDA1"))

non_inv_vent_post_als <- first_als_diag %>%
  inner_join(non_inv_vent, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_als_date,
    procedure_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_als = n()) %>%
  collect()

non_inv_vent_summary <- non_inv_vent_post_als %>%
  mutate(
    percentage = round(100 * n_post_als / total_als$n_total, 2),
    Signal = "Non-invasive Ventilation"
  )

summary_disease_progression = bind_rows(resp_infec_summary, acute_resp_infec_summary, icu_admission_summary, peg_summary, cahexia_summary, non_inv_vent_summary)

write_tsv(summary_disease_progression, "signals_of_disease_progression_als.txt", quote = "none")


als_mortality <- tbl(con, "death") %>%
  filter(cause_source_value %LIKE% "G122%") %>%
  inner_join(first_als_diag, by = "person_id", copy = TRUE) %>%
  summarise(n = n(), metric = "ALS mortality") %>%
  select(metric, n) %>%
  collect()

all_cause_mortality <- tbl(con, "death") %>%
  filter(!cause_concept_id == -1) %>%
  inner_join(first_als_diag, by = "person_id", copy = TRUE) %>%
  summarise(n = n(), metric = "All cause mortality") %>%
  select(metric, n) %>%
  collect()

mortality <- rbind(all_cause_mortality, als_mortality)

write_tsv(mortality, "mortality_counts_als.txt", quote = "none")
