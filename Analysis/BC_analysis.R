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

car_provider_id <- add_or_get_provider("car", con)


first_c50_diag <- tbl(con, "condition_occurrence") %>%
  filter(
    provider_id == car_provider_id,
    condition_source_value %LIKE% "C50%",
    condition_start_date >= as.Date("2000-01-01"),
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  group_by(person_id) %>%
  summarise(first_c50_date = min(condition_start_date)) %>%
  ungroup() %>%
  collect()

exclusion_by_status <- tbl(con, "observation") %>%
  inner_join(first_c50_diag, by = "person_id", copy = TRUE ) %>%
  filter(
    value_as_number %in% c(20, 30, 50, 60, 70, 80),
    observation_date >= first_c50_date - years(5),
    observation_date < first_c50_date,
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


diagnosis_prior_to_c50 <- tbl(con, "condition_occurrence") %>%
  inner_join(first_c50_diag, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_c50_date - years(5),
    condition_start_date < first_c50_date,
    (
      (condition_source_value %LIKE% "C%" & !(condition_source_value %LIKE% "C44%")) |
        condition_source_value %LIKE% "D0%" & !(
          condition_source_value %LIKE% "D04%" |
            condition_source_value %LIKE% "D06%"))
  ) %>%
  select(person_id) %>%
  distinct()

qualifying_people <- tbl(con, "person") %>%
  anti_join(exclusion_by_status, by = "person_id") %>%
  anti_join(diagnosis_prior_to_c50, by = "person_id") %>%
  anti_join(exlusion_by_missing_birthdate, by = "person_id") %>%
  collect()


first_c50_diag <- first_c50_diag %>%
  semi_join(qualifying_people, by = "person_id") %>%
  collect()


gender_stats <- qualifying_people %>%
  filter(person_id %in% first_c50_diag$person_id,
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

write_tsv(gender_stats, "gender_stats_F.txt", quote = "none")



age_data <- first_c50_diag %>%
  inner_join(tbl(con, "person"), by = "person_id", copy = TRUE) %>%
  select(person_id, first_c50_date, year_of_birth, month_of_birth, day_of_birth) %>%
  filter(!is.na(year_of_birth), !is.na(month_of_birth), !is.na(day_of_birth)) %>%
  collect()

age_data <- age_data %>%
  mutate(
    birth_date = make_date(year_of_birth, month_of_birth, day_of_birth),
    age_at_diagnosis = as.numeric(floor(interval(birth_date, first_c50_date) / years(1))),
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
write_tsv(age_results, "age_bins_F.txt", quote = "none")


##### Diagnosis by period in years #####

diagnosis_by_period <- first_c50_diag %>%
  mutate(
    diagnosis_period = case_when(
      year(first_c50_date) >= 2000 & year(first_c50_date) <= 2010 ~ "2000–2010",
      year(first_c50_date) >= 2011 & year(first_c50_date) <= 2021 ~ "2011–2021",
      year(first_c50_date) >= 2005 & year(first_c50_date) <= 2013 ~ "2005–2013",
      year(first_c50_date) >= 2014 & year(first_c50_date) <= 2022 ~ "2014–2022",
      TRUE ~ "Other"
    )
  )

period_summary <- diagnosis_by_period %>%
  count(diagnosis_period, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1)) %>% collect()

write_tsv(period_summary, "year_periods_F.txt", quote = "none")


##### Counting Pregnancies #####


pregnancy_diagnoses <- tbl(con, "condition_occurrence") %>%
  filter(
    (condition_source_value %LIKE% "DO80%" |
       condition_source_value %LIKE% "DO81%" |
       condition_source_value %LIKE% "DO82%" |
       condition_source_value %LIKE% "DO83%" |
       condition_source_value %LIKE% "DO84%" |
       condition_source_value %LIKE% "DO03%" |
       condition_source_value %LIKE% "DO04%" |
       condition_source_value %LIKE% "DO05%" |
       condition_source_value %LIKE% "DO06%")
  ) %>%
  select(person_id, condition_start_date, condition_source_value) %>% 
  collect()

pregnancy_within_window <- first_c50_diag %>%
  inner_join(pregnancy_diagnoses, by = "person_id", copy = TRUE) %>%
  filter(
    (
      str_detect(condition_source_value, "^DO8") & 
        first_c50_date >= condition_start_date - days(280) & 
        first_c50_date <= condition_start_date
    ) |
      (
        str_detect(condition_source_value,"^DO0") & 
          first_c50_date >= condition_start_date - days(84) & 
          first_c50_date <= condition_start_date
      )
  ) %>%
  distinct(person_id)

pregnancy_count <- pregnancy_within_window %>%
  summarise(n = n_distinct(person_id)) %>%
  collect()

write_tsv(pregnancy_count, "pregnancy_count_F.txt", quote = "none")

##### Parity #####

parity_first_after_c50 <- first_c50_diag %>%
  inner_join(tbl(con, "measurement"), by = "person_id", copy = TRUE) %>%
  filter(
    measurement_concept_id == 4264419,
    !is.na(value_as_number),
    measurement_date >= first_c50_date
  ) %>%
  group_by(person_id) %>%
  slice_min(order_by = measurement_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(reduced_parity = as.numeric(value_as_number) - 1) %>%
  mutate(
    parity_group = case_when(
      reduced_parity < 1 ~ "Parity 0",
      reduced_parity == 1 ~ "Parity 1",
      reduced_parity > 1 ~ "Parity 2",
      TRUE ~ "Parity 0"
    )
  ) %>%
  count(parity_group, name = "n") %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  collect()

write_tsv(parity_first_after_c50, "parity_F.txt", quote = "none")

##### Comorbidities #####

preexisting_diag_cardio <- first_c50_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_c50_date,
    condition_start_date >= first_c50_date - years(5),
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

total_eligible_count_cardio <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_cardio <- tibble(
  n_with_cardio_diag = cardio_diag_count$n,
  n_total = total_eligible_count_cardio$n_total,
  percentage = round(100 * cardio_diag_count$n / total_eligible_count_cardio$n_total, 1)
)

write_tsv(result_cardio, "comorb_cardio_F.txt", quote = "none")

resp_diag_all <- first_c50_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_c50_date,
    condition_start_date >= first_c50_date - years(5),
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

total_eligible_count_resp <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_respiratory <- tibble(
  n_with_resp_diag = included_resp_count$n,
  n_total = total_eligible_count_resp$n_total,
  percentage = round(100 * included_resp_count$n / total_eligible_count_resp$n_total, 1)
)

write_tsv(result_respiratory, "comorb_resp_F.txt", quote = "none")

dementia_diag_all <- first_c50_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date <= first_c50_date,
    condition_start_date >= first_c50_date - years(5),
    str_detect(condition_source_value, "^DF0[0-3]") |
      str_detect(condition_source_value, "^DG3[0-1]")
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_dementia_count <- dementia_diag_all %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()
  

total_eligible_count_dementia <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_dementia <- tibble(
  n_with_dementia_diag = included_dementia_count$n,
  n_total = total_eligible_count_dementia$n_total,
  percentage = round(100 * included_dementia_count$n / total_eligible_count_dementia$n_total, 1)
)

write_tsv(result_dementia, "comorb_dementia_F.txt", quote = "none")


included_psych_diag <- first_c50_diag %>%
  inner_join(tbl(con, "condition_occurrence"), by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date >= first_c50_date - years(5),
    condition_start_date <= first_c50_date,
    str_detect(condition_source_value, "^DF2[0-9]") |
      str_detect(condition_source_value, "^DF323")   |
      str_detect(condition_source_value, "^DF333")
  ) %>%
  select(person_id, visit_occurrence_id) %>%
  distinct()


included_psych_diag_count <- included_psych_diag %>%
  distinct(person_id) %>%
  summarise(n = n()) %>%
  collect()

total_eligible_count_psych <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  collect()

result_psych <- tibble(
  n_with_psych_diag = included_psych_diag_count$n,
  n_total = total_eligible_count_psych$n_total,
  percentage = round(100 * included_psych_diag_count$n / total_eligible_count_psych$n_total, 1)
)

write_tsv(result_psych, "comorb_psych_F.txt", quote = "none")

##### Drug administration #####

g03a_contraceptives <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %LIKE% "G03A%")

g03a_with_c50 <- first_c50_diag %>%
  inner_join(g03a_contraceptives, by = "person_id", copy = TRUE)

pre_c50_count <- g03a_with_c50 %>%
  filter(
    drug_exposure_start_date <= first_c50_date,
    drug_exposure_start_date >= first_c50_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre_c50 = n()) %>%
  collect()

total_c50 <- first_c50_diag %>%
  summarise(n_total = n()) %>%
  collect()

pre_c50_count <- pre_c50_count %>%
  mutate(
    percentage = round(100 * n_pre_c50 / total_c50$n_total, 2),
    category = "Hormonal contraceptives for systemic use"
  )

post_c50_count <- g03a_with_c50 %>%
  filter(
    drug_exposure_start_date > first_c50_date,
    drug_exposure_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_c50 = n()) %>%
  collect()

post_c50_count <- post_c50_count %>%
  mutate(
    percentage = round(100 * n_post_c50 / total_c50$n_total, 2),
    category = "Hormonal contraceptives post-C50"
  )

contraceptive_usage_summary <- bind_rows(pre_c50_count, post_c50_count)

write_tsv(contraceptive_usage_summary, "contraceptive_usage_F.txt", quote = "none")


### HRT use ###

systemic_routes <- c(
  4262914, 4023156, 4132161, 4171047, 4142048,
  4302612, 4292110, 4263689, 4290759)

excluded_vnr <- c(
  "075243", "085813", "092655", "111870", "156154", "164019", "382333",
  "415679", "437626", "498485", "518289", "113369", "169917", "520576",
  "559948", "000820", "045203", "117692", "149132", "171974", "372571", 
  "509273", "535526", "537811", "550079", "552927", "564971", "567400")

atc_vnr_pair <- tbl(con, "lms_epikur1") %>%
  collect() %>%
  select(ATC, VNR, uuid) %>%
  filter(VNR %in% excluded_vnr)

uuid_exposureid_pair <- tbl(con, "drug_exposure_relation") %>%
  collect() %>%
  filter(uuid %in% atc_vnr_pair$uuid)


g03c_filtered <- tbl(con, "drug_exposure") %>%
  filter(
    str_detect(drug_source_value, "^G03C") | 
    str_detect(drug_source_value, "^G03F"),
    !(drug_exposure_id %in% uuid_exposureid_pair$drug_exposure_id),
    route_concept_id %in% systemic_routes
  ) %>%
  select(person_id, drug_exposure_start_date)

g03c_with_c50 <- first_c50_diag %>%
  inner_join(g03c_filtered, by = "person_id", copy = TRUE)

pre_c50_users <- g03c_with_c50 %>%
  filter(
    drug_exposure_start_date <= first_c50_date,
    drug_exposure_start_date >= first_c50_date - years(5)
  ) %>%
  distinct(person_id) %>%
  summarise(n_pre_c50 = n()) %>%
  collect()

post_c50_users <- g03c_with_c50 %>%
  filter(
    drug_exposure_start_date > first_c50_date,
    drug_exposure_start_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_c50 = n()) %>%
  collect()

n_total <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  pull(n_total)

g03c_summary <- tibble(
  category = "G03C* systemic use (excluding selected VNRs)",
  n_pre_c50 = pre_c50_users$n_pre_c50,
  p_pre_c50 = round(100 * pre_c50_users$n_pre_c50 / n_total, 2),
  n_post_c50 = post_c50_users$n_post_c50,
  p_post_c50 = round(100 * post_c50_users$n_post_c50 / n_total, 2)
)

write_tsv(g03c_summary, "hrt_usage_summary_F.txt", quote = "none")



 ### Receptor Status ###

receptor_categories <- tibble::tibble(
  category = c(
    "Androgen receptor positive",
    "Androgen receptor negative",
    "Estrogen receptor positive",
    "Estrogen receptor negative",
    "Progesterone receptor positive",
    "Progesterone receptor negative",
    "HER2 negative",
    "HER2 receptor normal expression",
    "HER2 receptor borderline expression",
    "HER2 overexpression",
    "HER2 Ultra-low expression (1-10%)",
    "HER2 expression +1",
    "HER2 expression +2",
    "HER2 expression +3",
    "BRCA1 normal",
    "BRCA1 insertion",
    "BRCA1 mutation",
    "BRCA1 deletion",
    "BRCA1 amplification",
    "BRCA1 alteration unknown consequence",
    "BRCA1 fusion",
    "BRCA2 normal",
    "BRCA2 insertion",
    "BRCA2 mutation",
    "BRCA2 deletion",
    "BRCA2 amplification",
    "BRCA2 alteration unknown consequence",
    "BRCA2 fusion"
  ),
  codes = list(
    "F29511",
    "F29512",
    "F29521",
    "F29525",
    "F29551",
    "F29555",
    "F29600",
    c("F29601", "FE13B1"),
    c("F29602", "FE13B6"),
    "F29603",
    "F29604",
    "F29605",
    "F29606",
    "F29607",
    "FE13U1",
    "FE13U2",
    "FE13U3",
    "FE13U4",
    "FE13U5",
    "FE13U9",
    "FE13UX",
    "FE13V1",
    c("FE13V2", "FE13V3"),
    "FE13V3",
    "FE13V4",
    "FE13V5",
    "FE13V9",
    "FE13VX"
  )
)

receptor_codes <- receptor_categories %>%
  unnest(codes)

n_total <- first_c50_diag %>%
  summarise(n_total = n_distinct(person_id)) %>%
  pull(n_total)

receptor_counts <- first_c50_diag %>%
  inner_join(tbl(con, "observation"), by = "person_id", copy = TRUE) %>%
  filter(value_as_string %in% receptor_codes$codes) %>%
  filter(
    observation_date <= first_c50_date,
    observation_date >= first_c50_date - years(5)
  ) %>%
  collect() %>%
  inner_join(receptor_codes, by = c("value_as_string" = "codes")) %>%
  group_by(category) %>%
  summarise(count = n_distinct(person_id), .groups = "drop")

receptor_counts_full <- receptor_categories %>%
  select(category) %>%
  left_join(receptor_counts, by = "category") %>%
  mutate(
    count = replace_na(count, 0),
    percentage = round(100 * count / n_total, 2)
  )


write_tsv(receptor_counts_full, "receptor_status_F.txt", quote = "none")

### BC Specific mortality ###

bc_mortality <- tbl(con, "death") %>%
  filter(cause_source_value %LIKE% "C50%") %>%
  inner_join(first_c50_diag, by = "person_id", copy = TRUE) %>%
  summarise(n = n(), metric = "BC mortality") %>%
  select(metric, n) %>%
  collect()
  
all_cause_mortality <- tbl(con, "death") %>%
  filter(!cause_concept_id == -1) %>%
  inner_join(first_c50_diag, by = "person_id", copy = TRUE) %>%
  summarise(n = n(), metric = "All cause mortality") %>%
  select(metric, n) %>%
  collect()

mortality <- rbind(all_cause_mortality, bc_mortality)

write_tsv(mortality, "mortality_counts_F.txt", quote = "none")

### TNM Classification ###


tnm_def <- read_csv("tnm_def.csv")
growth_def <- read_delim("growth_def.csv", delim = ";")
metal_def <- read_delim("metal_def.csv", delim = ";")
metao_def <- read_delim("metao_def.csv", delim = ";")

tnm_df <- tibble(class = tnm_def$Stage_num, TNM_T = tnm_def$AZC...2, TNM_N = tnm_def$AZC...3, TNM_M = tnm_def$AZC...4)

dict_tnm_class <- tnm_df %>%
  mutate(
    TNM_N = str_replace_all(TNM_N, "\\s+", ""),
    TNM_M = str_replace_all(TNM_M, "\\s+", "")
  ) %>%
  separate_rows(TNM_N, sep = ";", convert = FALSE) %>%
  separate_rows(TNM_M, sep = ";", convert = FALSE) %>%
  distinct(TNM_T, TNM_N, TNM_M, class) %>%
  mutate(seq = paste(TNM_T, TNM_N, TNM_M, sep = ":"))

dict_growth <- growth_def %>%
  separate_rows(C_TNM_T, sep = ";\\s*", convert = TRUE) %>%
  mutate(C_TNM_T = na_if(trimws(C_TNM_T), "")) %>%
  filter(!is.na(C_TNM_T))

dict_metal <- metal_def %>%
  separate_rows(C_TNM_N, sep = ";\\s*", convert = TRUE) %>%
  mutate(C_TNM_N = na_if(trimws(C_TNM_N), "")) %>%
  filter(!is.na(C_TNM_N)) %>%
  distinct()

dict_metao <- metao_def %>%
  separate_rows(C_TNM_M, sep = ";\\s*", convert = TRUE) %>%
  mutate(C_TNM_M = na_if(trimws(C_TNM_M), "")) %>%
  filter(!is.na(C_TNM_M)) %>%
  distinct()

tnm_classification_occurrences <- tbl(con, "condition_occurrence") %>%
  filter(person_id %in% first_c50_diag$person_id) %>%
  group_by(person_id) %>% 
  inner_join(tbl(con, "measurement"), by = c("person_id" = "person_id",
                                             "condition_occurrence_id" = "measurement_event_id"), copy = TRUE) %>%
  collect()

tnm_counts <- tnm_classification_occurrences %>%
  filter(measurement_concept_id %in% c(4193505, 4175208, 4082019)) %>%
  mutate(msv = na_if(trimws(measurement_source_value), ""),
         class_T = case_when(measurement_concept_id == 4193505 ~ measurement_source_value, TRUE ~ NA_character_),
         class_N = case_when(measurement_concept_id == 4175208 ~ measurement_source_value, TRUE ~ NA_character_),
         class_M = case_when(measurement_concept_id == 4082019 ~ measurement_source_value, TRUE ~ NA_character_)
  ) %>%
  select(person_id, class_T, class_N, class_M, measurement_date) %>%
  group_by(person_id) %>%
  summarise(
    class_T = dplyr::first(class_T, default = NA_character_),
    class_N = dplyr::first(class_N, default = NA_character_),
    class_M = dplyr::first(class_M, default = NA_character_),
   .groups = "drop") %>%
   filter(!if_any(where(is.character), ~ str_trim(.) == "")) %>%
  mutate(seq = paste(class_T, class_N, class_M, sep = ":"))

TNM_class_counts <- tnm_counts %>%
  left_join(dict_tnm_class, by = "seq") %>%
  count(class, sort = TRUE) %>%
  mutate(pct = n / sum(n))

growth_counts <- tnm_counts %>%
  left_join(dict_growth, by = c("class_T" = "C_TNM_T")) %>%
  count(Growth, sort = TRUE) %>%
  mutate(pct = n / sum(n))

metastasis_lymph_counts <- tnm_counts %>%
  left_join(dict_metal, by = c("class_N" = "C_TNM_N")) %>%
  count(Metalymph, sort = TRUE) %>%     
  mutate(pct = n / sum(n))

spread_organ_count <- tnm_counts %>%
  left_join(dict_metao, by = c("class_M" = "C_TNM_M")) %>%
  count(Metaorg, sort = TRUE) %>%     
  mutate(pct = n / sum(n))

write_tsv(TNM_class_counts, "tnm_class_counts_F.txt", quote = "none")
write_tsv(growth_counts, "cancer_growth_F.txt", quote = "none")
write_tsv(metastasis_lymph_counts, "metastasis_lymph_F.txt", quote = "none")
write_tsv(spread_organ_count, "organ_spread_F.txt", quote = "none")


#### Signals of treatment ####

oophorectomy <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("KLAE20", "KLAE20A", "KLAE21",
                                       "KLAF10", "KLAF10A", "KLAF11"))

oophorectomy_post365 <- first_c50_diag %>%
  inner_join(oophorectomy, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

oophorectomy_summary <- oophorectomy_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Oophorectomy"
  )

breast_conservation <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("KHAB00", "KHAB00A", "KHAB00B",
                                       "KHAB40", "KHAB40A", "KHAB99"))

breast_conservation_post365 <- first_c50_diag %>%
  inner_join(breast_conservation, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

breast_conservation_summary <- breast_conservation_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Breast Conservation"
  )


lymphadenectomy <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("KPJD41", "KPJD41C", "KPJD41D", "KPJD42", "KPJD42A", "KPJD42C", "KPJD42E", "KPJD42G",
                                       "KPJD43", "KPJD43A", "KPJD44", "KPJD44A", "KPJD45", "KPJD45C", "KPJD46", "KPJD46C",
                                       "KPJD51", "KPJD52", "KPJD53", "KPJD54", "KPJD55", "KPJD63", "KPJD63A", "KPJD63B",
                                       "KPJD64", "KPJD64A", "KPJD74", "KPJD97", "KPJD98", "KPJD99", "KPJD99C", "KPJD99D", "KPJD99E"))

lymphadenectomy_post365 <- first_c50_diag %>%
  inner_join(lymphadenectomy, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

lymphadenectomy_summary <- lymphadenectomy_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Lymphadenectomy"
  )


mastectomy <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% c("KHAC15", "KHAC20", "KHAC25",
                                       "KHAC30", "KHAC99"))

mastectomy_post365 <- first_c50_diag %>%
  inner_join(mastectomy, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

mastectomy_summary <- mastectomy_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Mastectomy"
  )


chemotherapy <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %LIKE% "BWHA1%" |
         procedure_source_value %LIKE% "BWHA2%" |
           procedure_source_value %LIKE% "BWHA3%" |
           procedure_source_value %LIKE% "BWHA4%")

chemotherapy_post365 <- first_c50_diag %>%
  inner_join(chemotherapy, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

chemotherapy_summary <- chemotherapy_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Chemotherapy"
  )

radiotherapy_codes <- c("BWGC","BWGC1","BWGC2","BWGC21",
                        "BWGC22","BWGC23","BWGC24","BWGC29",
                        "BWGC3","BWGC31","BWGC32","BWGC4","BWGC4A",
                        "BWGC5","BWGC5A","BWGC6","BWGC6A","BWGC6B","BWGC7",
                        "BWGC7A","BWGC8","BWGC8A","BWGC8B","BWGC8C","BWGC8D",
                        "BWGC8E","BWGC8F","BWGC9","BWGC9A","BWGC9B","BWGG","BWGG1",
                        "BWGG1A","BWGG1B","BWGG1C","BWGG2","BWGG2A","BWGG2B","BWGG2C",
                        "BWGG3","BWGG3A","BWGG4","BWGG5","BWGG6","BWGG6A","BWGG6B","BWGG6C",
                        "BWGG6D","BWGG7","BWGG8","BWGG9","BWGJ","BWGJ1")

radiotherapy <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% radiotherapy_codes)

radiotherapy_post365 <- first_c50_diag %>%
  inner_join(radiotherapy, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

radiotherapy_summary <- radiotherapy_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Radiotherapy"
  )

acht_drug_codes <- c("L02BG03", "L02BG06", "L02BG04", "L02BA01", "L02BA03")
acht_proc_codes <- c("BWHC13", "BWHC20", "BWHC12", "BWHC10", "BWHC11", "BWHC2")


acht_proc <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% acht_proc_codes)

acht_drug <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% acht_drug_codes)

acht_proc_post365 <- first_c50_diag %>%
  inner_join(acht_proc, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

acht_drug_post365 <- first_c50_diag %>%
  inner_join(acht_drug, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_c50_date,
    drug_exposure_start_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

acht_summary <- bind_rows(acht_proc_post365, acht_drug_post365) %>%
  summarise(across(everything(),  ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Anti-cancer Hormone Therapy"
  )

acht_pmm_drug_codes <- c("L02AE02", "L02AE03")
acht_pmm_proc_codes <- c("BWHC32", "BWHC31", "BWHC33", "BWHC50", "BWHC51", "BWHC52", "BWHC53",
                         "BWHC54", "BWHC3", "BWHC30", "BWHC4", "BWHC40", "BWHC5")


acht_pmm_proc <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% acht_pmm_proc_codes)

acht_pmm_drug <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% acht_pmm_drug_codes)

acht_pmm_proc_post365 <- first_c50_diag %>%
  inner_join(acht_pmm_proc, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

acht_pmm_drug_post365 <- first_c50_diag %>%
  inner_join(acht_pmm_drug, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_c50_date,
    drug_exposure_start_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

acht_pmm_summary <- bind_rows(acht_pmm_proc_post365, acht_pmm_drug_post365) %>%
  summarise(across(everything(),  ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "Anti-cancer Hormone Therapy in Pre-menopausal Women and in Men"
  )


htki_drug_codes <- c("L01EH01","L01XE07", "L01XE45", "L01EH02", "L01EH03")
htki_proc_codes <- c("BWHA405", "ML01EH01", "BWHA414", "ML01EH03")


htki_proc <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% htki_proc_codes)

htki_drug <- tbl(con, "drug_exposure") %>%
  filter(drug_source_value %in% htki_drug_codes)

htki_proc_post365 <- first_c50_diag %>%
  inner_join(htki_proc, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

htki_drug_post365 <- first_c50_diag %>%
  inner_join(htki_drug, by = "person_id", copy = TRUE) %>%
  filter(
    drug_exposure_start_date > first_c50_date,
    drug_exposure_start_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

htki_summary <- bind_rows(htki_proc_post365, htki_drug_post365) %>%
  summarise(across(everything(),  ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "HER2 Tyrosine Kinase Inhibitors"
  )

herinhib_codes <- c("BOHJ", "BOHJ1", "BOHJ10", "BOHJ10A", "BOHJ10B",
                        "BOHJ11", "BOHJ11B", "BOHJ11C", "BOHJ12", "BOHJ13",
                        "BOHJ13A", "BOHJ14", "BOHJ16", "BOHJ16A", "BOHJ17",
                        "BOHJ18", "BOHJ18A", "BOHJ18A1", "BOHJ18A2", "BOHJ18A3",
                        "BOHJ18A4", "BOHJ18A5", "BOHJ18B", "BOHJ18B1", "BOHJ18B2",
                        "BOHJ18B3", "BOHJ18B4", "BOHJ18B5", "BOHJ18B6", "BOHJ18B7",
                        "BOHJ18B8", "BOHJ18B9", "BOHJ18C", "BOHJ18C1", "BOHJ19", "BOHJ19A",
                        "BOHJ19A1", "BOHJ19B", "BOHJ19B1", "BOHJ19B2", "BOHJ19C", "BOHJ19C1",
                        "BOHJ19C2", "BOHJ19D", "BOHJ19D1", "BOHJ19D2", "BOHJ19E", "BOHJ19E1",
                        "BOHJ19F", "BOHJ19G", "BOHJ19G1", "BOHJ19H", "BOHJ19H1", "BOHJ19H2",
                        "BOHJ19H3", "BOHJ19H4", "BOHJ19H5", "BOHJ19H6", "BOHJ19H7", "BOHJ19H8",
                        "BOHJ19H9", "BOHJ19I", "BOHJ19I1", "BOHJ19I2", "BOHJ19I3", "BOHJ19J",
                        "BOHJ19J1", "BOHJ19J2", "BOHJ19J3", "BOHJ19J4", "BOHJ19K", "BOHJ19K1",
                        "BOHJ19L", "BOHJ19L1", "BOHJ19M", "BOHJ19M1", "BOHJ19M2", "BOHJ19N",
                        "BOHJ19N1", "BOHJ19O", "BOHJ19O1", "BOHJ2", "BOHJ20", "BOHJ21", "BOHJ22",
                        "BOHJ22A", "BOHJ22B", "BOHJ23", "BOHJ24", "BOHJ25", "BOHJ26", "BOHJ27", "BOHJ28",
                        "BOHJ28A", "BOHJ28B", "BOHJ28C", "BOHJ28D", "BOHJ3")

herinhib <- tbl(con, "procedure_occurrence") %>%
  filter(procedure_source_value %in% herinhib_codes)

herinhib_post365 <- first_c50_diag %>%
  inner_join(herinhib, by = "person_id", copy = TRUE) %>%
  filter(
    procedure_date > first_c50_date,
    procedure_date <= first_c50_date + years(1)
  ) %>%
  distinct(person_id) %>%
  summarise(n_post365 = n()) %>%
  collect()

herinhib_summary <- herinhib_post365 %>%
  mutate(
    percentage = round(100 * n_post365 / total_c50$n_total, 2),
    treatment = "HER2 (Human Epidermal Growth Factor Receptor 2) inhibitors"
  )

summary_signals_of_treatment = bind_rows(oophorectomy_summary, mastectomy_summary, chemotherapy_summary, radiotherapy_summary, acht_summary, acht_pmm_summary,
                                         herinhib_summary, htki_summary, breast_conservation_summary, lymphadenectomy_summary)

write_tsv(summary_signals_of_treatment, "signals_of_treatment_summary_BC_F.txt", quote = "none")

primary_malignancy <- tbl(con, "condition_occurrence") %>%
  filter(condition_source_value %LIKE% "C%" & !(
    condition_source_value %LIKE% "C44%" |
      condition_source_value %LIKE% "C50%" |
      condition_source_value %LIKE% "C77%" |
      condition_source_value %LIKE% "C78%" |
      condition_source_value %LIKE% "C79%"
  ))

primary_malignancy_post_c50 <- first_c50_diag %>%
  inner_join(primary_malignancy, by = "person_id", copy = TRUE) %>%
  filter(
    condition_start_date > first_c50_date,
    condition_start_date <= as.Date("2021-12-31")
  ) %>%
  distinct(person_id) %>%
  summarise(n_post_c50 = n()) %>%
  collect()

primary_malignancy_summary <- primary_malignancy_post_c50 %>%
  mutate(
    percentage = round(100 * n_post_c50 / total_c50$n_total, 2),
    Signal = "New Primary Malignancy"
  )

write_tsv(primary_malignancy_summary, "primary_malignancy_BC_F.txt", quote = "none")