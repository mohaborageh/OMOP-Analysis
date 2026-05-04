library(rlang)
library(dplyr)
library(tidyr)

standard_population <- data.table::data.table(
  age_group <- c("0-0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                 "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                 "70-74", "75-79", "80-84", "85-89", "90-94", "95+"),
  pop <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000,
           7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 800, 200)
) %>% mutate(weight = pop/100000)
colnames(standard_population) <- c("age_group", "pop", "weight")

standard_population <- standard_population %>% 
  mutate(age_group = {x <- age_group; attr(x, "label") <- "Year Groups"; x},
         pop       = {x <- pop;       attr(x, "label") <- "Number of Persons"; x},
         weight    = {x <- weight;    attr(x, "label") <- "Weight for Standardization"; x})


compute_ASR <- function(data, esp_weights, ASR=TRUE) {
  
  # Compute crude rates
  data2 <- data %>%
    group_by(age_group) %>%
    summarise(
      cases = sum(death_flag),
      person_years = sum(PY),
      rate = cases / person_years
    ) %>%
    left_join(esp_weights, by = "age_group")
  
  # Compute age standardized mortality rates for 100000 PY
  if (ASR == TRUE) {
    num_vec <- ageadjust.direct(count = data2$cases, 
                                          pop = data2$person_years, 
                                          stdpop = data2$weight) 
    names_vec = names(num_vec)
    values_vec <- as.numeric(num_vec) * 100000
    
    DSR <- tidyr::pivot_wider(data.table::data.table(Name = names_vec, 
                                                     Rate = values_vec),
                              names_from = Name,
                              values_from = Rate)
    
    return(DSR)
  }
  
  # Compute standardizes rates for each age group for 100000 PY
  
  else {
    
    sum_weight = sum(data2$weight)
    
    SMR <- data2 %>% 
      mutate(stdwt = weight/sum_weight,
             dsr = stdwt * rate,
             dsr_var = (stdwt^2) * (cases/person_years^2),
             wm = stdwt/person_years,
             lcl = qgamma(0.95/2, shape = (dsr^2)/dsr_var, 
                          scale = dsr_var/dsr),
             ucl = qgamma(1 - 0.95/2, shape = ((dsr+wm)^2)/(dsr_var+wm^2), 
                          scale = (dsr_var+wm^2)/(dsr+wm))
      ) %>%
      mutate(crude.rate = rate * 100000,
             adj.rate = dsr * 100000,
             lci = lcl * 100000,
             uci = ucl * 100000
      ) %>%
      select(c(age_group, crude.rate, adj.rate, lci, uci))
    
    
    return(SMR)
  }
}


"ageadjust.direct" <-
  function (count, pop, rate = NULL, stdpop, conf.level = 0.95) 
  {
    if (missing(count) == TRUE & !missing(pop) == TRUE & is.null(rate) == 
        TRUE) 
      count <- rate * pop
    if (missing(pop) == TRUE & !missing(count) == TRUE & is.null(rate) == 
        TRUE) 
      pop <- count/rate
    if (is.null(rate) == TRUE & !missing(count) == TRUE & !missing(pop) == 
        TRUE) 
      rate <- count/pop
    alpha <- 1 - conf.level
    cruderate <- sum(count)/sum(pop)
    stdwt <- stdpop/sum(stdpop)
    dsr <- sum(stdwt * rate)
    dsr.var <- sum((stdwt^2) * (count/pop^2))
    wm<- max(stdwt/pop)
    gamma.lci <- qgamma(alpha/2, shape = (dsr^2)/dsr.var, scale = dsr.var/dsr)
    gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr+wm)^2)/(dsr.var+wm^2), 
                        scale = (dsr.var+wm^2)/(dsr+wm))
    c(crude.rate = cruderate, adj.rate = dsr, lci = gamma.lci, 
      uci = gamma.uci)
  }

set_attr <- function(x, attr_name, attr_value) {
  attr(x, attr_name) <- attr_value  
  return(x)
}

# compute for all time
compute_IR <- function(data) {
  num_vec <- ageadjust.direct(count = data$cases_by_age, 
                                        pop = data$person_time_by_age, 
                                        stdpop = data$weight) 
  names_vec = names(num_vec)
  values_vec <- as.numeric(num_vec) * 100000
  
  out_data <- tidyr::pivot_wider(data.table::data.table(Name = names_vec, 
                                                        Rate = values_vec),
                                 names_from = Name,
                                 values_from = Rate)
  return(out_data)
}


compute_IR_yearly <- function(data) {
  yr_data <- data  
  
  yearly_rate <- list()
  # compute for each year standardized rates
  for (i in unique(yr_data$year)){
    subset_df <- yr_data %>% filter(year == i)
    yearly_result <- ageadjust.direct(count = subset_df$cases_by_age,
                                                pop = subset_df$person_time_by_age,
                                                stdpop = subset_df$weight)
    names_vec = names(yearly_result)
    values_vec <- as.numeric(yearly_result) * 100000
    
    values_tab <- tidyr::pivot_wider(data.table::data.table(Name = names_vec, 
                                                            Rate = values_vec),
                                     names_from = Name,
                                     values_from = Rate)
    
    yearly_rate[[as.character(i)]] <- values_tab
  }
  
  # append tables together and return yearly rates
  out_data <- rownames_to_column(do.call(rbind, lapply(yearly_rate, 
                                                       function(x) as.data.frame(x)))) %>%
    rename(Year = rowname) %>%
    mutate(group_var = 1) %>%
    arrange(Year)
  
  return(out_data)
}