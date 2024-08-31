library(tidyverse)
library(broom)
library(modelsummary)
library(kableExtra)
library(ggdag)
library(scales)
library(truncnorm)
library(lubridate)
library(randomNames)

set.seed(80085)
n_ppl <- 50

esteem <- tibble(id = 1:n_ppl) %>% 
  # Dates
  mutate(year = sample(2017:2022, n_ppl, replace = TRUE)) %>% 
  mutate(month = sample(1:12, n_ppl, replace = TRUE)) %>% 
  mutate(day = sample(1:31, n_ppl, replace = TRUE),
         day = case_when(month == 2 & day > 28 ~ sample(1:28, n_ppl, replace = TRUE),
                         month %in% c(4, 6, 9, 11) & day > 30 ~ sample(1:30, n_ppl, replace = TRUE),
                         TRUE ~ day) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  # Demographics 
  mutate(age = round(rtruncnorm(n_ppl, 12, 65, 18, 10), 0)) %>% 
  mutate(race = sample(c("Black", "Latinx", "White", "Other"), n_ppl,
                       replace = TRUE, prob = c(.4, .2, .3, .1))) %>% 
  mutate(gender = sample(c("Man", "Woman"), n_ppl, replace = TRUE, 
                         prob = c(.45, .55))) %>% 
  # Social Media
  mutate(social_base = rtruncnorm(n_ppl, 1, 10, 5, 2.5),
         age_social = age * rnorm(n_ppl, .5, 1),
         gender_social = case_when(gender == "Man" ~ rnorm(n_ppl, 5, 2.5),
                                   gender == "Woman" ~ rnorm(n_ppl, 5, 2.5)),
         race_social = case_when(gender == "Black" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "Latinx" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "White" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "Other" ~ rnorm(n_ppl, 5, 2.5)),
         social = social_base + age_social + gender_social + race_social,
         social = case_when(social < 1 ~ 1, 
                            social > 10 ~ 10,
                            TRUE ~ round(social))) %>% 
  # Self-esteem
  mutate(esteem_base = rtruncnorm(n_ppl, 1, 10, 5, 1.25),
         age_esteem = age * rnorm(n_ppl, .5, 1),
         gender_esteem = case_when(gender == "Man" ~ rnorm(n_ppl, 5, 2.5),
                                   gender == "Woman" ~ rnorm(n_ppl, 5, 2.5)),
         race_esteem = case_when(gender == "Black" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "Latinx" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "White" ~ rnorm(n_ppl, 5, 2.5),
                                 gender == "Other" ~ rnorm(n_ppl, 5, 2.5)),
         esteem = esteem_base + age_esteem + gender_esteem + race_esteem,
         esteem = case_when(esteem < 1 ~ 1, 
                            esteem > 10 ~ 10,
                            TRUE ~ round(esteem)))
         
  