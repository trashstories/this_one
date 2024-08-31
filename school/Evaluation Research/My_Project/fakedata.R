library(tidyverse)
library(scales)
library(truncnorm)
library(broom)


# Seeds
# https://evalsp23.classes.andrewheiss.com/example/random-numbers.html#seeds

set.seed(80085)
rnorm(5)
rnorm(5)


# Distributions and random numbers
# https://evalsp23.classes.andrewheiss.com/example/random-numbers.html

fun <- tibble(income = rnorm(1000, 50000, 20000),
              age = rnorm(1000, 14, 3),
              age_good = rtruncnorm(1000, 12, 18, 14, 3),
              gender = sample(c("Male", "Female"), 1000, replace = TRUE, prob = c(.3, .7)),
              state = sample(c("GA", "SC", "NC", "VA"), 1000, replace = TRUE, prob = c(.3, .3, .2, .2)),
              fave = runif(1000, 1, 10),
              kids = rpois(1000, 2.2),
              score = rbeta(1000, 1, 100)) %>% 
  mutate(age_good = round(age_good, 0))

ggplot(fun, aes(income)) +
  geom_density(color = "blue", linewidth = 2) +
  geom_vline(xintercept = 50000, color = "purple", linetype = "dashed", linewidth = 2) +
  scale_x_continuous(breaks = seq(10000, 90000, 20000)) +
  theme_minimal()

ggplot(fun, aes(age_good)) +
  geom_histogram(binwidth = .5, color = "white", fill = "blue") +
  geom_vline(xintercept = 14, color = "purple", linetype = "dashed", linewidth = 2) +
  theme_minimal()

ggplot(fun, aes(gender)) +
  geom_bar(fill = "blue") +
  theme_minimal()

ggplot(fun, aes(fave)) +
  geom_histogram(binwidth = 1, color = "white", fill = "blue", boundary = 0) +
  theme_minimal()

ggplot(fun, aes(kids)) +
  geom_histogram(binwidth = 1, color = "white", fill = "blue", boundary = 0) +
  theme_minimal()

ggplot(fun, aes(score)) +
  geom_histogram(binwidth = .001, color = "white", fill = "blue", boundary = 0) +
  theme_minimal()




## Histograms, density plots, and bar charts
## Uniform (sample and runif)
## Normal
## Truncated normal
## Beta
## Binomial
## Poisson


# Rescaling things



# Connecting columns
## Draw a DAG
## Baseline value + effect from incoming nodes + noise

n_ppl <- 3278

happiness <- tibble(id = 1:n_ppl) %>% 
  mutate(blue = sample(c(0, 1), n_ppl, replace = TRUE)) %>% 
  mutate(cookies_base = rtruncnorm(n_ppl, 0, 10, 4, 2),
         blue_cookie = blue * rnorm(n_ppl, 2, 1),
         cookies = round(cookies_base + blue_cookie, 0)) %>% 
  mutate(happy_base = rbeta(n_ppl, 37, 63) * 100,
         blue_happy = blue * 20,
         cookie_happy = cookies * 3,
         happy = happy_base + blue_happy + cookie_happy) %>% 
  mutate(happy_nice = rescale(happy, to = c(20, 100)))

test <- lm(cookies ~ blue, data = happiness)
tidy(test)

test1 <- lm(happy ~ cookies + blue, data = happiness)
tidy(test1)

ggplot(happiness, aes(happy)) +
  geom_density(color = "blue", linewidth = 2) +
  theme_minimal()

# Case Number,	File Date,	Completion Date, Next/Last Setting Date,	Court,	
# Type of Action / Offense,	Defendant Status,	Disposition,	DOB, Judge, Race,	Sex, 
# Bond Amount, 	Criminal History,	Total Charges,	Plea,	
# Plea Date,	Address,	SES,	Length of Detention,	Prosecutor,	Dependents,	Jail Conditions
# Days until arraignment, 	Days until disposition/trial

n_ppl <- 10000

bail <- tibble(case = 1:n_ppl) %>% 
  mutate(age = round(rtruncnorm(n_ppl, 18, 65, 30, 10), 0)) %>% 
  mutate(race = sample(c("White", "Black", "Other"), n_ppl,
                       replace = TRUE, prob = c(.4, .5, .1))) %>% 
  mutate(gender = sample(c("Male", "Female"), n_ppl, replace = TRUE, 
                         prob = c(.85, .15))) %>% 
  mutate(ses_base = rbeta(n_ppl, 40, 60) * 100,
         black_ses = ifelse(race == "Black", rnorm(n_ppl, -10, 2), 0),
         white_ses = ifelse(race == "White", rnorm(n_ppl, 5, 1), 0),
         ses = ses_base + black_ses + white_ses) %>% 
  mutate(history_num = rbeta(n_ppl, .8, 8),
         history_num = round(rescale(history_num, to = c(1, 50)), 0),
         history = case_when(history_num > 1 ~ 1,
                             history_num <= 1 ~ 0)) %>%
  mutate(type = sample(c("B", "A", "Violence", "DWI"), n_ppl, 
                       replace = TRUE, prob = c(.3, .2, .2, .3))) %>% 
  mutate(bond = ifelse(type == "A", 1000 * history_num,
                       ifelse(type == "Violence", 1500 * history_num, 
                              500 * history_num)),
         bond = ifelse(bond > 5000, ifelse(type == "Violence", 
                                           ifelse(bond > 10000, 10000, bond), 
                                           5000), bond)) %>% 
  mutate(detention = rbeta(n_ppl, 1, 6),
         detention = round(rescale(detention, to = c(1, 500))), 0) %>% 
  mutate(dependents = rpois(n_ppl, 2)) %>% 
  mutate(time = sample(c("Before", "After"), n_ppl, replace = TRUE)) %>% 
  mutate(location = sample(c("Harris County", "Other Place"), 
                           n_ppl, replace = TRUE)) %>% 
  # influence the prob on trial and result
  mutate(trial = sample(c("Detained", "Released"), n_ppl, replace = TRUE)) %>% 
  mutate(result = sample(c("Dismissed", "Convicted", "Guilty Plea", "Aquitted"), 
                         n_ppl, replace = TRUE, prob = c(.3, .3, .3, .1))) %>% 
  mutate(bond = ifelse(time == "After" & location == "Harris County", 0, bond)) %>% 
  select(case, age, race, gender, ses, history, history_num, type, bond, 
         detention, dependents, time, location, trial, result)

ggplot(bail, aes(dependents)) +
  geom_histogram(binwidth = 1, color = "white", fill = "darkblue", boundary = 0) +
  theme_minimal()

ggplot(bail, aes(time, fill = location)) +
  geom_bar() +
  theme_minimal()


saveRDS(bail, "bail.RDS")
