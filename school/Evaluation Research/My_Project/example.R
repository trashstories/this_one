library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(ggdag)  # Make DAGs
library(scales)  # Format numbers with functions like comma(), percent(), and dollar()
library(broom)  # Convert models to data frames
library(patchwork)  # Combine ggplots into single composite plots

set.seed(1234)   # Make all random draws reproducible

village_randomized <- read_csv("data/village_randomized.csv")


library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(ggdag)  # Make DAGs
library(dagitty)  # Do DAG logic with R
library(broom)  # Convert models to data frames
library(MatchIt)  # Match things
library(modelsummary)  # Make side-by-side regression tables

set.seed(1234)   # Make all random draws reproducible

nets <- read_csv("data/mosquito_nets.csv")