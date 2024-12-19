# libraries --------------------------------------------------------


# loading libraries
library(tidyverse)
library(here)


# directory 
here::i_am("code/finalizing_data.R")


# FULL dataset  ----------------------------------------------------------

# step 1: load data

# voting data
clean_voting_all <- read_csv(here("data/clean/CountyPresReturns_2000-2020_Clean.csv")) %>%
  select(unique_id,
         year,
         state_po,
         county_name,
         votemargin_DR,
         proportion_explained_by_DR,
         totalvotes
  ) %>%
  mutate(proportion_explained_by_DR = round(proportion_explained_by_DR*100, 1), 
         votemargin_DR = round(votemargin_DR*100, 1))

# census data
clean_census_2020 <- read_csv(here("data/clean/census_selected_2020.csv")) %>%
  select(-NAME)


# step 2: join census and voting data

pipeline_ready <- clean_voting_all %>%
  inner_join(clean_census_2020, by = "unique_id") 

pipeline_ready %>%
  write_csv(here("data/clean/votemargins_census_joined_2020.csv"))




# SUBSET dataset  ----------------------------------------------------------

# step 1: load reduced (subset with only large margings) voting data

voting_clean_subset_largemargins <- read_csv(here("data/clean/CountyPresReturns_subset_largemargins.csv")) %>%
  select(unique_id,
         year,
         state_po,
         county_name,
         votemargin_DR,
         proportion_explained_by_DR,
         delta_2012_16,
         delta_2016_20,
         abs_delta_2012_16,
         abs_delta_2016_20,
         margin_2012,
         margin_2016,
         margin_2020,
         totalvotes
  ) %>%
  mutate(votemargin_DR = round(votemargin_DR*100, 1),
         proportion_explained_by_DR = round(proportion_explained_by_DR*100, 1),
         margin_2012 = round(margin_2012*100, 1),
         margin_2016 = round(margin_2016*100, 1),
         margin_2020 = round(margin_2020*100, 1),
         delta_2012_16 = round(delta_2012_16*100, 1),
         delta_2016_20 = round(delta_2016_20*100, 1),
         abs_delta_2012_16 = round(abs_delta_2012_16*100, 1),
         abs_delta_2016_20 = round(abs_delta_2016_20*100, 1)
  )

# census data is already loaded

# step 2: join census and reduced (subset with only large margings) voting data

pipeline_ready_subset_largemargins <- voting_clean_subset_largemargins %>%
  inner_join(clean_census_2020, by = "unique_id") 

pipeline_ready_subset_largemargins %>%
  write_csv(here("data/clean/votemargins_census_joined_2020_subset_largemargins.csv"))

