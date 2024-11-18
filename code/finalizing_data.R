# libraries --------------------------------------------------------


# loading libraries
library(tidyverse)
library(here)


# directory 
here::i_am("code/finalizing_data.R")



# Step 1 - load data -------------------------------------------------------------------

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
  


clean_census_2020 <- read_csv(here("data/clean/census_selected_2020.csv")) %>%
  select(-NAME)


# Step 2 - join data ' ------------------------------------------------------

pipeline_ready <- clean_voting_all %>%
  inner_join(clean_census_2020, by = "unique_id") 

pipeline_ready %>%
  write_csv(here("data/clean/votemargins_census_joined_2020.csv"))
