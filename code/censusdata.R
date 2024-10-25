# libraries -------------------------------------------------------------------

# loading
library(tidyverse)
library(here)
library(httr)
library(jsonlite)


# set directory
here::i_am("code/censusdata.R")








# DP2 - Social ----------------------------------------------------------------
# load data
DP2_2020_5Y <- read_csv(here("data/ACSDP5Y2020.DP02-Data.csv"))

DP2_2020_5Y_edited <- DP2_2020_5Y %>%
  select(-contains("Percent"),
         -contains("Margin"),
         -contains("GRANDPARENTS"))

#names(DP2_2020_5Y_edited)
# HOUSEHOLDS BY TYPE
# RELATIONSHIP
# MARITAL STATUS
# FERTILITY
# SCHOOL ENROLLMENT
# EDUCATIONAL ATTAINMENT
# VETERAN STATUS
# DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION
# RESIDENCE 1 YEAR AGO
# PLACE OF BIRTH
# U.S. CITIZENSHIP STATUS
# YEAR OF ENTRY
# WORLD REGION OF BIRTH OF FOREIGN BORN
# LANGUAGE SPOKEN AT HOME
# ANCESTRY

# Function to count "null" values in each column
count_nulls <- function(col) {
  sum(tolower(trimws(col)) == "null", na.rm = TRUE)
}

# Apply the function to each column in the dataframe
null_counts <- sapply(DP2_2020_5Y_edited, count_nulls)
# you can check this out to see it's 78 across all






# DP3 - Economic ----------------------------------------------------------------
# load data
DP3_2020_5Y <- read_csv(here("data/ACSDP5Y2020.DP03-Data.csv"))

DP3_2020_5Y_edited <- DP2_2020_5Y %>%
  select(-contains("Percent"),
         -contains("Margin"))

#names(DP3_2020_5Y_edited)
# EMPLOYMENT STATUS
# COMMUTING TO WORK
# OCCUPATION
# INDUSTRY
# CLASS OF WORKER
# INCOME AND BENEFITS (IN 2012 INFLATION-ADJUSTED DOLLARS)
# HEALTH INSURANCE COVERAGE
