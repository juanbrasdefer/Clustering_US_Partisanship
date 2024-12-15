# libraries -------------------------------------------------------------------
library(tidyverse)
library(here)
library(skimr)
library(DT)



# set directory
here::i_am("code/cleaning_censusdata.R")




# Cleaning - DP2 (Social) ------------------------------------------------------------

# load data
DP2_2020_5Y <- read_csv(here("data/raw/ACSDP5Y2020.DP02-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico")) # can't vote

DP2_2020_5Y_edited <- DP2_2020_5Y %>%
  select(-contains("Margin"),  # selecting columns
         -contains("GRANDPARENTS"),
         GEO_ID, 
         NAME,
         c("Estimate!!PLACE OF BIRTH!!Total population", # variables of interest
           "Estimate!!PLACE OF BIRTH!!Total population!!Foreign born",
           "Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
           "Percent!!MARITAL STATUS!!Males 15 years and over!!Never married" )) %>%
  rename( # renaming columns for easier manipulation
    DP2_placebirth_population = "Estimate!!PLACE OF BIRTH!!Total population",
    DP2_foreign_born = "Estimate!!PLACE OF BIRTH!!Total population!!Foreign born",
    DP2_education_bachelormin_PE = "Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    DP2_marital_male_nevermarried_PE = "Percent!!MARITAL STATUS!!Males 15 years and over!!Never married" ) %>%
  mutate(DP2_foreign_born_PE = (100*round((as.numeric(DP2_foreign_born)/ as.numeric(DP2_placebirth_population)),3))) %>%
  select(-DP2_foreign_born)




# Cleaning - DP3 (Economic) ------------------------------------------------------------

# load data
DP3_2020_5Y <- read_csv(here("data/raw/ACSDP5Y2020.DP03-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico")) # same deal

DP3_2020_5Y_edited <- DP3_2020_5Y %>%
  select(#-contains("Percent"),
    -contains("Margin"),
    GEO_ID,
    NAME,
    c("Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
      "Percent!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed" )) %>%
  rename(DP3_median_income = "Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
         DP3_employment_unemp_PE = "Percent!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed")

# find min and max for normalizing income values
min_income <- min(DP3_2020_5Y_edited$DP3_median_income, na.rm = TRUE)
max_income <- max(DP3_2020_5Y_edited$DP3_median_income, na.rm = TRUE)

DP3_2020_5Y_edited <- DP3_2020_5Y_edited %>%
  mutate(DP3_median_income_norm = (100*round((DP3_median_income - min_income) / 
                                               (max_income - min_income),3))) # minmax normalize





# Joining DP2 and DP3 together --------------------------------------------------------

currentyear <- "2020" # to add year value to our dataset

# JOINING CENSUS DATASETS TOGETHER
census_joined_2020 <- DP2_2020_5Y_edited %>%
  filter(!(GEO_ID == "0500000US48301"), # not for study
         !(GEO_ID == "0500000US48243")) %>% # texas JEFF DAVIS county; has no income val
  select(-"NAME") %>%
  inner_join(DP3_2020_5Y_edited, by = "GEO_ID") %>% # innejoin to get rid of NAs
  mutate(across(c(
    DP2_placebirth_population,
    DP2_foreign_born_PE,
    DP2_education_bachelormin_PE,
    DP2_marital_male_nevermarried_PE,
    DP3_median_income_norm,
    DP3_employment_unemp_PE), 
    as.numeric)) %>% # turn all our variable columns into numeric
  mutate(census_year = currentyear) %>% # add a value of 2020 to each observation
  mutate(county_fips = str_extract(GEO_ID, "(?<=US)\\d+")) %>% # quick regex for new id
  mutate(unique_id = paste0(census_year, "_", county_fips)) %>% # "YEAR_FIPS"
  select(-c(DP3_median_income,  # replaced by normalized version
            DP2_placebirth_population,  # drop gross population; unsure if want in analysis
            county_fips, # replaced by unique_id
            census_year, # replaced by unique_id
            GEO_ID)) # equivalent to fips

# save
census_joined_2020 %>%
  write_csv(here("data/clean/census_selected_2020.csv"))




# create table ------------------------------------------------------------

# rename variables for aesthetic purposes
census_joined_2020 <- census_joined_2020    # Select only numeric columns
rename(
  Foreign = "DP2_foreign_born_PE",
  Education = "DP2_education_bachelormin_PE",
  Marital = "DP2_marital_male_nevermarried_PE",
  Unemployment = "DP3_employment_unemp_PE",
  Income = "DP3_median_income_norm") 



# summarize all numeric columns in the dataframe
summary_table <- census_normalized_2020 %>%
  select(where(is.numeric)) %>%
  skim()

# create table
census_summary_table <- summary_table %>%
  select(-skim_type,
         -n_missing,
         -complete_rate) %>%
  rename(Variable = "skim_variable",
         Mean = "numeric.mean",
         St.Dev = "numeric.sd",
         P0 = "numeric.p0",
         P25 = "numeric.p25",
         P50 = "numeric.p50",  
         P75 = "numeric.p75",
         P100 = "numeric.p100" ,
         Histogram = "numeric.hist" ) %>%
  mutate(Mean = round(Mean, 3),
         St.Dev = round(St.Dev, 3))


# view table
datatable(census_summary_table) %>%
  formatStyle(
    columns = c("Variable", "Mean", "St.Dev", "P0", "P25", "P75", "P100","Histogram"),
    backgroundColor = "lightblue",
    fontWeight = "bold"
  ) 




