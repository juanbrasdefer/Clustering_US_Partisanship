# libraries -------------------------------------------------------------------

# loading
library(tidyverse)
library(here)
library(httr)
library(jsonlite)
library(skimr)
library(DT)



# set directory
here::i_am("code/censusdata.R")








# DP2 - Social ----------------------------------------------------------------
# load data
DP2_2020_5Y <- read_csv(here("data/ACSDP5Y2020.DP02-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico"))

DP2_2020_5Y_edited <- DP2_2020_5Y %>%
  select(-contains("Percent"),
         -contains("Margin"),
         -contains("GRANDPARENTS")) %>%
  select(GEO_ID,
         NAME,
         c(
    "Estimate!!PLACE OF BIRTH!!Total population",
    "Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea",
    "Estimate!!VETERAN STATUS!!Civilian population 18 years and over",
    "Estimate!!VETERAN STATUS!!Civilian population 18 years and over!!Civilian veterans", 
    "Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over",
    "Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    "Estimate!!MARITAL STATUS!!Males 15 years and over",
    "Estimate!!MARITAL STATUS!!Males 15 years and over!!Never married"
  )) %>%
  rename(
    DP2_placebirth_population = "Estimate!!PLACE OF BIRTH!!Total population",
    DP2_foreign_born = "Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea",
    DP2_veterans_population = "Estimate!!VETERAN STATUS!!Civilian population 18 years and over",
    DP2_veterans_civvets = "Estimate!!VETERAN STATUS!!Civilian population 18 years and over!!Civilian veterans", 
    DP2_education_population = "Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over",
    DP2_education_bachelormin = "Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    DP2_marital_male_population = "Estimate!!MARITAL STATUS!!Males 15 years and over",
    DP2_marital_male_nevermarried = "Estimate!!MARITAL STATUS!!Males 15 years and over!!Never married"
  )





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


#substitutes?
#Estimate!!LANGUAGE SPOKEN AT HOME!!Population 5 years and over
#Estimate!!LANGUAGE SPOKEN AT HOME!!Population 5 years and over!!English only
#Estimate!!PLACE OF BIRTH!!Total population!!Native!!Born in United States!!Different state
#Estimate!!RESIDENCE 1 YEAR AGO!!Population 1 year and over!!Different house (in the U.S. or abroad)!!Different house in the U.S.!!Different county!!Same state
#Estimate!!RESIDENCE 1 YEAR AGO!!Population 1 year and over!!Different house (in the U.S. or abroad)!!Different house in the U.S.!!Different county!!Different state
  # for educational attainment there's lots of detail, like lower than 9th grade, but bachelor's is most encompassing imo
  # for marriage using males instead of females since large voter base of republican high correlation (assuming small proportion of same sex marriage)

# drop all other
#Estimate!!ANCESTRY!!
  # remove because ancestry does not tell which generation/ how recent
#Estimate!!LANGUAGE SPOKEN AT HOME!!
#Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea
#Estimate!!YEAR OF ENTRY!!
#Estimate!!PLACE OF BIRTH!!
#Estimate!!RESIDENCE 1 YEAR AGO!!Population 1 year and over
#Estimate!!DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION
#Estimate!!VETERAN STATUS!!Civilian population 18 years and over
#Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over
#Estimate!!SCHOOL ENROLLMENT!!Population 3 years and over enrolled in school
#Estimate!!FERTILITY!!
#Estimate!!MARITAL STATUS!!
#Estimate!!RELATIONSHIP!!Population in households
#Estimate!!RELATIONSHIP!!Population in households
  # is maybe a better version of the "never married" variable, but just stick with never married
  # this one has too many variations of it to condense to one variable



# Function to count "null" values in each column
count_nulls <- function(col) {
  sum(tolower(trimws(col)) == "null", na.rm = TRUE)
}

# Apply the function to each column in the dataframe
null_counts <- sapply(DP2_2020_5Y_edited, count_nulls)
# you can check this out to see it's 78 across all






# DP3 - Economic ----------------------------------------------------------------
# load data
DP3_2020_5Y <- read_csv(here("data/ACSDP5Y2020.DP03-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico"))

DP3_2020_5Y_edited <- DP3_2020_5Y %>%
  select(-contains("Percent"),
         -contains("Margin")) %>%
  select(GEO_ID,
         NAME,
         c(
    "Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
    "Estimate!!COMMUTING TO WORK!!Workers 16 years and over!!Mean travel time to work (minutes)",
    "Estimate!!EMPLOYMENT STATUS!!Population 16 years and over",
    "Estimate!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed" 
  )) %>%
  rename(DP3_median_income = "Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
         DP3_mean_commuting = "Estimate!!COMMUTING TO WORK!!Workers 16 years and over!!Mean travel time to work (minutes)",
         DP3_employment_pop = "Estimate!!EMPLOYMENT STATUS!!Population 16 years and over",
         DP3_employment_unemp = "Estimate!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed")






#names
# EMPLOYMENT STATUS
# COMMUTING TO WORK
# OCCUPATION
# INDUSTRY
# CLASS OF WORKER
# INCOME AND BENEFITS (IN 2012 INFLATION-ADJUSTED DOLLARS)
# HEALTH INSURANCE COVERAGE


# keep?
#Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households
#Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)
#Estimate!!COMMUTING TO WORK!!Workers 16 years and over!!Mean travel time to work (minutes)
#Estimate!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed
#Estimate!!EMPLOYMENT STATUS!!Population 16 years and over


# drop
#Estimate!!HEALTH INSURANCE COVERAGE!!
#Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households
#Estimate!!CLASS OF WORKER!!
#Estimate!!INDUSTRY!!
#Estimate!!OCCUPATION!!
#Estimate!!COMMUTING TO WORK!!






# PLOTS ---------------------------------------------------------------------------------------------

# Basic histogram of Median Income  
DP3_2020_5Y_edited %>%
  ggplot( aes(x = DP3_median_income))+
  geom_histogram(binwidth = 5000,
                 fill = "#359c4d",
                 color = "black") +
  labs(title = "Median Income, ACSurvey - 2020", x = "Median Income, Thousands USD (2020 Inflation Adj.)", y = "Frequency") +
  scale_x_continuous(labels = scales::number_format(scale = 0.001)) +  # Adjust x-axis labels
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.25))

ggsave(here("plots/histogram_medianincome_2020.png"),
       dpi = 300)





# JOINING CENSUS DATASETS TOGETHER
census_joined_2020 <- DP2_2020_5Y_edited %>%
  filter(!(GEO_ID == "0500000US48301")) %>%
  select(-"NAME") %>%
  left_join(DP3_2020_5Y_edited, by = "GEO_ID") %>%
  mutate(across(c(
    DP2_placebirth_population,
    DP2_foreign_born,
    DP2_veterans_population,
    DP2_veterans_civvets,
    DP2_education_population,
    DP2_education_bachelormin,
    DP2_marital_male_population,
    DP2_marital_male_nevermarried,
    DP3_median_income,
    DP3_mean_commuting,
    DP3_employment_pop,
    DP3_employment_unemp), 
    as.numeric))



min_income_2020 <- min(census_joined_2020$DP3_median_income, na.rm = TRUE)
max_income_2020 <- max(census_joined_2020$DP3_median_income, na.rm = TRUE)

min_commute_2020 <- min(census_joined_2020$DP3_mean_commuting)
max_commute_2020 <- max(census_joined_2020$DP3_mean_commuting)




census_normalized_2020 <- census_joined_2020 %>%
  mutate(prop_foreign_born = round(DP2_foreign_born/DP2_placebirth_population,3),
         prop_veterans = round(DP2_veterans_civvets/DP2_veterans_population, 3),
         prop_education_bacmin = round(DP2_education_bachelormin/DP2_education_population,3),
         prop_marital_male = round(DP2_marital_male_nevermarried/DP2_marital_male_population,3),
         prop_unemmployment = round(DP3_employment_unemp/DP3_employment_pop,3)
         ) %>%
  mutate(norm_income = round((DP3_median_income - min_income_2020) / 
      (max_income_2020 - min_income_2020),3),
      norm_commuting = round((DP3_mean_commuting - min_commute_2020) / 
        (max_commute_2020 - min_commute_2020),3))

census_normalized_2020 <- census_normalized_2020 %>%
  select(-c(DP2_placebirth_population,
            DP2_foreign_born,
            DP2_veterans_population,
            DP2_veterans_civvets,
            DP2_education_population,
            DP2_education_bachelormin,
            DP2_marital_male_population,
            DP2_marital_male_nevermarried,
            DP3_median_income,
            DP3_mean_commuting,
            DP3_employment_pop,
            DP3_employment_unemp)) %>%
  select(c(prop_foreign_born,
           prop_veterans,
           prop_education_bacmin,
           prop_marital_male,
           prop_unemmployment,
           norm_income,
           norm_commuting
  )) %>%      # Select only numeric columns
  rename(
    Foreign = "prop_foreign_born",
    Veterans = "prop_veterans",
    Education = "prop_education_bacmin",
    Marital = "prop_marital_male",
    Unemployment = "prop_unemmployment",
    Income = "norm_income",
    Commuting = "norm_commuting"
  ) 





# Summarize all numeric columns in the dataframe
summary_table <- census_normalized_2020 %>%
  select(where(is.numeric)) %>%
  skim()

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



datatable(census_summary_table) %>%
  formatStyle(
    columns = c("Variable", "Mean", "St.Dev", "P0", "P25", "P75", "P100","Histogram"),
    backgroundColor = "lightblue",
    fontWeight = "bold"
  ) 

