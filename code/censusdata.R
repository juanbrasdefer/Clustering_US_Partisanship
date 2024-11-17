# libraries -------------------------------------------------------------------
library(tidyverse)
library(here)
library(skimr)
library(DT)



# set directory
here::i_am("code/censusdata.R")




# DP2 - Social ----------------------------------------------------------------
# load data
DP2_2020_5Y <- read_csv(here("data/raw/ACSDP5Y2020.DP02-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico"))

DP2_2020_5Y_edited <- DP2_2020_5Y %>%
  select(#-contains("Percent"),
         -contains("Margin"),
         -contains("GRANDPARENTS")) %>%
  select(GEO_ID,
         NAME,
         c(
    "Estimate!!PLACE OF BIRTH!!Total population",
    "Estimate!!PLACE OF BIRTH!!Total population!!Foreign born",
    "Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    "Percent!!MARITAL STATUS!!Males 15 years and over!!Never married"
  )) %>%
  rename(
    DP2_placebirth_population = "Estimate!!PLACE OF BIRTH!!Total population",
    DP2_foreign_born = "Estimate!!PLACE OF BIRTH!!Total population!!Foreign born",
    DP2_education_bachelormin_PE = "Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    DP2_marital_male_nevermarried_PE = "Percent!!MARITAL STATUS!!Males 15 years and over!!Never married"
  ) %>%
  mutate(DP2_foreign_born_PE = (100*round((as.numeric(DP2_foreign_born)/ as.numeric(DP2_placebirth_population)),3))) %>%
  select(-DP2_foreign_born)








# DP3 - Economic ----------------------------------------------------------------
# load data
DP3_2020_5Y <- read_csv(here("data/raw/ACSDP5Y2020.DP03-Data.csv")) %>%
  filter(!str_detect(NAME, "Puerto Rico"))

DP3_2020_5Y_edited <- DP3_2020_5Y %>%
  select(#-contains("Percent"),
         -contains("Margin")) %>%
  select(GEO_ID,
         NAME,
         c(
    "Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
    "Percent!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed" 
  )) %>%
  rename(DP3_median_income = "Estimate!!INCOME AND BENEFITS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)",
         DP3_employment_unemp_PE = "Percent!!EMPLOYMENT STATUS!!Population 16 years and over!!In labor force!!Civilian labor force!!Unemployed")


min_income <- min(DP3_2020_5Y_edited$DP3_median_income, na.rm = TRUE)
max_income <- max(DP3_2020_5Y_edited$DP3_median_income, na.rm = TRUE)

DP3_2020_5Y_edited <- DP3_2020_5Y_edited %>%
  mutate(DP3_median_income_norm = (100*round((DP3_median_income - min_income) / 
                             (max_income - min_income),3)))





# joining DP2 and DP3 together --------------------------------------------------------

currentyear <- "2020"

# JOINING CENSUS DATASETS TOGETHER
census_joined_2020 <- DP2_2020_5Y_edited %>%
  filter(!(GEO_ID == "0500000US48301")) %>%
  select(-"NAME") %>%
  inner_join(DP3_2020_5Y_edited, by = "GEO_ID") %>%
  mutate(across(c(
    DP2_placebirth_population,
    DP2_foreign_born_PE,
    DP2_education_bachelormin_PE,
    DP2_marital_male_nevermarried_PE,
    DP3_median_income_norm,
    DP3_employment_unemp_PE), 
    as.numeric)) %>%
  mutate(census_year = currentyear) %>%
  mutate(county_fips = str_extract(GEO_ID, "(?<=US)\\d+")) %>%
  mutate(unique_id = paste0(census_year, "_", county_fips)) %>% # "YEAR_FIPS"
  select(-c(DP3_median_income,  # replaced by normalized version
            DP2_placebirth_population,  # drop gross population; unsure if want in analysis
            county_fips,
            census_year,
            GEO_ID))


census_joined_2020 %>%
  write_csv(here("data/clean/census_selected_2020.csv"))













# PLOTS ---------------------------------------------------------------------------------------------

# Basic histogram of Median Income  
census_joined_2020 %>%
  ggplot( aes(x = DP3_median_income_norm))+
  geom_histogram(binwidth = 5,
                 fill = "#359c4d",
                 color = "black") +
  labs(title = "Median Income, ACSurvey - 2020", x = "Median Income, Normalized", y = "Frequency") +
  scale_x_continuous(labels = scales::number_format(scale = 0.001)) +  # Adjust x-axis labels
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25))

ggsave(here("plots/histogram_medianincome_2020.png"),
       dpi = 300)







# create table ------------------------------------------------------------

census_normalized_2020 <- census_normalized_2020    # Select only numeric columns
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

