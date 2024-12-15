

# libraries -------------------------------------------------------------------

# loading
library(tidyverse)
library(here)
library(stats) 


# set directory
here::i_am("code/cleaning_votingdata.R")


# Step 1 - load data ----------------------------------------------------------------------------------
countypres_raw <- read_csv(here("data/raw/countypres_2000-2020.csv"))

# Step 2 - Make Dataset Wide --------------------------------------------------------------------

# making the dataset wide
voting_as_wide <- countypres_raw %>%
  pivot_wider(names_from = party, 
              values_from = candidatevotes) %>%
  mutate(state_county = paste0(state_po,
                               "-",
                               county_name)) %>% # retain only our identifier columns
  filter(!(is.na(county_fips))) %>% # drop NA fips:
                                      # Connecticut - WRITE-IN
                                      # Maine - Uniformed Service & Overseas
                                      # Rhode Island - Federal Precinct 
  filter(county_fips != "2938000") %>% # remove Kansas City because jackson county already in dataset
                                          # and this long fips is nonexistent in census dataset
                                          # so it'll get dropped anyway
  filter(county_fips != 02099) # drop District 99 Arkansas which is empty in the data 

# Step 3 - Flatten dataset and calculate aggregates, proportions, margins--------------------------------------------------

flattened_voting_wide <- voting_as_wide %>%
  group_by(year,
           state,
           state_po,
           state_county,
           county_name,
           county_fips,
           totalvotes) %>%
  summarise(aggvotes_D = sum(DEMOCRAT, na.rm = TRUE),
            aggvotes_R = sum(REPUBLICAN, na.rm = TRUE),
            aggvotes_G = sum(GREEN, na.rm = TRUE),
            aggvotes_L = sum(LIBERTARIAN, na.rm = TRUE),
            aggvotes_O = sum(OTHER, na.rm = TRUE)) %>%
  mutate(county_fips = str_pad(county_fips, width = 5, side = "left", pad = "0"))


minvotes <- min(flattened_voting_wide$totalvotes)
maxvotes <- max(flattened_voting_wide$totalvotes)

voting_clean <- flattened_voting_wide %>%
  mutate(unique_id = paste0(year, "_", county_fips)) %>% # year fips unique identifier
  mutate(
    proportion_D = aggvotes_D / totalvotes,  # Democratic proportion
    proportion_R = aggvotes_R / totalvotes,  # Republican proportion
    proportion_G = aggvotes_G / totalvotes,  # Green proportion
    proportion_L = aggvotes_L / totalvotes,  # Libertarian proportion
    proportion_O = aggvotes_O / totalvotes   # 'Other' proportion
  ) %>%
  mutate(proportion_explained_by_DR = (proportion_D + proportion_R),
         votemargin_DR = (proportion_D - proportion_R)) %>%
  mutate(norm_votes = (totalvotes - minvotes) / (maxvotes - minvotes),
         log_votes = log(totalvotes + 1)) 


voting_clean %>%
  write_csv(here("data/clean/CountyPresReturns_2000-2020_Clean.csv"))




# PLOTS - Voting Exploration -----------------------------------------------------------

# Basic histogram of 2020 margins 
voting_clean %>%
  filter(year == 2020) %>%
  ggplot(aes(x = votemargin_DR, 
           fill = votemargin_DR > 0)) +
  geom_histogram(binwidth = 0.04, color = "black") +
  labs(title = "Distribution of Net Vote Margin - 2020", x = "Net Vote Margin (D-R)", y = "Frequency") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.25))

ggsave(here("plots/histogram_netmargin_2020.png"),
       dpi = 300)




# scatter 2020 margin vs log total votes
voting_clean %>%
  filter(year == 2020) %>%
  ggplot(aes(x = votemargin_DR, 
             y = log_votes, 
             color = votemargin_DR > 0 )) +
  geom_point(alpha = 0.6, 
             size = 1) +  
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "Net Vote Margin vs. County Population - 2020", 
       x = "Net Vote Margin (D-R)",
       y = "Total Votes in County")

ggsave(here("plots/scatter_margins_logvotes_2020.png"),
       dpi = 300)





# Step 4 - ALT DATASET: Analysis of counties with significant margin changes ------------------------

margins_between_years <- voting_clean %>%
  filter(year > 2011) %>%
  pivot_wider(id_cols = county_fips,
              names_from = year, 
              values_from = votemargin_DR) %>%
  rename(margin_2012 = "2012",
         margin_2016 = "2016",
         margin_2020 = "2020") %>%
  mutate(delta_2012_16 = (margin_2016 - margin_2012), # t1 minus t2
         delta_2016_20 = (margin_2020 - margin_2016)) %>%
  mutate(abs_delta_2012_16 = abs(delta_2012_16),
         abs_delta_2016_20 = abs(delta_2016_20)) %>%
  filter(!is.na(margin_2012),
         !is.na(margin_2016),
         !is.na(margin_2020)) 

margins_between_years %>%
  write_csv(here("data/clean/margins_between_years.csv"))


# Step 5 - ALT DATASET: percentile calculations for dataset reduction -------------------------------------
# 2012-16
pctls_delta_2012_16 <- quantile(margins_between_years$abs_delta_2012_16, 
                                probs = c(0.5, 0.67))
pctls_delta_2012_16
pctls_50_delta_2012_16 <-  pctls_delta_2012_16[1][[1]] # first probability threshold (50%)


# 2016-20
pctls_delta_2016_20 <- quantile(margins_between_years$abs_delta_2016_20, 
                                probs = c(0.5, 0.67))
pctls_delta_2016_20
pctls_50_delta_2016_20 <- pctls_delta_2016_20[1][[1]] # first probability threshold (50%)


# Step 6 - ALT DATASET: percentile calculations for dataset reduction -------------------------------------

margins_between_years_subset <- margins_between_years %>%
  filter((abs_delta_2012_16 > pctls_50_delta_2012_16) & (abs_delta_2016_20 > pctls_50_delta_2016_20))

voting_clean_subset_largemargins <- margins_between_years_subset %>%
  inner_join(voting_clean, 
             by = "county_fips")

voting_clean_subset_largemargins %>%
  write_csv(here("data/clean/CountyPresReturns_subset_largemargins.csv"))




# PLOTS 2: Delta Margins and Thresholds  ---------------------------------------------------------------

# Histogram - ABSOLUTE deltas 2012 to 2016
# 2012 to 2016 margins
margins_between_years %>%
  ggplot( 
    #aes(x = delta_2012_16)
    aes(x = abs_delta_2012_16)
  )+
  geom_histogram(binwidth = 0.02,
                 fill = "#351c99",
                 #fill = "#359c4d",
                 color = "black") +
  labs(
    #title = "delta_2012_16", 
    title = "ABS delta_2012_16",
    x = "Delta", y = "Frequency") +
  #scale_x_continuous(breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(breaks = c(0, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  geom_vline(xintercept = pctls_50_delta_2012_16,
             linetype=2, colour="red") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25))

ggsave(here(
  #"plots/delta_2012_16.png"
  "plots/delta_2012_16_ABS.png"
),
dpi = 300)




# Histogram - ABSOLUTE deltas 2016 to 2020
# 2016 to 2020 margins
margins_between_years %>%
  ggplot( 
    #aes(x = delta_2016_20)
    aes(x = abs_delta_2016_20)
  )+
  geom_histogram(
    #binwidth = 0.015,
    binwidth = 0.008,
    fill = "#351c99",
    #fill = "#359c4d",
    color = "black") +
  labs(
    #title = "delta_2016_20", 
    title = "ABS delta_2016_20", 
    x = "Delta", y = "Frequency") +
  #scale_x_continuous(breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(breaks = c(0, 0.05, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  geom_vline(xintercept=pctls_50_delta_2016_20,
             linetype=2, colour="red") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25))

ggsave(here(
  #"plots/delta_2016_20.png"
  "plots/delta_2016_20_ABS.png"),
  dpi = 300)


# PLOTS 2: Delta Margins and Thresholds - now with subset only ---------------------------------------------------------------

# histogram of positive/negative delta margins - subset
# 2012 to 2016
margins_between_years_subset %>%
  ggplot( 
    aes(x = delta_2012_16)
  )+
  geom_histogram(binwidth = 0.02,
                 fill = "#b079e8",
                 #fill = "#359c4d",
                 color = "black") +
  labs(
    title = "delta_2012_16, subset", 
    x = "Delta", y = "Frequency") +
  scale_x_continuous(breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  #geom_vline(xintercept = pctls_50_delta_2012_16,
  #           linetype=2, colour="red") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25))

ggsave(here(
  "plots/delta_2012_16_subset.png"
),
dpi = 300)


# histogram of positive/negative delta margins - subset
# 2012 to 2016
margins_between_years_subset %>%
  ggplot( 
    aes(x = delta_2016_20)
  )+
  geom_histogram(binwidth = 0.02,
                 fill = "#b079e8",
                 #fill = "#359c4d",
                 color = "black") +
  labs(
    title = "delta_2016_20, subset", 
    x = "Delta", y = "Frequency") +
  scale_x_continuous(breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1,0.2,  0.3, 0.4, 0.5, 0.6)) +
  #geom_vline(xintercept = pctls_50_delta_2012_16,
  #           linetype=2, colour="red") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25))

ggsave(here(
  "plots/delta_2016_20_subset.png"
),
dpi = 300)








