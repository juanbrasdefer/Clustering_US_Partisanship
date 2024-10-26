# libraries -------------------------------------------------------------------

# loading
library(tidyverse)
library(here)
library(ggrepel)

# set directory
here::i_am("code/votetotals_exploration.R")

countypres_raw <- read_csv(here("data/countypres_2000-2020.csv"))

# COMMON PRE-PROCESS ----------------------------------------------------------------

# making the dataset wide
voting_as_wide <- countypres_raw %>%
  pivot_wider(names_from = party, 
              values_from = candidatevotes) %>%
  mutate(state_county = paste0(state_po,
                               "-",
                               county_name))

# dropping all vote data and retaining county identifiers for future indexing
counties_only_all <- voting_as_wide %>%
  group_by(state,
           state_po,
           state_county,
           county_name,
           county_fips) %>%
  summarise(placeholder = sum(county_fips)) %>% # ugly workaround
  select(state,
         state_po,
         state_county,
         county_name,
         county_fips) %>% # retain only our identifier columns
  filter(!(is.na(county_fips))) # drop NA fips:
                                          # Connecticut - WRITE-IN
                                          # Maine - Uniformed Service & Overseas
                                          # Rhode Island - Federal Precinct 
  
# 2020 MANIPULATIONS ----------------------------------------------------------------

# 2020
voting_2020_wide <- voting_as_wide %>%
  filter(year == 2020) %>%
  rename(totalvotes_2020 = "totalvotes") %>%
  group_by(state,
           state_po,
           state_county,
           county_name,
           county_fips,
           totalvotes_2020) %>%
  summarise(aggvotes_D_2020 = sum(DEMOCRAT, na.rm = TRUE),
            aggvotes_R_2020 = sum(REPUBLICAN, na.rm = TRUE),
            aggvotes_G_2020 = sum(GREEN, na.rm = TRUE),
            aggvotes_L_2020 = sum(LIBERTARIAN, na.rm = TRUE),
            aggvotes_O_2020 = sum(OTHER, na.rm = TRUE))
  

minvotes_2020 <- min(voting_2020_wide$totalvotes_2020)
maxvotes_2020 <- max(voting_2020_wide$totalvotes_2020)

voting_DR_margins_2020 <- voting_2020_wide %>%
  mutate(
    proportion_D_2020 = aggvotes_D_2020 / totalvotes_2020,  # Democratic proportion
    proportion_R_2020 = aggvotes_R_2020 / totalvotes_2020,  # Republican proportion
    proportion_explained_by_DR_2020 = proportion_D_2020 + proportion_R_2020,
    net_DR_margin_2020 = proportion_D_2020 - proportion_R_2020) %>% # Net margin: positive = Dem, negative = Rep
  mutate(
    norm_votes_2020 = (totalvotes_2020 - minvotes_2020) / 
      (maxvotes_2020 - minvotes_2020),
    lognorm_votes_2020 = log(totalvotes_2020 + 1))


# seeing number of counties that went republican vs democrat
temp <- voting_DR_margins_2020 %>%
  summarise(positive_count = sum(net_DR_margin_2020 > 0, na.rm = TRUE),
            negative_count = sum(net_DR_margin_2020 < 0, na.rm = TRUE))

sum(temp$positive_count) # 559 dem
sum(temp$negative_count) # 2596 rep





# 2016 MANIPULATIONS ----------------------------------------------------------------


# 2016
voting_2016_wide <- voting_as_wide %>%
  filter(year == 2016) %>%
  rename(totalvotes_2016 = "totalvotes") %>%
  group_by(state,
           state_po,
           state_county,
           county_name,
           county_fips,
           totalvotes_2016) %>%
  summarise(aggvotes_D_2016 = sum(DEMOCRAT, na.rm = TRUE),
            aggvotes_R_2016 = sum(REPUBLICAN, na.rm = TRUE),
            aggvotes_G_2016 = sum(GREEN, na.rm = TRUE),
            aggvotes_L_2016 = sum(LIBERTARIAN, na.rm = TRUE),
            aggvotes_O_2016 = sum(OTHER, na.rm = TRUE))


minvotes_2016 <- min(voting_2016_wide$totalvotes_2016)
maxvotes_2016 <- max(voting_2016_wide$totalvotes_2016)

voting_DR_margins_2016 <- voting_2016_wide %>%
  mutate(
    proportion_D_2016 = aggvotes_D_2016 / totalvotes_2016,  # Democratic proportion
    proportion_R_2016 = aggvotes_R_2016 / totalvotes_2016,  # Republican proportion
    proportion_explained_by_DR_2016 = proportion_D_2016 + proportion_R_2016,
    net_DR_margin_2016 = proportion_D_2016 - proportion_R_2016) %>% # Net margin: positive = Dem, negative = Rep
  mutate(
    norm_votes_2016 = (totalvotes_2016 - minvotes_2016) / 
      (maxvotes_2016 - minvotes_2016),
    lognorm_votes_2016 = log(totalvotes_2016 + 1)) %>%
  filter(county_name != "STATEWIDE WRITEIN")



# JOINING 2016 and 2020 to Index DF -------------------------------------


voting_DR_margins_joined <- voting_DR_margins_2020 %>%
  ungroup() %>%
  select(-state,
         -state_po,
         -state_county,
         -county_name) %>%
  right_join(counties_only_all, by = "county_fips") 


voting_DR_margins_joined <- voting_DR_margins_2016 %>%
  ungroup() %>%
  select(-state,
         -state_po,
         -state_county,
         -county_name) %>%
  right_join(voting_DR_margins_joined, by = "county_fips") 




# PLOTS --------------------------------------------------------------------
# scatter 2020 margin vs 2016 margin
voting_DR_margins_joined %>%
  ggplot(aes(x = net_DR_margin_2020, 
             y = net_DR_margin_2016)) +
  geom_point(alpha = 0.6,
             color = "#b85abf") +  
  theme(panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "D-R Vote Margin 2020 vs 2016", 
       x = "Net D-R Margin 2020", 
       y = "Net D-R Margin 2016")

ggsave(here("plots/scatter_margins_logvotes_2020.png"),
       dpi = 300)



# 2020 plots ----------------------------------------------------------------------------
# Basic histogram of 2020 margins 
ggplot(voting_DR_margins_joined, 
       aes(x = net_DR_margin_2020, 
           fill = net_DR_margin_2020 > 0)) +
  geom_histogram(binwidth = 0.04, color = "black") +
  labs(title = "Distribution of Net Vote Margin - 2020", x = "Net Vote Margin (D-R)", y = "Frequency") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.25))

ggsave(here("plots/histogram_netmargin_2020.png"),
       dpi = 300)




# scatter 2020 margin vs log total votes
voting_DR_margins_2020 %>%
ggplot(aes(x = lognorm_votes_2020, 
           y = net_DR_margin_2020, 
           color = net_DR_margin_2020 > 0 )) +
  geom_point(alpha = 0.6, 
             size = 1) +  
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "Net Vote Margin vs. County Population - 2020", 
       x = "Log of Total Votes in County", 
       y = "Net Vote Margin (D-R)")

ggsave(here("plots/scatter_margins_logvotes_2020.png"),
       dpi = 300)









slice_prop_exp_DR_2020_lowest <- voting_DR_margins_2020 %>%
  arrange(proportion_explained_by_DR_2020) %>%  # Sort data by x_column (replace with your x variable)
  head(20)  # Select the 20 rows with the smallest x-values

# scatter 2020 DR margin vs proportion of totalvotes explained by DR
voting_DR_margins_2020 %>%
  ggplot(aes(x = proportion_explained_by_DR_2020, 
             y = net_DR_margin_2020 )) +
  geom_point(alpha = 0.6,
             color = "blue") +  
  geom_text_repel(data = slice_prop_exp_DR_2020_lowest, 
                  aes(label = state_county)) +
  theme(panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "D-R Vote Margin v Margin Explained by DR Votes - 2020", 
       x = "Proportion Explained by DR Votes", 
       y = "Net Vote Margin (D-R)")

ggsave(here("plots/scatter_margins_propexplainedbyDR_2020.png"),
       dpi = 300)









# 2016 plots ----------------------------------------------------------------------------
# Basic histogram of 2016 margins 
ggplot(voting_DR_margins_joined, 
       aes(x = net_DR_margin_2016, 
           fill = net_DR_margin_2016 > 0)) +
  geom_histogram(binwidth = 0.04, color = "black") +
  labs(title = "Distribution of Net Vote Margin - 2016", x = "Net Vote Margin (D-R)", y = "Frequency") +
  theme_minimal()  +
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.25))

ggsave(here("plots/histogram_netmargin_2016.png"),
       dpi = 300)




# scatter 2016 margin vs log total votes
voting_DR_margins_2016 %>%
  ggplot(aes(x = lognorm_votes_2016, 
             y = net_DR_margin_2016, 
             color = net_DR_margin_2016 > 0 )) +
  geom_point(alpha = 0.6, 
             size = 1) +  
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
        panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "Log of Total Votes in County vs. Vote Margin - 2016", 
       x = "Log of Total Votes in County", 
       y = "Net D-R Vote Margin")

ggsave(here("plots/scatter_margins_logvotes_2016.png"),
       dpi = 300)







