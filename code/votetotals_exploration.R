# libraries -------------------------------------------------------------------

# loading
library(tidyverse)
library(here)

# set directory
here::i_am("code/votetotals_exploration.R")

countypres_raw <- read_csv(here("data/countypres_2000-2020.csv"))

# USING FIPS ----------------------------------------------------------------

voting_as_wide <- countypres_raw %>%
  pivot_wider(names_from = party, 
              values_from = candidatevotes)

voting_2020_wide <- voting_as_wide %>%
  filter(year == 2020) %>%
  group_by(state,
           state_po,
           county_name,
           county_fips,
           totalvotes) %>%
  summarise(aggvotes_D = sum(DEMOCRAT, na.rm = TRUE),
            aggvotes_R = sum(REPUBLICAN, na.rm = TRUE),
            aggvotes_G = sum(GREEN, na.rm = TRUE),
            aggvotes_L = sum(LIBERTARIAN, na.rm = TRUE),
            aggvotes_O = sum(OTHER, na.rm = TRUE))
  

minvotes <- min(voting_2020_wide$totalvotes)
maxvotes <- max(voting_2020_wide$totalvotes)

voting_DR_margins <- voting_2020_wide %>%
  mutate(
    proportion_D = aggvotes_D / totalvotes,  # Democratic proportion
    proportion_R = aggvotes_R / totalvotes,  # Republican proportion
    net_DR_margin = proportion_D - proportion_R) %>% # Net margin: positive = Dem, negative = Rep
  mutate(
    norm_votes = (totalvotes - minvotes) / 
      (maxvotes - minvotes),
    lognorm_votes = log(totalvotes + 1))


`


plt_DRmargins_logvotes <- voting_DR_margins %>%
ggplot(aes(x = lognorm_votes, 
           y = net_DR_margin )) +
  geom_point(alpha = 0.6,
             color = "blue") +  
  theme(panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major grid lines
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25))+  # Minor grid lines
  labs(title = "Vote Margin v Totals", 
       x = "total votes in county", 
       y = "net margin")

ggsave(here("plots/scatter_margins_logvotes.png"),
       plot = plt_DRmargins_logvotes,
       dpi = 300)






