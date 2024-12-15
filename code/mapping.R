# mapping counties

# libraries
library(tidyverse)
library(here)
library(sf) # shapefile manipulation
library(tigris)  # For county shapefiles


here::i_am("code/mapping.R")

# load data ---------------------------------------------------------------

utah_counties <- read_csv(here("data/results/votecensus_2020_clustered_slice_largemargins.csv")) %>%
  filter(kmeans_labels == 4) %>%
  filter(dbscan_labels == 2) %>%
  mutate(county_fips = as.character(county_fips)) %>%
  mutate(dbscan_labels = as.character(dbscan_labels))


# set up SFs object and join -------------------------------------------------------

options(tigris_use_cache = TRUE)  # Cache shapefiles for faster access
sf_counties <- tigris::counties(cb = TRUE, year = 2020) %>%
  st_as_sf() %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP))


# Join with your data
utah_mappable <- sf_counties %>%
  filter(STATE_NAME == "Utah") %>%
  left_join(utah_counties, by = "county_fips")


# plot on map ----------------------------------------------------------------------
ggplot(data = utah_mappable) +
  geom_sf(aes(fill = dbscan_labels,
              geometry = geometry), color = "white", size = 0.1) +
  scale_fill_manual(values = c("2" = "#fc7303", 
                               "3" = "#fcb503")) +  # Set specific colors
  #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +  # continuous fill
  theme_minimal() +
  labs(title = "County-Level Data Map",
       fill = "DBScan Labels") +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(here("plots/utah_dbs2.png"),
       dpi = 300)









































# load data ---------------------------------------------------------------

south_counties <- read_csv(here("data/results/south_mappable.csv")) %>%
  mutate(county_fips = as.character(county_fips)) 


# set up SFs object and join -------------------------------------------------------

options(tigris_use_cache = TRUE)  # Cache shapefiles for faster access
sf_counties <- tigris::counties(cb = TRUE, year = 2020) %>%
  st_as_sf() %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP))


# Join with your data
south_mappable <- sf_counties %>%
  filter(STATE_NAME %in% c("Georgia", "North Carolina" ,"South Carolina","Alabama")) %>%
  left_join(south_counties, by = "county_fips")


# plot on map ----------------------------------------------------------------------
ggplot(data = south_mappable) +
  geom_sf(aes(fill = DR,
              geometry = geometry), color = "white", size = 0.1) +
  scale_fill_manual(values = c("D" = "#386aff", 
                               "R" = "#de4d43")) +  # Set specific colors
  #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +  # continuous fill
  theme_minimal() +
  labs(title = "County-Level Data Map",
       fill = "DBScan Labels") +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(here("plots/south_dr.png"),
       dpi = 300)





































# load data ---------------------------------------------------------------

pnw_counties <- read_csv(here("data/results/pnw_mappable.csv")) %>%
  mutate(county_fips = as.character(county_fips)) 


# set up SFs object and join -------------------------------------------------------

options(tigris_use_cache = TRUE)  # Cache shapefiles for faster access
sf_counties <- tigris::counties(cb = TRUE, year = 2020) %>%
  st_as_sf() %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP))


# Join with your data
pnw_mappable <- sf_counties %>%
  filter(STATE_NAME %in% c("California", "Washington" ,"Oregon")) %>%
  left_join(pnw_counties, by = "county_fips")


# plot on map ----------------------------------------------------------------------
ggplot(data = pnw_mappable) +
  geom_sf(aes(fill = dummy,
              geometry = geometry), color = "white", size = 0.1) +
  scale_fill_manual(values = c("one" = "#73f065")) +  # Set specific colors
  #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +  # continuous fill
  theme_minimal() +
  labs(title = "County-Level Data Map",
       fill = "DBScan Labels") +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(here("plots/pnw_dummy.png"),
       dpi = 300)





