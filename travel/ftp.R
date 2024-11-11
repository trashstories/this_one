library(tidyverse)     # ggplot, dplyr, and friends
library(jsonlite)      # Read JSON files
library(sf)            # Handle spatial data in a tidy way
library(tigris)        # Access deographic data from the US Census
library(ggrepel)       # Nicer non-overlapping labels
library(glue)          # Easier string interpolation
library(scales)        # Nicer labeling functions
library(patchwork)     # Combine plots nicely
library(ggspatial)     # Nicer map features like scale bars
library(leaflet)       # Make interactive maps
library(lutz)          # Look up time zones for lat/lon coordinates
library(gt)            # Make fancy tables
library(rcartocolor)   # Use CARTO colors (https://carto.com/carto-colors/)
library(ggnewscale)    # Use multiple scales for the same aesthetic in ggplot
library(extrafont)
library(tibbletime)
library(postmastr)
library(colorspace)

prisons_ <- read_csv("Prison_Boundaries.csv")

# load county zip code lookup table and fips codes
fips_codes <- fips_codes %>% 
  mutate(fips_county = paste0(state_code, county_code))

zip_lookup <- read_csv("zip_county.csv") %>% 
  rename(fips_county = COUNTY) %>% 
  left_join(fips_codes, by = "fips_county") %>% 
  select(ZIP, fips_county, county)

# pull in county labels
prisons <- prisons_ %>% 
  left_join(zip_lookup, by = "ZIP", 
            relationship = "many-to-many", 
            multiple = "first") %>%
  mutate(county = trimws(gsub("\\s+\\w*$", "", county), 
                         which = "both"))

write_csv(prisons, "Prison_Boundaries_w_county.csv")
