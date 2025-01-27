
library(tidyverse)    
library(sf)            
library(tigris)        
library(tidycensus)
library(googlesheets4)
library(tidygeocoder)

source("NILRA_config_utils.R")

## load/create files
# from ICE
if (file.exists("data/ice_facilities.RDS")) {
  ice_facilities <- readRDS("data/ice_facilities.RDS")
} else {
  ice_facilities <- read_csv("data/ice_facilities.csv") |>
    mutate(addr = paste0(Address, ", ", City, ", ", State, " ", Zip)) |>
    rename(name = Name) |>
    mutate(adp = rowSums(across(`Level A`:`Level D`))) |>
    rename(type_detailed = `Type Detailed`,
           state = State) |>
    geocode(addr, method = 'arcgis',
            lat = latitude , long = longitude) |> 
    select(name, addr, state, latitude, longitude, type_detailed)
  
  saveRDS(ice_facilities,
          "data/ice_facilities.RDS")
}

# from FOIA data
if (file.exists("data/all.csv")) {
  all <- read_csv("data/all.csv")
} else {
  source("detentions19-23.R")
}

# from Vera
vera_facilities <- read_csv("data/facilities_vera.csv") |> 
  mutate(name = toupper(detention_facility_name)) |> 
  filter(!is.na(latitude)) |> 
  select(name, city, state, latitude, longitude, type_detailed)

# from Torn Apart - https://xpmethod.columbia.edu/torn-apart/credits.html
if (file.exists("data/tornapart_geo.csv")) {
  tornapart_geo <- read_csv("data/tornapart_geo.csv")
} else {
  tornapart_geo <- read_csv("data/tornapart.csv") |> 
    mutate(name = toupper(Name),
           addr = toupper(Address)) |> 
    rename(state_name = State) |> 
    left_join(st_fips_codes, by = "state_name") |> 
    select(name, addr, state) |> 
    geocode(addr, method = 'arcgis',
            lat = latitude , long = longitude)
  
  write_csv(tornapart_geo, "data/tornapart_geo.csv")
}

# create lists of detention places
if (file.exists("data/fulllist.csv") &
    file.exists("data/fulllist_geo.csv") &
    file.exists("data/fulllist_no_geo.csv")) {
  fulllist <- read_csv("data/fulllist.csv")
  fulllist_geo <- read_csv("data/fulllist_geo.csv")
  fulllist_no_geo <- read_csv("data/fulllist_no_geo.csv")
} else {
  fulllist <- all |> 
    full_join(ice_facilities, by = "name") |> 
    full_join(vera_facilities, by = "name",
              suffix = c(".ice", ".vera")) |> 
    full_join(tornapart_geo, by = "name",
              suffix = c(".ice", ".tornapart")) |> 
    rename(city.vera = city,
           latitude.tornapart = latitude,
           longitude.tornapart = longitude,
           state.tornapart = state) |> 
    rowwise() |> 
    mutate(resolved = list(resolve_state(state.ice, state.vera, state.tornapart)),
           state = resolved[[1]],
           flag = as.logical(resolved[[2]])) |> 
    select(!c(resolved, state.ice, state.vera, state.tornapart)) |> 
    distinct()

  fulllist_no_geo <- fulllist |> 
    filter(is.na(addr.ice) & 
             is.na(addr.tornapart) & 
             is.na(latitude.ice) & 
             is.na(latitude.vera) & 
             is.na(latitude.tornapart)) |> 
    select(name, mean_n)
  
  fulllist_geo <- fulllist |> 
    filter(!name %in% fulllist_no_geo$name,
           flag == FALSE) |> 
    select(name, mean_n, type_detailed.ice, type_detailed.vera, city.vera, state) |> 
    rename(city = city.vera) |> 
    distinct()
  
  write_csv(fulllist, "data/fulllist.csv")
  write_csv(fulllist_geo, "data/fulllist_geo.csv")
  write_csv(fulllist_no_geo, "data/fulllist_no_geo.csv")
}

# create list for interactive map
if (file.exists("data/fulllist_map_no_sf.RDS")) {
  fulllist_map_no_sf <- readRDS("data/fulllist_map_no_sf.RDS")
} else {
  fulllist_map_no_sf <- fulllist |> 
    mutate(latitude = ifelse(is.na(latitude.ice), 
                             ifelse(is.na(latitude.vera), 
                                    latitude.tornapart, 
                                    latitude.vera),
                             latitude.ice)) |> 
    mutate(longitude = ifelse(is.na(longitude.ice), 
                              ifelse(is.na(longitude.vera), 
                                     longitude.tornapart, 
                                     longitude.vera),
                              longitude.ice)) |> 
    mutate(type = ifelse(is.na(type_detailed.ice), 
                         type_detailed.vera,
                         type_detailed.ice)) |> 
    select(name, mean_n, latitude, longitude, type, state) |> 
    distinct() |> 
    drop_na(latitude) 
  
  saveRDS(fulllist_map_no_sf, "data/fulllist_map_no_sf.RDS")
}

# create list for static map
if (file.exists("data/fulllist_map.RDS")) {
  fulllist_map <- readRDS("data/fulllist_map.RDS")
} else {
  fulllist_map <- fulllist |> 
    mutate(latitude = ifelse(is.na(latitude.ice), 
                             ifelse(is.na(latitude.vera), 
                                    latitude.tornapart, 
                                    latitude.vera),
                             latitude.ice)) |> 
    mutate(longitude = ifelse(is.na(longitude.ice), 
                              ifelse(is.na(longitude.vera), 
                                     longitude.tornapart, 
                                     longitude.vera),
                              longitude.ice)) |> 
    mutate(type = ifelse(is.na(type_detailed.ice), 
                         type_detailed.vera,
                         type_detailed.ice)) |> 
    select(name, mean_n, latitude, longitude, type, state) |> 
    distinct() |> 
    drop_na(latitude) |> 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs("EPSG:4326")) |> 
    filter(!str_detect(name, "HAGATNA"),
           !str_detect(name, "HONOLULU"),
           !str_detect(name, "SAIPAN"),
           !str_detect(name, "GUAYNABO"),
           name != "SAN JUAN STAGING",
           state != "AK",
           state != "HI",
           state != "GU",
           state != "PR",
           state != "MP",
           state != "VI") |> 
    group_by(type) |> 
    mutate(type_label = paste0(type, ' (', n(), ')'),
           count = n())
  
  saveRDS(fulllist_map, "data/fulllist_map.RDS")
}