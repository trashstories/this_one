# ## load FIPS codes
fips_codes <- fips_codes

# state fips codes
st_fips_codes <- fips_codes |> 
  group_by(state, state_name) |> 
  summarise(n = n())

# county fips codes
cty_fips_codes <- fips_codes |> 
  mutate(county_name = gsub(" County", "", county),
         fips_county = paste0(state_code, county_code))

# ACJ brand colors
colors <- c("#174337", "#F17B00", "#08BFBF", "#098989", "#EAAA1B", 
            "#C620C2", "#6D0EC1", "#F9DD2F", "#F25430")
more_colors <- c(colors, lighten(colors, .5), lighten(colors, .75), 
                 lighten(colors, .25), lighten(colors, .9))
whites <- c("white", "white", "white", "white", "white", "white", "white", "white", 
            "white", "white", "white", "white", "white", "white", "white", "white", 
            "white", "white", "white", "white", "white", "white", "white", "white", 
            "white", "white", "white", "white", "white", "white", "white", "white", 
            "white", "white", "white", "white", "white", "white", "white", "white")

# formatters for labels
my_comma <- label_number(accuracy = 1, big.mark = ",")
my_percent <- label_number(accuracy = .01, scale = 100, suffix = "%")
my_percent2 <- label_number(accuracy = .1, scale = 1, suffix = "%")

# ACJ theme  
theme_acj <- function() {
  theme_light(base_family = "Besley") +
    theme(plot.title = element_text(color = "#0B320C", 
                                    size = 14, 
                                    face = "bold", 
                                    family = "Besley"),
          plot.subtitle = element_text(color = "#0B320C", 
                                       size = 13, 
                                       family = "serif"),
          axis.text = element_text(color = "#0B320C", 
                                   family = "Public Sans"),
          axis.title = element_text(color = "#0B320C", 
                                    family = "Besley"),
          panel.border = element_blank(),
          panel.grid.major = element_line(color = "#F9DD2F", 
                                          linewidth = .35, 
                                          linetype = "longdash"),
          panel.grid.minor = element_line(color = "#F9DD2F", 
                                          linewidth = .35, 
                                          linetype = "dotted"),
          axis.ticks = element_blank(),
          legend.text = element_text(color = "#0B320C", 
                                     family = "Public Sans"))
  
}

theme_acj_map <- function() {
  theme_void(base_family = "Public Sans") +
    theme(
      plot.background = element_rect(fill = "white",
                                     color = "white"),
      plot.title = element_text(family = "Besley", face = "bold"),
      strip.text = element_text(
        family = "Besley Bold", face = "plain",
        size = rel(1.1), hjust = 0.5)
    )
}


# Function to resolve state and flag inconsistencies
resolve_state <- function(ice, vera, tornapart) {
  states <- c(ice, vera, tornapart) %>% na.omit() %>% unique()
  
  if (length(states) == 0) {
    return(c(state = NA, flag = FALSE))
  } else if (length(states) == 1) {
    return(c(state = states, flag = FALSE))
  } else {
    return(c(state = "FLAG", flag = TRUE))
  }
}

# Function to resolve lat and long and flag inconsistencies
resolve_lat <- function(ice, vera, tornapart) {
  lats <- c(ice, vera, tornapart) %>% na.omit() %>% unique()
  
  if (length(lats) == 0) {
    return(c(latitude = NA, flag = FALSE))
  } else if (length(lats) == 1) {
    return(c(latitude = lats, flag = FALSE))
  } else {
    return(c(latitude = "FLAG", flag = TRUE))
  }
}

resolve_long <- function(ice, vera, tornapart) {
  lats <- c(ice, vera, tornapart) %>% na.omit() %>% unique()
  
  if (length(longs) == 0) {
    return(c(longitude = NA, flag = FALSE))
  } else if (length(longs) == 1) {
    return(c(longitude = longs, flag = FALSE))
  } else {
    return(c(longitude = "FLAG", flag = TRUE))
  }
}

## create shapefiles for LSP boundaries
# LSP geo data
lsps <- read_csv("data/lsp_rr.csv")

# shapefiles
us_states <- states(resolution = "20m",
                    year = 2022, cb = TRUE) |>
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) |>
  rename(state = STUSPS)

us_counties <- counties(resolution = "20m",
                        year = 2022, cb = TRUE) %>%
  rename(fips_county = GEOID) %>%
  rename(county = NAME,
         state = STUSPS) |> 
  mutate(county = ifelse(state == "MD" & NAMELSAD == "Baltimore city",
                         "Baltimore City", county))

lsp_no_states <- lsps |> 
  pivot_longer(AL:WY,
               names_to = "state",
               values_to = "counties") |> 
  filter(!is.na(counties),
         counties != "All counties") |> 
  select(!states) |> 
  separate_longer_delim(counties, ",") |> 
  rename(county = counties)

lsp_counties <- us_counties %>% 
  left_join(lsp_no_states, 
            by = c("state", "county"),
            relationship = "one-to-many") |>
  drop_na(org) |> 
  group_by(org) |> 
  summarise(geometry = sf::st_union(geometry)) |> 
  ungroup()

whole_states <- lsps |> 
  pivot_longer(AL:WY,
               names_to = "state",
               values_to = "counties") |> 
  filter(counties == "All counties") |> 
  select(org, state)

lsp_states <- us_states %>% 
  left_join(whole_states, by = "state") |>
  drop_na(org) |> 
  group_by(org) |> 
  summarise(geometry = sf::st_union(geometry)) |> 
  ungroup()

lsp_boundaries <- lsp_states |> 
  rbind(lsp_counties) |> 
  group_by(org) |> 
  summarise(geometry = sf::st_union(geometry)) |> 
  ungroup()

## create shapefiles for ERO boundaries
# load ERO data
field_offices <- read_csv("data/field_offices.csv") |> 
  rename(state = abbr_name)

# CA and TX county data (manually created)
gs4_deauth()
tx_ca <- read_sheet("1aPftRpzR-c8RqqwdFUVJ520TD9m3FZEh3C0z46HzvxA", sheet = "tx_ca")

# combining data for field office areas
state_ero <- us_states |> 
  left_join(field_offices, by = "state") |> 
  drop_na(field_office) |> 
  mutate(field_office = factor(field_office)) |> 
  group_by(field_office) |> 
  summarise(geometry = sf::st_union(geometry)) |> 
  ungroup()

ero_boundaries <- us_counties %>% 
  left_join(tx_ca, by = c("state", "county"), 
            relationship = "one-to-one") |> 
  rename(field_office = ero) |> 
  group_by(field_office) |> 
  summarise(geometry = sf::st_union(geometry)) |> 
  ungroup() |> 
  rbind(state_ero)
