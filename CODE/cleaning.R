## A suite of functions for cleaning and merging datasets.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/CODE/convenience.R"))
source(glue("{here()}/CODE/text.R"))
library(sf)

##################
#### CLEANING ####
clean_311_service_requests <- function(raw) {
  raw %>%
    transmute(key = as.character(unique_key),
              created = mdy_hms(created_date),
              ## closed = mdy_hms(closed_date),
              ## due = mdy_hms(due_date),
              ## resolved = mdy_hms(resolution_date),
              ## agency = as.factor(agency_acronym),
              ## agency_name = as.character(agency_name),
              complaint_type = as.factor(complaint_type),
              description = as.character(descriptor),
              location_type = as.factor(location_type),
              ## zip = as.character(incident_zip),
              ## address = as.character(incident_address),
              ## street = as.character(street_name),
              ## city = as.character(city),
              ## status = as.factor(status),
              ## board = as.character(community_board),
              borough = as.factor(borough),
              lat = as.numeric(latitude),
              lon = as.numeric(longitude))
}

clean_food_venues <- function(raw) {
  raw %>% mutate(key = as.character(business_name),
                 street_address = as.character(street_address),
                 city = as.character(city),
                 state = as.character(state),
                 country = as.character(country),
                 latitude = as.numeric(latitude),
                 longitude = as.numeric(longitude),
                 categories = as.character(categories),
                 type = as.character(type))
}

clean_nta_demographics <- function(raw) {
  raw %>%
    transmute(key = as.character(nta_code),
              nta_name = as.character(nta_name),
              population = as.integer(population),
              people_per_acre = as.numeric(people_per_acre),
              households = as.integer(households),
              median_income = as.numeric(population),
              mean_income = as.numeric(population))
}

clean_geo <- function(raw) {
  get_odd <- function(vec) {vec[(1:length(vec)) %% 2 == 1]}
  get_even <- function(vec) {vec[(1:length(vec)) %% 2 == 0]}
  shapefile <-
    data_frame(polygon =
                 raw %>% names(),
               points =
                 raw %>% as.list() %>%
                 map(na.omit) %>%
                 {map2_chr(get_odd(.), get_even(.), ~ glue("({.x},{.y})"))})
}

clean_health_indicators <- function(raw) {
  year_tuple <- raw %$% str_match(year, "(\\d+)-(\\d+)")
  lat_lon = raw %$% str_match(lat_long, "([\\d\\.\\-]+), ([\\d\\.\\-]+)")
  raw %>%
    transmute(county = as.factor(county_name),
              topic = as.factor(health_topic),
              indicator = as.character(indicator),
              count = as.integer(event_count),
              denom = as.integer(avg_num_of_denominator),
              unit = as.factor(unit_measurement),
              value = as.numeric(precent_or_rate),
              start_year = as.numeric(year_tuple[,2]),
              end_year = as.numeric(year_tuple[,3]),
              lat = as.numeric(lat_lon[,2]),
              lon = as.numeric(lat_lon[,3]))
}

clean_county_demographics <- function(raw) {
  raw %>%
    mutate(borough = geography %>%
             recode("Richmond County, New York" = "Staten Island",
                    "New York County, New York" = "Manhattan",
                    "Bronx County, New York" = "Bronx",
                    "Kings County, New York" = "Brooklyn",
                    "Queens County, New York" = "Queens",
                    "New York" = "New York (state)",
                    .default = NA_character_) %>%
                as.factor(),
           population = as.integer(population),
           prop_w_health_insurance = pop_w_health_insurance / population,
           prop_w_private_health = pop_w_private_health / population,
           prop_w_public_health = pop_w_public_coverage / population,
           prop_w_no_health = pop_no_health_insurance / population) %>%
    select(- geography) %>%
    drop_na(borough) %>%
    select(borough, year,
           total_households, population,
           ends_with("household_income"), starts_with("mean"),
           food_stamp_benefits,
           starts_with("prop"))
}

clean_county_demographics_brackets <- function(raw) {
  raw %>%
    mutate(borough = geography %>%
             recode("Richmond County, New York" = "Staten Island",
                    "New York County, New York" = "Manhattan",
                    "Bronx County, New York" = "Bronx",
                    "Kings County, New York" = "Brooklyn",
                    "Queens County, New York" = "Queens",
                    "New York" = "New York (state)",
                    .default = NA_character_) %>%
             as.factor()) %>%
    select(- geography) %>%
    drop_na(borough) %>%
    gather(income_bracket, households, starts_with("$")) %>%
    mutate(income_bracket = as.factor(income_bracket),
           proportion = households / total_households) %>%
    select(borough, year,
           income_bracket, proportion, households, total_households,
           median_household_income, mean_household_income, mean_earnings)
}
##################

#################
#### JOINING ####
make_design_matrix.raw <- function(dir = "DATA/Datathon Materials") {
  dir <- glue("{here()}/{dir}/")
  service_requests <- read_csv(dir + "311_service_requests.csv.gz") %>% clean_311_service_requests()
  food_venues <- read_csv(dir + "food_venues.csv.gz") %>% clean_food_venues()
  health_indicators <- read_csv(dir + "community_health.csv") %>% clean_health_indicators()
  demographics <- read_csv(dir + "demographics_city.csv") %>% clean_nta_demographics()
  design_matrix <-
    list(food_venues, food_inspections) %>% reduce(left_join, by = c("latitude", "longitude"))
}
#################
