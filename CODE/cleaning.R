## A suite of functions for cleaning and merging datasets.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/CODE/convenience.R"))
source(glue("{here()}/CODE/text.R"))

##################
#### CLEANING ####
clean_311_service_requests <- function(raw) {
  raw %>%
    transmute(key = as.character(unique_key),
              created = mdy_hms(created_date),
              closed = mdy_hms(closed_date),
              due = mdy_hms(due_date),
              resolved = mdy_hms(resolution_date),
              agency = as.factor(agency_acronym),
              agency_name = as.character(agency_name),
              complaint_type = as.factor(complaint_type),
              description = as.character(descriptor),
              location_type = as.factor(location_type),
              zip = as.character(incident_zip),
              address = as.character(incident_address),
              street = as.character(street_name),
              city = as.character(city),
              status = as.factor(status),
              board = as.character(community_board),
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

clean_food_inspections <- function(raw) {
  raw
}

clean_health_indicators <- function(raw) {
  year_tuple <- raw %$% str_match(year, "(\\d+)-(\\d+)")
  lat_lon = raw %$% str_match(lat_long, "([\\d\\.\\-]+), ([\\d\\.\\-]+)")
  raw %>%
    transmute(county = as.factor(county_name),
              topic = as.factor(health_topic),
              indicator = as.character(topic),
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
  raw
}

clean_geo <- function(raw) {
  raw
}
##################

#################
#### JOINING ####
make_design_matrix.raw <- function(dir = "DATA/Datathon Materials") {
  dir <- glue("{here()}/{dir}/")
  service_requests <- read_csv(dir + "311_service_requests.csv.gz") %>% clean_311_service_requests()
  food_venues <- read_csv(dir + "") %>% clean_food_venues()
  food_inspections <- read_csv(dir + "") %>% clean_food_inspections()
  health_indicators <- read_csv(dir + "community_health.csv") %>% clean_health_indicators()
  county_demographics <- read_csv(dir + "") %>% clean_county_demographics()
  design_matrix <-
    list(food_venues, food_inspections) %>% reduce(left_join, by = c("latitude", "longitude"))
}
#################
