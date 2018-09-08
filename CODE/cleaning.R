## A suite of functions for cleaning and merging datasets.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/CODE/convenience.R"))

##################
#### CLEANING ####
clean_311_service_requests <- function(raw) {
  raw
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
  raw
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
make_design_matrix.raw <- function(dir = "DATA") {
  dir <- glue("{here()}/{dir}/")
  service_requests <- read_csv(dir + "") %>% clean_311_service_requests()
  food_venues <- read_csv(dir + "") %>% clean_food_venues()
  food_inspections <- read_csv(dir + "") %>% clean_food_inspections()
  clean_health_indicators <- read_csv(dir + "") %>% clean_health_indicators()
  clean_county_demographics <- read_csv(dir + "") %>% clean_county_demographics()
  design_matrix <-
    list(food_venues, food_inspections) %>% reduce(left_join, by = c("latitude", "longitude"))
}
#################
