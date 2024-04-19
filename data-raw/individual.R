## code to prepare `individual` dataset goes here
## Setup----
library(dplyr)
# This will execute a code within a  file
source(here::here("R", "geolocate.R"))  

## Combine individual tables ----
## Create paths to inputs
raw_data_path <- here::here("data-raw", "wood-survey-data-master")

individual_paths <- fs::dir_ls(fs::path(raw_data_path, "individual"))


# read in all individual tables into one
individual <- purrr::map(
  individual_paths,
  ~ readr::read_csv(
    file = .x,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
) %>%
  purrr::list_rbind() %>%
  readr::type_convert()

individual %>%
  readr::write_csv(file = fs::path(raw_data_path, "vst_individuals.csv"))

# Combine NEON data tables ----
# read in additional tables

maptag <- readr::read_csv(
  fs::path(raw_data_path, "vst_mappingandtagging.csv")
) %>%
  select(-eventID)

perplot <- readr::read_csv(
  fs::path(raw_data_path, "vst_perplotperyear.csv"),
  show_col_types = FALSE
)%>%
  select(-eventID)
 

# Left join tables to individual
individual%<>%
  left_join(maptag, by = "individualID",
            suffix = c("", "_map")) %>% 
  left_join(perplot, by = "plotID",
            suffix = c("", "_ppl")) %>%
  assertr::assert(
    assertr::not_na, stemDistance,stemAzimuth, pointID,
    decimalLatitude, decimalLongitude
  )                      

## Geolocate individuals ----
individual <- individual %>%
  dplyr::mutate(
    stemLat = get_stem_location(             # Extracting from Latitude
      decimalLongitude = decimalLongitude,   # the return of our function was a 
      decimalLatitude = decimalLatitude,     # a tibble with 2 columns, we have 
      stemAzimuth = stemAzimuth,             # to transform that into a single
      stemDistance = stemDistance            # column.
    )$lat,
    stemLon = get_stem_location(             # Extracting from Longitude
      decimalLongitude = decimalLongitude,
      decimalLatitude = decimalLatitude,
      stemAzimuth = stemAzimuth,
      stemDistance = stemDistance
    )$lon
  ) %>%
  janitor::clean_names()                     # all the column names have changed,
                                             # no capital letters and they are
                                             # separated by _
                               
              
fs::dir_create("data") # Creates a data directory in the route of our project


individual %>%
  readr::write_csv(
    here::here("data", "individual.csv")
  )
