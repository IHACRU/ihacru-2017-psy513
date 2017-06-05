# knitr::stitch_rmd(script="./manipulation/0-ellis-map.R", output="./manipulation/stitched-output/0-ellis-map.md")
# This script reads two files: encounter counts with location mapping and encounter timelines for selected individuals
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
# ---- load-packages -----------------------------------------------------------
library(ggplot2)  # graphing
library(dplyr)    # data wrandling
library(magrittr) # pipes

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE) # colors in graphs
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
path_input <- "./data-unshared/raw/Location_Map_Detailed_2017-05-23.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto_location_map"
# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object definitions

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
replace_with_na <- function(x){
  # x <- ds$facility_name
  na_tokens <- c(
    "^NULL$"
    ,"^-$"
    ,"^NA$"
    ,"^\\{blank\\}$"
    ,"^n/a$"
    ,"\\{Unknown\\}"
    ,"\\{Undefined\\}"
  )
  for(token in na_tokens){
    if(is.character(x)){
      x <- gsub(token,NA,x)
    }
  }
  return(x)
}
# ds %>% count_unique_program_ids()
# ---- load-data ---------------------------------------------------------------
ds <- readr::read_csv(path_input) %>% as.data.frame() 
ds <- read.csv(path_input,
               stringsAsFactors=FALSE)
# ---- tweak-data -------------------------------------------------------------
# reproduction in SRE causes the name of the first variable to have a prefex <U+FEFF>
names(ds)[1] <- "location_map_id" # correct for UTF-8-BOM
# standardize names : remove capital letters
colnames(ds) <- tolower(colnames(ds))
ds %>% glimpse()  
# Some variables have different character codes for missing values
# Translate various character values into NA values using 
# the function replace_with_na() defined above
ds <- ds %>% 
  dplyr::mutate_all(dplyr::funs(replace_with_na))
ds <- ds
# 
ds %>% distinct(site_name) %>% slice(20:30)

# ---- inspect-data ------------------------------------------
# count unique values in each column
ds %>% dplyr::summarise_all(n_distinct) %>% t()

### Count unique combinations ###
ds %>% count_unique_addresses()
ds %>% count_unique_addresses(keys=T)
ds %>% count_unique_classes()
# number of unique categories in each varaible
ds %>% dplyr::summarize_all(n_distinct) %>% t()

### Close up inspection of each location descriptor ###
# inspect categories in CERNER address
ds %>% group_by(site_name) %>% count() %>% print(n=nrow(.))
ds %>% group_by(facility_name) %>% count()%>% print(n=nrow(.))
ds %>% group_by(building_name) %>% count()%>% print(n=nrow(.))
ds %>% group_by(unit_name) %>% count()%>% print(n=50)
# inspect categories in DataWarehouse address
ds %>% group_by(location_category) %>% count()%>% print(n=nrow(.))
ds %>% group_by(location_grouping) %>% count() %>% print(n=nrow(.))
ds %>% group_by(location_type) %>% count() %>% print(n=nrow(.))
# inspect categories for compressors that comprise the classifier
ds %>% group_by(intensity_type) %>% count() %>% print(n=nrow(.))
ds %>% group_by(intensity_severity_risk) %>% count() %>% print(n=nrow(.))
ds %>% group_by(clinical_focus) %>% count() %>% print(n=nrow(.))
ds %>% group_by(service_type) %>% count() %>% print(n=nrow(.))
ds %>% group_by(service_location) %>% count() %>% print(n=nrow(.))
ds %>% group_by(population_age) %>% count() %>% print(n=nrow(.))
ds %>% group_by(provider_mix) %>% count() %>% print(n=nrow(.))
# inspect categories of location classes
ds %>% 
  dplyr::group_by(location_class_code, location_class_description) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    location_class_description = substr(location_class_description, 1,42)
  ) %>% 
  # dplyr::arrange(desc(n)) #%>%
  print(n=nrow(.))
# inspect categories in Palette
ds %>% 
  group_by(palette_code, palette_colour_name) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    palette_colour_name = substr(palette_colour_name, 1,42)
  ) %>% 
  print(n=nrow(.))
