# knitr::stitch_rmd(script="./manipulation/0-ellis-map.R", output="./manipulation/stitched-output/0-ellis-map.md")
# This script reads two files: encounter counts with location mapping and encounter timelines for selected individuals
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(dplyr)
library(magrittr) #Pipes
requireNamespace("readxl")

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.



# ---- declare-globals ---------------------------------------------------------

path_input_mapping_new          <- "./data-unshared/raw/Location_Classification_2017-05-04.csv"
path_input_mapping_old          <- "./data-unshared/raw/Location_Map_2017-04-05.csv"


testit::assert("File does not exist", base::file.exists(path_input_mapping_old))
testit::assert("File does not exist", base::file.exists(path_input_mapping_new))

path_save <- "./data-unshared/derived/dto_location_map"

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
replace_with_na <- function(x){
  # x <- ds_location_map$facility_name
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
# ds_location_map %>% count_unique_program_ids()
# ---- load-data ---------------------------------------------------------------
ds_old <- readr::read_csv(path_input_mapping_old) %>% as.data.frame()
ds_new <- readr::read_csv(path_input_mapping_new) %>% as.data.frame()
# ---- tweak-data -------------------------------------------------------------
# standardize names : remove capital letters
colnames(ds_old) <- tolower(colnames(ds_old))
colnames(ds_new) <- tolower(colnames(ds_new))

ds_old %>% dplyr::glimpse()
ds_new %>% glimpse()

# Some variables have different character codes for missing values
# Translate various character values into NA values using 
# the function replace_with_na() defined above


ds_old <- ds_old %>% 
  dplyr::rename(
    # loc_class_id               = location_mapping_id,
    location_class_code        = type_code,
    location_class_description = type_description
  ) %>% 
  dplyr::mutate_all(dplyr::funs(replace_with_na))

ds_new <- ds_new %>% 
  dplyr::mutate_all(dplyr::funs(replace_with_na)) %>% 
  dplyr::rename(
    location_mapping_id  = loc_class_id
  )


ds <- dplyr::full_join(ds_old, ds_new) %>% 
  dplyr::filter(!palette_code %in% c(0,995:999)) %>% 
  dplyr::select_(.dots = c(
    # "loc_class_id"
    "location_mapping_id"
    ,palette_colours
    ,program_classes
    ,compressor_names
    ,data_warehouse_address
    ,ehr_address_full
  ))
ds %>% glimpse()
ds_location_map <- ds
# # function to replace non-UTF-8 characters
# replace_with <- function(
#   x
#   # ,tokens
#   # ,replacement
#   ){
#   tokens <- c("<U\\+0092>")
#   replacement <- ""
#   for(token in tokens){
#     if(is.character(x)){
#       x <- gsub(token,replacement,x)
#     }
#   }
#   return(x)
# } 
# 
# ds_location_map <- ds_location_map %>% 
#   mutate(site_name = to_true_unicode(site_name))
#   # apply function defined in `./manipulation/function-support.R`
#   # dplyr::mutate_all(dplyr::funs(replace_with_na) ) %>% 
#   # dplyr::mutate_all(dplyr::funs(replace_with))
#   dplyr::mutate(site_name = gsub("<U\\+0092>","",site_name))
# 
# ds_location_map %>% distinct(site_name) %>% slice(20:30)

# ---- inspect-category-frequencies ------------------------------------------
# count unique values in each column
ds_location_map %>% dplyr::summarise_all(n_distinct) %>% t()

### Count unique combinations ###
ds_location_map %>% count_unique_addresses()
ds_location_map %>% count_unique_addresses(keys=T)
ds_location_map %>% count_unique_classes()
# number of unique categories in each varaible
ds_location_map %>% summarize_all(n_distinct) %>% t()



### Close up inspection of each location descriptor ###
# inspect categories in CERNER address
ds_location_map %>% group_by(site_name) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(facility_name) %>% count()%>% print(n=nrow(.))
ds_location_map %>% group_by(building_name) %>% count()%>% print(n=nrow(.))
ds_location_map %>% group_by(unit_name) %>% count()%>% print(n=50)
# inspect categories in DataWarehouse address
ds_location_map %>% group_by(location_category) %>% count()%>% print(n=nrow(.))
ds_location_map %>% group_by(location_grouping) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(location_type) %>% count() %>% print(n=nrow(.))
# inspect categories for compressors that comprise the classifier
ds_location_map %>% group_by(intensity_type) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(intensity_severity_risk) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(clinical_focus) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(service_type) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(service_location) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(population_age) %>% count() %>% print(n=nrow(.))
ds_location_map %>% group_by(provider_mix) %>% count() %>% print(n=nrow(.))
# inspect categories of location classes
ds_location_map %>% 
  dplyr::group_by(location_class_code, location_class_description) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    location_class_description = substr(location_class_description, 1,42)
  ) %>% 
  # dplyr::arrange(desc(n)) #%>%
  print(n=nrow(.))
# inspect categories in Palette
ds_location_map %>% 
  group_by(palette_code, palette_colour_name) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    palette_colour_name = substr(palette_colour_name, 1,42)
  ) %>% 
  print(n=nrow(.))




# Use dynamic tables to interact if needed
# d <- ds_location_map %>% group_by_(.dots=compressor_names) %>% count() %>% print(n=nrow(.))
# d %>% DT::datatable()

# ---- save-to-disk ----------------
saveRDS(ds_location_map, paste0(path_save,".rds"))
readr::write_csv(ds_location_map, paste0(path_save,".csv"))

# ----- bare-map -----------------------

# # one may need to get a bare map, with rows as unique programs
# uniqueness_dimension <- c(
#   ehr_address
#   ,compressor_names
#   ,phenotypes
#   ,palette_colours
# )
# d_map <- ds_location_map %>% 
#   dplyr::filter(!is.na(palette_code)) %>% 
#   dplyr::group_by_(.dots = uniqueness_dimension) %>% 
#   dplyr::summarize(
#     n_unique = n()
#   )
# 
# path_save <- "./data-unshared/derived/dto_bare_map"
# 
# saveRDS(d_map, paste0(path_save,".rds"))
# readr::write_csv(d_map, paste0(path_save,".csv"))





