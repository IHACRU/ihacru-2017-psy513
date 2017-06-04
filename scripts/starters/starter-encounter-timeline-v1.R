# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R") # custom manipulation functions
source("./manipulation/object-glossary.R") # vectors and lists of object names and factor levels
source("./scripts/graphing/graph-presets.R") # graphing and formatting functions, color palettes

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr)

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- declare-globals ---------------------------------------------------------
# dto_location_map.rds is products of `./manipulation/0-ellis-location-map.R` 
path_input_location_map        <- "./data-unshared/derived/dto_location_map.rds" 
path_input_event_table         <-  "./data-unshared/raw/MHSU_Data_Export_2017-04-18.csv" # research cohort
# Make sure the files are located where they supposed to be, in `./data-unshared/` folder
testit::assert("File does not exist", base::file.exists(path_input_location_map))
testit::assert("File does not exist", base::file.exists(path_input_event_table))
# list variables that constitute patient event table
components_event_table <- c(
  "id"                       # patient unique key
  ,"gender"                   # biological sex
  ,"age_group"                # in groups of 5 years
  ,"encounter_fact_key"       # unique identifier for the encounter 
  ,"encounter_class"          # value from the D_Location dimension table in the data warehous
  ,"encounter_type"           # value from the D_Location dimension table in the data warehous
  ,"event_type"               # high-level classification of the event (encounter itself, diagnosis, procedure, clinical order, laboratory, pharmacy event, etc.).           
  ,"event_title"              # a shorter, more rolled up category describing the event
  ,"event_detail"             # description of the event, can be long and specific
  ,"event_count"              # count of the events in the row- each row has a value of 1, and can be summed
  ,"event_year"               # year in which event occurred 
  ,"event_month"              # month in which event occured
  ,"event_start_date"         # might be the date of the event itself (e.g. date of diagnosis) or the start date of the encounter that the event is associated with     
  ,"event_end_date"           # might be the end date of the event itself (e.g. end date of prescriptions) or the end date of the encounter that the event is associated with
  ,"start_day"                # number of days between the first day the patient accessed Island Health services and the start date of this event. This is useful to mask dates, but still provides the relative time between events.
  ,"end_day"                  # number of days between the first day the patient accessed Island Health services and the end date of this event. This is useful to mask dates, but still provides the relative time between events.            
  ,"duration_days"            # number of days between the Start_Day and End_Day (End_Day - Start_Day)
  ,"addiction_location_count" # patient has accessed services at a location that was used for the selection of the cohort
  ,"location_mapping_id"      # unique id for VIHA program, connects to location map
  ,"palette_code"             # unique id for colours of this palette
  ,"palette_colour_name"      # labels for clusters of service programs (aka 3T palette colours)
)
# list variables that consitute location map table
components_location_map <- c(
  "location_mapping_id"     
  , "site_name"               # CERNER address          # EHR address   
  , "facility_name"           # CERNER address          # EHR address   
  , "building_name"           # CERNER address          # EHR address   
  , "unit_name"               # CERNER address          # EHR address   
  , "location_category"       # Data Warehouse address  # EHR address           
  , "location_type"           # Data Warehouse address  # EHR address           
  , "location_grouping"       # Data Warehouse address  # EHR address            
  , "site_key"                # Factual counterparts to CERNER address
  , "facility_key"            # Factual counterparts to CERNER address
  , "building_key"            # Factual counterparts to CERNER address
  , "unit_key"                # Factual counterparts to CERNER address
  , "intensity_type"          # Classifier, Compressor, Lense
  , "intensity_severity_risk" # Classifier, Compressor, Lense
  , "clinical_focus"          # Classifier, Compressor, Lense
  , "service_type"            # Classifier, Compressor, Lense
  , "service_location"        # Classifier, Compressor, Lense
  , "population_age"          # Classifier, Compressor, Lense
  , "type_code"               # Program Class, identifier
  , "type_description"        # Porgram Class, Descriptive label
  , "palette_code"            # Palette,  identifier       # cluster specific     
  , "palette_colour_name"     # Palette, descriptive label # cluster specific            
  , "grouping_level_1"        # not sure, what. ask Stan
  , "grouping_level_2"        # not sure, what. ask Stan
  , "grouping_level_3"        # not sure, what. ask Stan
  , "master_unit_key"         # not sure, what. ask Stan
  # ,"location_count"         # not sure, what. ask Stan
  # ,"encounter_count"        # not sure, what. ask Stan
)
# ---- utility-functions -------------------------------------------------------
# functions local to this script go here. 

# ---- load-data ---------------------------------------------------------------
ds_event_table  <- readr::read_csv(path_input_event_table) # %>% as.data.frame()
ds_location_map <- readRDS(path_input_location_map)

ds_event_table  %>% glimpse()
ds_location_map %>% glimpse()

# ---- tweak-data ------------------------------------------------------------
# standardize names : remove capital letters
colnames(ds_event_table) <- tolower(colnames(ds_event_table))
# ds_event_table %>% dplyr::glimpse()
# Some variables have different character codes for missing values
# Translate various character values into NA values
ds_event_table <- ds_event_table %>% 
  # apply function defined in `./manipulation/function-support.R`
  dplyr::mutate_all(dplyr::funs(replace_with_na) ) 
# ds_event_table %>% dplyr::glimpse()
ds_event_table <- ds_event_table %>% 
  dplyr::rename(
    id = cohort_patient_id # id for person in this cohort study
  ) %>% 
  dplyr::mutate(
    palette_code = ifelse(palette_code==0,NA,palette_code)
  ) %>% 
  # introducing the ds by structuring the cluster of columns
  dplyr::select_(.dots = components_event_table) %>%  # reorders variables
  dplyr::arrange(
    id, encounter_fact_key, event_start_date, event_end_date, event_type
    # many rows may be needed to express an encounter
    # see https://github.com/IHACRU/ihacru-encounter-timeline/issues/1
    # unique encounter has a unique encounter_fact_key
    # there may be multiple rows representing a single encounter
  )

# augment the event table with additional columns from location map
ds <- dplyr::left_join(
  ds_event_table,             # patient event table
  ds_location_map %>%         # location map 
    dplyr::select_(.dots = components_location_map)
  ,by = c("location_mapping_id","palette_code","palette_colour_name") 
)
ds_location_map %>% glimpse()
# from this point on, ds_location_map is needed only for selective reference
ds %>% glimpse()
# ds now contains full coordinates to events of the cohort down to unit level
# ehr_address + location_classifiers + palette_colours


# ---- inspect-data -----------------------------------------------------------
ds_event_table  %>% glimpse()
ds_location_map %>% glimpse()
ds %>% glimpse()





# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

# ---- save-to-disk ----------------------------------

# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

