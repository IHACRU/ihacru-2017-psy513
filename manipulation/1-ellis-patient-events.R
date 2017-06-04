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

path_input_mapping          <- "./data-unshared/derived/dto_location_map.rds" # see ellis
# path_input_events          <- "./data-unshared/raw/Addiction_Patient_Profiles_2017-04-10.csv"
# path_input_events          <- "./data-unshared/raw/Addiction_Patient_Profiles_2017-04-18.csv"
path_input_events          <- "./data-unshared/raw/Addiction_Patient_Profiles_2017-04-19.csv"
# path_input_events          <- "./data-unshared/raw/MHSU_Data_Export_2017-04-18.csv"

# path_save <- "./data-unshared/derived/dto_patient_profiles_addiction"
# path_save <- "./data-unshared/derived/dto_patient_events_mhsu_74"
path_save <- "./data-unshared/derived/dto_patient_events_addictions_4067"



testit::assert("File does not exist", base::file.exists(path_input_mapping))
testit::assert("File does not exist", base::file.exists(path_input_events))
# testit::assert("File does not exist", base::file.exists(path_input_service_types))

# ---- utility-functions ----------------------------------------------------- 


# ds_location_map %>% count_unique_program_ids()
# ---- load-data ---------------------------------------------------------------
# ds_location_map     <- readRDS(path_input_mapping) %>% as.data.frame()
ds_patient_events <- readr::read_csv(path_input_events) # %>% as.data.frame()
# ds_patient_events %>% dplyr::glimpse()

# ---- tweak-data -------------------------------------------------------------
# standardize names : remove capital letters
colnames(ds_patient_events) <- tolower(colnames(ds_patient_events))
ds_patient_events %>% dplyr::glimpse()


# Some variables have different character codes for missing values
# Translate various character values into NA values
ds_patient_events <- ds_patient_events %>% 
  # apply function defined in `./manipulation/function-support.R`
  dplyr::mutate_all(dplyr::funs(replace_with_na) ) 


ds_patient_events %>% dplyr::glimpse()

ds_patient_events <- ds_patient_events %>% 
  dplyr::rename(
    id = cohort_patient_id # id for person in this cohort study
    # id = patient_dim_key,
    # age = age_group
  ) %>% 
  dplyr::mutate(
    palette_code        = ifelse(palette_code==0,NA,palette_code)
    # , palette_colour_name = ifelse(palette_colour_name=="{blank}",NA,palette_colour_name)
    # , palette_colour_name_42 = substr(palette_colour_name,1,42)
    # , event_start_date = as.Date(event_start_date)
    # , event_end_date = as.Date(event_end_date)
  ) %>% 
  # dplyr::group_by(id) %>% 
  # dplyr::mutate(
  #   n_encounter = length(id)
  #   # n_fact_key = length(unique(encounter_fact_key)),
  #   # n_addiction_location = sum(addiction_location_count)
  # ) %>% 
  # dplyr::ungroup() %>% 
  dplyr::select(
     id                       # unique person identifier
    ,gender                   # biological sex
    ,age_group                # in groups of 5 years
    # encounter history
    ,encounter_fact_key       # unique identifier for the encounter 
    ,encounter_class          # value from the D_Location dimension table in the data warehous
    ,encounter_type           # value from the D_Location dimension table in the data warehous
    # each encounter may consists of multiple events
    ,event_type               # high-level classification of the event (encounter itself, diagnosis, procedure, clinical order, laboratory, pharmacy event, etc.).           
    ,event_title              # a shorter, more rolled up category describing the event
    ,event_detail             # description of the event, can be long and specific
    ,event_count              # count of the events in the row- each row has a value of 1, and can be summed
    ,event_year               # year in which event occurred 
    ,event_month              # month in which event occured
    ,event_start_date         # might be the date of the event itself (e.g. date of diagnosis) or the start date of the encounter that the event is associated with     
    ,event_end_date           # might be the end date of the event itself (e.g. end date of prescriptions) or the end date of the encounter that the event is associated with
    ,start_day                # number of days between the first day the patient accessed Island Health services and the start date of this event. This is useful to mask dates, but still provides the relative time between events.
    ,end_day                  # number of days between the first day the patient accessed Island Health services and the end date of this event. This is useful to mask dates, but still provides the relative time between events.            
    ,duration_days            # number of days between the Start_Day and End_Day (End_Day - Start_Day)
    # second, turn to analysis of encounter space
    ,addiction_location_count # patient has accessed services at a location that was used for the selection of the cohort
    ,location_mapping_id      # unique id for VIHA program, connects to location map
    ,palette_code             # unique id for colours of this palette
    ,palette_colour_name      # labels for clusters of service programs (aka 3T palette colours)
    # in case we missed something
    ,dplyr::everything()#esle  
  ) %>% 
  dplyr::arrange(
    id, encounter_fact_key # many rows may be needed to express an encounter
    # see https://github.com/IHACRU/ihacru-encounter-timeline/issues/1
    # unique encounter has a unique encounter_fact_key
    # there may be multiple rows representing a single encounter
  ) #%>% 
  # dplyr::group_by(id) %>% 
  # dplyr::mutate(
  #   n_encounters = length(unique(encounter_fact_key)), # number of unique encounter keys
  #   n_events     = sum(event_count)                    # number of distinct medical events 
  # ) %>%   
  # dplyr::ungroup() 

# Use dynamic tables to interact if needed
# d <- ds_location_map %>% group_by_(.dots=compressor_names) %>% count() %>% print(n=nrow(.))
# d %>% DT::datatable()

# ---- save-to-disk ----------------


saveRDS(ds_patient_events, paste0(path_save,".rds"))
# readr::write_csv(ds_location_map, paste0(path_save,".csv"))

 
