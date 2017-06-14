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
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# select the cohort, for which you wish to prepare the patient event table
# Fictional cohort used for testing and development of scripts
# path_input_events   <- "./data-public/derived/dto_patient_events_addictions_4264_anon.csv"
# path_save           <- "./data-unshared/derived/dto_patient_events_addictions_4264"
# # Addictions and Soberting 
# path_input_events   <- "./data-unshared/raw/Patient_Events-4264_Addictions_Sobering_2017-05-12.csv"
# path_save           <- "./data-unshared/derived/dto_patient_events_addictions_4264"
# Mental Health Diagnosis
# path_input_events   <- "./data-unshared/raw/Patient_Events-30662_MH_Diagnosis_2017-05-23.csv"
# path_save           <- "./data-unshared/derived/dto_patient_events_mh_30662"
# # # Full 3T Cohort
# path_input_events   <- "./data-unshared/raw/Patient_Events-170565_3T_Research_Full_2017-05-23.csv"
# path_save           <- "./data-unshared/derived/dto_patient_events_full3t_170656"
# Eating Disorder cohort
path_input_events   <- "./data-unshared/raw/Patient_Events-342_Eating_Disorder_2017-06-13.csv"
path_save           <- "./data-unshared/derived/dto_patient_events_eating_disorder_342"

testit::assert("File does not exist", base::file.exists(path_input_events))

# ---- utility-functions ----------------------------------------------------- 
# place for functions local to this script
# ---- load-data ---------------------------------------------------------------
# ds_patient_events     <- readRDS(path_input_events) %>% as.data.frame()
ds_patient_events <- readr::read_csv(path_input_events)#  %>% as.data.frame()
# reproduction in SRE causes the name of the first variable to have a prefex <U+FEFF>
names(ds_patient_events)[1] <- "cohort_patient_id" # correct for UTF-8-BOM

# ---- inspect-data -----------------------------------------------------------
ds_patient_events %>% dplyr::glimpse()

# ---- tweak-data -------------------------------------------------------------
# standardize names : remove capital letters
colnames(ds_patient_events) <- tolower(colnames(ds_patient_events))
ds_patient_events %>% dplyr::glimpse()
# Some variables have different character codes for missing values
# Translate various character values into NA values
ds_patient_events <- ds_patient_events %>% 
  # apply function defined in `./manipulation/function-support.R`
  dplyr::mutate_all(dplyr::funs(replace_with_na) ) 
# inspect the effect of the previous function
ds_patient_events %>% dplyr::glimpse()
ds_patient_events <- ds_patient_events %>% 
  dplyr::rename( # may need to rename a few variables to conform to convention
    id = cohort_patient_id    # id for person in this cohort study
  ) %>% 
  dplyr::select(
     id                       # unique person identifier
    ,gender                   # biological sex
    ,age_group                # in groups of 5 years
    # encounter history
    ,encounter_id               # unique identifier for the encounter 
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
    # ,location_mapping_id      # unique id for VIHA program, connects to location map
    ,location_map_id      # unique id for VIHA program, connects to location map
    ,palette_code             # unique id for colours of this palette
    ,palette_colour_name      # labels for clusters of service programs (aka 3T palette colours)
    # in case we missed to mention some variable by name
    ,dplyr::everything()#esle  
  ) %>% 
  dplyr::arrange(
    id, encounter_id # many rows may be needed to express an encounter
    # see https://github.com/IHACRU/ihacru-encounter-timeline/issues/1
    # unique encounter has a unique encounter_fact_key
    # there may be multiple rows representing a single encounter
  ) 
# Use dynamic tables to interact if needed
# d <- ds_location_map %>% group_by_(.dots=compressor_names) %>% count() %>% print(n=nrow(.))
# d %>% DT::datatable()

# ---- save-to-disk ----------------
# save the cleaned and formatted patient event table to be used in reports
saveRDS(ds_patient_events, paste0(path_save,".rds"))
# readr::write_csv(ds_location_map, paste0(path_save,".csv"))

# ---- explore-data ----------------
# PET - Patient Event Table
ds %>% dplyr::glimpse()
# How many patients are in this cohort?
ds %>% distinct(id) %>% count() %>% neat()
# What are basic demographics?
ds %>% unique_sums(c("gender")) %>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums(c("age_group"))  %>%  neat()
ds %>% unique_sums(c("gender","age_group")) %>%  neat()

# how may unique encounters are there in this set?
ds %>% distinct(encounter_fact_key) %>% count() %>% neat()
# tally engagement across encounter classes (as defined by data warehouse)
ds %>% unique_sums("encounter_class")%>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums("encounter_type") %>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums(c("encounter_class","encounter_type"))%>% arrange(desc(encounter_class,n_people)) %>% neat()

# how many event types  were there?
ds %>% unique_sums("event_type") %>% arrange(desc(n_people)) %>% neat()
# view event_title and event_details with a dynmaic table 
ds %>% unique_sums(c("event_type","event_title","event_detail"))%>% arrange(desc(n_people)) %>% neat_DT()
# what is the total number of events recorded for this cohort?
ds %>% summarize(n_event = sum(event_count)) %>% neat()

# what was the pattern of engagement over time?
ds %>% unique_sums(c("event_year")) %>% neat()
ds %>% unique_sums(c("event_month")) %>% neat()

# durations of events vary, what is this distribution?
d <- ds %>% unique_sums("duration_days")
d %>% slice(1:6) %>% neat() # there are 318 possible values, the first 10 shown here
d %>% tail(5) %>% neat() # the last value exposes a clear data error
d <- d %>% 
  filter(!duration_days==4910527) # remove the impossible value
# durations of events vary, what is this distribution?
d %>% 
  ggplot( aes(x = duration_days, y = n_people) )+
  geom_point()+
  theme_minimal()
# durations of events vary, what is this distribution?
d %>% 
  ggplot( aes(x = duration_days, y = n_encounters) )+
  geom_point()+
  theme_minimal()  
# durations of events vary, what is this distribution?
d %>% 
  ggplot( aes(x = duration_days, y = n_events) )+
  geom_point()+
  theme_minimal()  



 
