# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- declare-globals ---------------------------------------------------------
# dto_location_map.rds is products of `./manipulation/0-ellis-location-map.R` 
path_input_location_map   <- "./data-unshared/derived/dto_location_map.rds" 
path_input_patient_events <-  "./data-unshared/derived/dto_patient_events_addictions_4264.rds" # research cohort
# Make sure the files are located where they supposed to be, in `./data-unshared/` folder
testit::assert("File does not exist", base::file.exists(path_input_location_map))
testit::assert("File does not exist", base::file.exists(path_input_patient_events))
# list variables that constitute patient event table
components_patient_events <- c(
  "id"                           # patient unique key
  ,"gender"                      # biological sex
  ,"age_group"                   # in groups of 5 years
  ,"encounter_id"                # unique identifier for the encounter 
  ,"encounter_class"             # value from the D_Location dimension table in the data warehous
  ,"encounter_type"              # value from the D_Location dimension table in the data warehous
  ,"event_type"                  # high-level classification of the event (encounter itself, diagnosis, procedure, clinical order, laboratory, pharmacy event, etc.).           
  ,"event_title"                 # a shorter, more rolled up category describing the event
  ,"event_detail"                # description of the event, can be long and specific
  ,"event_count"                 # count of the events in the row- each row has a value of 1, and can be summed
  ,"event_year"                  # year in which event occurred 
  ,"event_month"                 # month in which event occured
  ,"event_start_date"            # might be the date of the event itself (e.g. date of diagnosis) or the start date of the encounter that the event is associated with     
  ,"event_end_date"              # might be the end date of the event itself (e.g. end date of prescriptions) or the end date of the encounter that the event is associated with
  ,"start_day"                   # number of days between the first day the patient accessed Island Health services and the start date of this event. This is useful to mask dates, but still provides the relative time between events.
  ,"end_day"                     # number of days between the first day the patient accessed Island Health services and the end date of this event. This is useful to mask dates, but still provides the relative time between events.            
  ,"duration_days"               # number of days between the Start_Day and End_Day (End_Day - Start_Day)
  ,"addiction_location_count"    # patient has accessed services at a location that was used for the selection of the cohort
  ,"location_map_id"             # unique id for VIHA program, connects to location map
  ,"palette_code"                # unique id for colours of this palette
  ,"palette_colour_name"         # labels for clusters of service programs (aka 3T palette colours)
)
# list variables that consitute location map table
components_location_map <- c(
  "location_map_id"              # unique id for VIHA program, connects to patient events
  , "site_name"                  # CERNER address          # EHR address   
  , "facility_name"              # CERNER address          # EHR address   
  , "building_name"              # CERNER address          # EHR address   
  , "unit_name"                  # CERNER address          # EHR address   
  , "location_category"          # Data Warehouse address  # EHR address           
  , "location_type"              # Data Warehouse address  # EHR address           
  , "location_grouping"          # Data Warehouse address  # EHR address            
  , "site_key"                   # Factual counterparts to CERNER address
  , "facility_key"               # Factual counterparts to CERNER address
  , "building_key"               # Factual counterparts to CERNER address
  , "unit_key"                   # Factual counterparts to CERNER address
  , "intensity_type"             # Classifier, Compressor, Lense
  , "intensity_severity_risk"    # Classifier, Compressor, Lense
  , "clinical_focus"             # Classifier, Compressor, Lense
  , "service_type"               # Classifier, Compressor, Lense
  , "service_location"           # Classifier, Compressor, Lense
  , "population_age"             # Classifier, Compressor, Lense
  , "provider_mix"               # Classifier, Compressor, Lense
  , "location_class_code"        # Program Class, identifier
  , "location_class_description" # Porgram Class, Descriptive label
  , "palette_code"               # Palette,  identifier       # cluster specific     
  , "palette_colour_name"        # Palette, descriptive label # cluster specific            
)

# define output format for the report
options(
  knitr.table.format = "html"
  ,tibble.width = 110
  #   ,bootstrap_options = c("striped", "hover", "condensed","responsive")
)
# ---- utility-functions -------------------------------------------------------
# functions local to this script go here. 

# ---- load-data ---------------------------------------------------------------
ds_patient_events <- readRDS(path_input_patient_events) # %>% as.data.frame()
ds_location_map   <- readRDS(path_input_location_map)

ds_patient_events  %>% glimpse(50)
ds_location_map %>% glimpse(50)

# ---- tweak-data ------------------------------------------------------------
# augment the event table with additional columns from location map
ds <- dplyr::left_join(
  ds_patient_events,             # patient event table
  ds_location_map %>%         # location map 
    dplyr::select_(.dots = components_location_map)
  ,by = c("location_map_id","palette_code","palette_colour_name") 
) %>%
  dplyr::rename(
    id = cohort_patient_id
  ) %>% 
  dplyr::mutate(
    
    location_class_description_display    = substr(location_class_description,1,42)
    ,palette_colour_name_display = substr(palette_colour_name,1,42)
  )
ds_location_map %>% glimpse()
# from this point on, ds_location_map is needed only for selective reference
ds %>% glimpse()
# ds now contains full coordinates to events of the cohort down to unit level
# ehr_address + location_classifiers + palette_colours
rm(ds_patient_events, ds_location_map)

# ---- graphing-settings-options ------------------
# ds %>% unique_sums("event_type")
# ds %>% unique_sums("encounter_class")


colors_encounter_class <- c(
  "Not defined"          = "black" # black # 2         2
  ,"Not assigned"         = "black" # black # 3        20
  ,"Cancelled Admission"  = "black" # black # 31        40
  ,"Other"                = "#999999" # grey  # 1783     15249
  ,"PreAdmit"             ="#377eb8"  # blue    # 111       403
  ,"Home Care"            = "#a65628" # brown   # 516       855
  ,"Recurring"            = "#984ea3" # purple  # 3534     24567
  ,"Outpatient"           = "#ff7f00" # orange    # 3331     34944
  ,"Inpatient"            = "#4daf4a" # green     # 4041    274263
  ,"Emergency"            = "#e41a1c" # red    # 3629     85637
)

colors_event_type <- c( 
  "Encounter Only"           ="black" # black   # 4067      153287  # ubiquitous event

  # ,"Admit/Transfer/Discharge" = "#999999" # grey #    6           7
  ,"Surgery"                  = "#e41a1c" # red  #  108         164
  
  ,"Evaluation and Management"= "#f781bf" # pink #  239        1235
  ,"Micro"                    = "#a65628" # brown  #  665        2090
  ,"Procedure"                = "#999999" # grey  # 1521        5512
  ,"Patient Care"             = "#ff7f00" # orange # 1825        6762
  ,"Diagnosis"                =  "#984ea3" # purple   # 1415        6808
  ,"Radiology"                = "#ffff33" # yellow # 1778        7761
  
  ,"General Lab"              = "#377eb8" # blue  # 1817      113725
  ,"Pharmacy"                 = "#4daf4a" # green  # 2631      138629
  
)
set.seed(42)

# ids_top <- get_ids(ds, bottom=.0 , top=.1, n_people =1 ) # 910503
ids_top <- get_ids(ds, bottom=.0 , top=.0, n_people =1 ) # 910517

# ---- graphing-functions --------------------------
data_encounter <- ds %>%  
  dplyr::filter(id == ids_top) %>% 
  dplyr::mutate(  
    palette_color_display = substr(palette_colour_name,1,42)   
  ) 

# define the graph
g1 <- data_encounter %>% 
  ggplot(aes(x=event_start_date,y=palette_colour_name))+
  geom_point(aes_string(color="encounter_class"),shape=124, size = 7)+
  scale_color_manual(values = colors_encounter_class)+
  # geom_point(aes_string(color=colors_event_type),shape=124, size = 7)+
  # scale_color_manual(values = colors_event_type)+
  labs(
    title = "Title"
    ,y = "Palette colors"
    ,x = "Admission Date"
  )+
  theme_minimal()


