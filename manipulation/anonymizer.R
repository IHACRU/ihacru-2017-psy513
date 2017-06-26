# distort the raw patient event table into anonymized form
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
# path_input_mapping  <- "./data-unshared/derived/dto_location_map.rds" # `./0-ellis-location-map.R`
path_input   <- "./data-unshared/derived/dto_patient_events_addictions_4264.rds"
path_save    <- "./data-unshared/derived/dto_patient_events_addictions_4264_anon"
testit::assert("File does not exist", base::file.exists(path_input))
# ---- utility-functions ----------------------------------------------------- 
# ---- load-data ---------------------------------------------------------------
ds <- readRDS(path_input)
# ---- inspect-data --------------------
ds %>% dplyr::glimpse()
# ---- tweak-data ---------------------
# ---- anonymize ------------------
x <- ds %>% 
  dplyr::select(id, gender, age_group, encounter_id, palette_colour_name)

d_patients <- ds %>% 
  dplyr::group_by(id, palette_code, palette_colour_name) %>%
  # dplyr::group_by(id ) %>% 
  dplyr::summarize(
    n_encounters = length(unique(encounter_id)),
    n_events     = sum(event_count) 
  )
x <- d_patients

patient_pallete_codes %>% unique(d_patients[,"pa"])
select_palette_code <- 
filter_criteria_palette <- lazyeval::interp(~ which_column %in% select_palette_code,which_column = as.name("palette_code"))
d <- ds %>% 
  # create a bucket
  dplyr::filter_(.dots = filter_criteria_palette )
x <- d 