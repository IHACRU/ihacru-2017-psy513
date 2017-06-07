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
ds %>% dplyr::glimpse(70)
# ---- tweak-data ---------------------
# ---- anonymize ------------------
scramble_one <- function(ds, person_id){
  # person_id = 906466
  ls_bucket <- list()
  all_palette_codes <- setdiff(unique(ds$palette_code), NA)
  for(pc in all_palette_codes ){
    ls_bucket[[pc]] <- ds %>% 
      dplyr::filter(palette_code == pc) %>% 
      dplyr::select(-id, -gender, -age_group)
  }
  
  d_roster <- ds %>% 
    dplyr::group_by(id, palette_code, gender,age_group) %>%
    # dplyr::group_by(id ) %>% 
    dplyr::summarize(
      n_encounters = length(unique(encounter_id))
    )
  ###
  ls_person <- list()
  dd <- d_roster %>% dplyr::filter(id == person_id) %>% as.data.frame() 
  person_codes <- dd[,"palette_code"]
  person_draws <- dd[,"n_encounters"]
  person_gernder <- unique(dd[,"gender"])
  person_age_group <- unique(dd[,"age_group"])
  for(p in seq_along(person_codes)){
    # p = 1
    # encounters_in_this_code <- ls_bucket[[person_codes[p]]]# %>% dplyr::distinct(encounter_id)# %>% as.data.frame()
    encounters_in_this_code <- ls_bucket[[person_codes[p]]] %>% as.data.frame()
    encounters_in_this_code <- unique(encounters_in_this_code[,"encounter_id"])
    # encounters_in_this_code <- encounters_in_this_code[,"encounter_id"]
    select_encounters <- sample(encounters_in_this_code, person_draws[p], replace = FALSE)  
    
    ls_person[[person_codes[p]]] <- ls_bucket[[person_codes[p]]] %>% 
      dplyr::filter(encounter_id %in% select_encounters)
    d_person <- dplyr::bind_rows(ls_person)
    d_person[,"id"] <- person_id
    d_person[,"gender"] <- person_gernder
    d_person[,"age_group"] <- person_age_group
    d_person <- d_person %>% dplyr::select(id,gender, age_group, dplyr::everything())
    
  } 
  return(d_person)
}
# Usage :
# d <- ds %>% scramble_one(906466)

scramble_many <- function(ds, sample_size){
  # sample_size = 10
  ls_temp <- list()
  select_persons <- sample(unique(ds$id), sample_size, replace = FALSE)
  for(i in select_persons ){  
    ls_temp[[i]] <- scramble_one(ds, i)
  }
  d <- ls_temp %>% dplyr::bind_rows()
  return(d)
}  

set.seed(3)
d <- ds %>% scramble_many(10)
d <- d %>% 
  dplyr::mutate(
    encounter_id = encounter_id + round(runif(1,1,10000),0)
  )
saveRDS(d, paste0(path_save,".rds"))
readr::write_csv(d, paste0(path_save,".csv"))




















