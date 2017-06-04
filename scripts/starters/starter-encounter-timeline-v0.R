# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: encounter counts with location mapping and encounter timelines for selected individuals

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R") # custom manipulation functions
source("./manipulation/object-glossary.R")  # vectors and lists of object names and factor levels
source("./scripts/graphing/graph-presets.R")  # graphing and formatting functions, color palettes

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- declare-globals ---------------------------------------------------------
# these data transfer objects are products of scripts
# ./manipulation/0-ellis-location-map.R 
# ./manipulation/0-ellis-patient-profiles.R 

path_input_mapping          <- "./data-unshared/derived/dto_location_map.rds" # 
path_input_profiles         <- "./data-unshared/derived/dto_patient_profiles.rds" # 

testit::assert("File does not exist", base::file.exists(path_input_mapping))
testit::assert("File does not exist", base::file.exists(path_input_profiles))

# ---- utility-functions ----------------------------------------------------- 
# place custom local functions here

# ---- load-data ---------------------------------------------------------------
dto <- list(
  "map"      = readRDS(path_input_mapping),
  "profiles" = readRDS(path_input_profiles)
)
ds_map <- dto$map
ds <- dto$profiles %>% 
  # introducing the ds by structuring the cluster of columns
  dplyr::select(
    id                                               # unique person identifier
    ,palette_code, palette_colour_name               # labels for clusters of service programs 
    ,event_start_date, event_end_date, duration_days # timing and duration
    ,event_type, event_title, event_detail           # description of events by ACRU
    ,encounter_class, encounter_type                 # description of encounters by DataWarehouse
    ,dplyr::everything()#esle                            
  ) %>% 
  dplyr::arrange(
    id, encounter_fact_key, event_start_date, event_end_date
    # many rows may be needed to express an encounter
    # see https://github.com/IHACRU/ihacru-encounter-timeline/issues/1
    # unique encounter has a unique encounter_fact_key
    # there may be multiple rows representing a single encounter
  )

# ---- inspect-data -----------------------------------------------------------
ds_map %>% dplyr::glimpse()
ds %>% dplyr::glimpse()

# ---- inspect-location-mapping ------------------------------------------
# count unique values in each column
ds_map %>% dplyr::summarise_all(dplyr::n_distinct) %>% t()
# count unique combinations
ds_map %>% count_unique_addresses()
ds_map %>% count_unique_addresses(keys=T)
ds_map %>% count_unique_compressors()
# close up inspection of each location descriptor
# inspect categories in CERNER address
ds_map %>% group_by(site_name) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(facility_name) %>% count()%>% print(n=nrow(.))
ds_map %>% group_by(building_name) %>% count()%>% print(n=nrow(.))
ds_map %>% group_by(unit_name) %>% count()%>% print(n=50)
# inspect categories in DataWarehouse address
ds_map %>% group_by(location_category) %>% count()%>% print(n=nrow(.))
ds_map %>% group_by(location_grouping) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(location_type) %>% count() %>% print(n=nrow(.))
# inspect categories used by each compressors ( lense )
ds_map %>% group_by(intensity_type) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(intensity_severity_risk) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(clinical_focus) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(service_type) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(service_location) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(population_age) %>% count() %>% print(n=nrow(.))
# inspect service types (unique combinations of compression values = type_code) # alt: service_type_id
ds_map %>% 
  dplyr::group_by(type_code, type_description) %>% 
  dplyr::count() %>% 
  dplyr::mutate(type_description = substr(type_description, 1,62)) %>% 
  print(n=nrow(.))
# inspect colors of the palette ( labels for clusters of services )
ds_map %>% 
  group_by(palette_code, palette_colour_name) %>% 
  dplyr::count() %>% 
  dplyr::mutate(palette_colour_name = substr(palette_colour_name, 1,62)) %>% 
  print(n=nrow(.))
# inspect grouping levels  (need comment from Stan)
ds_map %>% group_by(grouping_level_1) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(grouping_level_2) %>% count() %>% print(n=nrow(.))
ds_map %>% group_by(grouping_level_3) %>% count() %>% print(n=nrow(.))
# all levels together
ds_map %>% 
  group_by(grouping_level_1, grouping_level_2,grouping_level_3) %>% 
  count() %>%
  print(n=nrow(.))
# inspect location inclusion (need comment from Stan)
ds_map %>% 
  group_by(location_inclusion_code) %>% count() %>% print(n=nrow(.))

# ---- inspect-patient-profiles ---------------------------------------------------

# inspect patient profiles
ds %>% group_by(event_type) %>% summarize(n_id = length(unique(id)), n_records = n() )
ds %>% group_by(encounter_class) %>% summarize(n_ids = length(unique(id)), n_records = n() )
ds %>% group_by(encounter_type) %>% summarize(n_ids = length(unique(id)), n_records = n() ) %>% print(n=nrow(.))


# ---- tweak-data --------------------------------------------------------------

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

