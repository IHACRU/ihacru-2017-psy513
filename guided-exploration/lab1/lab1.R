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
path_save                 <-  "./data-unshared/derived/dto_addictions_4264" # research cohort
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
  # dplyr::rename(
  #   id = cohort_patient_id
  # ) %>% 
  dplyr::mutate(
    
    location_class_description_display    = substr(location_class_description,1,42)
    ,palette_colour_name_display = substr(palette_colour_name,1,42)
  )
ds_location_map %>% glimpse()
# from this point on, ds_location_map is needed only for selective reference
ds %>% glimpse(70)
# ds now contains full coordinates to events of the cohort down to unit level
# ehr_address + location_classifiers + palette_colours


# ---- inspect-data-1 -----------------------------------------------------------
ds_patient_events  %>% glimpse(70)
ds_location_map %>% glimpse(70)
ds %>% glimpse(50)

# ---- inspect-data-2 -----------------------------------------------------------
varnames <- c(cerner_address_keys, data_warehouse_address, compressor_names, program_classes, palette_colours)
setdiff(colnames(ds_location_map), varnames)
ds_location_map %>% 
  dplyr::distinct_(.dots = varnames  ) %>% 
  DT::datatable( 
    class   = 'cell-border stripe',
    caption = "Number of encounteres in each service TYPE",
    filter  = "top", 
    options = list(
      pageLength = 10, 
      autoWidth  = TRUE
    )
  )
rm(ds_patient_events, ds_location_map)

# ---- save-to-disk -------------------------------------------------------------
# saveRDS(ds, paste0(path_save,".rds"))
readr::write_csv(ds, paste0(path_save,".csv"))

# ---- utility-functions -------------------------------------------------------

# ---- dev-a-0 ---------------------------------------------------

# the data object containing events and location map 
ds %>% glimpse()
# What variables relate to patient event table?
# What variables relate to locations at which events took place?
# How many unique values does each variable/column contain? 
ds %>% select_(.dots=components_patient_events) %>% summarise_all(dplyr::n_distinct) %>% t() 
ds %>% select_(.dots=components_location_map) %>% summarise_all(dplyr::n_distinct) %>% t() 
# they share / connected by the fields:
intersect(components_patient_events, components_location_map)

# ---- dev-a-1 ---------------------------------------------------
# PET - Patient Event Table
# How many patients are in this cohort?
ds %>% distinct(id) %>% count() %>% neat()
# What are basic demographics?
ds %>% unique_sums(c("gender")) %>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums(c("age_group"))  %>%  neat()
ds %>% unique_sums(c("gender","age_group")) %>%  neat()

# ---- dev-a-2 ---------------------------------------------------
# how may unique encounters are there in this set?
ds %>% distinct(encounter_id) %>% count() %>% neat()
# tally engagement across encounter classes (as defined by data warehouse)
ds %>% unique_sums("encounter_class")%>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums("encounter_type") %>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums(c("encounter_class","encounter_type"))%>% arrange(desc(encounter_class,n_people)) %>% neat()

# ---- dev-a-3 ---------------------------------------------------
# how many event types  were there?
ds %>% unique_sums("event_type") %>% arrange(desc(n_people)) %>% neat()
# view event_title and event_details with a dynmaic table 
ds %>% unique_sums(c("event_type","event_title","event_detail"))%>% arrange(desc(n_people)) %>% neat_DT()
# what is the total number of events recorded for this cohort?
ds %>% summarize(n_event = sum(event_count)) %>% neat()

# ---- dev-a-4 ---------------------------------------------------
# what was the pattern of engagement over time?
ds %>% unique_sums(c("event_year")) %>% neat()
ds %>% unique_sums(c("event_month")) %>% neat()

# ---- dev-a-5 ---------------------------------------------------
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

# ---- dev-aa-1 -----------------------------------------------

# ---- interview-service-programs --------------------------------------------------

# ---- dev-b-0 ---------------------------------------------------
# count unique values in each column
ds %>% select_(.dots=c("id",components_location_map)) %>% summarise_all(dplyr::n_distinct) %>% t() 
# how many unique programs were engaged by the cohort?
ds %>% distinct(location_map_id) %>% count() %>% neat()

# what is the span of this cohort in the ehr_address space?
ds %>% count_unique_addresses() %>% neat()
ds %>% count_unique_addresses(keys = T) %>% neat()
# what is the span of this cohort int the classification scheme?
# how many unique combination of values on (6) classifiers
ds %>% count_unique_classes() %>% neat()
# ---- dev-b-1 ---------------------------------------------------

ds %>% unique_sums(c(
  "location_class_code", "location_class_description"
  ,compressor_names
)) %>% dplyr::arrange(desc(n_people)) %>% 
  dplyr::select(n_people, n_encounters, n_events, dplyr::everything()) %>% 
  DT::datatable(filter="top",options = list(pageLength = 6))

# ---- dev-b-2 ---------------------------------------------------
ds %>% unique_sums(c(
  "palette_colour_name", "location_class_description"
  ,"palette_code",        "location_class_code"
  ,compressor_names
)) %>% dplyr::arrange(desc(n_people)) %>% 
  dplyr::select(n_people, n_encounters, n_events, dplyr::everything()) %>% 
  DT::datatable(filter="top",options = list(pageLength = 6))

# ---- dev-b-3 ---------------------------------------------------
d <- ds %>% unique_sums(c(
  "palette_colour_name", "location_class_description"
  ,"palette_code",        "location_class_code"
  ,compressor_names
  ,setdiff(ehr_address,"site_name") # without
  ,cerner_address_keys              # just to be thorough
)) %>% dplyr::arrange(desc(n_people)) %>% 
  dplyr::select(n_people, n_encounters, n_events, dplyr::everything()) 
d %>% DT::datatable(filter="top",options = list(pageLength = 6))
# save this file to examine separately
# readr::write_csv(d,"./data-public/derived/addictions-4264-summative-2017-06-05.csv")
# ---- dev-b-4 ---------------------------------------------------
ds %>% unique_sums("provider_mix") %>% arrange(desc(n_people)) %>% neat()
ds %>% unique_sums("intensity_type") %>% arrange(desc(n_people)) %>%  neat()    
ds %>% unique_sums("intensity_severity_risk") %>% arrange(desc(n_people))  %>% neat()
ds %>% unique_sums("clinical_focus") %>% arrange(desc(n_people))  %>% neat()
ds %>% unique_sums("service_type") %>% arrange(desc(n_people))  %>% neat()         
ds %>% unique_sums("service_location") %>% arrange(desc(n_people)) %>% neat()       
ds %>% unique_sums("population_age") %>% arrange(desc(n_people)) %>% neat()


# ---- dev-b-5 ---------------------------------------------------
ds %>% unique_sums("facility_name") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("building_name") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("unit_name") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("location_category") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("location_grouping") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("location_type")%>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("facility_key") %>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("building_key")%>% arrange(desc(n_people)) %>% dt("none") 
ds %>% unique_sums("unit_key") %>% arrange(desc(n_people)) %>% dt("none")


# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_report_1 <- "./sandbox/lab1/lab1.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

# Visualisation scripts below

# ---- graphing-settings-options ------------------
# ds %>% unique_sums("event_type")
# ds %>% unique_sums("encounter_class")

# qualitative, 9 categories (max), printer friendly
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
# add colors in sequential order
acru_colors_9 <- c(
  "red"    = "#e41a1c" # red          
  ,"blue"   = "#377eb8" # blue           
  ,"green"  = "#4daf4a" # green            
  ,"purple" = "#984ea3" # purple             
  ,"orange" = "#ff7f00" # orange             
  ,"yellow" = "#ffff33" # yellow             
  ,"brown"  = "#a65628" # brown            
  ,"pink"   = "#f781bf" # pink           
  ,"grey"   = "#999999" # grey           
)

colors_encounter_class <- c(
  "Not defined"           = "black"   # black  
  ,"Not assigned"         = "black"   # black  
  ,"Cancelled Admission"  = "black"   # black  
  ,"Other"                = "#999999" # grey   
  ,"PreAdmit"             = "#377eb8" # blue   
  ,"Home Care"            = "#a65628" # brown  
  ,"Recurring"            = "#984ea3" # purple 
  ,"Outpatient"           = "#ff7f00" # orange 
  ,"Inpatient"            = "#4daf4a" # green  
  ,"Emergency"            = "#e41a1c" # red    
)

colors_event_type <- c( 
  "Encounter Only"           ="black"     # black  
  ,"Surgery"                  = "#e41a1c" # red    
  ,"Evaluation and Management"= "#f781bf" # pink   
  ,"Micro"                    = "#a65628" # brown  
  ,"Procedure"                = "#999999" # grey   
  ,"Patient Care"             = "#ff7f00" # orange 
  ,"Diagnosis"                = "#984ea3" # purple 
  ,"Radiology"                = "#ffff33" # yellow 
  ,"General Lab"              = "#377eb8" # blue   
  ,"Pharmacy"                 = "#4daf4a" # green  
  
)

colors_provier_mix <- c(
  "Nursing and allied health professional, physician oversight"  = "black"#
  ,"Physician directed, nursing delivered"                        = "black"#
  ,"Nursing and allied health professional"                       = "black"#
  ,"Medical specialist with tech supports"                        = "black"#
  ,"Physician, nurse delivered"                                   = "black"#
  ,"Miscellaneous"                                                = "black"#
  ,"Medical specialist"                                           = "black"#
  ,"Nursing delivered"                                            = "black"#
  ,"Allied health"                                                = "black"#
  ,"Psychologist"                                                 = "black"#
  ,"Physician, nurse, allied health professional - team"          = "black"#
  ,"Medical specialist service"                                   = "black"#
  ,"Morgue"                                                       = "black"#
  ,"Pharmacist"                                                   = "black"#
  ,"Nursing managed with physician oversight"                     = "black"#
  ,"Physician"                                                    = "black"#          
  
)

ds %>% distinct(provider_mix)
set.seed(42)
# ids_top <- get_ids(ds, bottom=.0 , top=.1, n_people =1 ) # 910503
ids_top <- get_ids(ds, bottom=.5 , top=.6, n_people =1 ) # 910517
# length(unique(ids_top))
# ---- graphing-functions --------------------------
# function to graph a timeline for a single person
graph_encounter_timeline <- function(
  d,
  person_id,
  color_by
  # palette = "auto"
  
){
  # d <- ds
  # # person_id <- 329656
  # # person_id <- 247722
  # person_id <- ids_top
  # # color_by <- "event_type"
  # color_by <- "encounter_class"
  # # palette = "encounter"
  # # palette = "event_type"
  
  # create ls_color object with custom palette colors
  ls_color <- list(
    # "auto"             = auto_palette
    "event_type"      = colors_event_type
    ,"encounter_class" = colors_encounter_class
    ,"provider_mix"    = colors_provier_mix
  )
  # custom_palette <- ls_color[[color_by]]
  # in case of exploring new variables
  if(palette=="auto"){
    unique_color_by <- table(d$event_type)
    unique_color_by <- sort(unique(as.data.frame(d)[,color_by]))
    if(length(unique_color_by)<10 ){
      auto_palette <-  RColorBrewer::brewer.pal(
        n = length(unique_color_by),
        name = "Set1"
      )
      names(auto_palette) <- unique_color_by
    }else{
      cat("\n There are too many categoriese to depict automatically")
      auto_palette <- NULL
    }
  }else{
    custom_palette <- ls_color[[color_by]]
  }
  
  #create dataset for a single person to draw the timeline
  data_encounter <- d %>%  
    dplyr::filter(id == person_id) %>% 
    dplyr::mutate(  
      palette_color_display = substr(palette_colour_name,1,42)   
    ) 
  testit::assert("Error: More than one person selected",length(unique(data_encounter$id))==1L)
  
  d_summary <- data_encounter %>% 
    dplyr::summarize(
      n_encounters = length(unique(encounter_id)),
      n_events     = sum(event_count) 
    ) 
  n_enc <- d_summary %>% select(n_encounters) %>% scales::comma() %>% as.character()
  n_evt <- d_summary %>% select(n_events) %>% scales::comma()%>% as.character()
  # create a main title that identifies the graph  
  main_title <- data_encounter %>%
    dplyr::slice(1) %>% 
    # dplyr::select(id, decade_age_group) %>% 
    dplyr::mutate(
      label = paste0("ID: ",id," - ",gender," - (", age_group, " ya) - Encounters: ",n_enc," - Events: ",n_evt)
    ) %>% 
    dplyr::select(label) %>% 
    as.character()
  
  
  
  # define the graph
  g1 <- data_encounter %>% 
    ggplot(aes(x=event_start_date,y=palette_colour_name))+
    # geom_point(aes(color=encounter_type),shape=124, size = 7)+
    # geom_point(aes_string(color=encounter_class),shape=124, size = 7)+
    geom_point(aes_string(color=color_by),shape=124, size = 7)+
    # scale_x_continuous(breaks = year_breaks)+
    # scale_x_date()
    scale_color_manual(values = custom_palette)+
    # geom_point(aes(color=encounter_days),shape=124, size = 7)+
    
    labs(
      title = main_title
      ,y = "Palette colors"
      ,x = "Admission Date"
      # ,color = "Event Type"
    )+
    # facet_grid(palette_colour_name~.)+
    theme_minimal()
  # g1
  
  
  return(g1)
  
}
ds %>% graph_encounter_timeline(ids_top,"event_type")

# function to print and save a timeline for a specific person
print_encounter_timeline <- function(
  d # dataframe with patient encounters
  ,person_id_ # unique id of a person of the timeline
  ,color_by_ # categorical variable for color mapping
  ,path_out_folder # where to save the graphs
  ,prefex # descriptive characters in file names (e.g. mhsu74, addiction) 
){
  # Values for testing and development
  # d <- ds
  # # color_by_ = "encounter_class"
  # color_by_ = "event_type"
  # person_id_ <- 910517
  # # person_id_ <- 329656
  # # person_id_ <- 413785
  # path_out_folder <- "./sandbox/lab1/prints/"
  # subset encounters of only his person
  data_encounter <- d %>%
    dplyr::filter(id == person_id_)
  testit::assert("Error: More than one person selected",length(unique(data_encounter$id))==1L)
  # How many unique palette colours used to describe timeline?
  n_colours <- data_encounter %>%
    dplyr::group_by(palette_colour_name) %>%
    dplyr::summarize(
      n = n()
    )
  n_colours
  # Produce the graphic
  g1 <- d %>% 
    graph_encounter_timeline(
      person_id = person_id_
      ,color_by = color_by_ # encounter_type encounter_class encounter_days
    )
  g1
  #save the graphic to disk
  (path_save = paste0(path_out_folder,prefex,"-",person_id_,"-",color_by_,".jpg"))
  jpeg(
    filename  =  path_save,
    width     = 900,
    height    = 25 + 20*nrow(n_colours),
    # height    = 300,
    units     = "px",
    pointsize = 12,
    quality   = 200
  )
  print(g1)
  dev.off()
}
# Usage:
# ds %>%
#   print_encounter_timeline(
#     person_id = 910517,
#     color_by_ = "encounter_class",
#     path_out_folder =  "./sandbox/lab1/prints/",
#     prefex = "mhsu74"
# 
#   )



# ----- explore-timelines ----------------------
set.seed(43)
ids <-ds %>%  get_ids(bottom = .9, top = 1, n_people = 1)
ids


# --- print-timelines ------------------
ds %>% dplyr::glimpse()
set.seed(43)

n_people_ = 3
ids_intensity <- list(
  "high" = get_ids(ds, bottom=.8, top=1,  n_people =n_people_ ),
  "mid"  = get_ids(ds, bottom=.4, top=6,  n_people =n_people_ ),
  "low"  = get_ids(ds, bottom=.0, top=.2, n_people =n_people_ )
)
for(s in c("high","mid","low")){
  for(col in c("event_type","encounter_class")){  
    ids <- ids_intensity[[s]]
    # ids <- unique(ds$id)
    # ids <- 910522
    for(i in ids){
      # i <- 413785
      ds %>% 
        print_encounter_timeline(
          person_id_ = i,
          color_by_ = col,
          path_out_folder =  paste0("./sandbox/lab1/prints/"),  
          prefex = "mhsu74"
        )
    }
  }
}

# ---- encounter-timelines-1 ------------------------------

path_folder <- paste0("./sandbox/lab1/prints/")
jpegs <- list.files(path_folder, full.names = T,recursive = F)
lst <- list()
regex_pattern <- "(\\w+)-(\\d+)-(\\w+).jpg$"
for(i in seq_along(jpegs)){
  (lst[["prefex"]][i]    = sub(regex_pattern,"\\1", basename(jpegs[i]) ) )
  (lst[["person"]][i]   = sub(regex_pattern,"\\2", basename(jpegs[i]) ) )
  (lst[["color_by"]][i] = sub(regex_pattern,"\\3", basename(jpegs[i]) ) )
  (lst[["path"]][i]     = sub("[./]","../..",jpegs[i]))
}
ds_jpegs <- dplyr::bind_rows(lst)
# ds_jpegs
# index_cycle    <- ds_jpegs$index %>% unique() %>% sort(decreasing = T)
# domain_cycle   <- ds_jpegs$domain %>% unique()
# subgroup_cycle <- ds_jpegs$subgroup %>% unique()
# for(ind in index_cycle){
#   cat("\n# Forest: ", ind,"\n")
#   for(dom in domain_cycle){
#     cat("\n## ",dom,"\n")
# for(gender in subgroup_cycle){
for(i in unique(ds_jpegs$person) ){
  # i <- 910525
  cat("\n## ", i,"\n")
  for(col in unique(ds_jpegs$color_by) ){  
    # col <- "encounter_class"
    #Don't specify width.  This maintains the aspect ratio.
    path <- ds_jpegs %>%
      dplyr::filter(person == i ) %>%
      dplyr::filter(color_by == col ) %>%
      dplyr::select(path) %>%
      as.character()
    # print(path)
    # testit::assert("File does not exist",file.exists(path))
    if(file.exists( sub("../../","./",path) ) ){
      cat("\n colored by ",toupper(col),"\n")
      cat('<img src="', path, '" alt="', basename(path),'">\n', sep="")
      cat("\n")
    }
    # cat("\n")
  }
}
# }

# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/lab1/lab1.Rmd"
# path_report_1 <- "./reports/encounter-timelines-v1/encounter-timelines-v1.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}



# --------   ----------------
print_encounter_timeline <- function(
  d # dataframe with patient encounters
  ,person_id # unique id of a person of the timeline
  ,color_by_ # categorical variable for color mapping
  ,path_out_folder # where to save the graphs
){
  # Values for testing and development
  # d <- ds_patient_profiles
  # color_by_ = "encounter_class"
  # person_id <- 329656
  # path_out_folder <- "./sandbox/1-encounter-timelines/graphs/"
  # subset encounters of only his person
  data_encounter <- d %>%
    dplyr::filter(id == person_id)
  testit::assert("Error: More than one person selected",length(unique(data_encounter$id))==1L)
  # How many unique palette colours used to describe timeline?
  n_colours <- data_encounter %>%
    dplyr::group_by(palette_colour_name) %>%
    dplyr::summarize(
      n = n()
    )  
  # Produce the graphic
  g1 <- ds_patient_profiles %>% 
    graph_encounter_timeline(
      person_id = 329656
      ,color_by = color_by_ # encounter_type encounter_class encounter_days
    )
  #save the graphic to disk
  (path_save = paste0(path_out_folder,"timeline-",person_id,".jpg"))
  jpeg(
    filename  =  path_save,
    width     = 900,
    height    = 5 + 20*nrow(n_colours),
    # height    = 300,
    units     = "px",
    pointsize = 12,
    quality   = 100
  )
  g1
  dev.off()
}
# Usage:
ds_patient_profiles %>% 
  print_encounter_timeline(
    person_id = 329656,
    color_by_ = "encounter_class",
    path_out_folder =  "./sandbox/cohort/graphs/"
    
  )




# ds_patient_profiles %>% print_encounter_timeline(329656,"encounter_class")


# save graphic




# ids <- ds_patient_profiles %>% 
#   # dplyr::arrange(desc(n_encounter)) %>% 
#   # dplyr::distinct(id) %>% 
#   dplyr::mutate(
#     pct_rank = percent_rank(n_encounter)
#   ) %>% 
#   dplyr::distinct(id, pct_rank) %>% 
#   dplyr::arrange(desc(id)) %>% 
#   dplyr::filter(pct_rank > .9 & pct_rank < 1) %>% 
#   dplyr::select(id) %>% 
#   as.data.frame() 
# ids <- sample(ids$id,size = 10, replace=F )



get_id <- function(d, chosen_group, seed=42){
  # seed_ = 42
  n_people = 1
  # chosen_group = 10
  ids <- ds_patient_profiles %>% 
    dplyr::mutate(
      # usage_intensity = cut(n_fact_key,breaks=5,include.lowest = TRUE),
      usage_intensity = Hmisc::cut2(n_fact_key,g=10)
      # usage_intensityF = factor(usage_intensity, labels = (usage_intensity))
    ) #%>% 
  table(ids$usage_intensity)
  
  set.seed(seed)
  # chosen_group <- 10  
  id <- ids %>% dplyr::filter(usage_intensity==levels(usage_intensity)[chosen_group]) %>% 
    dplyr::distinct(id) %>% 
    dplyr::filter(id %in% sample(id,n_people)) %>% as.data.frame() %>% 
    # id <- as.numeric(d[,1])# 
    as.numeric()
  # ids %>% glimpse()
  return(id)
  # 
}
# a <- ds_patient_profiles %>% get_id(2,10)



# ---- inspect-1 --------------------
ds %>% glimpse()
# count unique values in each column
sapply(ds,function(x)length(unique(x)))
# lapply(ds,function(x)length(unique(x)))


# How many individuals?
ds %>%  distinct(id) %>% count()
# How many encounter per each individual?
d1 <- ds %>%
  dplyr::group_by(id,cohort_color_code, cohor_palette_label, cohort_group) %>% 
  dplyr::count() %>%
  dplyr::arrange(cohort_color_code, cohort_group, desc(n)) %>% 
  print(n=100)

d1 %>% 
  ggplot2::ggplot(aes(y=cohor_palette_label,x=n))+
  geom_point(aes(color=cohort_group),
             size = 7, shape=124)+
  # scale_x_continuous(breaks = )+
  geom_text(aes(label = cohort_color_code, x=-50))+
  
  # theme(
  #   axis.title.x = "Number of encounters"
  # )+
  theme_minimal()

ds %>% dplyr::glimpse()
ds %>% head()
ids <- unique(ds$id)
ds %>% 
  dplyr::filter(id==ids[13]) %>% 
  dplyr::mutate(  
    # palette_color = factor(substr(palette_colour_name),1,42)   
    palette_color = substr(palette_colour_name,1,42)   
  ) %>%    
  ggplot(aes(x=admit_date, y=palette_colour_name))+
  geom_point(shape=124, size = 7)+
  # facet_grid(palette_colour_name~.)+
  theme_minimal() 

ds %>% distinct(palette_colour_name)

t <- table(ds$encounter_type, ds$encounter_class); t[t==0]<-".";t
ds %>% distinct(encounter_class)
ds %>% group_by(id) %>%  distinct(encounter_class)
ds %>% group_by(encounter_class) %>%  distinct(id) %>% count()

ds %>% distinct(encounter_class, encounter_type) 

t <- table(ds$id, ds$encounter_class); t[t==0]<-".";t


ds %>% dplyr::glimpse()
# -----  --------------------


#### Developmental code after this point ####
# ---------------------------------------# 

ds_prof %>% 
  dplyr::group_by(Cohortd)
# remove programs with a crude filter
nrow(locations)
locations <- locations %>% dplyr::filter(!Location_Type=="NULL") # legacy indicator
nrow(locations) # row count
locations <- locations %>% dplyr::filter(!(is.na(First_Admit) & is.na(Last_Admit))) # (still) inactive programs 
nrow(locations)  # row count

# shape up the mapping dataset

locations <- locations %>% 
  dplyr::select(
    Site_Name,                # CERNER
    Facility_Name,            # CERNER
    Building_Name,            # CERNER
    Unit_Name,                # CERNER 
    # Site_Key,                # CERNER
    # Facility_Key,            # CERNER
    # Building_Key,            # CERNER
    # Unit_Key,                # CERNER 
    
    Location_Category,        # Data Warehouse 
    Location_Type,            # Data Warehouse
    Location_Grouping,        # Data Warehouse 
    Num_Encounters,     
    Intensity_Type,           # compressor
    Intensity_Severity_Risk,  # compressor
    Clinical_Focus,           # compressor
    Service_Type,             # compressor
    Service_Location,         # compressor
    Population_Age,           # compressor
    Type_Code                 # Code of the service type
  ) #%>% as.data.frame()


types <- types %>% 
  dplyr::select(
    Type_Code,                # Code of the service type
    Type_Description,         # Description of the service type
    Intensity_Type,           # compressor
    Intensity_Severity_Risk,  # compressor
    Population_Age,           # compressor
    Service_Location,         # compressor
    Clinical_Focus,           # compressor
    Service_Type,             # compressor
    Condensed_Category 
  )
# %>% 
#   dplyr::filter(
#     !Type_Code %in% c(50, 145)
#   )

# ---- pivot-locations ----------------------
# rpivotTable::rpivotTable(data=locations)
# ---- pivot-types --------------------------
# rpivotTable::rpivotTable(data=types)

# ---- define-utility-functions ---------------
# get operational definition for the type of service from compressors-to-types map
# type_number <- 50
get_op_def <- function(
  types,
  type_number,
  print_friendly=F
){
  op_def <- types %>% 
    dplyr::filter(Type_Code == type_number) %>% 
    dplyr::select(
      Intensity_Type,           # compressor
      Intensity_Severity_Risk,  # compressor
      Population_Age,           # compressor
      Service_Location,         # compressor
      Clinical_Focus,           # compressor
      Service_Type              # compressor
    ) %>% 
    as.data.frame()
  testit::assert("The type is not defined uniquely", nrow(op_def)==1L) 
  if(nrow(op_def)==0L){
    op_def[1,] <- NA
    print("Compression is not defined")
  }
  if(print_friendly){
    op_def <- t(op_def) %>% as.data.frame()
    op_def <- data.frame(Compressor = row.names(op_def), Value = op_def[,1], row.names = NULL)
  }
  return(op_def)
}
# Usage:
# get_op_def(types, 96,T ) # print-friendly
# get_op_def(types, 1,F ) # merge-friendly


# view locations associated with a given type of service
get_locations_in_type <- function(
  locations, 
  types, 
  type_number
){
  # type_number = 2
  if("Type_Code" %in% colnames(locations)){
    d_locations <- locations %>% dplyr::select(-Type_Code)
  }else{
    d_locations <- locations 
  }
  
  (op_def <- get_op_def(types, type_number))
  d_type <- types %>% dplyr::filter(Type_Code == type_number)
  d <- dplyr::left_join(d_type, d_locations)
  d <- d %>% dplyr::select(
    Unit_Name, Location_Category, Location_Type, Location_Grouping,
    Num_Encounters
  ) %>% as.data.frame()
  return(d)
}
# Usage:
# get_locations_in_type(locations,types, 5)


# create a summary report for each service type
print_type_summary <- function(
  locations, 
  types, 
  type_number
){
  # extract operational definition of the type (in terms of compressor values)
  (op_def <- get_op_def(types,type_number, F))
  # ensure that the type is defined uniquely
  testit::assert("The type is not defined uniquely", nrow(op_def)==1L)
  # get the locations that match the operational definition of the type
  locations_in_type <- get_locations_in_type(locations,types,type_number)
  # extract the description of the type
  type_description <-types %>% 
    dplyr::filter(Type_Code ==type_number) %>% 
    dplyr::select(Type_Description) %>% 
    as.character()
  # extract the condensed category of the type
  condensed_category <-types %>% 
    dplyr::filter(Type_Code ==type_number) %>% 
    dplyr::select(Condensed_Category) %>% 
    as.character()
  # count the total number of encounters
  encounters_in_type <- locations_in_type %>% 
    dplyr::summarize(n = sum(Num_Encounters)) %>% 
    as.numeric() %>% 
    scales::comma()
  # Organize the printed summary
  cat("\n\n#Type ", type_number, "\n\n")
  # cat("\n\n#Type ", type_number, " - ",type_description,"\n\n")
  cat("**Type Description** :", type_description,"\n\n")
  cat("**Condensed Category** :", condensed_category,"\n\n")
  cat("**Operational Definition** :\n")
  # print(knitr::kable(op_def))
  print(knitr::kable(get_op_def(types, type_number, T)))
  cat("\nTotal **number of encounters** in this Service Type: ",encounters_in_type,"\n")
  cat("\n The following programs are included into this type of VIHA service : \n")
  # print(knitr::kable(list_types))
  print(knitr::kable(locations_in_type,format = "pandoc"))
  
}
# Usage:
# print_type_summary(locations, types, 159)

# ---- print-types -------------------------------------

all_type_numbers <- sort(unique(types$Type_Code))
# excluded_types <- c(9, 12, 60, 61, 65, 85, 96, 159)
# excluded_types <- c(42)
excluded_types <- NULL
selected_types <- setdiff(all_type_numbers, excluded_types) # keep unique to first
# length(all_type_numbers); length(excluded_types); length(selected_types)
# all_type_numbers <- all_type_numbers[all_type_numbers > 96]
for(type_number in selected_types){  
  print_type_summary(locations, types, type_number) 
}


# ---- make-summary-objects -------------------------------------
# merge the files
all_type_numbers <- sort(unique(types$Type_Code))
# excluded_types <- c(9, 12, 60, 61, 65, 85, 96, 159)
# excluded_types <- c(42)
excluded_types <- NULL
selected_types <- setdiff(all_type_numbers, excluded_types) # keep unique to first
# create files to be merged
ds_types <- types %>% dplyr::filter(Type_Code %in% selected_types)
ds_locations <- locations %>% dplyr::select(-Type_Code)
# merge
ds_combined <- dplyr::left_join(ds_types, ds_locations)
# brief diagnositics
# ds_types %>% dplyr::distinct(Type_Code)
# ds_combined %>% dplyr::distinct(Type_Code)
print_types <- ds_combined %>% 
  dplyr::group_by(Type_Code, Type_Description, Condensed_Category) %>% 
  dplyr::summarize(
    n_encounters = sum(Num_Encounters)
  )
print_categories <- ds_combined %>% 
  dplyr::group_by(Condensed_Category) %>% 
  dplyr::summarize(
    n_encounters = sum(Num_Encounters)
  ) 


# ----- print-dynamic-tables --------------
cat("\n#Dynamic Summary\n")
cat("\n##Type\n")
print_types %>% 
  # dplyr::mutate(
  #   Type_Code = factor(Type_Code),
  #   Type_Description = factor(Type_Description),
  #   Condensed_Category = factor(Condensed_Category)
  # ) %>% 
  DT::datatable(
    colnames = c("Type","Description", "Condensed Category", "Encounters"),
    class = 'cell-border stripe',
    caption = "Number of encounteres in each service TYPE",
    filter = "top", 
    options = list(
      pageLength = 10, 
      autoWidth = TRUE
    )
  )
cat("\n\n")

cat("\n##Category\n")
print_categories %>% 
  DT::datatable(
    colnames = c("Condensed Category of a Service Type", "Encounters"),
    class = 'cell-border stripe',
    caption = "Number of encounteres in each service CATEGORY",
    filter = "top", 
    options = list(
      pageLength = 10, 
      autoWidth = TRUE
    )
  )
cat("\n\n")

cat("\n##Program\n")
ds_combined %>% 
  dplyr::select(Type_Code, Type_Description, Condensed_Category, 
                Unit_Name, Location_Category, Location_Type, Location_Grouping,
                Num_Encounters) %>% 
  DT::datatable(
    # colnames = c("Condensed Category of a Service Type", "Encounters"),
    class = 'cell-border stripe',
    caption = "Unit Names and Location properties",
    filter = "top", 
    options = list(
      pageLength = 5, 
      autoWidth = TRUE
    )
  )
# ---- print-static-tables ------------------
cat("\n#Summary")
cat("\n##Type\n")
print_types %>% 
  dplyr::mutate(
    n_encounters = scales::comma(n_encounters)
  ) %>% 
  knitr::kable(
    format = "html",
    # format = "pandoc",
    align = c("c","l","l","r"),
    col.names = c("Type","Description", "Condensed Category", "Encounters")
  ) %>% print()

cat("\n\n")

cat("\n##Category\n")

print_categories %>% 
  dplyr::mutate(
    n_encounters = scales::comma(n_encounters)
  ) %>% 
  knitr::kable(
    format = "html",
    # format = "pandoc",
    align = c("l","r"),
    col.names = c("Condensed Category of a Service Type", "Encounters")
  ) %>% print()
cat("\n\n")

# ---- reproduce -------------------------
rmarkdown::render(
  input = "./reports/06-service-types-updated/service-types.Rmd" ,
  output_format="html_document",
  # output_format="word_document",
  # output_format = "pdf_document",
  clean=TRUE
)
