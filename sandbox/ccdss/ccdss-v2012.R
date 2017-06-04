# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(knitr)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals -------------------------

age_quarter_period <- c(
     "1 to 4"  , 1 , 1      
    ,"5 to 9"  , 1 , 2   
    ,"10 to 14", 1 , 3     
    ,"15 to 19", 1 , 4     
    ,"20 to 24", 1,  5     
    ,"25 to 29", 2 , 1     
    ,"30 to 34", 2 , 2     
    ,"35 to 39", 2 , 3     
    ,"40 to 44", 2 , 4     
    ,"45 to 49", 2 , 5     
    ,"50 to 54", 3 , 1     
    ,"55 to 59", 3 , 2     
    ,"60 to 64", 3 , 3     
    ,"65 to 69", 3 , 4     
    ,"70 to 74", 3 , 5     
    ,"75 to 79", 4 , 1     
    ,"80 to 84", 4 , 2     
    ,"85+"     , 4 , 3
)
ds_aqp <- matrix(age_quarter_period,18,3,byrow = T) %>% as.data.frame()
names(ds_aqp) <- c("age","quarter", "period")
# ---- utility-functions -----------------------

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for origin of the data
ds_wide <- readr::read_csv("./data-unshared/raw/ccdss-scsmc-eng.csv")
# ---- inspect-data ----------------------------
# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
# ds %>% glimpse()

# ---- tweak-data ------------------------------
ds_wide <- ds_wide %>% 
  dplyr::rename_(  
    "disease" = "Disease"                     
    ,"year"    = "`Fiscal Year`"
    ,"sex"     = "Gender"
    ,"age"     = "`Age Group`"
  )
# ds <- ds%>% 
  # dplyr::filter(disease == "Mental_Illness") %>% 
  # dplyr::filter(disease == "MoodAnxiety_Disorder") #%>%
  # dplyr::filter(year == 2007)
# d <- ds
# ds <- d
ds_wide %>% glimpse()
ds_wide %>% distinct(disease) %>% kable()
static_variables <- c("disease","year","sex","age") # 828 vs 720
# static_variables <- c("disease","year","sex","age","Population") # 828 vs 720
# static_variables <- c("disease","year","sex","age","population","incident_cases","prevalent_cases")
dynamic_variables <- colnames(ds_wide) %>% setdiff( static_variables)
ds_long <- ds_wide %>% 
  tidyr::gather_("variable", "value", dynamic_variables) #%>% 
# d <- ds_long
ds_long %>% head()
ds_long %>% distinct(variable) %>% kable() 
# ds_long %>% distinct(variable) %>% readr::write_csv("./data-unshared/derived/rename_stencil.csv")
# d <- ds_long
rename_stencil <- readr::read_csv("./data-unshared/derived/rename_stencil_1.csv")
# rename_stencil <- readr::read_csv("./data-unshared/derived/rename_stencil_2.csv")
rename_stencil %>% kable()
# structure for use
ds <- ds_long %>% 
  dplyr::left_join(rename_stencil, by = "variable") %>% 
  dplyr::left_join(ds_aqp, by = "age") %>% 
  dplyr::select_(.dots = c(
    "disease","year","sex","age","quarter","period",
    "condition_present",
    "variable","index","location", "unit_count","value"
  )) %>% 
  dplyr::mutate(
    value   = as.numeric(value),
    quarter = factor(quarter, levels = c(1:4),labels = c("0 - 25","26-50","51-75","76-85+")),
    period  = factor(period,  levels = c(1:5))
  )
ds %>% glimpse()
d <- ds
# ---- basic-table -----------------------------

# ---- basic-graph -----------------------------
# elemental_line <- function(
#   d 
# ){

  select_disease <- c("Mental_Illness")
  # select_disease <- c("MoodAnxiety_Disorder")
  # select_index <- c("Incidence")
  # select_index <- c("Prevalence")
  # select_index <- c("Mortality")
  select_index <- c("Hospitalization")
  # select_index <- c("Population")
  # select_location <- c("Population")
  select_unit_count <- c("person")
  select_condition_present <- c(TRUE)
  filter_criteria_disease           <- lazyeval::interp(~ which_column %in% select_disease,          which_column = as.name("disease"))
  filter_criteria_index             <- lazyeval::interp(~ which_column %in% select_index,             which_column = as.name("index"))
  filter_criteria_location          <- lazyeval::interp(~ which_column %in% select_location,          which_column = as.name("location"))
  filter_criteria_unit_count        <- lazyeval::interp(~ which_column %in% select_unit_count,        which_column = as.name("unit_count"))
  filter_criteria_condition_present <- lazyeval::interp(~ which_column %in% select_condition_present, which_column = as.name("condition_present"))
  d1 <- d %>% 
    dplyr::filter_(filter_criteria_disease) %>% 
    dplyr::filter_(filter_criteria_index) %>% 
    dplyr::filter_(filter_criteria_unit_count)
  # d1 %>% distinct(disease,year,sex,age,variable)  
  #   
  d1 %>% glimpse
  
  g1 <- d1 %>% 
    # dplyr::filter_(filter_criteria_location) %>% 
    # dplyr::filter_(filter_criteria_unit_count) %>%
    # dplyr::filter_(filter_criteria_condition_present) %>% 
    ggplot(aes_string(x="year",y="value")) +
    geom_line(aes_string(group = "age", color = "quarter"), stat="identity", size =3)+
    geom_text(aes_string(label = "period"))+
    scale_y_continuous(labels = scales::comma)+
    scale_color_brewer(type="qual")+
    # facet_grid(. ~ sex)+
    facet_grid(sex ~ condition_present)+
    labs(color = "Age group")+
    theme_minimal()
  g1

# }
# Usase
# d %>% elemental_line()

# ---- dev-a-0 ---------------------------------
# see ./data-unshared/contents.md for origin of the data
# see http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html for data dictionary
# d %>% glimpse()

# ---- dev-a-1 ---------------------------------
# what unique diseases are represented in teh report?
# ds_wide %>% group_by(disease) %>% summarize(incidence = sum(incident_cases)) 
ds_wide %>% group_by(disease) %>% summarize(incidence = sum(`Incident Cases`)) 
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/ccdss/ccdss-v2012.Rmd"
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

