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
# path_input <- "./data-unshared/derived/dto_addictions_4264.rds"
path_input <- "https://raw.githubusercontent.com/IHACRU/ihacru-2017-psy513/master/data-public/derived/dto_addictions_4264.csv"
# define output format for the report
options(
  knitr.table.format = "html"
  ,tibble.width = 110
  #   ,bootstrap_options = c("striped", "hover", "condensed","responsive")
)
# ---- utility-functions -------------------------------------------------------
# functions local to this script go here. 
# ---- load-data ---------------------------------------------------------------
# ds <- readRDS(path_input)
ds <- readr::read_csv(path_input)
# ---- inspect-data -----------------------------------------------------------
ds %>% dplyr::glimpse(70)
# ---- tweak-data ------------------------------------------------------------
ds %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(
    n = length(unique(encounter_id))
  ) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::select(id, n)

# ---- utility-functions ------------------------------------------------------
# flatten the timeline of one individual
flatten_one <- function(
  df,          # data frame  
  person_id,   # ids of person, whose timeline will be flattened
  pivot        # the discrete variable used for flattening (e.g. palette_code)
){
  # Temp values for testing and development
  # df        <- ds 
  # # person_id = 907750 #906466
  # person_id = 906466
  # # person_id = 908009
  # # person_id = sample(unique(ds$id),1)
  # pivot     = "palette_code"
  
  d1 <- df %>% 
    dplyr::filter(id == person_id) %>% 
    # dplyr::select(id,encounter_id,encounter_class, encounter_type, event_type,event_count, palette_code,
    #               # ,palette_colour_name_display
    #               duration_days) %>% 
    dplyr::group_by_(.dots = c("id",pivot)) %>% 
    dplyr::summarize(
      n_encounters = length(unique(encounter_id)),
      n_events     = sum(event_count),
      n_days       = sum(duration_days)
    ) 
  d_encounters <- d1 %>% dplyr::select(id, palette_code, n_encounters)%>% tidyr::spread(key = palette_code, value = n_encounters)
  d_events     <- d1 %>% dplyr::select(id, palette_code, n_events)%>% tidyr::spread(key = palette_code, value = n_events)
  d_days       <- d1 %>% dplyr::select(id, palette_code, n_days) %>% tidyr::spread(key = palette_code, value = n_days)

  ls_temp <- list(
    "encounters" = d_encounters,
    "events"     = d_events,
    "days"       = d_days   
  )
  d2 <- ls_temp %>% dplyr::bind_rows(.id = "metric") %>% dplyr::select(id, metric, dplyr::everything())
  return(d2)
}
# Usage:
# d <- ds %>% flatten_one(person_id = "102DBC2", pivot = "palette_code")
# d <- ds %>% flatten_one(person_id = "DC2302", pivot = "palette_code")

flatten_many <- function(
  df, 
  target_ids, 
  attach_vars = c("gender","age_group")
) {
  # Values for testing and development
  # df          <- ds
  # target_ids  <- c("102DBC2", "DC2302")
  # attach_vars <- c("gender","age_group")
  
  d_patients <- df %>% 
    dplyr::distinct_(.dots = c("id", attach_vars) )
  d_categories <- df %>%
    dplyr::distinct_(.dots = c("palette_code","palette_colour_name")) %>% 
    dplyr::arrange(palette_code) %>% 
    as.data.frame()
  # create a list object to capture a person in each element
  ls_temp <- list()
  for(i in target_ids){
    ls_temp[[as.character(i)]] <- ds %>% flatten_one(i, "palette_code")
  }
  d_wide <- ls_temp %>% dplyr::bind_rows()
  # head(dd)
  static_variables  <- c("id","metric")
  dynamic_variables <-  setdiff(colnames(d_wide), c("id","metric"))
  d_long <- d_wide %>% 
    # tidyr::gather_()
    tidyr::gather_("palette_code","value", dynamic_variables ) %>% 
    dplyr::mutate(palette_code = as.integer(palette_code)) %>% 
    dplyr::left_join(d_patients, by = "id") %>% 
    dplyr::left_join(d_categories, by = "palette_code") %>% 
    dplyr::select_(.dots = c(colnames(d_patients),"palette_code", "metric",  "value", "palette_colour_name")) %>% 
    dplyr::arrange(id) %>% 
    dplyr::mutate(
      gender              = factor(gender),
      age_group           = factor(age_group),
      metric              = factor(metric),
      palette_code        = factor(palette_code, 
                                   levels = d_categories[,"palette_code"], 
                                   labels = d_categories[,"palette_colour_name"]),
      palette_colour_name = factor(palette_colour_name)
    )
  return(d_long)
}
# Usage:
# select_ids <- sample(unique(ds$id), 10) 
select_ids <- unique(ds$id) 
d <- ds %>% flatten_many(select_ids)

quick_save <- function(
  g,            # ggplot object to be saved
  name,         # name of the file to be saved   
  width  = 1100, # width in pixels  
  height = 800, # height in pixesl  
  dpi    = 300  # resolution, dots per inch 
){
  ggplot2::ggsave(
    filename= paste0(name,".png"), 
    plot=g,
    device = png,
    path = "./guided-exploration/lab2/prints/",
    width = width,
    height = height,
    # units = "cm",
    dpi = dpi,
    limitsize = FALSE
  )
}

# ---- graphing-functions ----------------------------
g1 <- d %>% 
  dplyr::filter(metric == "encounters") %>% 
  # ggplot(aes(x=palette_code, y = value)) + 
  ggplot(aes(x=palette_colour_name, y = value)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal() 

g1 %>% quick_save("plot_1")


# ---- start-modeling --------------------------------
dm <- d %>% 
  dplyr::filter(metric == "encounters") %>% 
  dplyr::mutate()

m1 <- glm(formula = value ~ 1 + gender + age_group + palette_code, data = dm)
library(broom)
broom::glance(m1)
t1 <-broom::tidy(m1)
t1 <- t1 %>% 
  dplyr::mutate(
    term = sub("^palette_code", "", term)
  )

# remove leading zero
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

prettify_table <- function(x){
  x <- t1
  names(x) <- c("term","est","se","wald","pval")  
  
  x$sign <- ifelse(x$pval >.10, ">.10",
                   ifelse(x$pval <= .10 & x$pval > .05, "<=.10",
                          ifelse(x$pval <= .05 & x$pval > .01, "<=.05",
                                 ifelse(x$pval <= .01 & x$pval > .001, "<=.01",
                                        ifelse(x$pval <= .001, "<=.001", NA)))))
  x <- x %>% 
    dplyr::mutate( 
      est_pretty = numformat(est),
      se_pretty = numformat(se),
      wald_pretty = numformat(wald),
      pval_pretty = numformat(pval),
      dense = sprintf("%6s(%4s),p=%3s, %5s",est_pretty, se_pretty, pval_pretty, sign)
    ) %>% 
    dplyr::select(term,dense )
x
}



# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_report_1 <- "./guided-exploration/lab2/lab2.Rmd"
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


