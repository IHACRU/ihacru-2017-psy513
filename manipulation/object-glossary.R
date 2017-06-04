# Glossary of Objects and Names

### define-group-objects ###
cerner_address_names <- c(
  "site_name",          # CERNER   
  "facility_name",      # CERNER     
  "building_name",      # CERNER  
  "unit_name"           # CERNER 
)

cerner_address_keys <- c(
  "site_key",          # CERNER   
  "facility_key",      # CERNER     
  "building_key",      # CERNER  
  "unit_key"           # CERNER 
)

data_warehouse_address <- c(
  "location_category",  # DATA WAREHOUSE
  "location_grouping",  # DATA WAREHOUSE
  "location_type"       # DATA WAREHOUSE 
)
# combination of the above two defines EHR address
ehr_address <- c(
  cerner_address_names,
  data_warehouse_address
)

ehr_address_full <- c(
  ehr_address,
  cerner_address_keys
)
# lenses with which to look closely at a program
compressor_names <- c(
  "intensity_type",         
  "intensity_severity_risk",
  "clinical_focus",         
  "service_type",           
  "service_location",       
  "population_age",
  "provider_mix"
)
# salient properties shared by a group of programs
# defined manually, using the expressive language of compressors/classifiers
program_classes <- c(
  "location_class_code",
  "location_class_description"
)

palette_colours <- c(
  "palette_code",
  "palette_colour_name"
)

