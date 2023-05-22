# Working directory
setwd("~/Documents/datascience/MITREAP")


# Libraries ---------------------------------------------------------------
# Load and install tidyverse
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Load and install sf
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
}


# Data --------------------------------------------------------------------
# Boroughs GIS
colonias = read_sf('data/colonias_iecm')

# Raw economic units
files = list.files('data/raw/denue',full.names = TRUE)

# This gunction extract the id from each file
get_index_file = function(file){
  digits = str_extract(file, '\\d{4}') 
  index_file = str_c( str_extract(digits, '^\\d{2}'), '20', str_extract(digits, '\\d{2}$'))
  return(index_file)
}

# Iterate over files
for(file in files){
  # Read file
  df = read_csv(
    file = file,
    # Select columns
    col_types = cols_only(
      id = col_guess(), codigo_act = col_number(), nomb_asent = col_guess(),
      cve_mun = col_guess(), municipio = col_guess(), latitud = col_guess(), 
      longitud = col_guess()
      )
    )
  
  # Entrepreneurial Quality files
  entrepreneurial_quality = df %>% 
    left_join(
      # Read quality index (predicted probability)
      y = read_csv(paste0('data/outputs/entrepreneurial_quality/index/', get_index_file(file), '.csv')), 
      # Join by id
      by = 'id'
      ) %>% 
    # Add date
    mutate(period = lubridate::my(get_index_file(file))) %>% 
    # Make GIS
    st_as_sf(coords = c("longitud","latitud"), crs = 4326) %>% 
    # Transform projection
    st_transform(crs = st_crs(colonias)) %>% 
    # Spatial Join
    st_join(colonias) %>% 
    # Get rid of geometry
    st_drop_geometry() %>% 
    # By groups
    with_groups(
      .groups = c(period, ENT, CVEDT, NOMDT, CVEUT, NOMUT),
      # Aggregate
      summarise,
      eqi = mean(probability_1, na.rm = T),
      n = n()
    )
  
  # Write file to csv
  write_excel_csv(
    x = entrepreneurial_quality, 
    file = paste0('data/outputs/entrepreneurial_quality/counties/', get_index_file(file), '.csv')
    )
}
