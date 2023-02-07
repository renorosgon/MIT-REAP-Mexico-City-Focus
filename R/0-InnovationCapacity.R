setwd("~/Desktop/ITESM/DSSG")

# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Install - load p2distance                                                       
if(require(p2distance) == FALSE){                                                
  devtools::install_github('https://github.com/cran/p2distance')                                               
  library(p2distance)                                                            
}else{                                                                          
  library(p2distance)                                                            
}

# Install - load stratification                                                       
if(require(stratification) == FALSE){                                                
  install.packages('stratification')                                                 
  library(stratification)                                                            
}else{                                                                          
  library(stratification)                                                            
}

# Install - load ggcorrplot                                                       
if(require(ggcorrplot) == FALSE){                                                
  install.packages('ggcorrplot')                                                 
}else{                                                                          
  library(ggcorrplot)                                                            
}

# Data  -------------------------------------------------------------------
ici = readxl::read_xlsx('data/raw/ici_estatal.xlsx') %>% 
  # Clean names
  janitor::clean_names()

# Quick overview
glimpse(ici)
summary(ici)

# Correlation analysis
ici %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot()

# P2 Distance Index -------------------------------------------------------
ici_matrix = ici %>% 
  # Select relevant columns
  select(literacy_rate, secondary_gross_enrolment_ratio,
         tertiary_gross_enrolment_ratio, gini_index,electrification_rate:paved_roads,
         start_bussiness_days:start_bussiness_cost_as_percap_income_share,it_pc_users_pct,
         it_internet_users_pct, it_mobile_users_pct, it_tv_households_pct, 
         it_paytv_households_pct,it_fixed_internet_households_pct,rnd_patents_granted_pct,
         rnd_patents_requested_pct,rnd_sni_pmil, rnd_students_graduate_pmill, 
         rnd_indexed_production, rnd_quality_graduate_programs_pct_students,
         research_and_develoment_pct,students_in_science_and_technology_pct,
         trust_in_congress, trust_federal_gov, trust_police, trust_businesspeople,
         egov_procedures_rate, corrupcion_incidence_rate
  ) %>% 
  # Ensure all of them are numeric
  mutate_all(as.numeric) %>% 
  # Scale each colum
  scale() %>% 
  # Transform to a matrix
  as.matrix()

# Calculate p2distance
index = p2distance(ici_matrix, reference_vector_function = min)

# Add the index to the dataframe
ici = ici %>% 
  mutate(
    icap_index = pluck(index, 'p2distance') %>% 
           as.numeric(),
   icap_strata = factor(
     strata.cumrootf(icap_index, n = nrow(ici), Ls = 5) %>% pluck('stratumID'),
     levels = 5:1,
     labels = c('Very High','High','Neutral','Low','Very Low')
     )
   )

# A quick view on the results
ggplot(
  data = ici, 
  mapping = aes(x = icap_index, y = reorder(entidad, icap_index))
  ) +
  geom_col(aes(fill = icap_strata)) +
  labs(
    title = 'Mexico City has the highest innovation capacity',
    x = 'Index',
    fill = 'Innovation Capacity') +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'top'
  )

# Explaining the index
# Discrimination Coefficient
pluck(index, 'discrimination.coefficient') %>% 
  as_tibble( rownames = 'element' ) %>% 
  ggplot(aes(x = value, y = reorder(element, value))) +
  geom_col() +
  labs(
    title = 'I-Cap Index Discrimination Coefficient',
    x = 'Discrimination coefficient'
    ) +
  theme(
    axis.title.y = element_blank()
  )

# Correlation Coefficient
pluck(index, 'cor.coeff') %>% 
  as_tibble( rownames = 'element' ) %>% 
  ggplot(aes(x = p2distance.20, y = reorder(element, p2distance.20))) +
  geom_col() +
  labs(
    title = 'I-Cap Index Structure',
    x = 'Correlation coefficient'
  ) +
  theme(
    axis.title.y = element_blank()
  )


# Index Validation --------------------------------------------------------
# Install - load  rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}    

# Install - load  httr                                                       
if(require(httr) == FALSE){                                                
  install.packages('httr')                                                 
  library(httr)                                                            
}else{                                                                          
  library(httr)                                                            
}    

# Install - load  rjson                                                       
if(require(rjson) == FALSE){                                                
  install.packages('rjson')
}                                                 
  
# GET trade Economic Complexity Index from the Economics Ministry
eci = GET(
  # Set URL and path
  url = "https://datamexico.org/", path  = 'api/data',
  # Set query
  query = 
    list(Quarter = 20212, cube = "complexity_eci", drilldowns='ECI Ranking,State',
         measures = 'ECI',parents = 'false', sparse = 'false', locale = 'es'
         )
  ) %>% 
  # Get html text and structure the json format
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>% 
  bind_rows() %>% janitor::clean_names() %>% 
  # Uniform state names
  mutate(
    state = case_when(
      state == 'Coahuila de Zaragoza' ~ 'Coahuila',
      state == 'Michoacán de Ocampo' ~ 'Michoacán',
      state == 'Veracruz de Ignacio de la Llave' ~ 'Veracruz',
      TRUE ~ state
    )
  )

# Join i-cap index with economic complexity
validation = ici %>% 
  inner_join(eci, by = c('entidad'='state'))

# Correlation analysis
validation %>% 
  select(icap_index, eci) %>% 
  cor() 

# Dispersion
ggplot(validation, aes(x = eci, y = icap_index)) +
  geom_hline(
    aes(yintercept = mean(icap_index)),
    linetype = 'dashed', lwd = 0.5, col ='gray70'
    ) +
  geom_vline(aes(xintercept = mean(eci)),
             linetype = 'dashed', lwd = 0.5, col ='gray70'
  ) + 
  geom_point() +
  geom_text(
    aes(y = mean(icap_index)), 
    x = -2, label = 'Average I-Cap', hjust= 'left', 
    vjust = 1.5, col = 'gray50', family = 'Bebas Neue',
    size = 3
  ) +
  geom_text(
    aes(x = mean(eci)), 
    y = 25, label = 'Average Complexity Index', hjust= 'right', 
    vjust = 1.5, col = 'gray50', family = 'Bebas Neue', angle = 90,
    size = 3
  ) + 
  geom_text(aes(label=entidad), vjust = 1.5, family = 'Bebas Neue') +
  labs(
    title = 'Mexico City is a complex economy with high innovation capacity',
    x = ' Economic Complexity Index',
    y = 'Innovation Capacity Index'
  )

# Save the results
ici %>% 
  select(cve_ent, icap_index, icap_strata) %>% 
  write_excel_csv('data/outputs/icap_estatal.xlsx')
