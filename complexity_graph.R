# Set wroking directory
setwd("~/Desktop/ITESM/MIT REAP/")

# 1. Libraries ------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Install - load tidygraph                                                       
if(require(tidygraph) == FALSE){                                                
  install.packages('tidygraph')                                                 
  library(tidygraph)                                                            
}else{                                                                          
  library(tidygraph)                                                            
}

# Install - load igraph                                                       
if(require(igraph) == FALSE){                                                
  install.packages('igraph')                                                 
  library(igraph)                                                            
}else{                                                                          
  library(igraph)                                                            
}

# Install - load ggraph                                                       
if(require(ggraph) == FALSE){                                                
  install.packages('ggraph')                                                 
  library(ggraph)                                                            
}else{                                                                          
  library(ggraph)                                                            
}

# Install - load economiccomplexity                                                       
if(require(economiccomplexity) == FALSE){                                                
  install.packages('economiccomplexity')                                                 
  library(economiccomplexity)                                                            
}else{                                                                          
  library(economiccomplexity)                                                            
}   

# 2. Data  ----------------------------------------------------------------
catalogo_productos = read_csv('data/raw/censos_economicos/catalogos/tc_codigo_actividad.csv') %>% 
  select(CODIGO, DESC_CODIGO) %>% 
  janitor::clean_names()

catalogo_municipios = read_csv('data/raw/censos_economicos/catalogos/tc_entidad_municipio.csv') %>% 
  mutate(MUNICIPIO = paste0(ENTIDAD, MUNICIPIO),
         country = paste(NOMBRE_MUNICIPIO, NOMBRE_ENTIDAD)) %>% 
  select(country, MUNICIPIO)

censo_economico = list.files('data/raw/censos_economicos/conjunto_de_datos', full.names = TRUE) 

datos = read_csv("data/raw/censos_economicos/conjunto_de_datos/ce2019_cdmx.csv", 
                 col_types = cols_only(ENTIDAD = col_character(), MUNICIPIO = col_guess(), 
                                       CODIGO = col_character(), A131A = col_number()))


# 3. Complexity Calculations ----------------------------------------------
complexity_inputs = datos %>% 
  filter(
    !is.na(MUNICIPIO),
    !is.na(CODIGO),
    nchar(CODIGO) == 6
  ) %>% 
  mutate(
    MUNICIPIO = paste0(ENTIDAD,MUNICIPIO)
  )  %>% 
  left_join(catalogo_municipios, by = 'MUNICIPIO') %>% 
  unique() %>% 
  dplyr::transmute(country = country,
                   product = CODIGO,
                   value = A131A) %>% 
  filter(!is.na(value)) 

indice = balassa_index(complexity_inputs)

complejidad = complexity_measures(indice) %>% 
  lapply(as_tibble, rownames = 'name') %>% 
  lapply(rename, complexity_index = value)

municipality_complexity = pluck(complejidad, 'complexity_index_country') 
product_complexity = pluck(complejidad, 'complexity_index_product')


proximidad = proximity(indice)
proximity_country = pluck(proximidad, 'proximity_country')
proximity_product = pluck(proximidad, 'proximity_product')

gross_value_added = function(column){
  gross_value_added = aggregate(
    x = pull(complexity_inputs, value),
    by = list(name = pull(complexity_inputs, column)),
    FUN = sum
  ) %>% 
    rename(gross_value_added = x)
  return(gross_value_added)
}

municipality_gross_value = gross_value_added('country')
product_gross_value = gross_value_added('product')

networks = projections(
  proximity_country = pluck(proximidad, 'proximity_country'),
  proximity_product = pluck(proximidad, 'proximity_product'),
 # avg_links = ,
  tolerance = 0.01
) 
municipality_network = pluck(networks, 'network_country') %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  mutate(
    state = str_remove(str_extract(name,', .+'),', '),
    municipality = str_remove(name,', .+'),
    degree = degree(municipality_network),
    betweenness = betweenness(municipality_network)
  ) %>% 
  left_join(municipality_gross_value, by = "name") %>% 
  left_join(municipality_complexity, by = "name") 

ggraph(municipality_network, layout = 'graphopt') +
  geom_edge_link(edge_colour = "#a8a8a8", aes(alpha = 1/weight), show.legend = F) +
  geom_node_point(aes(color = log(complexity_index), size = betweenness)) +
  geom_node_text(
    aes(label = municipality), 
    size = 2, vjust = 2.2) +
  labs(col = 'I-CAP', size = 'Centrality') +
  ggtitle("Proximidad entre Alcaldías") +
  theme_void()



product_network = networks %>% 
  activate(nodes) %>% 
  left_join(product_gross_value, by = "name") %>% 
  left_join(product_complexity, by = "name") %>% 
  left_join(catalogo_productos, by = c('name'='codigo'))

ggraph(product_network, layout = 'kk') +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(col = 'orange') +
  geom_node_text(
    aes(label = name),
    size = 3) +
  ggtitle("Product Space") +
  theme_void()

product_tlalpan = datos %>% 
  filter(
    str_detect(country,'Ciudad de México'),
    str_detect(country,'Tlalpan')     
  ) %>% 
  pull(product)

