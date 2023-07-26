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
# Load and install tidymodels
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Load and install sparklyr
if(require(sparklyr) == FALSE){                                                
  install.packages('sparklyr')                                                 
  library(sparklyr)                                                            
}else{                                                                          
  library(sparklyr)                                                            
}

# Data --------------------------------------------------------------------
source('R/1-IcapDataPrep.R')

# Start Spark Cluster 
sc = spark_connect(master="local", version = '3.3.1')


files = list.files(path = 'data/denue_entidades/model_input', full.names = TRUE)

# Fit the best model
iq_model = logistic_reg(penalty = 0) %>% 
  set_mode('classification')%>% 
  set_engine("spark") 


for (file in files) {
  
  # Load data
  denue = spark_read_csv(sc, file, overwrite = TRUE, name = 'denue')
  
  # Feature engineering for modeling
  denue_k = denue %>% 
    ft_string_indexer(input_col = 'employees_30_or_more', output_col = 'y') %>%
    ft_string_indexer(input_col = 'tipo_vial', output_col = 'tipo_vial_index') %>%
    ft_string_indexer(input_col = 'tipo_asent', output_col = 'tipo_asent_index') %>%
    ft_string_indexer(input_col = 'tipoUniEco', output_col = 'tipoUniEco_index') %>% 
    ft_string_indexer(input_col = 'codigo_act', output_col = 'codigo_act_index') %>% 
    ft_one_hot_encoder(
      input_cols = c('tipo_vial_index','tipo_asent_index',
                     'tipoUniEco_index','codigo_act_index'),
      output_cols = c('tipo_vial_onehot','tipo_asent_onehot',
                      'tipoUniEco_onehot','codigo_act_onehot')
    ) %>% 
    ft_vector_assembler(
      input_cols =  c('tipo_vial_onehot','tipo_asent_onehot',
                      'tipoUniEco_onehot','codigo_act_onehot'), 
      output_col = 'one_hot') %>% 
    ft_vector_assembler(
      input_cols = c('length_name','edificio','telefono','correo_electronico','year_alta','month_alta',
                     "razon_cv","razon_rl","razon_ac","razon_sapi","razon_sab","razon_pr","razon_sofom",
                     "razon_sfp","razon_scl","razon_spr", "razon_sc","razon_inc","razon_sas","razon_sofi"), 
      output_col = 'numericas') 
  
  downsampling_size = pull(denue, employees_30_or_more) %>% 
    table() %>% 
    last()
  
  # Tuning ------------------------------------------------------------------
  # Balancing the dataset
  balanceo_true = denue_k  %>% 
    filter(y == 1)
  
  balanceo_false = denue_k  %>% 
    filter(y == 0) %>% 
    sample_n(size = downsampling_size)
  
  balanceo = sdf_bind_rows(balanceo_true, balanceo_false)
  
  # Split train/test sets
  datos_split = balanceo %>% 
    sdf_random_split(training = 0.75, test = 0.25, seed = 123)
  
  iq_fit = iq_model %>% fit(
    formula = y ~ numericas + one_hot,
    data = pluck(datos_split, 'training') 
  )
  
  # Check training performance
  print(
    iq_fit %>% 
      augment( pluck(datos_split, 'training') ) %>% 
      select(y, .prediction) %>% 
      collect() %>% 
      mutate_all(as.factor) %>% 
      conf_mat(truth = y, estimate = .prediction) %>% 
      summary()
    )
    
  # Check test performance
  print(
    iq_fit %>% 
      augment(pluck(datos_split, 'test') ) %>% 
      select(y, .prediction) %>% 
      collect() %>% 
      mutate_all(as.factor) %>% 
      conf_mat(truth = y, estimate = .prediction) %>% 
      summary()
  )
  
  # Check full dataset performance
  print(
    iq_fit %>% 
      augment(denue_k) %>% 
      select(y, .prediction) %>% 
      collect() %>% 
      mutate_all(as.factor) %>% 
      conf_mat(truth = y, estimate = .prediction) %>% 
      summary()
    )
  
  # Predict probabilities (IQ)
  iq_fit %>% 
    extract_fit_engine() %>% 
    ml_predict(denue_k) %>% 
    select(cve_ent:id, employees_30_or_more:razon_sofi, prediction:probability_1) %>% 
    collect() %>% 
    write_excel_csv(
      paste0('data/denue_entidades/outputs/probabilities/', str_extract(file, '\\d{2}'), '_112022.csv')
      )
  
  # Tidying coeficients
  tidy(iq_fit) %>% 
    write_excel_csv(
      paste0('data/denue_entidades/outputs/coefficients/', str_extract(file, '\\d{2}'), '_112022.csv')
    )

}
# We are done with spark
spark_disconnect(sc)


