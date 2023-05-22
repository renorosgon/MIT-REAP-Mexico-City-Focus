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
files = list.files('data/raw/model_inputs', full.names = TRUE)

# Define a logistic regression model
iq_model = logistic_reg() %>% 
  set_mode('classification')%>% 
  set_engine("spark") 

# Fit model for every file
for(file in files){
  # Start Spark Cluster 
  sc = spark_connect(master="local", version = '3.3.1')
  # Load data
  denue = spark_read_csv(sc, file, overwrite = TRUE, name = 'denue')
  # Feature engineering for modeling
  features = denue %>% 
    # Strings to factors
    ft_string_indexer(input_col = 'employees_30_or_more', output_col = 'y') %>%
    ft_string_indexer(input_col = 'tipo_vial', output_col = 'tipo_vial_index') %>%
    ft_string_indexer(input_col = 'tipo_asent', output_col = 'tipo_asent_index') %>%
    ft_string_indexer(input_col = 'tipoUniEco', output_col = 'tipoUniEco_index') %>% 
    ft_string_indexer(input_col = 'codigo_act', output_col = 'codigo_act_index') %>% 
    # One hot encoding
    ft_one_hot_encoder(
      input_cols = c('tipo_vial_index','tipo_asent_index','tipoUniEco_index','codigo_act_index'),
      output_cols = c('tipo_vial_onehot','tipo_asent_onehot','tipoUniEco_onehot','codigo_act_onehot')
    ) %>% 
    # One hot vector assembler
    ft_vector_assembler(
      input_cols =  c('tipo_vial_onehot','tipo_asent_onehot','tipoUniEco_onehot','codigo_act_onehot'), 
      output_col = 'one_hot'
      ) %>% 
    # Numeric vector assembler
    ft_vector_assembler(
      input_cols = c('length_name','edificio','telefono','correo_electronico','year_alta',
                     'month_alta','month_open', "razon_cv","razon_rl","razon_ac","razon_sapi",
                     "razon_sab","razon_pr","razon_sofom","razon_sfp","razon_scl","razon_spr", 
                     "razon_sc","razon_inc","razon_sas","razon_sofi"), 
      output_col = 'numericas'
      ) 
  
  
  # Dataset in unbalanced
  balance = pull(features, employees_30_or_more) %>% 
    table()
  
  # Balancing the dataset
  balance_true =filter(features, y == 1)
  
  balance_false = filter(features, y == 0) %>% 
    sample_n(size = balance[2])
  
  balanced = sdf_bind_rows(balance_true, balance_false)
  
  # Split train/test sets
  data_split = sdf_random_split(balanced, training = 0.75, test = 0.25, seed = 123)
  training = pluck(data_split, 'training') 
  test = pluck(data_split, 'test') 
  
  # Fit treining data
  iq_fit = iq_model %>% fit(
    formula = y ~ numericas + one_hot,
    data = training
  )
  
  # Check training performance
  iq_fit %>% 
    augment(training) %>% 
    select(y, .prediction) %>% 
    collect() %>% 
    mutate_all(as.factor) %>% 
    conf_mat(truth = y, estimate = .prediction) %>% 
    summary() %>%
    # Save metrics
    write_excel_csv(
      paste0(
        'data/outputs/entrepreneurial_quality/metrics/training_',
        str_extract(file,'[:digit:]+'),
        '.csv'
        )
      )
  
  # Check test performance
  iq_fit %>% 
    augment(test) %>% 
    select(y, .prediction) %>% 
    collect() %>% 
    mutate_all(as.factor) %>% 
    conf_mat(truth = y, estimate = .prediction) %>% 
    summary() %>%
    # Save metrics
    write_excel_csv(
      paste0(
        'data/outputs/entrepreneurial_quality/metrics/testing_',
        str_extract(file,'[:digit:]+'),
        '.csv'
      )
    )
  
  # Check full dataset performance
  iq_fit %>% 
    augment(features) %>% 
    select(y, .prediction) %>% 
    collect() %>% 
    mutate_all(as.factor) %>% 
    conf_mat(truth = y, estimate = .prediction) %>% 
    summary() %>%
    # Save metrics
    write_excel_csv(
      paste0(
        'data/outputs/entrepreneurial_quality/metrics/full_',
        str_extract(file,'[:digit:]+'),
        '.csv'
      )
    )
  
  # Predict probabilities (EQI)
  results = iq_fit %>% 
    extract_fit_engine() %>% 
    ml_predict(features) %>% 
    select(id, prediction:probability_1) %>% 
    collect() 
  
  # Write results
  write_excel_csv(
    x = results, 
    file = paste0(
      'data/outputs/entrepreneurial_quality/index/',
      str_extract(file,'[:digit:]+'),
      '.csv'
      )
    )
  
  # Tidying coefficients
  coefficients = tidy(iq_fit) %>% 
    mutate(multipliers = exp(coefficients))
  
  # Save coefficients
  write_excel_csv(
    x = coefficients, 
    file = paste0(
      'data/outputs/entrepreneurial_quality/coefficients/',
      str_extract(file,'[:digit:]+'),
      '.csv'
      )
    )
  
  # Save Model
  ml_save(
    x = extract_fit_engine(iq_fit),
    path = paste0(
      'data/outputs/entrepreneurial_quality/models/',
      str_extract(file,'[:digit:]+')
    ),
    overwrite = TRUE
    )
  
  # We are done with spark
  spark_disconnect(sc)
}


