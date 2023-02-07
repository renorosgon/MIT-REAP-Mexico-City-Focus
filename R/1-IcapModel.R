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
# Load and install lubridate
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
  library(lubridate)                                                            
}else{                                                                          
  library(lubridate)                                                            
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

# Load data
denue = spark_read_csv(sc, 'data/raw/model_input.csv', overwrite = TRUE, name = 'denue')

# Feature engeniereing for modeling
denue_k = denue %>% 
  ft_string_indexer(input_col = 'tipo_vial', output_col = 'tipo_vial_index') %>%
  ft_string_indexer(input_col = 'tipo_asent', output_col = 'tipo_asent_index') %>% 
  ft_string_indexer(input_col = 'municipio', output_col = 'municipio_index') %>% 
  ft_string_indexer(input_col = 'tipoUniEco', output_col = 'tipoUniEco_index') %>% 
  ft_string_indexer(input_col = 'codigo_act', output_col = 'codigo_act_index') %>% 
  ft_string_indexer(input_col = 'razon', output_col = 'razon_index') %>% 
  ft_one_hot_encoder(
    input_cols = c('tipo_vial_index','tipo_asent_index','municipio_index','tipoUniEco_index','codigo_act_index','razon_index'),
    output_cols = c('tipo_vial_onehot','tipo_asent_onehot','municipio_onehot','tipoUniEco_onehot','codigo_act_onehot','razon_onehot')
  ) %>% 
  ft_vector_assembler(
    input_cols =  c('tipo_vial_onehot','tipo_asent_onehot','municipio_onehot','tipoUniEco_onehot','codigo_act_onehot','razon_onehot'), 
    output_col = 'one_hot') %>% 
  ft_vector_assembler(
    input_cols = c('length_name','edificio','telefono','correoelec','year_alta','month_alta','month_open'), 
    output_col = 'numericas') %>%
  ft_interaction(input_cols = c('one_hot'), output_col = 'interacciones') %>% 
  select(employees_30_or_more, numericas, one_hot, interacciones)


# Dataset in unbalanced
pull(denue, employees_30_or_more) %>% 
  table()


# Tuning ------------------------------------------------------------------
# Balancing the dataset
balanceo_true = denue_k  %>% 
  filter(employees_30_or_more == 1)

balanceo_false = denue_k  %>% 
  filter(employees_30_or_more == 0) %>% 
  sample_n(size = 17174)

balanceo = sdf_bind_rows(balanceo_true, balanceo_false)

# Split train/test sets
datos_split = balanceo %>% 
  sdf_random_split(training = 0.75, test = 0.25, seed = 123)

training = datos_split %>% 
  pluck('training') 

test = datos_split %>% 
  pluck('test') 

# Create a pipeline
pipeline = ml_pipeline(sc) %>%
  ft_r_formula(employees_30_or_more ~ numericas + one_hot + interacciones) %>%
  ml_logistic_regression()

# Tuning grid
grid =  list(
  logistic_regression = list(
    elastic_net_param = 1, 
    reg_param = seq(0, 10, length = 100)    
  )
)

# Cross validation
cv = ml_cross_validator(
  sc, 
  estimator = pipeline, 
  estimator_param_maps = grid,
  evaluator = ml_binary_classification_evaluator(sc, metric_name = "areaUnderROC"),
  num_folds = 10,
  parallelism = 8
)

# Fit cross validation
cv_model <- ml_fit(cv, training) 

# Get performance metrics
metricas = ml_validation_metrics(cv_model) %>% 
  arrange(areaUnderROC)

print(metricas)

# Get the best parameter
reg_param = cv_model %>% 
  pluck('best_model') %>% 
  pluck('stages') %>% 
  pluck(2) %>% 
  pluck('param_map') %>% 
  pluck('reg_param')


# Tidymodeling and results ------------------------------------------------
# Fit the best model
iq_model = logistic_reg(penalty = reg_param) %>% 
  set_mode('classification')%>% 
  set_engine("spark") %>%
  fit(
    formula = employees_30_or_more ~ numericas + one_hot + interacciones,
    data = training
      )

# Check training performance
iq_model %>% 
  augment(training) %>% 
  select(employees_30_or_more, .prediction) %>% 
  collect() %>% 
  mutate_all(as.factor) %>% 
  conf_mat(truth = employees_30_or_more, estimate = .prediction) %>% 
  summary()

# Check test performance
iq_model %>% 
  augment(test) %>% 
  select(employees_30_or_more, .prediction) %>% 
  collect() %>% 
  mutate_all(as.factor) %>% 
  conf_mat(truth = employees_30_or_more, estimate = .prediction) %>% 
  summary()

# Tidying coeficients
coefficients = tidy(iq_model) 
write_excel_csv(coefficients, 'data/raw/coefficients.csv')

# We are done with spark
spark_disconnect(sc)


