setwd("~/Desktop/ITESM/MIT REAP/")
library(tidyverse)                                                       
if(require(sparklyr) == FALSE){                                                
  install.packages('sparklyr')                                                 
  library(sparklyr)                                                            
}else{                                                                          
  library(sparklyr)                                                            
}
sc <- spark_connect(master="local", version = '3.2.2')

y = read_csv("~/Downloads/denue_09_csv/conjunto_de_datos/denue_inegi_09_.csv", 
             locale = locale(encoding = "ISO-8859-1")) %>% 
  mutate(raz_social = str_remove_all(raz_social, '[\\.,]+'),
         razon = str_squish(str_extract(raz_social, 'S DE CV| S A P I DE CV| SA$| S[AC] DE CV$| SC?$| SC? [DEN]{2} [RLCS]{2}$| SAPI$| SAPI DE CV$| SC? DE RL DE CV| AC| SPR DE RL|SOFOM| S DE PR DE RL|SA DE RL| AR$|SA DE CV SFC|INC SA DE CV SOFOM ENR|INC|SA DE RL|SAPI DE CV SOFOM ENR| SAB DE CV SOFOM ENR|Y ASOCIADOS|SA DE RL DE CV|SCL|SRL DE CV|SAS DE CV|SC DE R|SRL DE CV|A C|SAS DE CV|S A  DE  C V| SA DE CV SOFOM ENR|SAB DE CV|S DE RL MI|SA DE CV SFP|SA DE CV SOFOM ENR|IAP|API DE CV|SRL DE CV|SA DE CV SOFI|SA DE CV|SAS DE CV')) ) %>% 
  transmute(
    employees_25_or_more = if_else(per_ocu %in% c("0 a 5 personas","6 a 10 personas","11 a 30 personas"), 0, 1),
    length_name = nchar(nom_estab),
    codigo_act = f(codigo_act),
    tipo_vial = factor(tipo_vial, exclude = NULL),
    tipo_v_e_1 = factor(tipo_vial, exclude = NULL),
    tipo_v_e_2 = factor(tipo_v_e_2, exclude = NULL),
    tipo_v_e_3 = factor(tipo_v_e_3, exclude = NULL),
    edificio = factor(!is.na(edificio)),
    tipo_asent = factor(tipo_asent, exclude = NULL),
    municipio = factor(municipio),
    telefono = factor(!is.na(telefono)),
    correoelec = factor(!is.na(correoelec)),
    pagina_web = factor(!is.na(www)),
    tipoUniEco = factor(tipoUniEco),
    year_alta = year(ym(fecha_alta)),
    month_alta = month(ym(fecha_alta)),
    month_open = as.numeric(ym('2022-11') - ym(fecha_alta))/30,
    razon = factor(razon, exclude = NULL),
    latitud, longitud 
  )



denue = spark_read_csv(sc, 'denue_test.csv')

denue_ml = denue %>% 
 select(
    latitud, longitud
    ) 

kmedias = ml_kmeans(
  denue, 
  features = c('latitud','longitud'),
  k =  10
  ) 



denue_k = ml_predict(kmedias,denue) %>% 
  select(-c(latitud, longitud, features))%>% 
    mutate(prediction = as.character(prediction)) %>% 
    ft_string_indexer(input_col = 'tipo_vial', output_col = 'tipo_vial_index') %>%
    ft_string_indexer(input_col = 'tipo_v_e_1', output_col = 'tipo_v_e_1_index') %>%
    ft_string_indexer(input_col = 'tipo_v_e_2', output_col = 'tipo_v_e_2_index') %>%
    ft_string_indexer(input_col = 'tipo_v_e_3', output_col = 'tipo_v_e_3_index') %>%
    ft_string_indexer(input_col = 'tipo_asent', output_col = 'tipo_asent_index') %>% 
    ft_string_indexer(input_col = 'municipio', output_col = 'municipio_index') %>% 
    ft_string_indexer(input_col = 'tipoUniEco', output_col = 'tipoUniEco_index') %>% 
    ft_string_indexer(input_col = 'prediction', output_col = 'prediction_index') %>% 
    ft_string_indexer(input_col = 'codigo_act', output_col = 'codigo_act_index') %>% 
    ft_string_indexer(input_col = 'razon', output_col = 'razon_index') %>% 
    ft_one_hot_encoder(
      input_cols = c('tipo_vial_index','tipo_v_e_1_index','tipo_v_e_2_index','tipo_v_e_3_index',
                     'tipo_asent_index','municipio_index','tipoUniEco_index','prediction_index',
                     'razon_index','codigo_act_index'),
      output_cols = c('tipo_vial_onehot','tipo_v_e_1_onehot','tipo_v_e_2_onehot','tipo_v_e_3_onehot',
                      'tipo_asent_onehot','municipio_onehot','tipoUniEco_onehot','prediction_onehot',
                      'razon_onehot','codigo_act_onehot')
    ) %>% 
    ft_vector_assembler(
      input_cols =  c('tipo_vial_onehot','tipo_v_e_1_onehot','tipo_v_e_2_onehot','tipo_v_e_3_onehot',
                      'tipo_asent_onehot','municipio_onehot','tipoUniEco_onehot','prediction_onehot',
                      'razon_onehot','codigo_act_onehot'), 
      output_col = 'one_hot') %>% 
    ft_vector_assembler(
      input_cols = c('length_name','edificio','telefono','correoelec','year_alta','month_alta',
                     'month_open'), 
      output_col = 'numericas') %>%
    ft_standard_scaler(input_col = 'numericas', output_col = 'scaled') %>% 
    select(employees_25_or_more, scaled, one_hot)
  
  
datos_split = denue_k %>% 
    sdf_random_split(training = 0.8, test = 0.2, seed = 123)
  
training = datos_split %>% 
    pluck('training') 
  
test = datos_split %>% 
    pluck('test') 
  
pipeline <- ml_pipeline(sc) %>%
    ft_r_formula(employees_25_or_more ~ scaled + one_hot) %>%
    ml_logistic_regression()
  
grid <-  list(
  logistic_regression = list(
      elastic_net_param = seq(0, 1, length = 100), 
      reg_param = seq(0, 1, length = 100)    
    )
  )
  
cv <- ml_cross_validator(
    sc, 
    estimator = pipeline, 
    estimator_param_maps = grid,
    evaluator = ml_multiclass_classification_evaluator(sc, metric_name = "f1"),
    num_folds = 10,
    parallelism = 6
  )
  
  
cv_model <- ml_fit(cv, training) 
  
metricas = ml_validation_metrics(cv_model) %>% 
    arrange(f1)





















glimpse(y)

y %>% 
  mutate(codigo_act = str_extract(codigo_act,'\\d{2}')) %>% 
  group_by(municipio,codigo_act, razon, per_ocu) %>% 
  count() %>% 
  group_by(municipio,codigo_act, razon) %>% 
  mutate(percentage = n/sum(n)) %>% View








    