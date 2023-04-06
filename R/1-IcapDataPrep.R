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
                                             
# Data --------------------------------------------------------------------
# Read DENUE from Mexico City
read_csv("data/raw/denue_inegi_09_.csv", locale = locale(encoding = "ISO-8859-1")) %>% 
  # Extract some patterns from text
  mutate(
    # Remove points and commas
    raz_social = str_remove_all(raz_social, '[\\.,]+'),
    # Extract fiscal registration
    razon = str_squish(str_extract(raz_social, 'S DE CV| S A P I DE CV| SA$| S[AC] DE CV$| SC?$| SC? [DEN]{2} [RLCS]{2}$| SAPI$| SAPI DE CV$| SC? DE RL DE CV| AC| SPR DE RL|SOFOM| S DE PR DE RL|SA DE RL| AR$|SA DE CV SFC|INC SA DE CV SOFOM ENR|INC|SA DE RL|SAPI DE CV SOFOM ENR| SAB DE CV SOFOM ENR|Y ASOCIADOS|SA DE RL DE CV|SCL|SRL DE CV|SAS DE CV|SC DE R|SRL DE CV|A C|SAS DE CV|S A  DE  C V| SA DE CV SOFOM ENR|SAB DE CV|S DE RL MI|SA DE CV SFP|SA DE CV SOFOM ENR|IAP|API DE CV|SRL DE CV|SA DE CV SOFI|SA DE CV|SAS DE CV')),
    razon = str_replace(razon, 'A C$', 'AC'),    
    razon = str_replace(razon, 'S A P I', 'SAPI'),
    razon = ifelse(is.na(razon),'Sin Razón', razon)
    ) %>% 
  # Transmute to dataset
  transmute(
    # Business with 30 employes or more(Create dependent variable)
    employees_30_or_more = factor(
      if_else(per_ocu %in% c("0 a 5 personas","6 a 10 personas","11 a 30 personas"), 0, 1),
      levels = 0:1,
      labels = c('<30 employees', '30 or more employees')
      ),
    # Business length name
    length_name = nchar(nom_estab),
    # SCIAN Code
    codigo_act = factor(str_extract(codigo_act, '^\\d{6}')),
    # Road types
    tipo_vial = factor(coalesce(tipo_vial,'Sin vialidad')),
    # Has building
    edificio = !is.na(edificio),
    # Where it lies
    tipo_asent = factor(coalesce(tipo_asent, 'Sin asentamiento')),
    # Municipality
    municipio = factor(municipio),
    # Has phone
    telefono = !is.na(telefono),
    # Has email
    correo_electronico = !is.na(correoelec),
    # Has webpage
    pagina_web = !is.na(www),
    # Type of economic unit
    tipoUniEco = factor(tipoUniEco),
    # Year of kickstart
    year_alta = year(ym(fecha_alta)),
    # Month of kickstart
    month_alta = month(ym(fecha_alta)),
    # Months open
    month_open = as.numeric(ym('2022-11') - ym(fecha_alta))/30,
    # Fiscal type dummies
    razon_sa = str_detect(razon, 'SA'),
    razon_cv = str_detect(razon, 'CV'),
    razon_rl = str_detect(razon, 'RL'),
    razon_ac = str_detect(razon, 'AC'),
    razon_sapi = str_detect(razon, 'API'),
    razon_sab = str_detect(razon, 'SAB'),
    razon_pr = str_detect(razon, 'PR'),
    razon_sofom = str_detect(razon, 'SOFOM'),
    razon_sfp = str_detect(razon, 'SFP'),
    razon_scl = str_detect(razon, 'SCL'),
    razon_spr = str_detect(razon, 'PR'),
    razon_sc = str_detect(razon, 'SC'),
    razon_inc = str_detect(razon, 'INC'),
    razon_sas = str_detect(razon, 'SAS'),
    razon_sc = str_detect(razon, 'SC'),
    razon_sofi = str_detect(razon, 'SOFI'),
  ) %>% 
  mutate_if(is.logical, as.numeric) %>% 
  write_excel_csv('data/raw/model_input.csv')
