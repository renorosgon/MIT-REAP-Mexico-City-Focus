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
                                                     
# Load and install lubridate
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
  library(lubridate)                                                            
}else{                                                                          
  library(lubridate)                                                            
}

# Data --------------------------------------------------------------------
# Read DENUE from Mexico City
read_csv("data/raw/denue_inegi_09_.csv", locale = locale(encoding = "ISO-8859-1")) %>% 
  # Extract some patterns from text
  mutate(
    # Remove points and commas
    raz_social = str_remove_all(raz_social, '[\\.,]+'),
    # Extract fiscal registration
    razon = str_squish(str_extract(raz_social, 'S DE CV| S A P I DE CV| SA$| S[AC] DE CV$| SC?$| SC? [DEN]{2} [RLCS]{2}$| SAPI$| SAPI DE CV$| SC? DE RL DE CV| AC| SPR DE RL|SOFOM| S DE PR DE RL|SA DE RL| AR$|SA DE CV SFC|INC SA DE CV SOFOM ENR|INC|SA DE RL|SAPI DE CV SOFOM ENR| SAB DE CV SOFOM ENR|Y ASOCIADOS|SA DE RL DE CV|SCL|SRL DE CV|SAS DE CV|SC DE R|SRL DE CV|A C|SAS DE CV|S A  DE  C V| SA DE CV SOFOM ENR|SAB DE CV|S DE RL MI|SA DE CV SFP|SA DE CV SOFOM ENR|IAP|API DE CV|SRL DE CV|SA DE CV SOFI|SA DE CV|SAS DE CV')) 
    ) %>% 
  # Transmute to dataset
  transmute(
    # Business with 30 employes or more(Create dependent variable)
    employees_30_or_more = if_else(per_ocu %in% c("0 a 5 personas","6 a 10 personas","11 a 30 personas"), 0, 1),
    # Business length name
    length_name = nchar(nom_estab),
    # SCIAN Code
    codigo_act = str_extract(codigo_act, '^\\d{6}'),
    # Road types
    tipo_vial = factor(tipo_vial, exclude = NULL),
    tipo_v_e_1 = factor(tipo_vial, exclude = NULL),
    tipo_v_e_2 = factor(tipo_v_e_2, exclude = NULL),
    tipo_v_e_3 = factor(tipo_v_e_3, exclude = NULL),
    # Has building
    edificio = factor(!is.na(edificio)),
    # Where it lies
    tipo_asent = factor(tipo_asent, exclude = NULL),
    # Municipality
    municipio = factor(municipio),
    # Has phone
    telefono = factor(!is.na(telefono)),
    # Has email
    correoelec = factor(!is.na(correoelec)),
    # Has webpage
    pagina_web = factor(!is.na(www)),
    # Type of economic unit
    tipoUniEco = factor(tipoUniEco),
    # Year of kickstart
    year_alta = year(ym(fecha_alta)),
    # Month of kickstart
    month_alta = month(ym(fecha_alta)),
    # Months open
    month_open = as.numeric(ym('2022-11') - ym(fecha_alta))/30,
    # Fiscal reason
    razon = factor(razon, exclude = NULL)
  ) %>% 
  write_excel_csv('data/raw/model_input.csv')
