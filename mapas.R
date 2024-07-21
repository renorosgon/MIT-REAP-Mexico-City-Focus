setwd("~/Desktop/ITESM/MIT REAP/")
# Libraries ---------------------------------------------------------------
# install.packages(c('terra','leafem','sf'))
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load sf                                                       
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
}
# Install - load ggsn                                                       
if(require(ggsn) == FALSE){                                                
  install.packages('ggsn')                                                 
  library(ggsn)                                                            
}else{                                                                          
  library(ggsn)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}


# Loading a shape file
imuc = read_sf('~/Desktop/ITESM/Cursos/TC2001B/data/imuc/colonias_imc2020.shp') 

data = read_csv('data/results.csv', locale = locale(encoding = "latin1")) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
  st_transform(crs = st_crs(imuc))
poligonos = read_sf('data/poligonos/00mun.shp')
coefs = read_csv('data/final.csv')
poligonos = read_sf('data/poligonos/00mun.shp') %>% 
  filter(CVE_ENT %in% c('08','09','11','14','19','22'))


state = st_join(
  imuc,
  data
  ) %>% 
  st_drop_geometry() %>% 
  with_groups(
    .groups = c(CVE_ENT,CVE_MUN,CVE_COL,NOM_ENT, NOM_MUN, COLONIA, CP),
    summarise,
    quality_index = mean(probability_1),
    std_dev = sd(probability_1),
    n = n()
  ) %>% 
  mutate(
    n = if_else(is.na(quality_index),0, n),
    quality_index = coalesce(quality_index, 0),
    cohort_potential = n * quality_index
  )

mun = st_join(
  imuc,
  data
) %>% 
  st_drop_geometry() %>% 
  with_groups(
    .groups = c(CVE_ENT,CVE_MUN,CVE_COL,NOM_ENT, NOM_MUN,),
    summarise,
    quality_index = mean(probability_1),
    std_dev = sd(probability_1),
    n = n()
  ) %>% 
  mutate(
    n = if_else(is.na(quality_index),0, n),
    quality_index = coalesce(quality_index, 0),
    cohort_potential = n * quality_index
  )

imuc = st_simplify(imuc, preserveTopology = TRUE, dTolerance = 1000)
imuc = st_make_valid(imuc)
maps = imuc %>% 
  select(CVE_ENT,CVE_MUN,CVE_COL,NOM_ENT, NOM_MUN, COLONIA, CP) %>% 
  left_join(mun)

library(tmap)
library(tmaptools)



mapa = maps %>% 
  filter(CVE_ENT %in% c('08','09','11','14','19','22')) %>% 
  tm_shape() +
  tm_fill(
    col = "quality_index",
    id = "COLONIA", 
    title = "Bussines Quality Index",
    popup.vars = c(
      "Municipio" = "NOM_MUN",
      "Entidad"="NOM_ENT",
      "Bussiness Quality Index" = "quality_index",
      "Number of bussiness" = "n",
      "Cohort Potencial" = "cohort_potential"
      ),
    popup.format=list(quality_index=list(digits=2),
                      cohort_potential = list(digits = 0)),
    palette = c("gray80", "#D11E7C"),
    lwd = 0.05,
    ) +
  tm_shape(poligonos) +
  tm_borders(lwd = 0.5)


mapa = tmap_leaflet(mapa)

tmap_mode("view")

mapa 

top = filter(maps, CVE_ENT == '19', NOM_MUN == 'Monterrey') %>% 
  mutate(quality_index = ifelse(quality_index > 0.5, 0.5, quality_index)) %>% 
  ggplot() +
  geom_sf(data = filter(poligonos, CVE_ENT == '19', CVE_MUN == '039'), fill = 'gray90', col = 'black') +
  geom_sf(aes(fill = 100 * quality_index), col = NA) +
  geom_sf(data = filter(poligonos, CVE_ENT == '19', CVE_MUN == '039'), fill = NA, col = 'white') +
  scale_fill_gradient(low = "gray80", high = '#D11E7C', limits = c(0,50)) +
  labs(fill = 'Entrepreneurial Quality Index') +
  # Modify scale color bar
  guides(
    fill = guide_colourbar(
      title.position="top", 
      title.hjust = 0.5, 
      title.vjust = -8, 
      barwidth = 15
      ) 
  ) +
  # Modify theme
  theme_minimal()  +
  theme(
    # Modify legend
    legend.position = 'top',
    legend.title = element_text(color = 'white'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.background = element_rect(fill='transparent', color= NA),
    legend.background = element_rect(fill='transparent',color= NA), 
    legend.box.background = element_rect(fill='transparent',color= NA)
  ) 

png(filename = 'eqi.png', width = 1500, height = 1500, units = 'px',
    res = 300,
    bg = 'transparent')
top
dev.off()

my_breaks = c(1, 10, 100, 1000)

bottom = filter(maps, CVE_ENT == '19', NOM_MUN == 'Monterrey') %>% 
  ggplot() +
  geom_sf(data = filter(poligonos, CVE_ENT == '19', CVE_MUN == '039'), fill = 'gray90', col = 'black') +
  geom_sf(aes(fill = cohort_potential), col = NA) +
  geom_sf(data = filter(poligonos, CVE_ENT == '19', CVE_MUN == '039'), fill = NA, col = 'white') +
  scale_fill_gradient(low = "gray80", high = '#D11E7C',
                      trans = "log",
                      breaks = my_breaks, labels = my_breaks,
                      limits = c(1,1000)
                      ) +
  labs(fill = 'Cohort Potential') +
  # Modify scale color bar
  guides(
    fill = guide_colourbar(
      title.position="top", 
      title.hjust = 0.5, 
      title.vjust = -8, 
      barwidth = 15
    ) 
  ) +
  # Modify theme
  theme_minimal() +
  theme(
    # Modify legend
    legend.position = 'top',
    legend.title = element_text(color = 'white'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.background = element_rect(fill='transparent', color= NA),
    legend.background = element_rect(fill='transparent',color= NA), 
    legend.box.background = element_rect(fill='transparent',color= NA)
  ) 



png(filename = 'cop.png', width = 1500, height = 1500, units = 'px',
    res = 300,
    bg = 'transparent')
bottom
dev.off()

png(filename = 'bar.png', width = 3000, height = 1500, units = 'px',
    res = 300,
    bg = 'transparent')
filter(state, CVE_ENT == '09') %>% 
  with_groups(
    .groups = NOM_MUN,
    reframe,
    quality_index = sum(quality_index * n) /sum(n),
    quality_index = coalesce(quality_index, 0) ,
    total = sum(n),
    cohort_potential = quality_index * total
    ) %>% 
  top_n(n = 30, wt = cohort_potential) %>%
  ggplot(
    aes(x =  cohort_potential,
        y = reorder(NOM_MUN, cohort_potential),
        fill = 100 * quality_index
        )) +
  geom_col()+
  scale_fill_gradient(low = "gray80", high = '#D11E7C', limits = c(0,50)) +
  labs(fill = 'Business Quality Index') +
  # Modify scale color bar
  guides(
    fill = guide_colourbar(
      title.position="top", 
      title.hjust = 0.5, 
      title.vjust = -8, 
      barwidth = 38
    ) 
  ) +
  labs(x = 'Business Cohort Potential') +
  # Modify theme
  theme_minimal() +
  theme(
    # Modify legend
    legend.position = 'top',
    legend.title = element_text(color = 'white'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.background = element_rect(fill='transparent', color= NA),
    legend.background = element_rect(fill='transparent',color= NA), 
    legend.box.background = element_rect(fill='transparent',color= NA),
    axis.title.y = element_blank()
  ) 
dev.off()







19.4881529,-99.1847789
# Create a spatial point
tec_ccm = tibble(
  name = 'Tec de Monterrey',
  latitud = 19.2840616,
  longitud = -99.1409753
)%>% 
  st_as_sf(coords = c("longitud","latitud"), crs = 4326) %>% 
  # Transform to crs
  st_transform(crs = st_crs(imuc))

# Create a buffer
buffer = st_buffer(tec_ccm, 10000)

# Get the intersection
distrito_tlalpan = st_intersection(imuc, buffer) %>% 
  # Left join with eqi data
  left_join(state)

png(filename = 'dt.png', width = 1500, height = 1500, units = 'px',
    res = 300,
    bg = 'transparent')

distrito_tlalpan  %>% 
  ggplot() +
  geom_sf(aes(fill = 100 * quality_index), show.legend = F) +
  geom_sf(data = tec_ccm, fill = '#D11E7C', col = 'white', size = 4, shape = 21) +
  scale_fill_gradient(low = "gray80", high = '#D11E7C', limits = c(0,100)) +
  labs(fill = 'Business Quality Index') +
  north(distrito_tlalpan, location = 'topleft') +
  # Modify scale color bar
  guides(
    fill = guide_colourbar(
      title.position="top", 
      title.hjust = 0.5, 
      title.vjust = -8, 
      barwidth = 20
    ) 
  ) +
  # Modify theme
  theme_minimal() +
  theme(
    # Modify legend
    legend.position = 'none',
    legend.title = element_text(color = 'white'),
    text = element_text('Bebas Neue'),
    plot.background = element_rect(fill='transparent', color='transparent'),
  ) 

dev.off()



png(filename = 'cop.png', width = 1500, height = 1500, units = 'px',
    res = 300,
    bg = 'transparent')

coefs %>% 
  filter(str_detect(features, 'numericas'),
         !str_detect(features, '_cv')) %>% 
  mutate(
    features = str_remove_all(features, 'numericas_'),
    features = str_replace_all(features, '_',' '),
    odds = exp(coefficients)     
         ) %>% 
  ggplot(aes(x = odds, y = reorder(features, odds), fill = odds))  +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(odds,2)), col = 'white', hjust = 1.25) +
  scale_fill_gradient(low = "gray80", high = '#D11E7C') +
  labs(x = 'Odds') +
  # Modify theme
  theme_minimal() +
  theme(
    # Modify legend
    legend.position = 'top',
    legend.title = element_text(color = 'white'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.background = element_rect(fill='transparent', color= NA),
    legend.background = element_rect(fill='transparent',color= NA), 
    legend.box.background = element_rect(fill='transparent',color= NA),
    axis.title.y = element_blank()
  ) 
dev.off()
