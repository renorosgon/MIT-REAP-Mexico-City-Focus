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
# Load and install patchwork
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Data --------------------------------------------------------------------
# Enterperenurial Quality (by borough)
eqi_df = map_df(
  .x = list.files(path = 'data/outputs/entrepreneurial_quality/counties/', full.names = TRUE),
  .f = read_csv
)

# External events
dates = tibble(
  date = c(ymd("2017-09-17"), ymd("2020-03-23")),
  title = c('Earthquake', 'COVID-19')
)


# Time series plots -------------------------------------------------------
# Mexico City's overall
eqi_df %>% 
  # Remove NA
  filter(!is.na(NOMDT)) %>% 
  # Summarise by period
  with_groups(
    .groups = c(period),
    summarise,
    EQI = mean(eqi),
    std = sd(eqi),
    n = sum(n)
  ) %>% 
  # Create plot
  ggplot(aes(x=period, y = 100 * EQI))+ 
  # Add points
  geom_point(col = '#D11E7C')+
  # Add line
  geom_line(size=1.2, col = '#D11E7C') +
  # Add vertical lines
  geom_vline(
    xintercept = c(ymd("2017-09-17"), ymd("2020-03-23")),
    linetype = 'dashed', col = 'gray40'
  ) +
  # Add text for vertical lines
  geom_text(
    data = dates, aes(x = date, label = title), 
    y = -Inf, angle = 90, hjust = 'left', vjust = 1.4, size = 3,
    family = 'Roboto', col = 'gray40'
  ) +
  # Add labels
  labs(
    title = 'México City - Entrepreneurial Quality Index',
    subtitle = 'Average probability of success',
    caption = 'Own elaboration with data from INEGI\nAuthor: René Rosado'
  ) +
  # Modify theme
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    text = element_text(family = 'Roboto')
  )


# Plot by borough
alcaldias = eqi_df %>% 
  # Filter NAs
  filter(!is.na(NOMDT)) %>% 
  # Summarise by borough
  with_groups(
    .groups = c(period, NOMDT),
    summarise,
    EQI = mean(eqi),
    std = sd(eqi),
    n = sum(n)
  ) %>% 
  # Add index
  with_groups(
    .groups = NOMDT,
    mutate,
    index = 100 * EQI / first(EQI),
    highlight = NOMDT
  )

# Create a plot
ggplot(alcaldias, aes(x=period, y = 100 * EQI))+
  # Add all lines
  geom_line(
    data = select(alcaldias, -NOMDT), 
    aes(group = highlight), 
    col ='gray', linewidth = 0.25
  ) +
  # Highlight a specific borough
  geom_line(aes(color= NOMDT), size=1.2, col = '#D11E7C') +
  # Add vertical lines
  geom_vline(
    xintercept = c(ymd("2017-09-17"), ymd("2020-03-23")),
    linetype = 'dashed', col = 'gray40'
  ) +
  # Add text to vertical lines
  geom_text(
    data = dates, aes(x = date, label = title), 
    y = -Inf, angle = 90, hjust = 'left', vjust = 1.4, size = 3,
    family = 'Roboto', col = 'gray40'
  ) +
  # Add labels
  labs(
    title = 'México City - Entrepreneurial Quality Index',
    subtitle = 'Average probability of success',
    caption = 'Own elaboration with data from INEGI\nAuthor: René Rosado'
  ) +
  # Facet by borough
  facet_wrap(~NOMDT, scales = 'free_y') +
  # Modify theme
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    text = element_text(family = 'Roboto')
  )




# Maps and compound plots -------------------------------------------------
# Boroughs GIS
colonias = read_sf('data/colonias_iecm')

# Create a spatial point
tec_ccm = tibble(
  name = 'Tec de Monterrey',
  latitud = 19.2840616,
  longitud = -99.1409753
)%>% 
  st_as_sf(coords = c("longitud","latitud"), crs = 4326) %>% 
  # Transform to crs
  st_transform(crs = st_crs(colonias))

# Create a buffer
buffer = st_buffer(tec_ccm, 5000)

# Get the intersection
distrito_tlalpan = st_intersection(colonias, buffer) %>% 
  # Mutate to numeric
  mutate_at(.vars = vars(ENT, CVEDT), as.numeric) %>% 
  # Left join with eqi data
  left_join(eqi_df, by = join_by(ENT, CVEDT, NOMDT, CVEUT, NOMUT)) %>% 
  # Filtar NAs
  filter(!is.na(period)) %>% 
  # Summarise by groups
  with_groups(
    .groups = c(period, CVEUT, NOMUT),
    summarise,
    eqi = mean(eqi),
    n = n()
  )%>% 
  # Create index
  with_groups(
    .groups = c(CVEUT, NOMUT),
    mutate,
    index = 100 * eqi / first(eqi)
  )


# Aggregate by period
agg = distrito_tlalpan %>% 
  st_drop_geometry() %>% 
  mutate(x = n * eqi) %>% 
  group_by(period) %>%
  summarise(x = 100 * sum(x)/sum(n)) 

# Time series plot
timeseries = agg  %>% 
  ggplot(aes(x = period, y = x)) +
  # Add point
  geom_point(aes(col = x)) +
  # Set scale color bar
  scale_color_gradient(low = "gray80", high = '#D11E7C', limits = c(0,100)) +
  # Modify scale color bar
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5, 
                                  title.vjust = -8, barwidth = 20)
  ) +
  # Add trend line
  stat_smooth(
    method = 'lm', formula = y ~ poly(x,2), 
    se = FALSE, col = '#D11E7C', size = 1.25
    ) + 
  # Modify x acis
  scale_x_date(limits = c(ymd("2016-01-01"), ymd("2022-12-01")),
               date_breaks = '1 year', date_labels = '%Y', 
               expand = c(0.095,0,-.04,0)
               ) +
  # Add labels
  labs(
    title = 'Tlalpan Innovation District - Entrepreneurial Quality Index',
    subtitle = 'Average probability of success',
    col = 'Entrepreneurial Quality Index'
  ) +
  # Modify theme
  theme_minimal() +
  theme(
    # Remove axis text an titles
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    # Modify text
    text = element_text(family = 'Roboto'),
    # Modify legend
    legend.position = 'top',
    legend.title = element_text(color = 'white'),
    # Play with margins
    legend.box.margin=margin(-10,-10,1,-10),
    legend.margin=margin(0,0,0,0),
    plot.margin = margin(-1,-1,-1,-1, "pt"),
    plot.background = element_rect(fill='transparent', color='transparent'),
    panel.grid.minor = element_blank(),
  ) 

# Maps
maps = distrito_tlalpan %>% 
  # Filter specific dates
  filter(period %in% c(ymd("2016-10-01"), ymd("2017-11-01"), ymd("2018-11-01"), 
                       ymd("2019-11-01"), ymd("2020-11-01"), ymd("2021-11-01"),
                       ymd("2022-11-01"))
  ) %>% 
  # Get year
  mutate(year = year(period)) %>% 
  # Start plot
  ggplot() +
  # Add polygons
  geom_sf(aes(fill = 100 * eqi), col = "gray90", show.legend = F) +
  # Add point
  geom_sf(data = tec_ccm, fill = '#D11E7C', col = 'white', size = 4, shape = 21) +
  # Add label
  geom_sf_label(data = tec_ccm, aes(label = name), label.size = 0,
                col = 'gray10', fill = 'white', alpha = 0.5, vjust = -0.5) +
  # Modify fill colors
  scale_fill_gradient(low = "gray80", high = '#D11E7C', limits = c(0,100)) +
  # Add caption
  labs(caption = 'Own elaboration with data from INEGI\nAuthor: René Rosado González | @renorosgon') +
  # Facet by year
  facet_wrap(~year, nrow = 1) +
  # Modify theme
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'Roboto'),
    plot.margin = margin(0,0,0,0, "pt"),
    plot.background = element_rect(fill='transparent', color = 'transparent')
  )

# Final plot
timeseries / maps +
  # Modify plot layout
  plot_layout(design = c(area(t = 0, l = 0, b = 5, r = 10), area(t = 6, l = 0, b = 7, r = 10)) 
  )


