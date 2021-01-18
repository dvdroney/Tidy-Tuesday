#Tidy Tuesday Week 2

##Libraries ----
library(tidyverse)
library(ggplot2)
library(ggtext)
library(patchwork)

##Load Data ----
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

## Cleaning and Graphing ----

#Making sure it is all in one unit
artwork %>% count(units)

#Finding the largest surface area
largest_piece <- artwork %>% 
  mutate(area= height*width) %>%
  filter(area == max(area, na.rm = TRUE)) 

##First Graph -----  
artwork %>% 
  mutate(wider = if_else(width == height, 'Square', 
                         if_else(width > height, 'Wide Rectangle', 'Tall Rectangle'))) %>% 
  filter(!is.na(wider)) %>% 
  #filter(height < 10000, width < 5000) %>% 
  ggplot(aes(x = width, y = height)) + 
  geom_point(alpha = 0.4, size = rel(2), aes(color = wider))+
  annotate(
    geom = 'curve', x = (largest_piece$width*.8), y = (largest_piece$height*1.2), xend = largest_piece$width, yend = largest_piece$height,
    curvature = .3, arrow = arrow())+
  annotate(geom = 'text', x = (largest_piece$width*.8), y = (largest_piece$height*1.35), 
           label = "paste('The largest piece in the Tate Collection is 132,462m'^ 2)", parse = TRUE)+
  labs(
  title = "What are the general sizes of the paintings in the Tate Collection?",
  subtitle = "Each dot represents a painting in the Tate Collection",
  x = "Width (mm)",
  y = "Height (mm)")+
  theme_minimal(base_family = "Helvetica", base_size = 20)+
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )

##Second Graph - Hit timeline - DNF ----
artwork %>% 
  mutate(wider = if_else(width == height, 'Square', 
                         if_else(width > height, 'Wide Rectangle', 'Tall Rectangle'))) %>% 
  filter(!is.na(wider)) %>% 
  count(wider) %>% 
  mutate(freq = n/sum(n))

artwork %>% 
  mutate(wider = if_else(width == height, 'Square', 
                         if_else(width > height, 'Wide Rectangle', 'Tall Rectangle'))) %>% 
  filter(!is.na(wider)) %>% 
  filter(height < 10000, width < 5000) %>% 
  ggplot(aes(x = width, y = height)) + 
  geom_point(alpha = 0.4, size = rel(2), aes(color = wider))+
  annotate(geom = 'text', x = 1000, y = 7500, 
           label = '54.6% of the collection are tall rectangles')+
  labs(
    x = "Width (mm)",
    y = "Height (mm)")+
  theme_minimal(base_family = "Helvetica", base_size = 20)+
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )



