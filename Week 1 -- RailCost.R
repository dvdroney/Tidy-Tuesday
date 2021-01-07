#This is my first attempt at a Tidy Tuesday.

##Libraries ----
library(tidyverse)
library(ggplot2)
##Loading Data----
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
#Some data is missing quite a few variables, removing those
transit_cost_c <- transit_cost %>% filter(!is.na(city))

##How many projects are started each year? ----
transit_cost_c %>% count(start_year)
#I'd prefer to bin this so it is easier to interpret
transit_cost_c %>%
  mutate(start_year = as.numeric(start_year)) %>% 
  filter(!is.na(start_year)) %>% 
  mutate(start_year_bin = cut(start_year, breaks = c(0, 1990, 2000, 2005, 2010, 2015, 2020, 2025))) %>% 
  ggplot(aes(x = start_year_bin)) +
  geom_bar()

##How has the cost changed by start year?----
transit_cost_c %>% 
  group_by(start_year_bin) %>%
  summarize(average_proj_cost = mean(cost_km_millions, na.rm = TRUE)) %>% 
  ggplot(aes(x = start_year_bin, y = average_proj_cost))+
  geom_col()


##Do projects that take more time cost more? Assuming yes----
transit_cost_c %>%
  mutate(end_year = as.numeric(end_year)) %>% 
  filter(!is.na(end_year)) %>% 
  mutate(duration = end_year-start_year) %>% 
  mutate(duration_bins = cut(duration, breaks = c(0,2,4,6,8,10,15,25))) %>% 
  group_by(duration_bins) %>%
  summarize(average_proj_cost = mean(cost_km_millions, na.rm = TRUE)) %>% 
  ggplot(aes(x = duration_bins, y = average_proj_cost))+
  geom_col()


##Does a project get cheaper by the KM if it is a longer line? That is, is there 'bulk' discount?-------
transit_cost_c %>%
  #filter(length < 100, cost_km_millions < 1000) %>% 
  ggplot(aes(x = length, y = cost_km_millions))+
  geom_point()

##Which ones cost so much?
transit_cost_c %>% filter(cost_km_millions >1000) %>% View()
#these all have high percent of tunnels so maybe that could be a good predictor of cost
transit_cost_c %>% 
  mutate(tunnel_per_real = tunnel/length) %>% 
  mutate(tunnel_per_bin = cut(tunnel_per_real, breaks = c(-Inf, .2, .4, .6, .8, Inf))) %>% 
  filter(!is.na(tunnel_per_bin)) %>% 
  group_by(tunnel_per_bin) %>%
  summarize(average_proj_cost = mean(cost_km_millions, na.rm = TRUE)) %>% 
  ggplot(aes(x = tunnel_per_bin, y = average_proj_cost))+
  geom_col()


#Does a project get cheaper if we account for percentage of tunnel?
transit_cost_c %>% 
  #filter(length < 100, cost_km_millions < 1000) %>% 
  mutate(tunnel_per_real = tunnel/length) %>% 
  mutate(tunnel_per_bin = cut(tunnel_per_real, breaks = c(-Inf, .2, .4, .6, .8, Inf))) %>% 
  filter(!is.na(tunnel_per_bin)) %>% 
  #filter(tunnel_per_real <.80) %>% 
  ggplot(aes(x = length, y = cost_km_millions, color = tunnel_per_bin))+
  geom_point()



