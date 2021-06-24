library(tidyverse)
library(riem)
library(httr)
library(sf)
library(gganimate)
library(gifski)
library(png)
library(dplyr)
library(patchwork)
library(ggthemes)
library(distill)

h_raw_url <- "https://api.covidactnow.org/v2/states.timeseries.csv?apiKey=a13da1a6818345c697cc89ea4554529a"
h_raw_data <- as_tibble(read_csv(h_raw_url))

h_anim_plot <- h_raw_data %>% 
  mutate(day_vaccine = actuals.vaccinesAdministered - lag(actuals.vaccinesAdministered, default = first(actuals.vaccinesAdministered))) %>%
  select(date, state, actuals.newCases, actuals.vaccinesAdministered, day_vaccine) %>% 
  filter(actuals.newCases != 0) %>% 
  filter(day_vaccine < 400000) %>% 
  filter(day_vaccine > 0) %>% 
  drop_na() %>% 
  mutate(year = str_sub(date, 1, 4))

anim_plot <- h_anim_plot %>%
  ggplot(aes(day_vaccine, actuals.newCases, fill = state, color = state))+
  geom_point()+
  transition_time(date)+
  scale_x_continuous(labels = function(day_vaccine) format(day_vaccine, scientific = FALSE))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs( title = "The relationship between new Covid cases and vaccination rates in the \n US, per State",
        x = "Daily vaccination count",
        y = "Daily new case count",
        caption = "Covidactnow")+
  labs(subtitle = "Date: {frame_time}")

write_rds(anim_plot, "anim_plot.rds")

#add average line for all states combined??? 