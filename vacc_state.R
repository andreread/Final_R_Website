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
h_raw_data <- read_csv(file = h_raw_url)

vacc_state <- h_raw_data %>%
  mutate(day_vaccine = actuals.vaccinesAdministered - lag(actuals.vaccinesAdministered, default = first(actuals.vaccinesAdministered))) %>%
  select(date, state, actuals.newCases, actuals.vaccinesAdministered, day_vaccine) %>%
  filter(actuals.newCases != 0) %>%
  drop_na() %>%
  ggplot(aes(date, day_vaccine, fill = state, color = state))+
  geom_smooth(formula = y ~ x, method = loess, se = FALSE)+
  scale_y_continuous(labels = function(day_vaccine) format(day_vaccine, scientific = FALSE))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs( title = "Daily Vaccination Counts per State Over Time in the US",
        subtitle = "New Vaccinations have been steadily declining",
        x = "Date",
        y = "Daily Vaccination counts",
        caption = "Covidactnow")

write_rds(vacc_state, "vacc_state.rds")
