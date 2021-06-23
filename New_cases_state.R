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
library(plotly)

h_raw_url <- "https://api.covidactnow.org/v2/states.timeseries.csv?apiKey=a13da1a6818345c697cc89ea4554529a"
h_raw_data <- read_csv(file = h_raw_url)

new_cases_state_plot <- h_raw_data %>%
  mutate(day_vaccine = actuals.vaccinesAdministered - lag(actuals.vaccinesAdministered, default = first(actuals.vaccinesAdministered))) %>%
  select(date, state, actuals.newCases, actuals.vaccinesAdministered, day_vaccine) %>%
  drop_na() %>%
  filter(actuals.newCases >= 0) %>%
  ggplot(aes(date, actuals.newCases, fill = state, color = state, text = paste("State:", state)))+
  geom_smooth(formula = y ~ x, method = loess, se = FALSE)+
  theme_minimal()+
  theme(legend.position = "none")+
  labs( title = "New Covid Cases per State Over Time in the US",
        subtitle = "New Covid cases have been steadily declining",
        x = "Date",
        y = "Daily new case count",
        caption = "Covidactnow")

new_cases_state_plot_interactive <- ggplotly(new_cases_state_plot, tooltip = "text")

write_rds(new_cases_state_plot_interactive, "new_cases_state_plot.rds")