$Cedric

title: "Data Visualization Challenge 4"
author: "Sadi Tomtulu"
date: "2022-05-10"
output:
  html_document:
  toc: true
toc_float: true
collapsed: false
number_sections: false
toc_depth: 4
#code_folding: hide
---
  
  
  ```{css, echo=FALSE}

body {
  color: grey;
  background-color: #344495e;
    font-family: Futura;
}
.list-group-item, .list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
  color: black;
  background-color: #ede0d1 ;
    border-color: #black;
    border-width: 5px;
}

```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE, cache=TRUE)
```

```{r t1p0, fig.height=7, fig.width=10, message=FALSE}

# Libraries

library(ggplot2)
library(ggrepel)
library(xopen)
library(scales)
library(jsonlite)
library(stringi)
library(httr)
library(tidyverse)
library(lubridate)
library(geometries)
library(forcats)
library(wesanderson)
library(maps)

```
Challenge 1
Goal: Map the time course of the cumulative Covid-19 cases! 
  
  ```{r}
# Read COVID Data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
#over time
worldwide_cases <- covid_data_tbl %>%
  select(continent, date, total_cases) %>%
  drop_na(continent) %>%
  group_by(date, continent) %>%
  summarise(Total = sum(total_cases, na.rm = TRUE))

# present case numbers for the labels
data_ends <- worldwide_cases %>% 
  group_by(continent) %>%
  filter(Total == max(Total))



worldwide_cases %>% ggplot(aes(as.Date(date), Total, color = continent)) +
  geom_line(size = 1) +
  theme_linedraw() +
  scale_x_date(date_labels = "%B %Y", 
               date_breaks = "1 month", 
               expand = c(0,NA)) +
  labs(
    title = "Total COVID cases",
    subtitle = "As of 11/05/22"
  ) +
  xlab("Date") + ylab("Cumulative Cases") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust= 1)) +
  scale_color_manual(values = wes_palette("BottleRocket2",7, type = "continuous")) + geom_label_repel(aes(label=Total),
                                                                                                      data=data_ends,
                                                                                                      show.legend = FALSE)

```{r}
g5_countries <- c("France", "Germany", "Spain", "United Kingdom", "United States")

g5_countries_cases <- covid_data_tbl %>% filter(location %in% g5_countries) %>%
  select(location, date, total_cases) %>%
  add_row(worldwide_cases %>% 
            filter(continent=="Europe") %>% 
            rename(location = continent, 
                   total_cases = Total))

# Get data from Europe and the US for labels

data_ends2 <- g5_countries_cases %>% 
  filter(location %in% c("United States", "Europe")) %>%
  group_by(location) %>%
  filter(total_cases == max(total_cases))
# Plotting
g5_countries_cases %>% ggplot(aes(as.Date(date), total_cases, color = location)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "COVID cases in G5 countries",
    subtitle = "as of 11/05/22"
  ) +
  xlab("Date") + ylab("Cumulative Cases") +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_x_date(date_labels = "%B %Y", 
               date_breaks = "1 month", 
               expand = c(0,NA)) +
  scale_color_manual(values = wes_palette("Zissou1", 
                                          7, 
                                          type = "continuous"))
```
Part 2: Map version
Goal: Visualize the distribution of the mortality rate (deaths / population)


world <- map_data("world")

g5_countries["United States"] <- "USA"
g5_countries["United Kingdom"] <- "UK"

worldwide <- world %>%
  filter(region %in% g5_countries)

worldwide %>% ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "white") +
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "Around 6.2 Million confirmed COVID-19 deaths worldwide"
  ) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) 
```
```{r t4p2, fig.height=7, fig.width=10, message=FALSE}

worldwide_mortality <- covid_data_tbl %>%
  select(location, date, total_deaths, population) %>%
  drop_na(location) %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Czechia" ~ "Czech Republic",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    location == "Cote d'Ivoire" ~ "Ivory Coast",
    location == "Congo" ~ "Republic of Congo",
    TRUE ~ location )) %>% 
  distinct() %>%
  group_by(location) %>% 
  filter(date == max(date)) %>%
  mutate(death_rate = total_deaths / population) %>%
  rename(region = location) %>%
  left_join(world, by = "region")
worldwide_mortality %>% ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = death_rate), color = "white") +
  labs(
    title = "Mortality Rate Around the World (deaths / population)",
    subtitle = "More than 3.1 million reported COVID deaths worldwide",
    fill = "Mortality Rate (%)"
  ) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  scale_fill_gradientn(na.value="grey",
                       colours = wes_palette("Moonrise1", 
                                             19, 
                                             type = "continuous")) 
```


