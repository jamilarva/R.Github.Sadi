---
  title: "Data Acquisition. Challenge 2"
author: "Sadi Tomtulu"
date: "2022-05-09"
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
  background-color: #bdc3c7 ;
    border-color: #black;
    border-width: 5px;
}
```
```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE, cache=TRUE)
```

Part 1: Find an API


```{r t1p0, fig.height=7, fig.width=10, message=FALSE}

# Libraries

library(tidyverse)
library(jsonlite)
library(xopen) 
library(glue)
library(readxl)
library(httr)
library(broom)
library(dplyr)
library(lubridate)
library(ggplot2)

```

```{r}

#API Data
KEY <- "e8f8b27a07acd73ce972e8bab7726df9"

# Find Colin Firth

Actress <- "Colin"

ID <- GET(glue("https://www.themoviedb.org/person/5472-colin-firth")) %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>% 
  as.data.frame

ActorID <- ID$results.id[ID$results.name=="Scarlett Johansson"]

Query <- "sort_by=popularity.desc&vote_count.gte=150"

db <- GET(glue("https://www.themoviedb.org/movie/207703-kingsman-the-secret-service")) %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>%
  as.data.frame %>%
  # Appearance
  rename_all(~stringr::str_replace(.,"^results.","")) %>%
  separate(col = release_date,
           into = c ("year", "month", "day"), 
           sep = "-") %>%
  select(title, popularity, vote_average, year)

# Graph
db %>% ggplot(aes(x=reorder(title, as.numeric(year)), y=popularity)) +
  
  
  geom_col(fill = wes_palette("Moonrise1", 2)[2]) +
  geom_label(aes(label = year)) + 
  labs(
    title = "Populartiy of Movies", 
    subtitle = "Year - movies are released. Data from The Movie DB."
  ) +
  xlab("Movie") + ylab("Popularity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



```{r}
library(RSQLite)
library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(rvest)
library(stringi)
library(xopen)
library(dplyr)

```

``` {r}

base_url <- 'https://www.rosebikes.com/bikes'

get_bike_family_urls <- function(base_url) {
  
  bike_family_urls <- read_html(base_url) %>%
    html_nodes(css = ".catalog-categories-item > a") %>%
    html_attr('href') %>%
    
    
    enframe(name = "position", value = "subdirectory") %>%
    
    mutate(
      url = glue('https://www.rosebikes.com{subdirectory}')
    ) 
  
  bike_family_urls <- bike_family_urls %>% 
    filter(!grepl('sale', url)) %>%
    filter(!grepl('kids', url))
  bike_family_urls <- bike_family_urls['url']
  
}
```

```{r}

get_model_urls <- function(url) {
  
  bike_type_url <- read_html(url) %>%
    html_nodes(css = ".catalog-category-bikes__content > a") %>%
    html_attr('href') %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue('https://www.rosebikes.com{url}')) 
}


get_bike_names <- function(url) {
  
  bike_model_name_tbl <- read_html(url) %>%
    html_nodes(css = ".catalog-category-model__title") %>%
    html_text() %>%
    # Convert vector to tibble
    as_tibble()
  
  
}
```



get_bike_prices <- function(url) {
  
  bike_model_price_tbl <- read_html(url) %>%
    html_nodes(css = ".product-tile-price__current-value") %>%
    html_text() %>%
    # Convert vector to tibble
    as_tibble()
  
}

bike_family_url_tbl <- get_bike_family_urls(base_url)
bike_family_url_tbl <- bike_family_url_tbl %>%
  slice(1:3)


# Create a table with bike model URLS
bike_model_url_tbl <- tibble()

for (i in seq_along(bike_family_url_tbl$url)) {
  
  web <- toString(bike_family_url_tbl$url[i])
  bike_model_url_tbl <- bind_rows(bike_model_url_tbl, get_model_urls(web))
  
}


# Create a table with bike model names
bike_model_names_tbl <- tibble()

for (i in seq_along(bike_model_url_tbl$url)) {
  
  web <- toString(bike_model_url_tbl$url[i])
  bike_model_names_tbl <- bind_rows(bike_model_names_tbl, get_bike_names(web))
  
}

# Rename cols
names(bike_model_names_tbl)[1] <- "Bike Model"



# Create a table with bike prices
bike_model_prices_tbl <- tibble()

for (i in seq_along(bike_model_url_tbl$url)) {
  
  web <- toString(bike_model_url_tbl$url[i])
  bike_model_prices_tbl <- bind_rows(bike_model_prices_tbl, get_bike_prices(web))
  
}

# Rename cols
names(bike_model_prices_tbl)[1] <- "Bike Prices"

# Join into one table
table_of_prices <- bind_cols(bike_model_names_tbl,bike_model_prices_tbl)

knitr::kable(table_of_prices[1:10, ], caption = 'Rosebike Bike Model & Prices')

```

