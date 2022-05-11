
  title: "Data Wrangling. Challenge 3"
author: "Sadi Tomtulu
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

```{r t1p0, fig.height=7, fig.width=10, message=FALSE}

# Libraries

library(jsonlite)
library(xopen) 
library(readxl)
library(httr)
library(lubridate)
library(ggplot2)
library(RSQLite)
library(tidyverse)
library(purrr)
library(dplyr)
library(data.table)
library(vroom)

```
# Data Wrangling 

# Part 1: Top Patents
10 US companies with the most assigned/granted patents.

```{r t3p1, fig.height=7, fig.width=10, message=FALSE}

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)


old_dir <- getwd()
setwd("C:\Users\sedit\OneDrive\Desktop\R.Github.Jamila\jamilarva")
patent_tb1 <- vroom(
  file       = 'patent.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

assignee_tb1 <- vroom(
  file       = 'assignee.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_assignee_tb1 <- vroom(
  file       = 'patent_assignee.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

uspc_tb1 <- vroom(
  file       = 'uspc.tsv', 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")) %>%
  transform(patent_id = as.character(patent_id))


setwd(old_dir)

# Combine all of the databases into one large table;
wrangled <- assignee_tb1 %>%
  left_join(patent_assignee_tb1, by = c("id" = "assignee_id")) %>%
  left_join(patent_tb1, by = c("patent_id" = "id")) %>%
  left_join(uspc_tb1, by = "patent_id")

# Get the Organizations with the most patents using table() and sort()
tops <- sort(table(wrangled$organization), decreasing = T)[1:100] %>%
  as.data.frame() %>%
  mutate(Var1 = Var1 %>% str_to_title())


color <- toString(wes_palette("Royal2", 3, type="discrete")[3])
longString = paste("$(this.api().table().header()).css({'color':'#4073ff', 'background-color' : '", color, "'});")

datatable(tops, class = 'cell-border stripe', 
          rownames = FALSE,
          colnames = c('Organization', 'Number of Patents (2014)'),
          options = list( initComplete = JS( "function(settings, json) {",longString,"}")))
```


# Part 2: The most granted patents in August 2014

```{r}
# Relevant data from the main database
wrangled_aug <- wrangled %>% 
  select(organization, date) %>%
  
  filter(date >= "2014-08-01" & date <= "2014-08-31")

Augusttop <- sort(table(wrangled_aug$organization), decreasing=T)[1:100] %>%
  as.data.frame() %>%
  mutate(Var1 = Var1 %>% str_to_title())


color <- toString(wes_palette("Royal2", 3, type="discrete")[3])
longString = paste("$(this.api().table().header()).css({'color':'#4073ff', 'background-color' : '", color, "'});")

datatable(Augusttop, class = 'cell-border stripe', 
          rownames = FALSE,
          colnames = c('Organization', 'Number of Patents (Aug 2014'),
          options = list(initComplete = JS("function(settings, json) {",longString,"}")))

```

