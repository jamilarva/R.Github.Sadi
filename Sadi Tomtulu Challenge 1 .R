§Cedrick

title: "Into the TidyVerse"
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
  
  {css, echo=FALSE}
body {
  color: purple;
  background-color: #ac94f4;
    font-family: Futura;
}
.list-group-item, .list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
  color: black;
  background-color: #2ecc711 ;
    border-color: #black;
    border-width: 5px;
}


{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE, cache=TRUE)


{r t1p0, fig.height=7, fig.width=10, message=FALSE}

library(tidyverse)
library(jsonlite)
library(xml2)
library(readxl)
library(httr)
library(rvest)
library(broom)
library(lubridate)
library(wesanderson)
library(ggplot2)




#Import Data

bikes <- read_excel(path = "C:/Users/sedit/OneDrive/Desktop/R.Github.SadiTomtulu/R.Github.Sadi/bikes.xlsx")

bikeshop <- read_excel(path = "C:/Users/sedit/OneDrive/Desktop/R.Github.Jamila/bikeshops.xlsx")

orderlines <- read_excel(path = "C:/Users/sedit/OneDrive/Desktop/R.Github.Jamila/orderlines.xlsx")

{r}
orderlines 
bikes


{r}
bike_orderlines_joined <- orderlines %>% 
  left_join(bikes, by = c("product.id" = "bike.id")) %>% 
  left_join(bikeshop, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_wrangled <- bike_orderlines_joined %>%
  # Split up location data for Part 1
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  # Split up date data for Part 2
  separate(col = order.date, 
           into = c("year", "month", "day"),
           sep = "-") %>%
  # Calculate and add total price data
  mutate(total.price = price * quantity)


{r t1p1, fig.height=7, fig.width=10, message=FALSE}

sales_by_state <- bike_orderlines_wrangled %>%
  
  
  select(state, total.price) %>%
  
  
  group_by(state) %>%
  summarize(sales=sum(total.price)) %>%
  
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))



sales_by_state %>% ggplot(aes(x=state, y=sales)) +
  
  geom_col(fill = wes_palette("FantasticFox1", 2)[2]) +
  geom_label(aes(label = sales_text)) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", suffix = " €")) +
  labs(
    title = "Lifetime Sales",
    subtitle = "Overview of lifetime sales in German states"
  ) +
  xlab("German State") + ylab("Lifetime Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


{r t1p2, fig.height=7, fig.width=10, message=FALSE}
# Create frame with price data by Bundesland by year
sales_by_state_by_year <- bike_orderlines_wrangled %>% 
  # Select relevant data (state, year, total price)
  select(year, state, total.price) %>%
  # Group by year and sum the order totals for each state
  group_by(year, state) %>%
  summarize(sales=sum(total.price)) %>%
  # Pretty labels
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_by_year %>%  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year",
    subtitle = "Revenue in each Bundesland by year"
  ) +
  
  scale_fill_manual(values = wes_palette("Darjeeling1", 
                                         12, 
                                         type = "continuous")) +
  # Remove fill legend becasuse it's not necessary, 
  # angle years for readability
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))
        
        