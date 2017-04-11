library("gapminder")
library("tidyverse")


gapminder <- gapminder %>% mutate(year1950 = year-1950)


#nested data

by_country <- gapminder%>%
  group_by(country,continent) %>%
  nest()

#country_model() fits each country data to a linear model

country_model <- function(df){
  lm(lifeExp ~ year1950, data = df)
}


models <- by_country %>%
  mutate(model=map(data, country_model))

#look through some rows
models %>% filter(continent=="Africa")



######################################################
#what can we do with a list of linear models? not very much

#we can convert our data in tidy data using the broom package

#what sort of data can we get from our models?
##In BROOM, glance() gives the model summaries,
##tidy() gives the estimates, and augment() gives the stats per observation
library("broom")

models <- models %>%
  mutate(
    glance = map(model,broom::glance),
    tidy = map(model, broom::tidy),
    augment = map(model, broom::augment)
  )




#######################################################
library("ggplot2")
library("dplyr")
data("economics",package = "ggplot2")
ggplot(data = economics) + geom_line(aes(x=date,y=unemploy))

library(reshape2)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)