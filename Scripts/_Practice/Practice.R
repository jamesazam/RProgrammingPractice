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
    rsq =glance %>%map_dbl("r.squared"),
    tidy = map(model, broom::tidy),
    augment = map(model, broom::augment)
  )
models
unnest(models,data)
unnest(models,glance, .drop = TRUE) %>%View()
unnest(models,rsq)%>%View()
unnest(models,tidy)%>%View()
unnest(models,augment)%>%View()

<<<<<<<


#' CONCLUSION
#' 1.Store related objects in list-clumns
#' 2. Learn functional programming to concentrate on the verb
#' and not the object
#' 3. Use broom to convert models to tidy data

=======



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
>>>>>>>