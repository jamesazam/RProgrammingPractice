# #purrr practice
# checkpackages <- function(){
#     if(!require("tidyverse")){
#         install.packages("tidyverse")
#         library("tidyverse")}
#             library("tidyverse")}
# checkpackages()


library("gapminder")
library("tidyverse")


gapminder <- gapminder %>% mutate(year1950 = year - 1950)

by_country <- gapminder %>%
    group_by(continent, country)%>%
    nest()

head(by_country)
by_country$data[[1]] #first element in the nested list 