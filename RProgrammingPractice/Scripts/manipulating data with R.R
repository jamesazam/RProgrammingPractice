install.packages('RPostgreSQL')
library(RPostgreSQL)

driver       <- dbDriver("PostgreSQL")
connection   <- dbConnect(driver,
                          user     = "mhairi.mcneill@deltadna.com",
                          password = "tempbl0gp@ssword", 
                          host     = "data.deltadna.net",
                          dbname   = "demo-co.deltacrunch")

# Only select events that involve spending currency
query <- readLines('query.txt', warn = FALSE)
query <- paste(query, collapse = " ")
data  <- dbGetQuery(connection, query)

##############
# Dataframes #
##############

data

str(data)

View(data)

#################################################
# What is the average amount of currency spent? #
#################################################

data$spend_amount

str(data$spend_amount)

mean(data$spend_amount)

median(data$spend_amount)

###############
# Basic dplyr #
###############

install.packages('dplyr')
library(dplyr)

# Tabular data frames
data <- tbl_df(data)
data

# Verb 1: Select
select(data, time, gender)

# Verb 2: Filter
filter(data, spend_amount > 999)

# Verb 3: Arrange
arrange(data, time)

# Verb 4: Mutate
mutate(data, spend_amount = spend_amount/100)

# Verb 5: Summarise
summarise(data, mean_spend   = mean(spend_amount),
          median_spend = median(spend_amount))



#####################################################
# Is average spend different for different genders? #
#####################################################

data2 <- mutate(data, spend_amount = spend_amount/100) # Change to dollars
data3 <- group_by(data2, gender)                       # Group by gender
data4 <- summarise(data3,                              # Summarise each group
                   mean = mean(spend_amount),
                   median = median(spend_amount),
                   count = n())
data4



####################
# Now with piping! #
####################

identical(4, 4)
identical(5, 4)

4 %>% identical(4)
5 %>% identical(4)

data %>%
    mutate(spend_amount = spend_amount/100) %>%
    group_by(gender) %>%
    summarise(mean = mean(spend_amount),
              median = median(spend_amount),
              count = n())



###############################################
# Is this different for different age groups? #
###############################################

data %>%
    filter(gender != 'UNKNOWN') %>% 
    filter(age_group != 'UNKNOWN') %>%
    mutate(spend_amount = spend_amount/100) %>%
    group_by(age_group, gender) %>%
    summarise(mean = mean(spend_amount),
              median = median(spend_amount),
              count = n()) %>%
    ungroup() %>%
    arrange(gender, age_group)



#######################################################################
# Does the amount people spend increase with increasing spend number? #
#######################################################################

data %>%
    group_by(user_id) %>%
    arrange(time) %>%
    mutate(spend_number = row_number()) %>%
    ungroup() %>%
    group_by(spend_number) %>%
    summarise(mean = mean(spend_amount),
              median = median(spend_amount),
              count = n())









