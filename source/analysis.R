library(tidyverse)
# Load ggplot2
library(ggplot2)

# The functions might be useful for A4
source("~/Documents/info201/assignments/a4-HibbaAbas/source/a4-helpers.R")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# How many people have died in custody from 2000 to 2013
total_jail_deaths <- incarceration_data %>%
  summarize(total <- sum(total_jail_pop_dcrp, na.rm = TRUE)) %>%
  return(total)

total_jail_deaths_pretty <- prettyNum(total_jail_deaths,big.mark=",",scientific=FALSE)

# how has this number cahnged over the years? 
jail_deaths_2000 <- incarceration_data %>%
  filter(year == 2000) %>%
  summarise(total_2000 <- sum(total_jail_pop_dcrp, na.rm = TRUE)) %>%
  return(total_2000)
jail_deaths_2000_pretty <- prettyNum(jail_deaths_2000, big.mark=",", scientific=FALSE)

jail_deaths_2013 <- incarceration_data %>%
  filter(year == 2013) %>%
  summarize(total_2013 <- sum(total_jail_pop_dcrp, na.rm = TRUE)) %>%
  return(total_2013)
jail_deaths_2013_pretty <- prettyNum(jail_deaths_2013, big.mark=",", scientific=FALSE)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns a data frame for the chart
get_year_jail_pop <- function() {
 
  incarceration_years <- incarceration_data %>%
    distinct(year) %>%
    select(year)

  incarceration_pop <- incarceration_data %>%
    group_by((year)) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm=TRUE)) %>%
    select(total_jail_pop)
  
  incarcertation_dataframe <- data.frame(incarceration_years, incarceration_pop)
return(incarcertation_dataframe)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  dataframe <- get_year_jail_pop()

  # make bar chart
  jail_pop_chart <- ggplot(dataframe, aes(y=total_jail_pop, x=year)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Increase of jail population in U.S (1970 - 2018)",
         caption = "This graph shows how the population across U.S jails has changed from 1970 to 2018")
  
  # print(jail_pop_chart)
  return(jail_pop_chart)
}


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
    
  # gets year and corresponding jail pop for state  
  jail_pop_state <- incarceration_data %>%
    filter(str_detect(state, paste(states, collapse = "|"))) %>%
    mutate(state_with_year = paste(state, year)) %>%
    mutate(state_t = paste(state)) %>%
    group_by(state_with_year) %>%
    summarise(jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    mutate(state = str_sub(state_with_year, start = 1, end = 3)) %>%
    mutate(year = strtoi(str_sub(state_with_year, start = 3))) %>%
    select(jail_pop, state, year)
    
  jail_pop_by_state <- data.frame(jail_pop_state)
  return(jail_pop_by_state)
}

plot_jail_pop_by_states <- function(states) {
  jail_pop_by_state <- get_jail_pop_by_states(states)
  
  jail_pop_by_state_chart <- ggplot(jail_pop_by_state, aes(y=jail_pop, x=year, group=state, color=state)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Jail population in U.S states from 1970 to 2018",
         caption = "A line graph showing the ail population in California, 
         Texas, and Washington from 1970 to 2018") +
    scale_x_continuous(breaks=seq(1980, 2020, 10), limits = c(1970, 2020))
  
  return(jail_pop_by_state_chart)
}





## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_rate_by_race <- function() {
  
  white_jail_rate <- incarceration_data %>%
    filter(year == 2018) %>%
    summarise(total = sum(white_jail_pop_rate, na.rm = TRUE) / sum(total_jail_pop_rate, na.rm = TRUE)) %>%
    select(total)
  
  black_jail_rate <- incarceration_data %>%
    filter(year == 2018) %>%
    summarise(total = sum(black_jail_pop_rate, na.rm = TRUE) / sum(total_jail_pop_rate, na.rm = TRUE)) %>%
    select(total)
  
  native_total <- incarceration_data %>%
    filter(year == 2018) %>%
    summarise(total = sum(native_jail_pop_rate, na.rm = TRUE) / sum(total_jail_pop_rate, na.rm = TRUE)) %>%
    select(total)
  
  latinx_total <- incarceration_data %>%
    filter(year == 2018) %>%
    summarize(total = sum(latinx_jail_pop_rate, na.rm = TRUE) / sum(total_jail_pop_rate, na.rm = TRUE)) %>%
    select(total)
  
  
  jail_by_race <- data.frame(
    x = rep(c("jail population rate"), 4),
    race = c("white", "black", "native american","latinx"),
    rate = c(0.815909, 4.126458, 1.713269, 1.593295)
  )
  
  return(jail_by_race)
}

plot_jail_pop_by_race <- function() {
  jail_rates <- get_jail_rate_by_race()
  
  # plot it
  chart_race <- ggplot(jail_rates, aes(fill=race, x=x, y=rate)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = "ratio of jail population by race in 2018",
         caption = "This graph shows the ratio of white, black, native
         american, and latinx population in U.S jails in 2018") +
    scale_y_continuous(labels = scales::comma)
  
  return(chart_race)
}




## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


