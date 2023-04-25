# Bringing in necessary libraries
library(tidyverse)

# Reading in tree data
trees = read_csv('TS3_Raw_tree_data.csv')

# Question 1
# Creating new columns for the cities and states using a regular expression
trees$just_city = str_match(trees$City, "^(.+),")[,2]
trees$state = str_match(trees$City, ", ([:alpha:]{2})\\b")[,2]

# Checking for N/A values
View(trees)
is.na(trees$just_city)

# Grouping by state and summarizing with the number of records per state
states = group_by(trees, state) %>%
  summarize(record_num = n())
# Making a bar graph with the grouping
ggplot(states, aes(x=state, y=record_num)) +
  geom_col()

# Question 2
# Filtering the trees by the records from North or South Carolina

carolina_trees = filter(trees, state == "NC" | state == "SC")
# Grouping and summarizing by cities to count the number of cities
carolina_cities = group_by(carolina_trees, just_city) %>%
  summarize()
# Viewing the grouping to count the number of cities
View(carolina_cities)
