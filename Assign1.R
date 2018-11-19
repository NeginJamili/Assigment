# Q1 ----------------------------------------------------------------------

library(tidyverse)
forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$net_worth <- parse_number(forbes$net_worth)
forbes$age <- parse_number(forbes$age)

# Q2 ----------------------------------------------------------------------

# Those 3-digit numbers are in million, so they must be removed 
forbes <- forbes %>%
  filter (nchar(trunc(net_worth)) <= 2)

# Although other codes (such as the below one) are also applicable
forbes <- forbes %>%
  filter (net_worth < 954)

# We could also separate the net_worth into 2 columns (before using parse_number)
# by separate(forbes, into = c("net_worth", "letter"), sep = " ")
# and then use parse_number and filter those with B letters

# Q3 ----------------------------------------------------------------------

ggplot (forbes, aes (x = age, y = net_worth)) +
  geom_point()

# Since the data includes some numbers much larger than the majority, 
# it is useful to use logarithmic scales in this chart 
ggplot (forbes, aes (x = age, y = log(net_worth))) +
  geom_point()

# For improving the plot, it's better to ommit the rows with no specific age
ggplot(data = filter(forbes, age > 0), aes(x = age, y = log(net_worth))) +
  geom_point()

# In spite of the negligibe increase of the worth observed in older people,
# in order to provide a better plot, I am going to manipulate the data 
# and categorize the age of people into 8 groups

forbes <- forbes %>%
  filter (age > 0) %>%
  mutate (age_group = age %/% 10 - 1) 

forbes$age_group <- factor(forbes$age_group,
                           levels = c(1:8),
                           labels = c("20-30", "30-40", "40-50", "50-60", "60-70", 
                                      "70-80", "80-90", ">90"))

ggplot(data = forbes, mapping = aes(x = age_group, y = log(net_worth), fill = age_group)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Age Group") +
  ylab("Log of net worth") 

# Considering the plot, now we can observe that older people have more wealth
# in average, although the larger amounts are possessed by 70-80 years old people.

# Q4 ----------------------------------------------------------------------

group_by_counry <- forbes %>%
  group_by(country) %>%
  summarise(count = n(), range = max(net_worth) - min(net_worth)) %>%
  filter(count >= 6) %>%
  arrange(range)

# Q5 ----------------------------------------------------------------------

ggplot(group_by_counry, aes(x = country, y = range)) +
  geom_bar(stat = "identity", color = "cadetblue4", fill = "aquamarine2") + 
  coord_flip() +
  ylab("difference between the highest and lowest net worth") +
  theme_bw ()

# Q6 ----------------------------------------------------------------------

ggplot(group_by_counry, aes(x = reorder(country,range), y = range)) +
  geom_bar(stat = "identity", color = "cadetblue4", fill = "aquamarine2") + 
  coord_flip() +
  ylab('difference between the highest and lowest net worth') +
  theme_bw () +
  ylab("Country")

# Q7 ----------------------------------------------------------------------

group_by_rank <- forbes %>% 
  group_by(rank) %>% 
  summarise(count = n()) %>% 
  filter (count > 1) %>% 
  View ()

# There are 19 ranks where more than one person are assigned to them (6, 27, ...)
# The given table shows how many persons are assigned to shared ranks

