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
