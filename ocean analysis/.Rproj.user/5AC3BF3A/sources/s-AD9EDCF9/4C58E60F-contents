# I will analyze the OCEAN personality test data from an open source project from
# 2012. The info is in the codebook.txt file. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(psych)
library(visdat)
library(stringr)


# Data importation --------------------------------------------------------
setwd("~/github/OCEAN/ocean analysis")

df <- read_table("data.csv")

df1 <-  as.data.frame(df)
# Data scanning -----------------------------------------------------------

# info from txt
# cols 1:7 = demographics
# cols 8:17 = extroversion items 
# cols 18:27 = neurotism items
# cols 28:37 = agreeableness items 
# cols 38:47 = consciousness items 
# cols 48:57 = openness items 

# NA 

sum(is.na(df))
  # 9 official NA values (0 are missed values)

df_missed <- df %>% 
  gather(df, value, ) %>% 
  group_by(df) %>% 
  tally(value == 0)
  # this tells the number of 0 in every col

  # it is worth mentioning that in the items cols there's only 1 missing value.
  # this makes me think that the whole record is missed
  # lets confirm

df %>% 
  select(1, 2, 8:57) %>% 
  filter(E1 == 0)
  # this confirms that it repeats along the items cols 
  


# confirms values of cols 

under_limits <- sapply(df, unique)
  # most values are equal to those showed in the codebook, except age, and O10, which
  # seems ti have some NA values

# Age scan 

describe(df$age)
  # data from 13 to 1e+09 ### fix needed


####################
# Data cleaning -----------------------------------------------------------


# Age 

df %>% 
  select(age) %>% 
  arrange(age) %>% 
  tail()
# The previous code allows to see what was the problem with the range of age. 
# seems that the year of birth is placed instead of age in years for some records
# and there is one record which age is 999999999... 
# so to resolve this: 
# 1. calculate the age in years for every record that is a year of birth
# 2. Substitute for the mean age every record that is not a year of birth and 
# its > 80 years  

# 1. convert years of birth into age

df$age <- if_else(df$age > 1960 & df$age < 2012, 2012 - df$age, df$age)

# here I chose to look at the extreme values of records with 4 digits, resulting in 
# 1961 the oldest year of birth registered. So I indicated R to choose the records between 
# 1960 and 2012 (the last is the year of data recollection)

# 2.  Substitute for the mean age
df$age <- if_else(df$age > 80, 26, df$age)

# Here i simply set 80 as max age since they were record at 91 and 99 that seemed 
# odd. Then I calculated mean excluding >80 records (= 26,2). So I only put 26
# as replacing value

# assessing age 

describe(df$age)

# There is 1 record which age is 12. According to the codenotes, subjects < 13 age
# were removed. However that record will be kept just to be subset to a young sample

# NA values 

# data subseting ----------------------------------------------------------

demographic_data <- df %>% 
  select(1:7)

ext_items <- df %>% 
  
neurotism_items

agree_items

cons_items

open_items





