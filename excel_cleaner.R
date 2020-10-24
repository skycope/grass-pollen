# Read in excel files
# output a nice csv
rm(list = ls())
library(readxl)
library(tidyverse)

setwd("/Users/skycope/Documents/UCT/Stats\ Honours/Project/Data\ and\ code")
setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen/Fw__grass_pollen_data_since_2015")


# Between 2019-11-04 and 2019-11-24 the machine wasn't working
# I set these observations to NA (were recorded as 0)
file1 = read_xls("Cape Town aug19-feb20.xls", sheet = 4, skip = 2) %>% 
        filter(Category == 'Grass') %>% 
        select(-1:-4) %>%
        t() %>%
        data.frame() %>%
        mutate(pollen_count = X1 + X2 + X3 + X4) %>%
        select(pollen_count) %>%
        mutate(date = seq(as.Date("2019-08-12"), as.Date("2020-02-09"), by = 1)) %>%
        mutate(pollen_count = 
                 ifelse(date >= as.Date("2019-11-04") & date <= as.Date("2019-11-24"), NA, pollen_count))

file2 = read_xls("Cape Town mar-sept 2019.xls", sheet = 4, skip = 2) %>% 
  filter(Category == 'Grass') %>% 
  select(-1:-4) %>%
  t() %>%
  data.frame() %>%
  mutate(pollen_count = as.numeric(X1) + as.numeric(X2) + as.numeric(X3) + as.numeric(X4)) %>%
  select(pollen_count) %>%
  mutate(date = seq(as.Date("2019-03-12"), as.Date("2019-09-09"), by = 1))
file2  = file2[1:153,]

# Unusable I think -- every observation is 0 
#file3 = read_excel("SAAO July 2017- jan 2018.xlsx", sheet = 4, skip = 2) %>% 
#  filter(Category == 'Grass') %>% 
#  select(-1:-4) %>%
#  t() %>%
#  data.frame() %>%
#  mutate(pollen_count = as.numeric(X1) + as.numeric(X2) + as.numeric(X3) + as.numeric(X4)) %>%
#  select(pollen_count) %>%
#  mutate(date = seq(as.Date("2017-07-26"), as.Date("2018-01-23"), by = 1))

file4 = read_xls("SAAO-sep2018-mar2019.xls", sheet = 4, skip = 2) %>% 
  filter(Category == 'Grass') %>% 
  select(-1:-4) %>%
  t() %>%
  data.frame() %>%
  mutate(pollen_count = as.numeric(X1) + as.numeric(X2) + as.numeric(X3) + as.numeric(X4)) %>%
  select(pollen_count) %>%
  mutate(date = seq(as.Date("2018-09-11"), as.Date("2019-03-11"), by = 1))

# Merge files together
merged = rbind(file1, file2, file4)
merged = merged[order(merged$date),]

# Read in new data
old_data = read.csv("grassdata.csv", h = T) %>% 
  select(value, date) %>%
  rename(pollen_count = value) %>% mutate(date = as.Date(date))


# Merge all into one file
new_data = rbind(merged, old_data)

ggplot(new_data, aes(x = date, y = pollen_count)) +
  geom_line() +
  theme_minimal()


write.csv(new_data, "all_counts.csv")

