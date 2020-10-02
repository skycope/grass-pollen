# Exploritory data analysis on APi data

{
library(tidyverse)
library(reshape2)
library(ggcorrplot)
library(coin)
library(factoextra)
library(GGally)
}

load('countAPI.RData')
corrplot(data[,-c('date','Conditions','Visibility')])
data_cor <- data %>% na.omit() %>% select(-date, -Conditions, -Visibility) %>% cor()
ggcorrplot(data_cor) 

# Histogram of pollen count
data %>% ggplot(aes(x = value)) +
  geom_histogram(colour = 'black', fill = 'skyblue') +
  theme_bw()
# Positively skewed
# Filter out observations > 35
data %>% filter(value <= 35) %>%
  ggplot(aes(x = value)) +
  geom_histogram(colour = 'black', fill = 'skyblue') +
  theme_bw()

# Histograms of other meterological covariates
data %>% select(2:9) %>% melt() %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(colour = 'black') +
  facet_wrap(. ~ variable, 
             scales = 'free', nrow = 3) +
  theme_minimal() +
  theme(legend.position = 'none') 

# Plot seasonality, omitting very large pollen counts
data %>% filter(value <= 50) %>% 
  select(3:9, ds) %>% 
  melt(id.vars = 'ds') %>%
  ggplot(aes(x = ds, y = value, colour = variable)) +
  geom_point(colour = 'black', size = 0.8, alpha = 0.6) +
  geom_point(alpha = 0.8, size = 0.7) +
  geom_smooth(colour = 'black', linetype = 'solid') +
  facet_wrap(. ~ variable, 
             scales = 'free', nrow = 3) +
  theme_bw() +
  xlab("Days Since Start of Year") +
  theme(legend.position = 'none') 