# Some exploratory data analysis
# Libraries
{
  library(tidyverse)
  library(reshape2)
  library(ggcorrplot)
  library(coin)
  library(factoextra)
  library(GGally)
}

# Read in data
dataG <- read.csv("grassdata.csv", h = T)
names(dataG)
# Get a sense as to the correlations
# First remove NA, and deselect X, date, logvalue; then calculate correlation matrix
data_cor <- data %>% na.omit() %>% select(-X, -date, -logvalue) %>% cor()
ggcorrplot(data_cor)
# Value fairly highly correlated with df, likely because of seasonality
# Value slightly positively correlated with temp
# Temp negatively correlated with rain at each lag
# Fairly strong negative correlation between humidity and maximum temperature

# Histogram of pollen count
data %>% ggplot(aes(x = value)) +
  geom_histogram(colour = 'black', fill = 'skyblue') +
  theme_bw()
# Extremely positively skewed
# Filter out observations > 50
data %>% filter(value <= 50) %>%
  ggplot(aes(x = value)) +
  geom_histogram(colour = 'black', fill = 'skyblue') +
  theme_bw()
# The mode is clearly 0, and values > 30 are quite rare. 
# This might make forecasting higher counts quite difficult as the data are quite unbalanced.

# Histograms of other meterological covariates
data %>% select(4:9) %>% melt() %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(colour = 'black') +
  facet_wrap(. ~ variable, 
             scales = 'free', nrow = 3) +
  theme_minimal() +
  theme(legend.position = 'none') 
# Here we can see the wind blows in two main directions: 
# around 180 degrees and around 300 degrees.
# Rain is very strongly positively skewed.
# Humidity, temp seem normally distributed.
# Wind speed seems to be slightly right skewed, with some zeros recorded (and not much below 1): maybe an error?

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

# Investigating differences in pollen counts at different wind directions
# Make a binary variable for wind direction
# Make another binary variable 
data_new <- data %>% filter(value <= 50) %>% 
  mutate(wind_dir_bin = case_when(
    value.wind_dir > 100 & value.wind_dir < 200 ~ "dir1",
    value.wind_dir <= 100 | value.wind_dir >= 200 ~ "dir2"
  )) %>% 
  mutate(pollen_bin = case_when(value == 0 ~ 'zero',
                                value > 0 ~ '> zero')) %>%
  na.omit()

# Make a histogram for pollen count at each wind direction
ggplot(data_new, aes(x = value, fill = wind_dir_bin)) +
  geom_histogram(aes(y = ..density..), colour = 'black') +
  facet_wrap(.~ wind_dir_bin) +
  theme_bw()
# At wind direction 1 there are higher pollen counts on average!
# Might be interesting to explore further
# Permutation t test for differences in pollen count at different wind directions
value_dir1 <- data_new %>% filter(wind_dir_bin == 'dir1') %>% select(value) %>% as.matrix() # select values when wind is dir1
value_dir2 <- data_new %>% filter(wind_dir_bin == 'dir2') %>% select(value) %>% as.matrix() # select values when wind is dir2

values <- c(value_dir1, value_dir2)
directions <- factor(rep(c("dir1", "dir2"), c(length(value_dir1), length(value_dir2))))

oneway_test(values ~ directions, alternative = "greater", 
                   distribution=approximate(nresample = 9999))  # Permutation t-test using coin package

# try PCA
pca_pollen <- data_new %>% na.omit() %>% select(-X, -date, -logvalue, -wind_dir_bin, -pollen_bin, -value) %>% prcomp(scale = T)
summary(pca_pollen)

# Biplot
fviz_pca_biplot(pca_pollen, geom = 'point')

# Project data points onto PCA axes
# See whether pollen categories are well seperated in two dimensions
proj <- as.data.frame(pca_pollen$x)
ggplot(proj) + geom_point(aes(x = proj[,1], y = proj[,2], colour = data_new$pollen_bin))
# Reasonably separated, but lots of overlap!

# Some bivariate plots:
data_new %>% na.omit() %>% select(-X, -date, logvalue, -wind_dir_bin, -pollen_bin, -logvalue, -rain.lag1, -rain.lag2) %>%
  ggpairs()





