# Interface for pollen forecast
rm(list = ls())

library(r2d3)
library(tidyverse)
library(reshape2)
library(directlabels)

# Predictions
predictions = data.frame(day = c("Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday", "Sunday"),
                         Very_Low  = c(0, 0, 0.1, 0.7, 0.9, 0.4, 0.01),
                         Low       = c(0, 0.1, 0.2, 0.2, 0.05, 0.3, 0.1),
                         Moderate  = c(0.02, 0.1, 0.4, 0.05, 0.05, 0.1, 0.1),
                         High      = c(0.03, 0.5, 0.2, 0.05, 0, 0.1, 0.29),
                         Very_High = c(0.95, 0.3, 0.1, 0, 0, 0.1, 0.5)) %>%
  mutate(day = ordered(day, levels =  c("Monday", "Tuesday", "Wednesday",
                                    "Thursday", "Friday", "Saturday", "Sunday") ))


colours = c('#40b101', '#ffe500', '#ffa800', '#ff5801', '#aa0e00')

predictions %>% rename(`Very Low` = Very_Low, `Very High` = Very_High) %>%
  melt(id.vars = 'day') %>% ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
  theme_minimal() + 
  scale_fill_manual(values = colours) +
  facet_wrap(.~ day, scales = 'free') + theme(legend.position = 'none')

# New plot
predictions %>%  rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
  melt(id.vars = 'day') %>% group_by(day) %>%
  mutate(probability = max(value)) %>% mutate(test = ifelse(probability == value, 1, NA)) %>% 
  na.omit() %>%  mutate(location = "location") %>% select(-value, -test) %>% 
  ggplot(aes(x = day, y = location, colour = variable)) +
  geom_point(size = 31, colour = 'black') +
  geom_point(size = 30, colour = 'white') +
  geom_point(size = 30, aes(alpha = probability*2)) +
  geom_text(aes(label = paste(variable, '\n', probability*100, "%")), colour = 'black') +
  geom_text(aes(label = day), colour = 'black', nudge_y = 0.14) +
  theme_void() +
  scale_colour_manual(values = colours) +
  theme(legend.position = 'none')

