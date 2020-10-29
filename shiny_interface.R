# Interface for pollen forecast
rm(list = ls())

library(r2d3)
library(tidyverse)
library(reshape2)
library(directlabels)

# Predictions
predictions = data.frame(day = c("Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday", "Sunday"),
                         Very_Low  = c(0.1, 0, 0.1, 0.7, 0.9, 0.4, 0.01),
                         Low       = c(0.8, 0.1, 0.2, 0.2, 0.05, 0.3, 0.1),
                         Moderate  = c(0.1, 0.1, 0.4, 0.05, 0.05, 0.1, 0.1),
                         High      = c(0, 0.5, 0.2, 0.05, 0, 0.1, 0.29),
                         Very_High = c(0, 0.3, 0.1, 0, 0, 0.1, 0.5)) %>%
  mutate(day = ordered(day, levels =  c("Monday", "Tuesday", "Wednesday",
                                    "Thursday", "Friday", "Saturday", "Sunday") ))

new = predictions %>%  rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
  melt(id.vars = 'day') %>% group_by(day) %>%
  mutate(probability = max(value)) %>% mutate(test = ifelse(probability == value, 1, NA)) %>% 
  na.omit() %>%  mutate(location = "location") %>% select(-value, -test) %>% data.frame()

colours = c('#40b101', '#ffe500', '#ffa800', '#ff5801', '#aa0e00')

predictions %>% rename(`Very Low` = Very_Low, `Very High` = Very_High) %>%
  melt(id.vars = 'day') %>% ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
  theme_minimal() + 
  scale_fill_manual(values = colours) +
  facet_wrap(.~ day, scales = 'free') + theme(legend.position = 'none')

# New plot
new %>% 
  ggplot(aes(x = day, y = location, colour = variable)) +
  geom_point(size = 31, colour = 'black') +
  geom_point(size = 30, colour = 'white') +
  geom_point(size = 30, alpha = 0.7) +
  geom_text(aes(label = paste0(variable, '\n', probability*100, "%")), colour = 'black') +
  geom_text(aes(label = day), colour = 'black', nudge_y = 0.1) +
  theme_void() +
  scale_colour_manual(values = colours) +
  theme(legend.position = 'none')


ibrary(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)

# We'll use a subset of the mtcars data set, with fewer columns
# so that it prints nicely
selected_points <- new[0, ]

ui <- fluidPage(
  fluidRow(
    column(width = 10,
           plotOutput("plot1", height = 300,
                      click = "clicked"
                      )
           )
    ),
  fluidRow(
    column(width = 6,
           plotOutput("plot2", height = 300
                      )
           )
    )
  )


server <- function(input, output) {
  rv <- reactiveValues(selected_points = new[0, ])
  
  observe({
    # add clicked
    rv$selected_points <- rbind(isolate(rv$selected_points), 
                                nearPoints(new, input$clicked))

    rv$selected_points <- isolate(
      rv$selected_points[!(duplicated(rv$selected_points) | 
                             duplicated(rv$selected_points, fromLast = TRUE)), ])
    str(rv$selected_points)
  })
  
  output$plot1 <- renderPlot({
    new %>% 
      ggplot(aes(x = day, y = location, colour = variable)) +
      geom_point(size = 31, colour = 'black') +
      geom_point(size = 30, colour = 'white') +
      geom_point(size = 30, alpha = 0.7) +
      geom_text(aes(label = paste0(variable, '\n', probability*100, "%")), colour = 'black') +
      geom_text(aes(label = day), colour = 'black', nudge_y = 0.2) +
      theme_void() +
      scale_colour_manual(values = colours) +
      theme(legend.position = 'none')
    
  })

  output$plot2 <- renderPlot({
    predictions %>% 
      filter(day == rv$selected_points[,1]) %>%
      melt(id.vars = 'day') %>%
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste(rv$selected_points[,1], "prediction distribution")) +
      xlab("") +
      ylab("Probability")

  })
}



shinyApp(ui, server)

