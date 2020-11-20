
# Shiny app without buttons
library(shiny)
library(tidyverse)
library(reshape2)
library(plotly)
library(directlabels)

# Predictions
predictions = read.csv("https://raw.githubusercontent.com/skycope/grass-pollen/master/Workflow/predictions.csv") %>%
  select(-X) %>%
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



# User Interface -------
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           reactableOutput("table", height = 250)
    )
  ),
  fluidRow(
    column(width = 12,
           plotlyOutput("plot2", height = 400)
    )
  )
)


orders <- data.frame(
  Order = 2300:2304,
  Created = seq(as.Date("2019-04-01"), by = "day", length.out = 5),
  Customer = sample(rownames(MASS::painters), 5),
  Status = sample(c("Pending", "Paid", "Canceled"), 5, replace = TRUE)
)


# Server ---------
red_pal <- function(x){
  return(sample(c('#DA1600', '#FFDAD5', '#FFDAD5')))}

plot(red_pal(3))

reactable(
  data,
  columns = list(
    Petal.Length = colDef(
      style = function(value) {
        normalized <- (value - min(data$Petal.Length)) / (max(data$Petal.Length) - min(data$Petal.Length))
        color <- red_pal(normalized)
        list(background = color)
      }
    )
  )
)


server <- function(input, output){
  
  output$table <- renderReactable(reactable(orders, columns = list(
    Status = colDef(cell = function(value) {
      class <- paste0("tag status-", tolower(value))
      htmltools::div(class = class, value)
      
    })
  )))
  
  output$plot2 = renderPlotly({
    q  = predictions %>% rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
      melt(id.vars = 'day') %>%
      group_by(day, variable) %>%
      rename(Category = variable) %>%
      ggplot(aes(x = day, y = value, fill = Category, group = Category)) +
      geom_area(position = 'stack', alpha = 0.8) +
      scale_fill_manual(values = colours) +
      ylab("Probability") +
      xlab("Day") +
      theme_minimal()     
    ggplotly(q)
  })
}

# Run App ----------
shinyApp(ui, server)

reactable(
  iris,
  rowStyle = function(index) {
    if (iris[index, "Sepal.Width"] > 3.5) {
      list(fontWeight = "bold")
    }
  }
)

library(htmltools)

  library(plotly)
library(reactable)
library(shiny)
