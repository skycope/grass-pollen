
# Shiny app without buttons
library(shiny)
library(tidyverse)
library(reshape2)
library(plotly)
library(directlabels)
library(reactable)

# Predictions
predictions = read.csv("predictions.csv") %>%
  mutate(dates = as.Date(dates)) %>% rename(Date = dates)

predictions$Date = format(predictions$Date, "%d %b")
predictions$day = factor(predictions$day, levels = as.character(predictions$day))



new = predictions %>%  rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
  melt(id.vars = 'day') %>% group_by(day) %>%
  mutate(probability = max(value)) %>% mutate(test = ifelse(probability == value, 1, NA)) %>% 
  na.omit() %>%  mutate(location = "location") %>% select(-value, -test) %>% data.frame()

colours = c('#40b101', '#b1eb36', '#ffe100', '#ffa200', '#ff4a36')




# User Interface -------
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           reactableOutput("table", height = 120),
           div(style = 'padding:20px;')
    )
  ),
  fluidRow(
    column(width = 12,
           plotlyOutput("plot2", height = 400)
    )
  )
)




# Server ---------


risk_df = predictions %>% group_by(day) %>% mutate(risk = High + Very_High) %>%
  select(day, risk, Date) %>% mutate(`Pollen Risk` = case_when(
    risk >= 0.5 ~ "High",
    risk >= 0.3 & risk < 0.5 ~ 'Moderate',
    risk < 0.3 ~ 'Low'
  )) %>% select(-risk) %>% t() %>% data.frame()

names(risk_df) = predictions$day
risk_df = risk_df[-1,]

server <- function(input, output){
  
  output$table <- renderReactable(
    reactable(
      risk_df,
      columns = list(
        Monday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Tuesday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Wednesday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Thursday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Friday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Saturday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Sunday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        )
      )
    ))
  
  output$plot2 = renderPlotly({
    q  = predictions %>% rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
      select(-Date) %>%
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

