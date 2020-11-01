# Shiny App with buttons

library(shinyWidgets)

# User Interface -------
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 250)
    )
  ),
  actionGroupButtons(
    inputIds = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"),
    labels = list("Monday's Distribution", "Tuesday's Distribution",
                  "Wednesday's Distribution", "Thursday's Distribution",
                  "Friday's Distribution","Saturday's Distribution","Sunday's Distribution"),
    status = "primary",
    fullwidth = T),
  fluidRow(column(height = 1, width = 12)),
  fluidRow(
    column(width = 12,
           plotOutput("plot", height = 600)
    )
  )
  # plotOutput('plot', width = 12)
)

# Server ---------

server <- function(input, output){
  
  # v <- reactiveValues(data = iris,
  #                     plot = NULL,
  #                     text = NULL)
  v <- reactiveValues(data = predictions,
                      plot = NULL )
  output$plot1 <- renderPlot({
    new %>% 
      ggplot(aes(x = day, y = location, colour = variable)) +
      geom_point(size = 50, colour = 'black') +
      geom_point(size = 49, alpha = 1) +
      geom_text(aes(label = paste0(variable, '\n', probability*100, "%")), colour = 'black', size = 6) +
      geom_text(aes(label = day), colour = 'black', nudge_y = 0.4, size = 10) +
      theme_void() +
      scale_colour_manual(values = colours) +
      theme(legend.position = 'none')
    })
  
  # observeEvent(input$Bar, {
  #   v$plot <- ggplot(v$data, aes(Species,Petal.Length)) +
  #     geom_bar(stat="identity") 
  #   v$text <- "Bar"
  # })
  observeEvent(input$Monday, {
    v$plot <-     predictions %>% 
      filter(day == "Monday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Monday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    
    v$text <- "Monday"
  })

  observeEvent(input$Tuesday, {
    v$plot <-     predictions %>% 
      filter(day == "Tuesday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Tuesday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Tuesday"
  })
  observeEvent(input$Wednesday, {
    v$plot <-     predictions %>% 
      filter(day == "Wednesday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Wednesday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Wednesday"
  })
  observeEvent(input$Thursday, {
    v$plot <-     predictions %>% 
      filter(day == "Thursday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Thursday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Thursday"
  })
  observeEvent(input$Friday, {
    v$plot <-     predictions %>% 
      filter(day == "Friday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Friday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Friday"
  })
  observeEvent(input$Saturday, {
    v$plot <-     predictions %>% 
      filter(day == "Saturday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Saturday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Saturday"
  })
  observeEvent(input$Sunday, {
    v$plot <-     predictions %>% 
      filter(day == "Sunday") %>%
      melt(id.vars = 'day') %>% 
      ggplot(., aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black') + 
      theme_minimal() + 
      scale_fill_manual(values = colours) +
      theme(legend.position = 'none') +
      ggtitle(paste("Sunday's ", "prediction distribution")) +
      xlab("") +
      ylab("Probability") +
      theme(text = element_text(size=20))
    v$text <- "Sunday"
  })
  
  output$plot <- renderPlot({
    if (is.null(v$plot)) return()
    v$plot
  })
  
  output$text <- renderText({
    
    if (is.null(v$text)) return()
    v$text
    
  })
}

# Run App ----------
shinyApp(ui, server)


  