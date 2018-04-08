library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidyr)
library(htmlwidgets)
library(readr)

dfm.simil.m <- read.csv(
  "https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/similcombined_tidy.csv", 
  stringsAsFactors = F, header = T, check.names = F)

plot_data <- read.csv("https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/plot_data.csv")


# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Trend Analysis of State of the Union Addresses",
    # Set theme
    #theme = shinytheme("spacelab"),
#   h2("Trend Analysis of State of the Union Addresses"),
#   h4("Investigating linguistic changes from Washington to Obama using text mining tools"),
#   
#   # Vertical space
#   tags$hr(),
    
        tabPanel("Topic Modelling",
                 sidebarLayout(
                   sidebarPanel("Topic Modelling is an unsupervised learning technique that models each document in a corpus as a mixture of topics, and each topic as a mixture of words. 
                                At the document level, the output for each document is represented as a percent of each topic, ranging from 0 to 1."),
                mainPanel(plotlyOutput(outputId = "timeseries", width = "100%")))
        ),
        tabPanel("Text Similarity",
                 sidebarLayout(
                  sidebarPanel(
                  radioButtons("simil_type", h4("Similarity Measures"), 
                             c("Cosine" = "Cosine Function",
                               "Correlation" = "Correlation",
                               "Jaccard" = "Jaccard"), 
                             "Cosine Function", inline = TRUE),
                  verbatimTextOutput(outputId = "heatmap_topvalues")
                  ),
              mainPanel(plotlyOutput(outputId = "plot", height = "550px"))),
       tags$hr(),
       tags$blockquote(
         div(
           p(HTML("There are a range of measurements for text similarity among the documents in a corpus. The heatmap on the right demonstrates three of the most common
                  measures (Cosine Function, Correlation, and Jaccard). Check out my ", 
                  paste0(a(href = 'https://danbernstein.netlify.com/post/text-mining-sotu/', 'blog post '),
                         'to better understand the differences and technical considerations when working with each.'))))
       )
      )
))




# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$timeseries <- renderPlotly({
     p <- plot_ly(source = "source") %>% 
         add_lines(data = plot_data, x = ~year, y = ~round(Topic_1, 3), name = "Topic 1", mode = "lines", line = list(width = 3)) %>% 
         add_lines(data = plot_data, x = ~year, y = ~round(Topic_2, 3), name = "Topic 2", mode = "lines", line = list(width = 3)) %>% 
         add_lines(data = plot_data, x = ~year, y = ~round(Topic_3, 3), name = "Topic 3",mode = "lines", line = list(width = 3)) %>% 
       layout(hoverlabel= list(opacity = 0.9),
              title = '',
              yaxis = list(title = 'Topic Percentage'),
              xaxis = list(title = 'Year'),
              showlegend = F)
     
     p
   })
   
   heatmap.df <- reactive({
     switch(input$simil_type,
            "Cosine Function" = dfm.simil.m,
            "Correlation" = dfm.simil.m)
   })
   
   output$plot <- renderPlotly({
     
     eventdata <- event_data("plotly_click", source = "source")

     datapoint <- as.numeric(eventdata$x)[1]
     
     simil.plot <- 
       if(input$simil_type == 'Cosine Function'){
       dfm.simil.m %>% 
       plot_ly(
         x = ~Var1, y = ~Var2
       ) %>% 
       add_heatmap(
         z = ~similarity_cosine, zmin = 0, zmax = 1,
         text = ~paste0(
           president.x, " (", Var1, ") ", "<br>",
           president.y, " (", Var2, ") ","<br>text similarity: ", round(similarity_cosine, 3)),
         hoverinfo = "text", opacity = 0.85, showscale = F
       ) 
       } else if (input$simil_type == 'Correlation') {
         dfm.simil.m %>% 
           plot_ly(
             x = ~Var1, y = ~Var2
           ) %>% 
           add_heatmap(
             z = ~similarity_correlation, zmin = 0, zmax = 1,
             text = ~paste0(
               president.x, " (", Var1, ") ", "<br>",
               president.y, " (", Var2, ") ","<br>text similarity: ", round(similarity_correlation, 3)),
             hoverinfo = "text", opacity = 0.85, showscale = F)
       } else if (input$simil_type == 'Jaccard') {
         dfm.simil.m %>% 
           plot_ly(
             x = ~Var1, y = ~Var2
           ) %>% 
           add_heatmap(
             z = ~similarity_jaccard, zmin = 0, zmax = 1,
             text = ~paste0(
               president.x, " (", Var1, ") ", "<br>",
               president.y, " (", Var2, ") ","<br>text similarity: ", round(similarity_correlation, 3)),
             hoverinfo = "text", opacity = 0.85, showscale = F)
       }
     simil.plot %>% 
       layout(title = paste("Text Similarity based on", input$simil_type),
              xaxis = list(title = "", nticks = 10),
              yaxis = list(title = "", autorange = "reversed"), autosize = T) 
   })
   output$heatmap_topvalues <- renderText({
     
     eventdata <- event_data("plotly_click", source = "source")
     validate(need(!is.null(eventdata), "Hover over the heatmap to see the president comparisons"))
     
     datapoint <- as.numeric(eventdata$x)[1]
     
     datapoint
   })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
