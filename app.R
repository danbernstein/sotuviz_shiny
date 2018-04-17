library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidyr)
library(htmlwidgets)
library(readr)

dfm.simil.m <- read.csv(
  "https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/similcombined_tidy041718.csv", 
  stringsAsFactors = F, header = T, check.names = F)

plot_data <- read.csv("https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/plot_data.csv")

tidy.count <- read.csv("https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/tidy_countbypresidentyear.csv")

topic.words <- read.csv("https://raw.githubusercontent.com/danbernstein/sotuviz_shiny/master/data/topicmodelling_logratio_topwordspertopic.csv")

ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "Estimated Term Frequency",
  showgrid = FALSE,
  showline = FALSE
)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Trend Analysis of State of the Union",
                         
        tabPanel("Topic Modelling",
                 sidebarLayout(
                   sidebarPanel(
                               selectizeInput(inputId = "plot_word", 
                                           label = "Choose from the most unique terms in each topic, or enter your own term", 
                                           choices = 
                                             as.list(topic.words$x),
                                           options = list(
                                             placeholder = 'Please select an option below',
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )),
                               textInput("text_input1", "",
                                           placeholder = 'Enter another word'
                                         )
                               ),
                   mainPanel(plotlyOutput(outputId = "timeseries", width = "90%"))),
                tags$hr(),
                tags$blockquote(
                  div(
                    p(HTML("Topic Modelling is an unsupervised learning technique that models each document in a corpus as a mixture of topics, and each topic as a mixture of words. 
                           At the document level, the output for each document is represented as a percent of each topic, ranging from 0 to 1. The addresses have been partitioned into three 
                           topics using the Latent Dirichlet Allocation (LDA) model, where the document is made up of a proportion of each topic, and each topic is a collection of terms, which can 
                           contribute to one or more topics. To better understand how terms can contribute to more than one topic, you can use the sidepanel to overlay the term trend usage, generated as a smoothed spline."))))
                ),
        tabPanel("Text Similarity",
                 sidebarLayout(
                  sidebarPanel(
                  radioButtons("simil_type", h4("Similarity Measures"), 
                             c("Cosine" = "Cosine Function",
                               "Correlation" = "Correlation",
                               "Jaccard" = "Jaccard"), 
                             "Cosine Function", inline = TRUE)
                  ),
              mainPanel(plotlyOutput(outputId = "plot", height = "550px"))),
       tags$hr(),
       tags$blockquote(
         div(
           p(HTML("There are a range of measurements for text similarity among the documents in a corpus. The heatmap on the right demonstrates three of the most common
                  measures (Cosine, Correlation, and Jaccard). Check out my ", 
                  paste0(a(href = 'https://danbernstein.netlify.com/post/text-mining-sotu/', 'blog post '),
                         'to better understand the differences and technical considerations when working with each.'))))
       )
      )
))


# server logic

server <- function(input, output) {
  
  word.df <- reactive({
    ok <- tidy.count %>% 
      filter(word == tolower(input$text_input1)) 
  })
  
  dropdown.word.df <- reactive({
    ok <- tidy.count %>% 
      filter(word == tolower(input$plot_word)) 
  })
  

  output$timeseries <- renderPlotly({
     if (input$text_input1 != "" & input$plot_word == "") 
       {
     overlay_word.df <- word.df()
     overlay_word.smooth <- ksmooth(overlay_word.df$year, overlay_word.df$n, "normal", bandwidth = 20)
 
         p <- plot_ly(source = "source") %>% 
         add_lines(data = overlay_word.smooth, x = ~round(x), y = ~y, yaxis = "y2", name = input$text_input1, line = list(shape = "spline")) %>% 
           layout(
             yaxis2 = ay
           )
    } else if(input$text_input1 != "" & input$plot_word != "")
        {
        overlay_word.df <- word.df()
        overlay_word.smooth <- ksmooth(overlay_word.df$year, overlay_word.df$n, "normal", bandwidth = 20)
        
        overlay_chosenword.df <- dropdown.word.df()
        overlay_chosenword.smooth <- ksmooth(overlay_chosenword.df$year, overlay_chosenword.df$n, "normal", bandwidth = 20)
        
        p <- plot_ly(source = "source") %>% 
          add_lines(data = overlay_word.smooth, x = ~round(x), y = ~y, yaxis = "y2", name = input$text_input1, line = list(shape = "spline")) %>% 
          add_lines(data = overlay_chosenword.smooth, x = ~round(x), y = ~y, yaxis = "y2", name = input$plot_word, line = list(shape = "spline")) %>% 
          layout(
            yaxis2 = ay
          )
      }
    else if(input$text_input1 == "" & input$plot_word != "")
    {
      overlay_chosenword.df <- dropdown.word.df()
      overlay_chosenword.smooth <- ksmooth(overlay_chosenword.df$year, overlay_chosenword.df$n, "normal", bandwidth = 20)
      
      p <- plot_ly(source = "source") %>% 
        add_lines(data = overlay_chosenword.smooth, x = ~round(x), y = ~y, yaxis = "y2", name = input$plot_word, line = list(shape = "spline")) %>% 
        layout(
          yaxis2 = ay
        )
    }else{
      p <- plot_ly(source = "source") 
    }

  p %>% 
    add_lines(data = plot_data, x = ~year, y = ~round(Topic_1, 3), name = "Topic 1", mode = "lines", line = list(width = 3), showlegend = F) %>% 
    add_lines(data = plot_data, x = ~year, y = ~round(Topic_2, 3), name = "Topic 2", mode = "lines", line = list(width = 3), showlegend = F) %>% 
    add_lines(data = plot_data, x = ~year, y = ~round(Topic_3, 3), name = "Topic 3",mode = "lines", line = list(width = 3), showlegend = F) %>% 
    layout(hoverlabel= list(opacity = 0.9),
           title = '',
           yaxis = list(title = 'Topic Percentage'),
           xaxis = list(title = 'Year'),
           showlegend = T,
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
           legend = list(x = 105, y = 1.05))
  
    
 })
  
## analyze the words contributing to high correlation in some pockets
  ## use tf-idf rather than tf in cosine, and others?
output$plot <- renderPlotly({
     
     eventdata <- event_data("plotly_click", source = "source")

     datapoint <- as.numeric(eventdata$x)[1]
     
     simil.plot <- 
       if(input$simil_type == 'Cosine Function'){
       dfm.simil.m %>% 
       plot_ly(
         x = ~item1_year, y = ~item2_year
       ) %>% 
       add_heatmap(
         z = ~cosine, zmin = 0, zmax = 1,
         text = ~paste0(
           item1_president, " (", Var1, ") ", "<br>",
           item2_president, " (", Var2, ") ","<br>text similarity: ", round(cosine, 3)),
         hoverinfo = "text", opacity = 0.85, showscale = F
       ) 
       } else if (input$simil_type == 'Correlation') {
         dfm.simil.m %>% 
           plot_ly(
             x = ~item1_year, y = ~item2_year
           ) %>% 
           add_heatmap(
             z = ~correlation, zmin = 0, zmax = 1,
             text = ~paste0(
               item1_president, " (", Var1, ") ", "<br>",
               item2_president, " (", Var2, ") ","<br>text similarity: ", round(correlation, 3)),
             hoverinfo = "text", opacity = 0.85, showscale = F)
       } else if (input$simil_type == 'Jaccard') {
         dfm.simil.m %>% 
           plot_ly(
             x = ~item1_year, y = ~item2_year
           ) %>% 
           add_heatmap(
             z = ~jaccard, zmin = 0, zmax = 1,
             text = ~paste0(
               item1_president, " (", Var1, ") ", "<br>",
               item2_president, " (", Var2, ") ","<br>text similarity: ", round(jaccard, 3)),
             hoverinfo = "text", opacity = 0.85, showscale = F)
       }
     simil.plot %>% 
       layout(title = paste("Text Similarity based on", input$simil_type),
              xaxis = list(title = "", nticks = 10),
              yaxis = list(title = "", autorange = "reversed"), autosize = T) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

