
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(feather)

theme_set(theme_minimal() + 
              theme(
                  text = element_text(size = 12, colour = "black"),
                  plot.title = element_text(size = 16)))
                 
adj_sentiment_summary <- read_feather(path = "df_summary.feather")
episodes <- adj_sentiment_summary$episode_name
characteristics <- read_feather(path = "df_characteristics.feather")
sentiment_change <- read_feather(path = "df_sentiment_change.feather")

ui <- fluidPage(
    
    theme = shinytheme("sandstone"), 
    
    navbarPage("GDS Podcast Text Analysis",
               tabPanel("Sentiment",
                        sidebarLayout(
                            sidebarPanel(
                                pickerInput(
                                    inputId = "episode_trend",
                                    label = "Select episode to display",
                                    choices = episodes,
                                    selected = "An interview with Neil Williams"
                                ),
                                numericInput(
                                    inputId = "word_bin", 
                                    label = "Select number of word bins",
                                    value = 100, 
                                    min = 50, 
                                    max = 300, 
                                    step = 10
                                ), 
                               h5("Word bins will group the text to analyse the number of positive and negative words")
                                
                            ),
                            mainPanel(
                                plotOutput("plot_trend_both"),
                                plotOutput("plot_trend_one"), 
                                plotOutput("plot_trend_all")
                                
                            )
                        )
                        
               ),
               
               tabPanel("Summary",
                        DT::dataTableOutput("sentiment")
               ),
               tabPanel("Characteristics",
                        sidebarLayout(
                            sidebarPanel(
                                pickerInput(
                                    inputId = "episode_char",
                                    label = "Select episodes to display",
                                    choices = episodes,
                                    selected = "An interview with Neil Williams"
                                )
                                
                                
                            ),
                            mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            plotOutput("plot_char", width = "100%"), 
                            
                            )
                        )
                        
               ),
               
               tabPanel("About",
                        fluidRow(
                            column(6, includeMarkdown("gds_about.Rmd"), 
                                   img(src = "zero_zero_one_circle.png", 
                                       height = 60, width = 60)
                            )
                            
                        )
               )
               
    )
)

server <- function(input, output, session) {
    
    # Sentiment Tab
    
    sentiment_both <- reactive({
        
        sentiment_change %>%
            filter(episode_name %in% input$episode_trend) %>%
            count(episode_name, index = row_number() %/% input$word_bin, sentiment) %>%
            filter(sentiment != 0) %>% 
            arrange(index) 
        
    })
    
    output$plot_trend_both <- renderPlot({
        
        ggplot(sentiment_both(), aes(index, n, colour = sentiment)) +
            geom_line(size = 2) +
            ggtitle(label = str_c("Sentiment frequencies per ",
                                  input$word_bin,
                                  " words"), 
                    subtitle = input$episode_trend
            ) +
            theme(legend.position = "top", 
                  legend.title = element_blank(), 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(), 
                  ) +
            xlab("Duration of episode") +
            ylab("Sentiment frequencies") 
        
 
    })
    
    sentiment_one <- reactive({
        
        sentiment_change %>%
            filter(episode_name %in% input$episode_trend) %>%
            count(episode_name, index = row_number() %/% input$word_bin, sentiment) %>%
            arrange(index) %>% 
            pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
            mutate(sentiment = Positive - Negative)
        
    })
    
    output$plot_trend_one <- renderPlot({
        
        ggplot(sentiment_one(), aes(index, sentiment, fill = sentiment)) +
            geom_col()  +
             ggtitle(label = str_c("Sentiment change per ",
                                   input$word_bin,
                                  " words"), 
                    subtitle = input$episode_trend
                                    ) +
            theme(legend.position = "none", 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) +
            xlab("Duration of episode") +
            ylab("Sentiment change (Positive - Negative)") 
            
        
    })
    
    sentiment_all <- reactive({
        
        sentiment_change %>%
            count(index = row_number() %/% input$word_bin, sentiment) %>%
            arrange(index) %>% 
            pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
            mutate(sentiment = Positive - Negative)
        
    })
    
    output$plot_trend_all <- renderPlot({
        
        ggplot(sentiment_all(), aes(index, sentiment, fill = sentiment)) +
            geom_smooth(se = FALSE, 
                        colour = "#325d88", 
                        method = 'loess', 
                        formula = 'y ~ x', 
                        size = 2) +
            ggtitle(label = str_c("Sentiment change per ",
                                  input$word_bin,
                                  " words"), 
                    subtitle = "All episodes"
            ) +
            theme(legend.position = "none", 
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) +
            xlab("Duration of all episodes") +
            ylab("Sentiment change (Positive - Negative)") 
    })
    
    # Characteristics Tab
    
    output$plot_char <- renderPlot({
        
        ggplot(characteristics[characteristics$episode_name %in% input$episode_char,], aes(x = reorder(word, n), n)) +
            geom_col(fill = "#325d88") +
            coord_flip() +
            theme(legend.position = "none") +
            xlab("") +
            ylab("Word frequencies") +
            ggtitle(label = "Words characteristic of episode", 
                    subtitle = input$episode_char)
               
            
    })
    
    output$plot <- renderPlot({
        plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(cars)
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(cars)
    })
    
    # Summary Tab
    
    output$sentiment <- DT::renderDataTable({
        DT::datatable(adj_sentiment_summary, rownames = FALSE, style = "bootstrap" )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

