
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(feather)

theme_set(theme_minimal() + 
              theme(
                  text = element_text(size = 12),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12),
                  plot.title = element_text(size = 16),
                  plot.subtitle = element_text(size = 12),
                  strip.text = element_text(size = 12))
)

adj_sentiment_summary <- read_feather(path = "D:/R/gds_podcast/gds_podcast_app/df.feather")
episodes <- adj_sentiment_summary$episode_name
characteristics <- read_feather(path = "D:/R/gds_podcast/gds_podcast_app/df_characteristics.feather")
sentiment_change <- read_feather(path = "D:/R/gds_podcast/gds_podcast_app/df_sentiment_change.feather")

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
                                    value = 30, 
                                    min = 10, 
                                    max = 100, 
                                    step = 10
                                )
                                
                            ),
                            mainPanel(
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
                                    multiple = TRUE,
                                    choices = episodes,
                                    selected = "An interview with Neil Williams",
                                    options = list(`actions-box` = TRUE)
                                )
                                
                                
                            ),
                            mainPanel(  tags$style(type="text/css",
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
    
    sentiment_one <- reactive({
        
        sentiment_change %>%
            filter(episode_name %in% input$episode_trend) %>%
            count(episode_name, index = row_number() %/% input$word_bin, sentiment) %>%
            arrange(index) %>% 
            pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
            mutate(sentiment = positive - negative)
        
    })
    
    output$plot_trend_one <- renderPlot({
        
        ggplot(sentiment_one(), aes(index, sentiment, fill = sentiment)) +
            geom_col() +
            ggtitle(label = str_c("Sentiment change of ",
                                  input$episode_trend,
                                  " per ",
                                  input$word_bin,
                                  " words")) +
            theme(legend.position = "none") +
            xlab("") +
            ylab("")
        
    })
    
    sentiment_all <- reactive({
        
        sentiment_change %>%
            count(index = row_number() %/% input$word_bin, sentiment) %>%
            arrange(index) %>% 
            pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
            mutate(sentiment = positive - negative)
        
    })
    
    output$plot_trend_all <- renderPlot({
        
        ggplot(sentiment_all(), aes(index, sentiment, fill = sentiment)) +
            geom_smooth(se = FALSE, colour = "#325d88", method = 'loess', formula = 'y ~ x') +
            ggtitle(label = str_c("All episodes sentiment change per ", input$word_bin, " words")) +
            ggtitle(label = str_c("Sentiment change of all episodes per ",
                                  input$word_bin,
                                  " words")) +
            theme(legend.position = "none") +
            xlab("") +
            ylab("")
    })
    
    # Characteristics Tab
    
    output$plot_char <- renderPlot({
        
        ggplot(characteristics[characteristics$episode_name %in% input$episode_char,], aes(x = reorder(word, n), n)) +
            geom_col(fill = "#325d88") +
            coord_flip() +
            theme(legend.position = "none") +
            xlab("") +
            ylab("Word Count") +
            ggtitle(label = "Words characteristic of episodes") +
            facet_wrap(~episode_name, ncol = 3, scales = "free_y")
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

