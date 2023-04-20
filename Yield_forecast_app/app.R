#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(viridis)
library(plotly)
library(sf)
library(bslib)

##### Loading in the datasets ####

Future_predictions <- readRDS("Future_predictions.RDS") %>% 
                                           rename(Longitude = LON,
                                                  Latitude = LAT)

ALE <- readRDS("ALE.RDS") %>% 
                         rename(Model_Type=`Model Type`)


############## 

# ui
ui <-  fluidPage(
  
  theme = bs_theme(),
  
  titlePanel("SunScope-Prototype 1"),
  tabsetPanel(
    tabPanel("About",
             h1("Description"),
             p("This app visualizes future sunflower yield predictions for 
                185 US counties across seven states, based on four climate 
               scenarios and three machine learning algorithms. 
               
               The Future Predictions tab displays choropleth maps of predicted 
               yields while the Historical trends tab show how each important 
               factor impact Yield. Train refers to data that was used to train
               the predictive models and contains data from all 186 counties 
               before 2005 while Test includes data after 2005. 
               The app includes state-level models, which outperformed the national 
               model for some states. Error estimates are also provided for each 
               county. Data was sourced from USDA and NOAA.
               
               Please feel free to play around with the theme of the app. My 
               favourite is the Superhero :) 
               
               This app was developed by Sambadi Majumder, a PhD candidate at the
               University of Central Florida as of April 20th 2023. If you have
               any questions about the app or would like to have access to the data,
               please feel free to drop an email to sambadimajumder@gmail.com
               
               The code used to create this app can found in this repository,
               link: https://github.com/SamMajumder/Yield_forecast_Sunflowers_Shiny"),
          ),
    
    tabPanel(title = "Future Predictions",
             fluidRow(
             column(width = 6,
               selectInput("State",
                         "State",
                         choices = unique(Future_predictions$State))),
             column(width = 6,
                    selectInput("Timeframe",
                                "Timeframe",
                                choices = NULL))
             ),
             plotlyOutput("Future_Predictions_map")
            ), 
    
    tabPanel(title = "Historical trends",
             fluidRow(
               column(width = 6,
                      selectInput("Model_Type",
                                  "ModelType",
                                  choices = unique(ALE$Model_Type))),
               column(width = 6,
                      selectInput("Feature",
                                  "Feature",
                                  choices = NULL))
             ),
             
             plotlyOutput("ALE_plots"))
    )
  )

  

# server
server <- function(input, output, session) {
  
  bs_themer()
  
  
  ###### Hierarchical dropdowns for future predictions ###
  
  ## Hierarchical drop downs for Timeframe 
  
  Timeframe_choices <- reactive({
    
    Future_predictions %>% 
      filter(State == input$State) %>% 
      pull(Timeframe)
  })
  
  ### Update Timeframe dropdown based on selected State ###
  
  observe({
    updateSelectInput(session, "Timeframe",
                      choices = Timeframe_choices())
  }) 
  
  ### Map showing future predictions across 4 RCPs ##
  
  output$Future_Predictions_map <- renderPlotly({
    
    p1 <- Future_predictions %>% 
                             filter(State == input$State,
                             Timeframe == input$Timeframe) %>% 
      ggplot(aes(fill=Predictions,
                 text = str_c(County, " Avg Error: ", Mean_Error))) +
      geom_sf(colour = NA)  +
      facet_wrap(~RCP) +
      scale_fill_viridis("Yield predictions (LBs/Acre)",
                         direction = -1) +
      theme_void() 
    
      ggplotly(p1)
                              
  })    
  
  
  ##### Hierarchichal dropdowns for ALE plots ### 
  
  ### Hierarchichal dropdowns for Variables ##
  
  Feature_choices <- reactive({
    
    ALE %>% 
      filter(Model_Type == input$Model_Type) %>%
      pull(Feature)
  })
  
  ### update the state dropdown based on dataset ###
  
  observe({
    updateSelectInput(session, "Feature",
                      choices = Feature_choices())
  }) 
  
  ### THE ALE PLOT ###
  
  output$ALE_plots <- renderPlotly({
    
    p4 <- ALE %>% 
      filter(Model_Type == input$Model_Type,
             Feature == input$Feature) %>% 
      ggplot(aes(x = Variable_values,
                 y = ALE)) + 
      geom_line(aes(color = Feature)) + 
      facet_wrap(~Dataset) +
      labs(x = "Factor Value",
           y = "Accumulated Local Effects") +
      theme(text = element_text(size=10)) +
      theme(legend.position = "None")
    
    ggplotly(p4)
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)




