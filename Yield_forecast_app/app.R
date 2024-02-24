

library(shiny)
library(tidyverse)
library(ggplot2)
library(viridis)
library(plotly)
library(sf)
library(bslib)
library(leaflet)


##### Loading in the datasets ####

Future_predictions <- readRDS("Future_predictions.RDS") %>% 
                                           rename(Longitude = LON,
                                                  Latitude = LAT)

# Transform the CRS to EPSG:4326
Future_predictions <- st_transform(Future_predictions, crs = 4326)


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
                                  choices = NULL)),
               column(width = 4,
                      selectInput("RCP",
                                  "RCP",
                                  choices = NULL))
             ),
             leafletOutput("Future_Predictions_map") # Changed this line
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
  
  
  ## Reactive expression for RCP choices
  RCP_choices <- reactive({
    unique(Future_predictions$RCP[Future_predictions$State == input$State])
  })
  
  ### Update Timeframe dropdown based on selected State ###
  
  observe({
    updateSelectInput(session, "Timeframe",
                      choices = Timeframe_choices())
  })  
  
  ## Update RCP dropdown based on selected State
  observe({
    updateSelectInput(session, "RCP",
                      choices = RCP_choices())
  })
  
  
  ## Hierarchical drop downs for Timeframe - Now also reactive to RCP choice
  Timeframe_choices <- reactive({
    Future_predictions %>% 
      filter(State == input$State, RCP == input$RCP) %>% 
      pull(Timeframe)
  })
  
  ### Map showing future predictions across 4 RCPs ##
  
  ### Map showing future predictions across 4 RCPs using Leaflet ##
  
  output$Future_Predictions_map <- renderLeaflet({
    
    # Filter the data based on the input selections and remove NA values
    data <- Future_predictions %>% 
      filter(State == input$State,
             Timeframe == input$Timeframe,
             RCP == input$RCP) %>%
      na.omit()
    
    # Create labels for the map
    labels <- sprintf(
      "<strong>%s</strong><br/>Predictions: %s<br/>Avg Error: %s",
      data$County, data$Predictions, data$Average_Error
    ) %>% lapply(htmltools::HTML)
    
    # Set bins for the legend
    bins <- quantile(data$Predictions, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
    pal <- colorBin("YlOrRd", domain = data$Predictions, bins = bins)
    
    # Create the leaflet map
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -96.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(Predictions),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 5,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Predictions,
        title = "Yield Predictions (LBs/Acre)",
        labels = bins, # Set the labels for the legend using the bins
        opacity = 1
      )
                              
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
      labs(x = "Predictor Value",
           y = "Accumulated Local Effects") +
      theme(text = element_text(size=10)) +
      theme(legend.position = "None")
    
    ggplotly(p4)
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)




