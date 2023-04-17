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

Errors <- readRDS("Errors.RDS")

Errors_test <- Errors %>% 
                     filter(Dataset == "Test") %>% 
                     select(Error,geometry)

Future_predictions <- readRDS("Future_predictions.RDS") %>% 
                                                rename(Longitude = LON,
                                                       Latitude = LAT)

Future_predictions <- Future_predictions %>% 
                                          st_join(Errors_test,
                                                  join = st_intersects,
                                                  left = TRUE)
      

Variable_Imp <- readRDS("Var_IMP_total.RDS") 

ALE <- readRDS("ALE.RDS") %>% 
               rename(Model_Type=`Model Type`)


############## 

# ui
ui <-  fluidPage(
  
  theme = bs_theme(),
  
  titlePanel("SunScope"),
  tabsetPanel(
    tabPanel("About",
             h1("Description"),
             p("This app visualizes future sunflower yield predictions for 
                177 US counties across seven states, based on four climate 
               scenarios and three machine learning algorithms. 
               
               The Future Predictions tab displays choropleth maps of predicted 
               yields. The app includes state-level models, which outperformed 
               the national model for some states. Error estimates are also 
               provided for each county. Data was sourced from USDA and NOAA.
               
               This app was developed by Sambadi Majumder, a PhD student at the
               University of Central Florida as of April 16th 2023. If you have
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
    
    tabPanel(title = "Important factors",
             fluidRow(
               column(width=6,
                      selectInput("Dataset",
                                  "Dataset",
                                  choices = unique(Variable_Imp$Dataset))),
               column(width = 6,
                      selectInput("Model_Type",
                                  "Model_Type",
                                  choices = NULL))
             ),
             plotlyOutput("Variable_Importance")
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
                 text = str_c(County, " Error: ", Error))) +
      geom_sf(colour = NA)  +
      facet_wrap(~RCP) +
      scale_fill_viridis("Yield predictions (LBs/Acre)",
                         direction = -1) +
      theme_void() 
    
      ggplotly(p1)
                              
  })   
  
  
  #### Hierarchichal dropdowns for Dataset #### 
  
  Model_choices <- reactive({
    
    Variable_Imp %>% 
      filter(Dataset == input$Dataset) %>% 
      pull(Model_Type)
  })
  
  ### update the Dataset dropdown based on model ###
  
  observe({
    updateSelectInput(session, "Model_Type",
                      choices = Model_choices())
  })
  
  output$Variable_Importance <- renderPlotly({
    
    p2 <- Variable_Imp %>% 
      filter(Dataset == input$Dataset,
             Model_Type == input$Model_Type) %>% 
      ggplot(aes(x=reorder(Features,Overall), 
                 y = Overall, 
                 fill = `Variable Type`)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("#E31A1C", "#A6CEE3",
                                   "#FDBF6F", "#B2DF8A",
                                   "#FF7F00")) +
      labs(x= "Factors", y= "Variable Importance(mean decrease of RMSE)") +
      coord_flip() + 
      theme_bw() +
      theme(text = element_text(size = 10))  
    
    ggplotly(p2)
    
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
    
    p3 <- ALE %>% 
      filter(Model_Type == input$Model_Type,
             Feature == input$Feature) %>% 
      ggplot(aes(x = Variable_values,
                 y = ALE)) + 
      geom_line(aes(color = Feature)) + 
      facet_wrap(~Dataset) +
      labs(x = "Trait Value",
           y = "Accumulated Local Effects") +
      theme(text = element_text(size=10)) +
      theme(legend.position = "None")
    
    ggplotly(p3)
    
  })
  
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)




