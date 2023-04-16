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

Future_predictions <- readRDS("Future_predictions.RDS")

Variable_Imp <- readRDS("Var_IMP_total.RDS")

ALE <- readRDS("ALE.RDS") %>% 
                         rename(Model_Type=`Model Type`)

############## 

# ui
ui <-  fluidPage(
  
  theme = bs_theme(),
  
  titlePanel("SunScope"),
  tabsetPanel(
    tabPanel(title = "Errors",
             fluidRow(
               column(width = 6,
                    selectInput("Dataset",
                         "Dataset",
                         choices = unique(Errors$Dataset))),
               column(width = 6,
                      selectInput("State",
                                  "State",
                                  choices = NULL))
                    ),
             plotlyOutput("Error_map")),
    
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
  
  ### Plotting the errors ###
  
  ### Hierarchichal dropdowns for State ##
  
  State_choices <- reactive({
    
    Errors %>% 
      filter(Dataset == input$Dataset) %>% 
      pull(State)
  })
  
  ### update the state dropdown based on dataset ###
  
  observe({
    updateSelectInput(session, "State",
                      choices = State_choices())
  })
  
  #### Maps showing errors ###
  
  output$Error_map <- renderPlotly({
    
    p1 <- Errors %>%
             filter(Dataset == input$Dataset,
                    State == input$State) %>% 
      
      ggplot(aes(fill = Error,
                 text = str_c(County, ": ",
                              Error))) +
      geom_sf(colour = NA) + 
      scale_fill_viridis("Errors (Lbs/Acre)",
                         direction = -1) +
      theme_void() 
    
    ggplotly(p1)
    
  }) 
  
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
    
    p2 <- Future_predictions %>% 
                             filter(State == input$State,
                             Timeframe == input$Timeframe) %>% 
      ggplot(aes(fill=Predictions,
                 text = str_c(County, ": ", Predictions))) +
      geom_sf(colour = NA)  +
      facet_wrap(~RCP) +
      scale_fill_viridis("Yield predictions (LB/Acre)",
                         direction = -1) +
      theme_void() 
    
      ggplotly(p2)
                              
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
    
    p3 <- Variable_Imp %>% 
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
    
    ggplotly(p3)
    
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
      labs(x = "Trait Value",
           y = "Accumulated Local Effects") +
      theme(text = element_text(size=10)) +
      theme(legend.position = "None")
    
    ggplotly(p4)
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)




