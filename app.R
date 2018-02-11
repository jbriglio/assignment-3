#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#library(dplyr)
require(gridExtra)
library(ggplot2)

BP2010 <- read.csv("BP apprehensions 2010.csv")
PB2017 <- read.csv("PB Apprehensions 2017.csv")
PBSum <- read.csv("PB monthly summaries.csv")

# Define UI for application that draws a barplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Border Patrol Apprehensions"),
   
   #Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("whatever",
                     "Sector:",
                     choices = BP2010$Sector)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel("2010",plotOutput("distPlot")),
           tabPanel("2017",plotOutput("distPlot2")),
           tabPanel("Comparison",plotOutput("distPlot3")),
           tabPanel("Summary")
         )
      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
  
   #bothYears <- rbind(BP2010,PB2017)
   #print(bothYears)

   output$distPlot <- renderPlot({
      barplot(height = as.matrix(BP2010[BP2010$Sector == input$whatever, 2:13]),
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
   })
   
   output$distPlot2 <- renderPlot({
     barplot(height = as.matrix(PB2017[PB2017$Sector == input$whatever,2:13]), 
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
     #print(height)
     
   })
   
}
# Run the application 
shinyApp(ui = ui, server = server)

