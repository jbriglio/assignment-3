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
library(reshape2)

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
         ),
         textOutput("selected_var")
      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
   #print(BP2010)
   #print(head(PB2017[1:13],-1))
   PB2017 = head(PB2017[1:13],-1)
   print(PB2017)
   bothYears <- rbind(BP2010,PB2017)
   print(bothYears)
   #print(testgraph)
   #gg <- melt(bothYears)
   #print(gg)
   

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
   
   output$distPlot3 <- renderPlot({
     barplot(height = as.matrix(bothYears[bothYears$Sector == input$whatever,2:13]), beside=TRUE,
             col = c("red", "blue"), bty="n",
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
     legend(x=500,y=100, c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n")
     #print(height)
   })
   
   output$selected_var <- renderText({ 
     paste(input$whatever,"Average: ", 
           input$whatever,"Mean: ",
           input$whatever,"Max: ")
   })
   
}
# Run the application 
shinyApp(ui = ui, server = server)

