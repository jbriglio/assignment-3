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

mydata <- read.csv("BP apprehensions 2010.csv")

# Define UI for application that draws a barplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Border Patrol Apprehensions in 2010"),
   
   #Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("whatever",
                     "Sector:",
                     choices = mydata$Sector)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     barplot(height = as.matrix(mydata[mydata$Sector == input$whatever, 2:13]), 
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

