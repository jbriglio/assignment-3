library(datasets)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Give the page a title
  titlePanel("Total Apprehensions by Location in 2010"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Location:", 
                  choices=colnames(newdata1)),
      hr(),
      helpText("Data from 2010 Boarder Patrol Information")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("apprehensionPlot")  
    )
    
  )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$apprehensionPlot <- renderPlot({
    
    # Render a barplot
    barplot(newdata1[,input$region], 
            main=input$region,
            ylab="Number Apprehensions",
            xlab="Month")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

