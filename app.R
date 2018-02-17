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
           tabPanel("Comparison By Sectors",plotOutput("distPlot3")),
           tabPanel("2010 Totals", plotOutput("distPlot4")),
           tabPanel("2017 Totals", plotOutput("distPlot5")),
           tabPanel("T Test"),
           tabPanel("Monthly Summary", plotOutput("timeSeries"))
         )
         # textOutput("selected_var")
      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
   #print(BP2010)
   #print(head(PB2017[1:13],-1))
   PB2017 = head(PB2017[1:13],-1)
   #print(PB2017)
   bothYears <- rbind(BP2010,PB2017)
   #print(bothYears)
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

   
   rownames(BP2010) <- BP2010[,1]
   View(BP2010)
   x <- subset(BP2010, select= -c(Sector))
   x <- rbind(x, colSums(x))
   rownames(x) <- c(rownames(x)[-length(rownames(x))], "Total")
   x <- cbind(x,rowSums(x))
   colnames(x) <- c(colnames(x)[-length(colnames(x))], "Total")
   View(x)
   output$distPlot4 <- renderPlot({
     barplot(x[1:9, 13], names.arg = rownames(x)[1:9], 
             las=2,
             axisnames=TRUE,
             main="2010 Border Patrol Apprehensions by Sector",
             border="black",
             col="blue")
   })
   
   rownames(PB2017) <- PB2017[,1]
   y <- subset(PB2017, select= -c(Sector))
   y <- rbind(y, colSums(y))
   rownames(y) <- c(rownames(y)[-length(rownames(y))], "Total")
   y <- cbind(y, rowSums(y))
   colnames(y) <- c(colnames(y)[-length(colnames(y))], "Total")
   View(y)
   output$distPlot5 <- renderPlot({
     barplot(y[1:9, 13], names.arg = rownames(y)[1:9], 
             las=2,
             axisnames=TRUE,
             main="2017 Border Patrol Apprehensions by Sector",
             border="black",
             col="blue")  
   })
   
   

   
   # output$selected_var <- renderText({ 
   #   paste(input$whatever,"Average: ", 
   #         input$whatever,"Mean: ",
   #         input$whatever,"Max: ")
   # })
   
   output$timeSeries <- renderPlot({
     ts.plot(ts(as.vector(rev(t(rev(PBSum[-1])))), start = c(2000,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[18,-1])),each=12)  )  )), start = c(2000,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[17,-1])),each=12)  )  )), start = c(2001,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[16,-1])),each=12)  )  )), start = c(2002,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[15,-1])),each=12)  )  )), start = c(2003,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[14,-1])),each=12)  )  )), start = c(2004,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[13,-1])),each=12)  )  )), start = c(2005,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[12,-1])),each=12)  )  )), start = c(2006,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[11,-1])),each=12)  )  )), start = c(2007,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[10,-1])),each=12)  )  )), start = c(2008,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[9,-1])),each=12)  )  )), start = c(2009,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[8,-1])),each=12)  )  )), start = c(2010,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[7,-1])),each=12)  )  )), start = c(2011,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[6,-1])),each=12)  )  )), start = c(2012,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[5,-1])),each=12)  )  )), start = c(2013,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[4,-1])),each=12)  )  )), start = c(2014,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[3,-1])),each=12)  )  )), start = c(2015,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[2,-1])),each=12)  )  )), start = c(2016,10), frequency=12),
             ts(as.vector(  t(  rev(rep(c(rowMeans(PBSum[1,-1])),each=12)  )  )), start = c(2017,10), frequency=12),
             
             gpars=list(xlab="Year", ylab="Apprehensions", 
                        lty=1, col=c('blue',rep(c('red'),times=18), ltw=2))
      );text(rev(PBSum[,1])+1,rev(c(rowMeans(PBSum[-1])))+4000,labels=paste(rev(PBSum[,1])),cex=0.6,col="red")
     })
}
# Run the application 
shinyApp(ui = ui, server = server)

