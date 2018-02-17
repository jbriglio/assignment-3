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
           tabPanel("Sector Comparisons",plotOutput("distPlot3")),
           tabPanel("2010 Totals", plotOutput("distPlot4")),
           tabPanel("2017 Totals", plotOutput("distPlot5")),
<<<<<<< HEAD
           tabPanel("T Test",
                    h3("T Test Results by Month"),
                    htmlOutput("txtout")),
                
           tabPanel("Time Series", plotOutput("timeSeries"))
=======
           tabPanel("T Test"),
           tabPanel("Monthly Summary", plotOutput("timeSeries"))
>>>>>>> origin/master
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
     legend("topright", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n")
   })

   
   #2010 total apprhensions by sector 
   rownames(BP2010) <- BP2010[,1]
   #View(BP2010)
   x <- subset(BP2010, select= -c(Sector))
   x <- rbind(x, colSums(x))
   rownames(x) <- c(rownames(x)[-length(rownames(x))], "Total")
   x <- cbind(x,rowSums(x))
   colnames(x) <- c(colnames(x)[-length(colnames(x))], "Total")
   #View(x)
   output$distPlot4 <- renderPlot({
     barplot(x[1:9, 13], names.arg = rownames(x)[1:9], 
             las=2,
             axisnames=TRUE,
             main="2010 Total Apprehensions by Sector",
             border="black",
             col="blue")
   })
   
   #2017 total apprehensions by sector 
   rownames(PB2017) <- PB2017[,1]
   y <- subset(PB2017, select= -c(Sector))
   y <- rbind(y, colSums(y))
   rownames(y) <- c(rownames(y)[-length(rownames(y))], "Total")
   y <- cbind(y, rowSums(y))
   colnames(y) <- c(colnames(y)[-length(colnames(y))], "Total")
   print(y)
   output$distPlot5 <- renderPlot({
     barplot(y[1:9, 13], names.arg = rownames(y)[1:9], 
             las=2,
             axisnames=TRUE,
             main="2017 Total Apprehensions by Sector",
             border="black",
             col="blue")  
   })
   
   output$txtout <- renderText({
     test1 = (t.test(BP2010$October, PB2017$October, paired=TRUE))$statistic
     test2 = (t.test(BP2010$November, PB2017$November, paired=TRUE))$statistic
     test3 = (t.test(BP2010$December, PB2017$December, paired=TRUE))$statistic
     test4 = (t.test(BP2010$January, PB2017$January, paired=TRUE))$statistic
     test5 = (t.test(BP2010$February, PB2017$February, paired=TRUE))$statistic
     test6 = (t.test(BP2010$March, PB2017$March, paired=TRUE))$statistic
     test7 = (t.test(BP2010$April, PB2017$April, paired=TRUE))$statistic
     test8 = (t.test(BP2010$May, PB2017$May, paired=TRUE))$statistic
     test9 = (t.test(BP2010$June, PB2017$June, paired=TRUE))$statistic
     test10 = (t.test(BP2010$July, PB2017$July, paired=TRUE))$statistic
     test11 = (t.test(BP2010$August, PB2017$August, paired=TRUE))$statistic
     test12 = (t.test(BP2010$September, PB2017$September, paired=TRUE))$statistic
     paste("October 2010 vs 2017: ",test1, "<br>", "November 2010 vs 2017: ",test2, 
           "<br>", "December 2010 vs 2017: ",test3, "<br>","January 2010 vs 2017: ", test4,
           "<br>", "February 2010 vs 2017: ", test5, "<br>", "March 2010 vs 2017: ", test6,
           "<br>", "April 2010 vs 2017: ", test7, "<br>", "May 2010 vs 2017: ", test8, 
           "<br>", "June 2010 vs 2017: ", test9, "<br>", "July 2010 vs 2017: ", test10,
           "<br>", "August 2010 vs 2017: ", test11, "<br>", "September 2010 vs 2017", test12)
     #cat("1st line\n2nd line\n") 
   })
   
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

