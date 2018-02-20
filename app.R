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
      
      # Tabs for shiny app - each displays different graph version 
      mainPanel(
         tabsetPanel(
           tabPanel("2010",plotOutput("distPlot"), "This barplot displays 2010 apphrensions by sector and month"),
           tabPanel("2017",plotOutput("distPlot2"), "This barplot displays 2017 apprensions by sector and month"),
           tabPanel("Sector Comparisons",plotOutput("distPlot3"), "This barplot compares the 2010 and 2017 CBP data by sector and month. It is 
                    apparent that like the CNN article identified, there has been a significant drop in apprehensions compared to 2010, with the exception
                    of the El Paso sector."),
           tabPanel("2010 Totals", plotOutput("distPlot4"), "This barplot identifies the summation of apprehensions in 2010 by each sector. Tuscon
                    is exceptionally high, with a total of 212,202 apprehensions, making it the sector with the most apphrensions. On the other hand, Big Bend 
                    has the least apphrensions in 2010 with 5,288"),
           tabPanel("2017 Totals", plotOutput("distPlot5"), "This barplot identifies the summation of apprehensions in 2017 by each sector. There is a change
                    in the maximum compared to 2010. This time Rio Grande Valley 
                    has the greatest total apprhensions, with 137,562 total (230% more than in 2010). While Tuscon had the max apprehensions in 2010, 
                    it has 38,657 this year, 81% less than 2010."),

           tabPanel("T Test",
                    h3("T Test Results by Month"),
                    htmlOutput("txtout")),
                
           tabPanel("Time Series", plotOutput("timeSeries"), "This time series graph represents the total yearly apprehensions from 2000-2017. The trend shows 
                    the total apprhensions slowly decreasing as the years move towards 2017.")

         )

      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
  
   #remove non-numerical data from 2017 apprhensions csv
   PB2017 = head(PB2017[1:13],-1)
   #combine 2010 and 2017 data for side by side plot
   bothYears <- rbind(BP2010,PB2017)
   
   #2010 Apprehensions by sector bar plot
   output$distPlot <- renderPlot({
      barplot(height = as.matrix(BP2010[BP2010$Sector == input$whatever, 2:13]),
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
   })
   
   #2017 Apprehensions by sector bar plot
   output$distPlot2 <- renderPlot({
     barplot(height = as.matrix(PB2017[PB2017$Sector == input$whatever,2:13]), 
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
   })
   
   #2010 and 2017 side by side bar plot comparison organized by sector
   #uses both rbind of 2017 and 2010 data 
   
   bothYears <- rbind(BP2010,PB2017)
   
   output$distPlot3 <- renderPlot({
     barplot(height = as.matrix(bothYears[bothYears$Sector == input$whatever,2:13]), beside=TRUE,
             col = c("red", "blue"), bty="n",
             main = input$whatever,
             ylab = "Number of Apprehensions",
             xlab = "Month")
     legend("topright", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n")
   })

   
   #Total Apprehnsions by Sector 2010 bar graph 
   #data taken from BP Apprehensions 2010
   rownames(BP2010) <- BP2010[,1]
   x <- subset(BP2010, select= -c(Sector))
   x <- rbind(x, colSums(x))
   rownames(x) <- c(rownames(x)[-length(rownames(x))], "Total")
   x <- cbind(x,rowSums(x))
   colnames(x) <- c(colnames(x)[-length(colnames(x))], "Total")
   print(x)
   output$distPlot4 <- renderPlot({
     barplot(x[1:9, 13], names.arg = rownames(x)[1:9], 
             las=2,
             axisnames=TRUE,
             main="2010 Total Apprehensions by Sector",
             border="black",
             col="red")
   })
   
   #Total Apprehnsions by Sector 2017 bar graph 
   #data taken from PB Apprehensions 2017
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
   
   #T Test results - output 2010 and 2017 monthly comparison t-statistic
   #Data taken from BP Apprehenions 2010 and PB Apphrensions 2017
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
           "<br>", "August 2010 vs 2017: ", test11, "<br>", "September 2010 vs 2017", test12,
           "<br>", "<br>", "By looking at the resulting T-test values, March April and May have the highest T-test values
           which matches the 2010 CBP total apprehensions data - as March, April and May have the highest total apprensions in 2010 but the lowest in 2017.
           Comparatively, October November and December have the lowest T-test values, going into the negative range. This 3 month period has the highest total
           apprensions in 2017 but is not too different from it's 2010 data, which is why the t-test values are closer to zero.")
   })
   
   #Time Series Plot
   #data taken from PB Monthly summaries
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

