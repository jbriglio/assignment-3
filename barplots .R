data1 <- read.csv("BP apprehensions 2010.csv", header = TRUE, stringsAsFactors = FALSE)
rownames(data1) <- data1[,1]
View(data1)
x <- subset(data1, select= -c(Sector))
x <- rbind(x, colSums(x))
rownames(x) <- c(rownames(x)[-length(rownames(x))], "Total")
x <- cbind(x,rowSums(x))
colnames(x) <- c(colnames(x)[-length(colnames(x))], "Total")
View(x)
barplot(x[1:9, 13], names.arg = rownames(x)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2010 Border Patrol Apprehensions by Sector",
        border="black",
        col="blue")

data2 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = FALSE)
rownames(data2) <- data2[,1]
y <- subset(data2, select= -c(Sector))
y <- rbind(y, colSums(y))
rownames(y) <- c(rownames(y)[-length(rownames(y))], "Total")
y <- cbind(y, rowSums(y))
colnames(y) <- c(colnames(y)[-length(colnames(y))], "Total")
View(y)
barplot(y[1:9, 13], names.arg = rownames(y)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2017 Border Patrol Apprehensions by Sector",
        border="black",
        col="blue")