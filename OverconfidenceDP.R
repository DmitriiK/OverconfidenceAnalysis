# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(plyr)
library(RColorBrewer)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("D:/GitHub/OverconfidenceAnalysis/")


# Load the  data
RawData <- read.csv(file = "Predictions - Sheet1.csv")

# Take only outcomes that we already know
RawData<- RawData[is.na(RawData$outcome) == FALSE,]

n = dim(RawData)[1]
for (i in 1:n ) {
        if (RawData$p[i] < 50) 
        {
                RawData$p[i] = 100 - RawData$p[i]
                RawData$outcome[i] = 1 - RawData$outcome[i]
        }
}
RawData$p = RawData$p/100

# Plot p density
# qplot(p, data=RawData, geom="histogram")

# Summarise data more effectively
p = unique(RawData$p)
forPlot <- data.frame(p)
q = dim(forPlot)[1]
for (i in 1:q ) {
     forPlot$total[i] = sum(RawData$p == forPlot$p[i])
     forPlot$valid[i] = sum((RawData$p[     RawData$outcome == 1   ]) == forPlot$p[i])
}
forPlot$validPercentage = forPlot$valid/forPlot$total
forPlot$accuracy = rep("accuracy", dim(forPlot)[1])
forPlot$ideal = rep("ideal", dim(forPlot)[1])



# Plot the accuracy line

png(filename = "overconfidence_plot.png", width = 480, height = 800)
overconfidence_plot <- ggplot(forPlot, aes(x = p, y = validPercentage, color = accuracy)) +
        geom_line(size=2) + 
        scale_y_continuous(limits = c(0, 1)) +
        scale_x_continuous(limits = c(0.5, 1)) +
        geom_abline(slope = 1, intercept = 0, aes(color = ideal)) + 
        scale_color_manual(values=c( "#9999CC", "hotpink2"))
overconfidence_plot
dev.off()

