data <- read.csv("~/RStudio/Math300-Haiti-Final/mental-illnesses-prevalence.csv")

haitidata <- subset(data, Entity=="Haiti")

haiti <- subset(haitidata, select = c(3, 5, 6))

colnames(haiti)[1] <- "year"
colnames(haiti)[2] <- "depressive"
colnames(haiti)[3] <- "anxiety"
