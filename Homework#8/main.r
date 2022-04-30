library(tidyverse)

mydata <- read.csv("breast-cancer-wisconsin.csv")

nrow(mydata)

benignCases <- mydata %>% filter(Class == "B")

nrow(benignCases)

malignantCases <- mydata %>% filter(Class == "M")

nrow(malignantCases)

km <- kmeans(mydata[,c(2,3,7)], centers = 2, nstart = 10)

km$cluster

table(mydata$Class, km$cluster)