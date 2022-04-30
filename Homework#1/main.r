library(dplyr)
library(plyr)

data("iris")

rowNum <- nrow(iris)
colNum <- ncol(iris)
rowHead <- head(iris,10)
sepalLen <- iris$Sepal.Length
sepalLenMean <- mean(iris$Sepal.Length)
sepalLenRow <- subset(iris,Sepal.Length > 7.6)
sepalLenRowIndex <- which(iris > 7.6)
setosaRow <- iris[grep("setosa", iris$Species), ]
setosaRowBoth <- filter(iris, Sepal.Length > 7.6 & Species == "setosa")
sepalMaxLength <- ddply(iris, ~Species, summarise, max=max(Sepal.Length))
sepalRowIndex <- which(iris >= 7.8)



print(paste("a. Get number of rows (Hint: nrow):", rowNum))
cat("\n")
print(paste("b. Get number of columns (Hint: ncol):", colNum))
cat("\n")
print("c. Show first 10 rows:")
rowHead
cat("\n")
print("d. Show column Sepal.Length:")
sepalLen
cat("\n")
print(paste("e. Calculate the mean Sepal.Length?:", sepalLenMean))
cat("\n")
print("f. Show all rows where Sepal.Length > 7.6:")
sepalLenRow
cat("\n")
print("fi. What are the row indexes where Sepal.Length > 7.6? (Hint: which):" )
sepalLenRowIndex
cat("\n")
print("g. Show all rows where Species is setosa:")
head(setosaRow)
cat("\n")
print("h. Show all rows where Sepal.Length > 3.0 and Species is setosa:")
setosaRowBoth
cat("\n")
print("i. Get the largest value of Sepal.Length:")
sepalMaxLength
cat("\n")
print(paste("ii. Get the row index that contains this value:", sepalRowIndex))
cat("\n")
print("j. What Species corresponds to this largest Sepal.Length?: virginica")


