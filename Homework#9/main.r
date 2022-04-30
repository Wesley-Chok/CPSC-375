library(tidyverse)

load("myvec.RData")

myvec

myvecACF <- acf(myvec, lag.max=498)

myvecACF

myvects <- ts(myvec, frequency = 55)

myvec.ts <- plot(decompose(myvects))

dtw <- function (A, B) {
  M <- nrow(A)
  N <- nrow(B)
  Cost <- matrix(0,M,N) # Initialize with zeros
  for (i in 1:M) {
    for (j in 1:N) {
      Cost[i,j] <- as.numeric((A[i,1] - B[j,1])^2 + (A[i,2] - B[j,2])^2) # distance function
    }
  }
  C <- matrix(0,M,N) # Initialize with zeros
  C[1,1] <- Cost[1,1] # Initialize top left cell
  for (i in 2:M) { # Initialize first column
    C[i,1] <- C[i-1,1] + Cost[i,1]
  }
  for (j in 2:N) { # Initialize first row
    C[1,j] <- C[1,j-1] + Cost[1,j]
  }
  #
  # Complete the main loop
  #
  for(i in 2:M) {
    for(j in 2:N) {
      C[i,j] = min(C[i-1,j], C[i,j-1], C[i-1,j-1]) + Cost[i,j]
    }
  }
  return (C[M,N])
}

A <- tibble("x" = c(2, 0, 2, 4), "y" = c(2, 4, 6, 5))
B <- tibble("x" = c(1, 0, 4), "y"=c(1,6,4))

dtwFunc <- dtw(A, B)

dtwFunc

ts2 <- read_csv("ts2.csv")
ts3 <- read_csv("ts3.csv")
ts4 <- read_csv("ts4.csv")
ts5 <- read_csv("ts5.csv")
tsX <- read_csv("tsX.csv")

ts2Plot <- ggplot(ts2, aes(x=x,y=y))
ts2Plot + geom_path()
ggsave("ts2.pdf")

ts3Plot <- ggplot(ts3, aes(x=x,y=y))
ts3Plot + geom_path()
ggsave("ts3.pdf")

ts4Plot <- ggplot(ts4, aes(x=x,y=y))
ts4Plot + geom_path()
ggsave("ts4.pdf")

ts5Plot <- ggplot(ts5, aes(x=x,y=y))
ts5Plot + geom_path()
ggsave("ts5.pdf")

tsXPlot <- ggplot(tsX, aes(x=x,y=y))
tsXPlot + geom_path()
ggsave("tsX.pdf")

