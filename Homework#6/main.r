library(ggplot2)
library(tidyverse)
library(modelr)

Bodyfat <- read_csv("Bodyfat.csv")

ggplot(data=Bodyfat)+geom_point(mapping=aes(y=bodyfat,x=Height)) + 
  ggtitle('a. Plot bodyfat vs. Height')
ggsave("homework#6.1.pdf")

BodyfatRow <- Bodyfat[-c(42),]
ggplot(data=BodyfatRow)+geom_point(mapping=aes(y=bodyfat,x=Height)) + 
  ggtitle('b. Remove the corresponding row from the data')
ggsave("homework#6.2.pdf")

m <- lm(data=Bodyfat, formula=bodyfat~Height)
summary(m)

cf <- coef(m)

ggplot(data=Bodyfat)+geom_point(mapping=aes(y=bodyfat,x=Weight)) + 
  ggtitle('a. Plot bodyfat vs. Weight')
ggsave("homework#6.3.pdf")

m1 <- lm(data=Bodyfat, formula=bodyfat~Weight)
summary(m1)

ggplot(data = Bodyfat, aes(y = bodyfat,x = Weight)) +
  geom_point(aes(color = "red")) +
  geom_smooth(method = "lm") +
  labs(title = "bodyfat vs Weight best line of it",)
ggsave("homework#6.4.pdf")

hello <- residuals(m1)
ggplot(data = Bodyfat) + geom_histogram(aes(x = hello))
ggsave("homework#6.5.pdf")

new.dat <- data.frame(Weight=150)
predict(m1, newdata = new.dat, interval = 'confidence', level=0.99)

new.dat <- data.frame(Weight=300)
predict(m1, newdata = new.dat, interval = 'confidence', level=0.99)

m2 <- lm(data=Bodyfat, formula=bodyfat~Weight+Height)
summary(m2)

ggplot(data=BodyfatRow)+geom_point(mapping=aes(y=Weight,x=Height)) + 
  ggtitle('e.2 Bodyfat vs Weight and Height')

new.dat <- data.frame(Weight=150, Height=70)
predict(m2, newdata = new.dat, interval = 'confidence', level=0.99)

new.dat <- data.frame(Weight=300, Height=70)
predict(m2, newdata = new.dat, interval = 'confidence', level=0.99)
ggsave("homework#6.6.pdf")

Bodyfat.copy <- Bodyfat %>% mutate(BMI = Weight/Height^2)

m3 <- lm(data=Bodyfat.copy, formula=bodyfat~BMI)
summary(m3)

ggplot(data=Bodyfat.copy)+geom_point(mapping=aes(y=bodyfat,x=BMI)) + 
  ggtitle('e.2 Bodyfat VS BMI')
ggsave("homework#6.7.pdf")

ggplot(data = Bodyfat.copy, aes(y = bodyfat,x = BMI)) +
  geom_point(aes(color = "red")) +
  geom_smooth(method = "lm") +
  labs(title = "Bodyfat VS BMI best line of it",)
ggsave("homework#6.8.pdf")

new.dat <- data.frame(Weight=150, Height=70)
predict(m2, newdata = new.dat, interval = 'confidence', level=0.99)

new.dat <- data.frame(Weight=300, Height=70)
predict(m2, newdata = new.dat, interval = 'confidence', level=0.99)

Bodyfat.copy <- Bodyfat %>% mutate(cut(Age, breaks = c(-Inf,40,60,Inf))) %>% mutate(BMI = Weight/Height^2)

m4 <- lm(data=Bodyfat.copy, formula=BMI~breaks)
summary(m4)