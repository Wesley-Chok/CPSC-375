library(ggplot2)

data("esoph")

naCount <- sum(is.na(esoph))

print(paste("a. Does the dataset contain any NAs: ", naCount))
str(esoph)
print("What is the type of variable tobgp: Ord.factor")
cat("\n")

print("b. Does this variable contain outliers: Yes this variable contains outliers")
print("b. Do you think these values are really outliers or legitimate values?: These values seem to be legitimate values as they are not outside the scope.")


ggplot(esoph, aes(x=ncases, y=agegp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ggtitle('Visualize variable ncases') +
  theme_classic() + xlab('Number of cases')

ggplot(esoph) + geom_bar(aes(y = agegp)) + ggtitle('Visualize variable agegp')

ggplot(data=esoph, aes(x=agegp, y=alcgp, group=1)) +
  geom_line()+
  geom_point() + ggtitle('Visualize variables agegp and alcgp.')

ggplot(esoph, aes(x=alcgp, y=ncontrols)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ggtitle('Visualize variables alcgp and ncontrols') +
  theme_classic()

ggplot(esoph, aes(x=ncases , y=ncontrols)) + geom_point() + ggtitle('Visualize variables ncases and ncontrols')

ggplot(esoph, aes(x=ncases , y=ncontrols, color=alcgp)) + geom_point() + ggtitle('Visualize variables ncases and ncontrols and alcgp')
ggsave("homework#3.pdf")

print(plot)