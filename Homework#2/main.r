library(ggplot2)

data("mpg")

ggplot(mpg, aes(x=displ, y=cty)) + geom_point()

p <- ggplot(mpg, aes(x=displ, y=cty)) + geom_point() +
  xlim(0, 10) + ylim(0, 40)
p+ggtitle('Scatterplot of City Gas Economy vs Displacement') +xlab('Displacement (L)') + ylab('City Gas Economy (MPH)')

p <- ggplot(mpg, aes(x=displ, y=cty, color=as.factor(year))) + geom_point() +
  xlim(0, 10) + ylim(0, 40)
p+ggtitle('Scatterplot of City Gas Economy vs Displacement') +xlab('Displacement (L)') + ylab('City Gas Economy (MPH)')


p <- ggplot(mpg, aes(x=displ, y=cty, color=as.factor(year))) + geom_point() +
  xlim(0, 10) + ylim(0, 40) +
  facet_wrap(~class)
p+ggtitle('Scatterplot of City Gas Economy vs Displacement') +xlab('Displacement (L)') + ylab('City Gas Economy (MPH)')


ggplot(mpg, aes(x=class)) + geom_bar()

ggsave("homework#2.pdf")

print(plot)