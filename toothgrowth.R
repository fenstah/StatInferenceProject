library(datasets)
library(ggplot2)

summary(ToothGrowth)

require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth, 
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

g<-ggplot(ToothGrowth, aes(x=dose, y=len, color=supp))
g<-g+geom_smooth(method="lm")
print(g)