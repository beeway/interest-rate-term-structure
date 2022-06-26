
## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("1953S8.xlsx")
library(tidyverse); dm=read.csv("1953S8.csv")

library(psych);describe(dm)
summary(dm); str(dm)

# Five typical shapes
2000-12 B
1980-03 D
2006-02 F
1982-07 H
2010-04 U

yk1=subset(dm,date=="2000-12");yk1=as.numeric(yk1)
yk2=subset(dm,date=="1980-03");yk2=as.numeric(yk2)
yk3=subset(dm,date=="2006-02");yk3=as.numeric(yk3)
yk4=subset(dm,date=="1982-07");yk4=as.numeric(yk4)
yk5=subset(dm,date=="2010-04");yk5=as.numeric(yk5)
yk1=yk1[3:13]; yk2=yk2[3:13]; yk3=yk3[3:13]; yk4=yk4[3:13]; yk5=yk5[3:13]

library(ggplot2)

# Line plot with multiple groups
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization

maturity=rep(c(1:11),5)
shape=rep(c("B","D","F","H","U"), each=11)
yields=c(yk1,yk2,yk3,yk4,yk5)
yc=data.frame(maturity, shape, yields)

ggplot(yc, aes(x=maturity, y=yields, group=shape)) + theme_bw() +
  geom_line(aes(color=shape))+ geom_point(aes(color=shape)) + 
  theme(legend.title = element_blank(), legend.position = c(0.1,0.87), axis.title.y.right = element_text()) +
  scale_color_discrete(labels=c("B 2000-12", "D 1980-03", "F 2006-02", "H 1982-07", "U 2010-04")) +
  scale_x_continuous(breaks=seq(0,12, by=1)) + 
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name = "")) +
  labs(y="Treasury Yields (%)", x="Maturity Index", caption="Source: Biwei Chen (2022). Data: FRB H.15") 
