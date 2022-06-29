
## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("1945S5.xlsx")
library(tidyverse); dm=read.csv("1945S5.csv")

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
y1=yk1[3:13]; y2=yk2[3:13]; y3=yk3[3:13]; y4=yk4[3:13]; y5=yk5[3:13]


# Line plot with multiple groups
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization

maturity=rep(c(1:11),5)
shape=rep(c("B","D","F","H","U"), each=11)
yields=c(y1,y2,y3,y4,y5)
yc=data.frame(maturity, shape, yields)

library(ggplot2)
g1=ggplot(yc, aes(x=maturity, y=yields, group=shape)) + theme_bw() +
  geom_line(aes(color=shape))+ geom_point(aes(color=shape)) + 
  theme(legend.title = element_blank(), legend.position = c(0.1,0.88), axis.title.y.right = element_text()) +
  scale_color_discrete(labels=c("B 2000-12", "D 1980-03", "F 2006-02", "H 1982-07", "U 2010-04")) +
  scale_x_continuous(breaks=seq(0,12, by=1)) + 
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name="")) +
  labs(y="Treasury Yields (%)", x="Maturity Index", caption="Source: Biwei Chen (2022). Data: FRB H.15") 
g1

#######################################
# maturity in years
xm=c(1/12,3/12,6/12,1,2,3,5,7,10,20,30); maturity=rep(xm,5)
shape=rep(c("B","D","F","H","U"), each=11)
yields=c(y1,y2,y3,y4,y5)
yc=data.frame(maturity, shape, yields)

library(ggplot2)
g2=ggplot(yc, aes(x=maturity, y=yields, group=shape)) + theme_bw() +
  geom_line(aes(color=shape))+ geom_point(aes(color=shape)) + 
  theme(legend.title = element_blank(), legend.position = c(0.85,0.88), axis.title.y.right = element_text()) +
  scale_color_discrete(labels=c("B 2000-12", "D 1980-03", "F 2006-02", "H 1982-07", "U 2010-04")) +
  scale_x_continuous(breaks=seq(0,30, by=1)) + 
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name = "")) +
  labs(y="Treasury Yields (%)", x="Maturity (Years)", caption="Source: Biwei Chen (2022). Data: FRB H.15") 
g2

# combine graphs in one plot
install.packages("ggpubr")
library(ggpubr)
ggarrange(g1, g2, ncol = 2, nrow = 1)


