
## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("1945S5.xlsx")
library(tidyverse); yd=read.csv("yd.csv")

library(psych);describe(yd)
summary(yd); str(yd)

# regular plot
dy=matrix(NA,nrow(yd), 11); for (j in 1: nrow(yd)) {dy[j,]=as.numeric(yd[j,2:12])}
plot(dy[1, ], type='o', lty=1, lwd=1, col=1, main=expression('D-Shape Yield Curve'),
     ylab="Treasury Yield (%)",xlab="Maturity (Index)", xlim=c(0,12),ylim=c(0,18))
opt=c(2:nrow(dy)); for(i in 2:nrow(dy)){lines(dy[i,], lwd=1, type="b",lty=opt[i], col=opt[i]) }


# ggplot
maturity=rep(c(1:11), nrow(yd))
date=rep(yd$date, each=11)
dmat=yd[ ,2:12]; dmat=as.matrix(dmat); dmat=t(dmat); yields=c(dmat)
gd1=data.frame(maturity, date, yields)

library(ggplot2)
# no legend no label
g1=ggplot(gd1, aes(x=maturity, y=yields, group=date)) + theme_bw() +
  geom_line(aes(color=date))+ geom_point(aes(color=date)) + 
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
  scale_x_continuous(breaks=seq(0,12, by=1)) + 
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name="")) +
  labs(y="Treasury Yields (%)", x="Maturity (Index)", caption="Source: Biwei Chen (2022). Data: FRB H.15") 
g1

# with legend and text labels to each data point
ggplot(gd1, aes(x=maturity, y=yields, group=date)) + theme_bw() +
  geom_line(aes(color=date))+ geom_point(aes(color=date)) + geom_text(aes(label=date, color=date)) +
  theme(legend.title = element_blank(), legend.position = c(0.2,0.8), legend.direction = "horizontal", axis.title.y.right = element_text()) +
  scale_x_continuous(breaks=seq(0,12, by=1)) + 
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name="")) +
  labs(y="Treasury Yields (%)", x="Maturity (Index)", caption="Source: Biwei Chen (2022). Data: FRB H.15") 

# Line plot with multiple groups
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization

#######################################
# maturity in years

xm=c(1/12,3/12,6/12,1,2,3,5,7,10,20,30); maturity=rep(xm,nrow(yd))
date=rep(yd$date, each=11)
dmat=yd[ ,2:12]; dmat=as.matrix(dmat); dmat=t(dmat); yields=c(dmat)
gd2=data.frame(maturity, date, yields)

library(ggplot2)
g2=ggplot(gd2, aes(x=maturity, y=yields, group=date)) + theme_bw() +
  geom_line(aes(color=date))+ geom_point(aes(color=date)) + 
  theme(legend.title = element_blank(), legend.position = "none", axis.title.y.right = element_text()) +
  scale_x_continuous(breaks=seq(0,30, by=1)) +   scale_color_discrete(labels=date) +
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18, by=1), sec.axis=dup_axis(name = "")) +
  labs(y="Treasury Yields (%)", x="Maturity (Years)", caption="Source: Biwei Chen (2022). Data: FRB H.15") 
g2


# combine graphs in one plot
install.packages("ggpubr")
library(ggpubr)
ggarrange(g1, g2, ncol = 2, nrow = 1)


