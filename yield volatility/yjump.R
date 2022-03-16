
# pick the highest and lowest jumps in dys  http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

library(ggplot2)
library(readxl)

dm=read_excel("1920spreads.xlsx")

attach(dm)
dys=diff(ys); dym=diff(ym); dyl=diff(yl)
detach(dm)
head(ys); head(dys)

ndm=data.frame(dys, dym, dyl); ndm$date=date[2:1203]

View(ndm[which(ndm$date=="1980-05"),])

# save ndm to check the corresponding shape
write.csv(ndm, file="dyields.csv")

########## diverging dot plot for dys
ndys=ndm[c(1,4)]; mean(dys)
maxdys=ndys[order(dys, decreasing=T), ]
mindys=ndys[order(dys, decreasing=F), ]

# x=c(30:1)
xlab=c(maxdys[1:15, 2], mindys[1:15, 2]) #xlab=factor(c(maxdys[1:15, 2], mindys[1:15, 2]), levels=c(maxdys[1:15, 1], mindys[1:15, 1]))
y=round(c(maxdys[1:15,1], mindys[1:15,1]),2);  # y=round(sort(y, decreasing = T), 2)
z=c(rep("max", 15), rep("min",15))
d1=data.frame(xlab,y,z)

ggplot(d1, aes(xlab, y, label=y)) + geom_point(stat='identity', aes(col=z), size=8) + geom_text(color="white", size=2) +
ylim(-4, 2) + coord_flip() + theme_bw()+ theme(legend.position = "none") + labs(x="", y="") +
  geom_hline(aes(yintercept = mean(dys)), size = 1, color = "gray50", linetype = "dashed") 

# in descending order but the dating is wrong
d1=d1[order(d1$y, decreasing = T),]; d1$x=c(30:1)
ggplot(d1, aes(x, y, label=y)) + geom_point(stat='identity', aes(col=z), size=8) + geom_text(color="white", size=2) +
  ylim(-4, 2) + coord_flip() + theme_bw()+ theme(legend.position = "none") + labs(x="", y="") +
  geom_hline(aes(yintercept = mean(dys)), size = 1, color = "gray50", linetype = "dashed") +
  scale_x_discrete(limits=c(d2$xlab))

revxlab=rev(d1$xlab) # reverse the date
g1=ggplot(d1, aes(x, y, label=y)) + geom_point(stat='identity', aes(col=z), size=8) + geom_text(color="white", size=2) +
  ylim(-4, 2) + coord_flip() + theme_bw()+ theme(legend.position = "none") + labs(x="", y="") +
  geom_hline(aes(yintercept = mean(dys)), size=0.5, color = "gray50", linetype = "dashed") +
  scale_x_discrete(limits=c(revxlab)) + labs(title="Sharpest jumps in the average short yield")
g1

########## diverging dot plot for dyl

ndyl=ndm[c(3,4)]; mean(dyl)
maxdyl=ndyl[order(dyl, decreasing=T), ]
mindyl=ndyl[order(dyl, decreasing=F), ]

xlab=c(maxdyl[1:15, 2], mindyl[1:15, 2]) 
y=round(c(maxdyl[1:15,1], mindyl[1:15,1]),2);  
z=c(rep("max", 15), rep("min",15))
d2=data.frame(xlab,y,z)

d2=d2[order(d2$y, decreasing = T),]; d2$x=c(30:1); revxlab=rev(d2$xlab)
g2=ggplot(d2, aes(x, y, label=y)) + geom_point(stat='identity', aes(col=z), size=8) + geom_text(color="white", size=2) +
  ylim(-4, 2) + coord_flip() + theme_bw()+ theme(legend.position = "none") + labs(x="", y="") +
  geom_hline(aes(yintercept = mean(dyl)), size = 0.5, color = "gray50", linetype = "dashed") +
  scale_x_discrete(limits=c(revxlab)) + labs(title="Sharpest jumps in the average long yield")
g2


########## diverging dot plot for dym
ndym=ndm[c(2,4)]; mean(dym, na.rm=T)
maxdym=ndym[order(dym, decreasing=T), ]
mindym=ndym[order(dym, decreasing=F), ]

xlab=c(maxdym[1:15, 2], mindym[1:15, 2]) 
y=round(c(maxdym[1:15,1], mindym[1:15,1]),2);  
z=c(rep("max", 15), rep("min",15))
d3=data.frame(xlab,y,z)

d3=d3[order(d3$y, decreasing = T),]; d3$x=c(30:1); revxlab=rev(d3$xlab)
g3=ggplot(d3, aes(x, y, label=y)) + geom_point(stat='identity', aes(col=z), size=8) + geom_text(color="white", size=2) +
  ylim(-4, 2) + coord_flip() + theme_bw()+ theme(legend.position = "none") + labs(x="", y="") +
  geom_hline(aes(yintercept = mean(dyl)), size = 0.5, color = "gray50", linetype = "dashed") +
  scale_x_discrete(limits=c(revxlab)) + labs(title="Sharpest jumps in the average median yield")
g3


library(ggpubr)
ggarrange(g1, g3, g2, ncol = 3, nrow = 1)




