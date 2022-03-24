

library(readxl); 
library(psych);
library(ggplot2)

# read data
md=read_excel("53.xlsx")
describe(md)

# formating for ggplot
attach(md); yields=c(t1m, t3m, t6m, t1y, t2y, t3y, t5y, t7y, t10y, t20y, t30y)
yid=rep(c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X","XI" ), each=804)
ydp=data.frame(yields, yid)

# graphing
theme_set(theme_bw())  
ggplot(ydp, aes(yid, yields)) + geom_violin(aes(fill=yid), trim=F, width=1.5, color="white") + 
  geom_boxplot(width=0.2, fill="grey90")+   scale_y_continuous(limits=c(0,17), sec.axis=dup_axis()) +
  labs(y="", x="") +  theme(legend.position = "none",legend.title = element_blank()) +
  scale_x_discrete(labels=c("1-Month", "3-Month", "6-Month", "1-Year", "2-Year", "3-Year", "5-Year",
                            "7-Year", "10-Year", "20-Year", "30-Year"))
  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

  scale_fill_discrete(labels = c("Treasury 3-month bill (192001-193403)", "Treasury 3 to 6-month bills (193101-196911)", 
                                 "Treasury 9 to 12-month bills (194301-197011)", "Treasury 3 to 5-year notes (194501-196512)", 
                                 "Treasury 20-year bond (194201-196201)", "Treasury 8 to 20-year bonds (191901-194402)", 
                                 "Treasury 10 to 20-year bonds (194110-196712)"))
  
  
# Reference  
http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
  
