
install.packages("ggplot2")
library(ggplot2); library("readxl"); dm=read_excel("1945ms.xlsx")

x=rep(c(1:36), 25)
y=c(rep(c(1:25), each=36))
s=dm$ms
    
newd=data.frame(x, y, s)
# y=as.numeric(c(dm$ns, rep(c("NA"), t)))


# baseline graph
z=c("1945-1947", "1948-1950", "1951-1953", "1954-1956", "1957-1959", "1960-1962", 
    "1963-1965", "1966-1968", "1969-1971", "1972-1974", "1975-1977", "1978-1980",
    "1981-1983", "1984-1986", "1987-1989", "1990-1992", "1993-1995", "1996-1998",
    "1999-2001" ,"2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016",
    "2017-2019")

w=sort(z, decreasing = T)
#xax=paste(rep(c(1:12),3), "m",sep = "")
#c("J","F", "M","A","M","J","J","A","S","O","N", "D",)
#xax=rep(c("J","F", "M","A","M","J","J","A","S","O","N", "D"),3)

theme_set(theme_bw())  


ggplot(newd, aes(x, y))  + geom_point(aes(col=s), size=5.5) + 
  scale_y_discrete(limits=z) + labs(x="", y="")+
  geom_vline(xintercept =13, size = 0.5, color = "grey40", linetype = "dashed") +
  geom_vline(xintercept =25, size = 0.5, color = "grey40", linetype = "dashed") +
  theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal") +
  scale_x_continuous(breaks=seq(1,36,1), sec.axis=dup_axis())  +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) 

  