

install.packages("roll")
library(roll)
library(ggplot2)
library(readxl)

dm=read_excel("1920spreads.xlsx")

attach(dm)
ws=12; rvs=roll_sd(ys, width=ws); rvm=roll_sd(ym, width=ws); rvl=roll_sd(yl, width = ws)
date=seq(as.Date('1920-01-01'), to=as.Date('2020-03-01'), format="%Y%m" , by="month")
ndm=data.frame(date, rvs, rvm, rvl)

# labels and breaks for X axis text
g=ggplot(ndm, aes(x=date)) + geom_area(aes(y=rvs, fill="rvs")) + theme_bw() + 
  geom_area(aes(y=rvm, fill="rvm")) + geom_area(aes(y=rvl, fill="rvl")) + 
  theme(legend.position = c(0.925,0.88)) + labs(x="", y="") + 
  scale_x_date(date_breaks = "3 years", date_minor_breaks="1 years", date_labels="%Y")+
  scale_fill_discrete(name = "Rolling volatility (SD)", labels = c("Average long yield", 
    "Average median yield", "Average short yield")) + guides(fill = guide_legend(reverse=TRUE)) 
 
xl=as.Date(c( "1920-02-01", "1923-06-01", "1926-11-01", "1929-09-01", "1937-06-01", "1945-03-01", "1948-12-01", 
              "1953-08-01", "1957-09-01", "1960-05-01", "1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", 
              "1990-08-01", "2001-04-01", "2008-01-01", "2020-03-01"))
xr=as.Date(c("1921-07-01", "1924-07-01", "1927-11-01", "1933-03-01", "1938-06-01", "1945-10-01", "1949-10-01", 
             "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", 
             "1991-03-01", "2001-11-01", "2009-06-01", "2020-08-01"))
g+ annotate("rect", xmin=xl, xmax=xr, ymin =rep(0,18), ymax =rep(3,18), alpha = .3) 

# other colors
g=ggplot(ndm, aes(x=date)) + geom_area(aes(y=rvs, fill="rvs")) + theme_bw() + 
  geom_area(aes(y=rvm, fill="rvm")) + geom_area(aes(y=rvl, fill="rvl")) + 
  theme(legend.position = c(0.925,0.88)) + labs(x="", y="") + 
  scale_x_date(date_breaks = "3 years", date_minor_breaks="1 years", date_labels="%Y")+
  scale_fill_brewer(palette="Paired",name = "Rolling volatility (SD)", labels = c("Average long yield", 
        "Average median yield", "Average short yield")) + guides(fill = guide_legend(reverse=TRUE)) 
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

### reduntant way
  annotate("rect", xmin=as.Date("1920-02-01"), xmax=as.Date("1921-07-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1923-06-01"), xmax=as.Date("1924-07-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1926-11-01"), xmax=as.Date("1927-11-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1929-09-01"), xmax=as.Date("1933-03-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1937-06-01"), xmax=as.Date("1938-06-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1945-03-01"), xmax=as.Date("1945-10-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1948-12-01"), xmax=as.Date("1949-10-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1953-08-01"), xmax=as.Date("1954-05-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1957-09-01"), xmax=as.Date("1958-04-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1960-05-01"), xmax=as.Date("1961-02-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1970-01-01"), xmax=as.Date("1970-11-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1973-12-01"), xmax=as.Date("1975-03-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1980-02-01"), xmax=as.Date("1980-07-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1981-08-01"), xmax=as.Date("1982-11-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("1990-08-01"), xmax=as.Date("1991-03-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("2001-04-01"), xmax=as.Date("2001-11-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("2008-01-01"), xmax=as.Date("2009-06-01"), ymin = 0, ymax = 3, alpha = .3) +
  annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2020-08-01"), ymin = 0, ymax = 3, alpha = .3) 

  