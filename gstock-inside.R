#load libraries

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

#read data

insiders1 <- read.csv("./SA/g-insiders12m.csv")
insiders2 <- read.csv("./SA/g-insiders12m-mktcap.csv")
gdiv1 <- read.csv("./SA/gold-div.csv")

#clean data

insiders11 <- insiders1
insiders11$Symbol <- gsub(".*:","",insiders1$Symbol)
insiders22 <- insiders2[,c(1,3,8:10)]
insiders22 <- mutate(insiders22,Market.Cap=Market.Cap*1000000)
insiders.clean <- left_join(insiders11,insiders22,by="Symbol")
insiders.clean$Trans.Date <- gsub("\\/","-",insiders.clean$Trans.Date)
insiders.clean$Trans.Date <- parse_date_time(insiders.clean$Trans.Date,orders="mdy")
insiders.clean <- mutate(insiders.clean,Size=Cost/Market.Cap)
insiders.clean <- filter(insiders.clean,Buy.Sell!="unknown")

#plot buys and sells

insiders.clean.p <- ggplot(aes(x=Trans.Date,y=Symbol,colour=Buy.Sell,size=Size),data=insiders.clean) +geom_point()
insiders.clean.p <- insiders.clean.p + scale_colour_manual(values=c("#009933","red"))

#compare by market cap, ROE, and ROA

insiders222 <- filter(insiders22,ROE>0)
insiders222 <- insiders222[-c(3:5,10,14,17,21),]
plot1_bar <- ggplot(aes(x=Symbol,y=ROE,fill=ROA),data=insiders222) + geom_bar(stat="identity")
plot1_point <- ggplot(aes(x=Symbol,y=ROE,size=Market.Cap,color=ROA),data=insiders222) + geom_point()

#plot by div stats

plotdiv_bar <- ggplot(aes(x=Symbol,y=Yield,fill=Payout.Ratio),data=gdiv1) + geom_bar(stat="identity")
plotdiv_bar <- plotdiv_bar+ scale_fill_gradient("Payout Ratio",low="green",high="red")