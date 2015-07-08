#load libraries

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

#read data

base1 <- read.csv("./SA/base-stocks.csv")

#clean data

basestocks <- base1[,c(1,3,5,11,12,14,32,51,53,54,55,73,74,80,83,84,86,87,97,98,100,106,107)]

#compare by market cap, ROE, and ROA

base_r <- filter(basestocks,ROE>0) 
base_r <- filter(base_r,Market.Cap..Mil.>=500)
base_r_bar <- ggplot(aes(x=Symbol,y=ROE,fill=ROA),data=base_r) + geom_bar(stat="identity")
base_r_point <- ggplot(aes(x=Symbol,y=ROE,size=Market.Cap..Mil.,color=ROA),data=base_r) + geom_point()

#compare by dividend

div_bar <- ggplot(aes(x=Symbol,y=Yield,fill=PayoutRatio),data=base_r) + geom_bar(stat="identity")
div_bar <- div_bar + scale_fill_gradient("PayoutRatio",low="green",high="red")
div_point <- ggplot(aes(x=Symbol,y=Yield,size=Net.Margin,color=PayoutRatio),data=base_r) + geom_point()
div_point  <- div_point + scale_colour_gradient("PayoutRatio",low="#00cc33",high="#cc0000")

#compare by price ratios

priceset <- filter(base_r,P.E<=30)
price_bar <- ggplot(aes(x=Symbol,y=P.E,fill=P.FCF),data=priceset) + geom_bar(stat="identity")
price_bar <- price_bar +  scale_y_reverse()
price_point <- ggplot(aes(x=Symbol,y=-P.E,size=-P.FCF,color=Revenue.Growth.Rate.1.Year),data=priceset) + geom_point()
price_point <- price_point + scale_colour_gradient2("1 Yr Rev Gr",low="white",high="red", mid="#04861a", midpoint=median(priceset$Revenue.Growth.Rate.1.Year), space="Lab", guide="colourbar")

#compare by ownership ratios

own_point <- ggplot(aes(x=Symbol,y=Short.Ratio,size5,color=Institutional.Ownership),data=base_r) + geom_point()