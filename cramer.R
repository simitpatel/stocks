#load libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

#read data, eliminate partial years

cramer1 <-read.csv("./SA/cramer1.csv")
cramer1 <- cramer1[2:14,]

#rename variables, melt

names(cramer1) <- c("Time","AAP","SP500","SP500Div")
cramer1.melt <- melt(cramer1,id.vars=c("Time"))

#plot

cramer1.melt.p <- (ggplot(cramer1.melt, aes(Time, value)) +   
                     geom_bar(aes(fill = variable), position = "dodge", stat="identity"))
cramer1.melt.p2 <- ggplot(data=cramer1.melt, aes(x=Time, y=value, group = variable, colour = variable)) +
       geom_line(size=2) +
       geom_point( size=4, shape=21, fill="white")


#get key stats

cramer.median <- median(as.numeric(cramer1$AAP))
cramer.sd <- sd(as.numeric(cramer1$AAP))

sp500div.median <- median(as.numeric(cramer1$SP500Div))
sp500div.sd <- sd(as.numeric(cramer1$SP500Div))