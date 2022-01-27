library(tidyverse)

getwd()
setwd('D:/Spring 2021/AIT 580_Prof.Harry Foxwell/5. week5/')

df1 <- read.csv('1950-2019_actual_tornadoes.csv',header=TRUE,sep=",")
head(df1)
tail(df1)

 
#	Display the range and related summary statistics 
#for the length and width of tornados (Boxplots)

boxplot(df1$wid,main="Summary Statistics of Tornado Width",col='tan')
boxplot(df1$len,main="Summary Statistics of Tornado Length",col='tan')




df1wid <- subset(df1,subset=wid>500)
df1wid

boxplot(df1wid$wid,col='tan',main="Summary Statistics of Tornado Width")


range(df1wid$wid)

summary(df1wid$wid)


points(min(df1wid$wid),cex=1.0,pch=1,col='brown')
points(quantile(df1wid$wid,0.25),cex=1.0,pch=2,col="red")
points(median(df1wid$wid),cex=1.0,pch=3,col='yellow')
points(mean(df1wid$wid),cex=1.0,pch=4,col='darkviolet')
points(quantile(df1wid$wid,0.75),cex=1.0,pch=5,col='blue')
points(max(df1wid$wid),cex=1.0,pch=6,col='red')
points(quantile(df1wid$wid,0.75)-quantile(df1wid$wid,0.25),cex=1.0,pch=7,col='black')
legend("topright",c("Min",'Q1',"Median","Mean",'Q3','Max','IQR'),cex=1.0,pch=c(1,2,3,4,5,6,7), col=c('brown',"red","yellow",'darkviolet','blue','red','black'), text.font=1)



df1len <- subset(df1,subset=len>30)
df1len
boxplot(df1len$len,col='tan',main="Summary Statistics of Tornado Length")

range(df1len$len)
summary(df1len$len)

points(min(df1len$len),cex=1.2,pch=1,col='brown')
points(quantile(df1len$len,0.25),cex=1.2,pch=2,col="red")
points(median(df1len$len),cex=1.2,pch=3,col='yellow')
points(mean(df1len$len),cex=1.2,pch=4,col='darkviolet')
points(quantile(df1len$len,0.75),cex=1.2,pch=5,col='blue')
points(max(df1len$len),cex=1.2,pch=6,col='red')
points(quantile(df1len$len,0.75)-quantile(df1len$len,0.25),cex=1.0,pch=9,col='black')
legend("topright",c("Min",'Q1',"Median","Mean",'Q3','Max','IQR'),cex=0.8,pch=c(1,2,3,4,5,6,9), col=c('brown',"red","yellow",'darkviolet','blue','red','black'), text.font=1)



#o	Create a table (crosstab) showing the mean number of
#injuries and fatalities for each tornado magnitude

install.packages('expss')
library(expss)

cro(df1$inj,df1$mag)
cro(df1$mag)
cro_mean(df1$inj,df1$mag)
cro_mean(df1$fat,df1$mag)

df1magnine <- subset(df1,subset=mag==-9)
df1magnine 
summary(df1magnine$inj)
summary(df1magnine$fat)

df1mag0<- subset(df1,subset=mag==0)
df1mag0 
summary(df1mag0$inj)
summary(df1mag0$fat)

df1mag1 <- subset(df1,subset=mag==1)
df1mag1 
summary(df1mag1$inj)
summary(df1mag1$fat)


df1mag2<- subset(df1,subset=mag==2)
df1mag2 
summary(df1mag2$inj)
summary(df1mag2$fat)


df1mag3 <- subset(df1,subset=mag==3)
df1mag3 
summary(df1mag3$inj)
summary(df1mag3$fat)

df1mag4 <- subset(df1,subset=mag==4)
df1mag4 
summary(df1mag4$inj)
summary(df1mag4$fat)


df1mag5 <- subset(df1,subset=mag==5)
df1mag5
summary(df1mag5$inj)
summary(df1mag5$fat)

#	Plot the loss for each tornado magnitude

Magnitude <- df1$mag
Loss <- df1$loss
plot(Magnitude,Loss,col='brown',main='Loss for each Tornado Magnitude',type='line')
plot(Magnitude,Loss,col='brown',main='Loss for each Tornado Magnitude')

#o	Which 3 states have the highest injuries and fatalities?

Injuries <- df1$inj
States <- df1$st



Fatalities <- df1$fat

#
s <- aggregate(Injuries~States,df1,sum)
s
ordering <- s[order(s[,'Injuries'],decreasing=TRUE),]
ordering
top3withhighinj <- ordering[1:3,'States']
top3withhighinj


fat <- aggregate(Fatalities~States,df1,sum)
fat
ordering <- fat[order(fat[,'Fatalities'],decreasing = TRUE),]
ordering
top3withhighfat <- ordering[1:3,'States']
top3withhighfat

#o	Is there a trend in the number of tornados from 1950 through 2019?
Year <-  df1$yr
hist(Year)

p <- ggplot(data=df1) +geom_histogram(mapping=aes(x = yr), binwidth=1,fill="cornsilk",color="black") +
  labs(x = "Year", y = "Count of Tornados",title = "Tornado Trend from 1950 - 2019")
p




