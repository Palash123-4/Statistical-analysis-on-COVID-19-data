

library(dplyr)
library(ggplot2)
library(ggmap)

#Uploading the data
covid=read.csv(file.choose(),header=T)
#knowing the data
head(covid) #visualize the few rows of the data to have a general idea
attach(covid)
#detach(covid)
names(covid)
table(id)# 193 countries with 139 observations in each

counts.covid=data.frame(id,date,confirmed,recovered,deaths,
                        icu+vent,longitude,latitude,stringency_index)
str(counts.covid)
counts.covid$date=as.POSIXct(counts.covid$date, format="%Y-%m-%d")
# calculate the SUM of all precipitation that occurred on each day(adding the 
#countries which had appeared in the same day)

total.recovery <- counts.covid %>%
  mutate(day = counts.covid$date)%>%
  group_by(day) %>% # group by the day column
  summarise(total_recovery=sum(recovered)) %>% 
  na.omit() 

Total.confirmed<- counts.covid %>%
  mutate(day = counts.covid$date)%>%
  group_by(day) %>% # group by the day column
  summarise(confirmed=sum(confirmed)) %>% 
  na.omit() 

Total.death<- counts.covid %>%
  mutate(day = counts.covid$date)%>%
  group_by(day) %>% # group by the day column
  summarise(total.deaths=sum(deaths)) %>% 
  na.omit() 

critical<- counts.covid %>%
  mutate(day = counts.covid$date)%>%
  group_by(day) %>% # group by the day column
  summarise(total.critical=sum(icu...vent)) %>% 
  na.omit() 
plot.data=data.frame(total.recovery$day,total.recovery$total_recovery,
                     Total.confirmed$confirmed,Total.death$total.deaths,critical$total.critical)
names(plot.data)=c("day","recovery","confirmed","death","critical.cases")

#Recovery rate with death rate plot--------------------------------------------------------------
ggplot() + 
  geom_line(data=plot.data,aes(x=day,y = 100*death/(recovery+death) , color = "darkred")) + 
  geom_line(data=plot.data,aes(x=day,y = 100*recovery/(recovery+death),color="steelblue"))+
  scale_color_discrete(name = "percentages", labels = c("Death ", "Recovery"))+
  ylab("percentages")

#Daily plots of deaths,Confirmed,Recoveries------------------------------------------------------------------------------
summary(diff(plot.data$death))# summary of daily deaths

plot(plot.data$day[-c(138,139)],diff(plot.data$death)[-c(138)],type="h",
     xlab="Days",ylab="Counts",main="Daily death cases")

plot(plot.data$day[-c(138,139)],diff(plot.data$confirmed)[-c(138)],type="l",col="blue",
     xlab="Days",ylab="Counts",main="Daily Confirmed cases vs Recoveries")
points(plot.data$day[-c(138,139)],diff(plot.data$recovery)[-c(138)],type="l",col="red",
     xlab="Days",ylab="Counts")
legend(locator(1),c("Total Confirmed cases","Total Recoveries"),cex=0.8,col=c("blue","red"),lty=c(1,2))

summary(plot.data)# to know overall information

##Country wise summary plots-----------------------------------------------------------------------------------
Total.confirmed.1<- counts.covid %>%
  group_by(id) %>% # group by longitude
  summarise(confirmed=max(confirmed)) %>% 
  na.omit()

Total.death.1<- counts.covid %>%
  group_by(id) %>% # group by the day column
  summarise(total.deaths=max(deaths)) %>% 
  na.omit()
mydata=data.frame(id,latitude,longitude)
mydata=mydata[!duplicated(mydata$id), ]
#final data.frame for plot-------------------------------------------------------------
Plot.data.1=data.frame(Total.confirmed.1$id,Total.confirmed.1$confirmed,
                       Total.death.1$total.deaths,mydata$latitude,mydata$longitude)
names(Plot.data.1)=c("id","total.confirmed","total.death","Latitude","Longitude")
dim(Plot.data.1)
summary(longitude)
summary(latitude)
#Based on the above summary the global map is going to be drawn------------------------
bbox <- c(left = -170.00, bottom = -40.901, right = 178.06, top = 64.963)
p<-ggmap(get_stamenmap(bbox, zoom = 4))

p+  geom_point(data = Plot.data.1, aes(x =Longitude , y = Latitude, size=total.confirmed))


#Regression coefficient---------------------------------------------------------------------------------
names(covid)
attach(covid)

#overall stringency index----------------------------------------------------------
overall.stringency_index<- counts.covid %>%
  mutate(day = counts.covid$date)%>%
  group_by(day) %>% # group by the day column
  summarise(s.index=mean(stringency_index)) %>% 
  na.omit() 
#To check whether cumulative no. of infections(or total deaths) over time has a significant association with stringency_index

#correlation coefficient-----------------------------------------------------------
cor.test(diff(Total.confirmed$confirmed)[-138],overall.stringency_index$s.index[-c(138,139)])

cor.test(diff(Total.death$total.deaths)[-138],overall.stringency_index$s.index[-c(138,139)])

#plots of correlation--------------------------------------------------------------
df=data.frame(diff(Total.confirmed$confirmed)[-138],overall.stringency_index$s.index[-c(138,139)])
names(df)=c("Total.infections","Average.stringency_index")
ggplot(data = df, aes(x = Average.stringency_index,
                      y = Total.infections)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE)

df.1=data.frame(diff(Total.death$total.deaths)[-138],overall.stringency_index$s.index[-c(138,139)])
names(df.1)=c("Total.deaths","Average.stringency_index")
ggplot(data = df.1, aes(x = Average.stringency_index,
                      y = Total.deaths)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE)

