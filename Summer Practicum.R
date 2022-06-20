#top Sales
library("readxl")
library("dplyr")
library("xlsx")
library("tidyr")
library("tidyverse")
library("fpp2")


setwd("C:/Users/a2452/Desktop/Summer practicum")
Dispending = read_excel("Dispensing052022.xlsx")
InventoryReceipt = read_excel("InventoryReceiptDetails 05-01-2020 - 05-31-2022.xlsx")
Formulary= read_excel("ActiveFormulary.xlsx")


newdata=merge(x=Formulary,y=Dispending,by="NDC")
head(newdata)
newdata$totalcost=newdata$AvgUnitCost*newdata$Qty
newdata2=newdata %>% group_by(GPI) %>% summarise(cost=sum(totalcost))
newdata2=newdata2 %>% drop_na()
newdata2=arrange(newdata2,desc(cost))


total=sum(newdata2$cost)
current_total=0
i=1
total_percent=c()
while (current_total/total<0.9)
  {
  current_total=newdata2[i,2]+current_total
  total_percent[i]=current_total/total
  i=i+1
}
finalresult=head(newdata2,i-1)
finalresult

interest=newdata %>% filter(newdata$GPI %in% finalresult$GPI)
head(interest)

breaks = seq(1, 305, by=1)
plot(breaks, total_percent,            # plot the data 
        main="cumulative graph",  # main title 
        xlab="Index",        # x−axis label 
        ylab="Cumulative percentage")   # y−axis label 



#######################################################################################
cleandata=function(the_data,the_month,the_year){
  need= the_data[-c(1:15,31)]
  colnames(need)[1]="Drug Name"
  colnames(need)[2]="NDC"
  colnames(need)[3]="QTY"
  colnames(need)[4]="PK QTY"
  colnames(need)[5]="Form"
  colnames(need)[6]="Avg Cost"
  colnames(need)[7]="Acg Cost"
  colnames(need)[8]="AWP"
  colnames(need)[9]="Hcfa"
  colnames(need)[10]="Otc"
  colnames(need)[11]="Brand"
  colnames(need)[12]="AA"
  colnames(need)[13]="AB"
  colnames(need)[14]="AC"
  colnames(need)[15]="Download time"
  need$Month=rep(the_month,nrow(need))
  need$Year=rep(the_year,nrow(need))
  return(need)
}


sprxd = read.csv(file = 'sprxd012021.csv')
Jan2021=cleandata(sprxd,1,2021)
sprxd = read.csv(file = 'sprxd032021.csv')
Mar2021=cleandata(sprxd,3,2021)
sprxd = read.csv(file = 'sprxd042021.csv')
April2021=cleandata(sprxd,4,2021)
sprxd = read.csv(file = 'sprxd052021.csv')
May2021=cleandata(sprxd,5,2021)
sprxd = read.csv(file = 'sprxd062021.csv')
June2021=cleandata(sprxd,6,2021)
sprxd = read.csv(file = 'sprxd072021.csv')
July2021=cleandata(sprxd,7,2021)
sprxd = read.csv(file = 'sprxd082021.csv')
Aug2021=cleandata(sprxd,8,2021)
sprxd = read.csv(file = 'sprxd092021.csv')
Sep2021=cleandata(sprxd,9,2021)
sprxd = read.csv(file = 'sprxd102021.csv')
Oct2021=cleandata(sprxd,10,2021)
sprxd = read.csv(file = 'sprxd112021.csv')
Nov2021=cleandata(sprxd,11,2021)
sprxd = read.csv(file = 'sprxd122021.csv')
Dec2021=cleandata(sprxd,12,2021)
sprxd = read.csv(file = 'sprxd012022.csv')
Jan2022=cleandata(sprxd,1,2022)
sprxd = read.csv(file = 'sprxd022022.csv')
Feb2022=cleandata(sprxd,2,2022)
sprxd = read.csv(file = 'sprxd032022.csv')
Mar2022=cleandata(sprxd,3,2022)
sprxd = read.csv(file = 'sprxd042022.csv')
April2022=cleandata(sprxd,4,2022)
sprxd = read.csv(file = 'sprxd052022.csv')
May2022=cleandata(sprxd,5,2022)

total_data=rbind(Jan2021,Mar2021)
total_data=rbind(total_data,April2021)
total_data=rbind(total_data,May2021)
total_data=rbind(total_data,June2021)
total_data=rbind(total_data,July2021)
total_data=rbind(total_data,Aug2021)
total_data=rbind(total_data,Sep2021)
total_data=rbind(total_data,Oct2021)
total_data=rbind(total_data,Nov2021)
total_data=rbind(total_data,Dec2021)
total_data=rbind(total_data,Jan2022)
total_data=rbind(total_data,Feb2022)
total_data=rbind(total_data,Mar2022)
total_data=rbind(total_data,April2022)
total_data=rbind(total_data,May2022)


fastrunner=head(newdata2,304)
interest=Formulary %>% filter(Formulary$GPI %in% fastrunner$GPI)
nrow(interest)
finaldata=merge(x=interest,y=total_data,by="NDC")
finaldata2=finaldata[c("NDC","GPI","QTY","Month","Year")]
finaldata2$QTY=as.numeric(gsub(",","",finaldata2$QTY))
finaldata2=finaldata2 %>% group_by(GPI,Month,Year) %>% summarise(totalQty=sum(QTY),Year=Year,Month=Month)


#################################################################
GPI_Monthly = read_excel("GPI Monthly Data_Updated.xlsx")
#Simple Exponential smoothing
GPI_1_training=GPI_Monthly %>% filter(GPI=="02100015132030",Year=="2021")
QTY_1_training=GPI_1_training$Qty
ses_GPI1=ses(QTY_1_training,alpha=.2,h=10)
autoplot(ses_GPI1)
GPI_1_testing=GPI_Monthly %>% filter(GPI=="02100015132030",Year=="2022",Month=="01")
QTY_1_testing=GPI_1_testing$Qty
accuracy(ses_GPI1,QTY_1_testing)

alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(QTY_1_training, alpha = alpha[i],
             h = 10)
  RMSE[i] <- accuracy(fit,QTY_1_testing)
}
alpha.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(alpha.fit,RMSE == min(RMSE))
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, RMSE),
             size = 2, color = "red")
ses_GPI1=ses(QTY_1_training,alpha=.99,h=10)
autoplot(ses_GPI1)
accuracy(ses_GPI1,QTY_1_testing)
