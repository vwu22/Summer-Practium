#top Sales
library("readxl")
library("dplyr")
#("xlsx")
library("tidyr")
library("tidyverse")
library("fpp2")
library("zoo")
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



##########################################################################
#Draw a cumlative graph
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
GPI_Monthly = read_excel("GPI Monthly Data.xlsx")
#Simple Exponential smoothing

GPI_1=GPI_Monthly %>% filter(GPI=="03530025000320")
GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]
GPI_1_training=GPI_1[1:18,]
QTY_1_training=GPI_1_training$Qty
GPI_1$Date=as.yearmon(paste(GPI_1$Year,GPI_1$Month),"%Y %m")
as.Date(GPI_1$Date)
ggplot(GPI_1,aes(x=as.Date(Date),y=Qty))+geom_line()+geom_point()+scale_x_date(name="Date",date_breaks = "1 month")
ses_GPI1=ses(QTY_1_training,alpha=.2,h=20)
autoplot(ses_GPI1)
GPI_1_testing=GPI_1[19,]
QTY_1_testing=GPI_1_testing$Qty


res=ses_GPI1$residuals
res[(length(res)-6):length(res)]
obs=ses_GPI1$x
obs[(length(obs)-6):length(obs)]
sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7

#accuracy(ses_GPI1,QTY_1_testing)

alpha <- seq(.01, .99, by = .01)
MPE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(QTY_1_training, alpha = alpha[i],
             h = 20)
  res=fit$residuals
  res[(length(res)-6):length(res)]
  obs=fit$x
  obs[(length(obs)-6):length(obs)]
  MPE[i] <- sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
}
alpha.fit <- data_frame(alpha, MPE)
alpha.min <- filter(alpha.fit,MPE == min(MPE))
ggplot(alpha.fit, aes(alpha, MPE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, MPE),
             size = 2, color = "red")
ses_GPI1=ses(QTY_1_training,alpha=alpha.min$alpha,h=20)
autoplot(ses_GPI1)
qcement.f2 <- forecast(ses_GPI1,
                       h = 1)
res=ses_GPI1$residuals
res[(length(res)-6):length(res)]
obs=ses_GPI1$x
obs[(length(obs)-6):length(obs)]
MeanPercentageError <- sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
MeanPercentageError
#####################################################################################
#Holt-Winter's Seasonal Method (Doesn't work, gamma can't be estimated)
#Three parameters:  alpha:level, beta:trend, gamma:seasonal

GPI1_time=ts(QTY_1_training,start = c(2021,1),end=c(2022,3),frequency = 12)
GPI1_time
autoplot(decompose(GPI1_time))
GPI1_additive=hw(GPI1_time,seasonal = "additive")
summary(GPI1_additive)
GPI1_multiplicative=hw(GPI1_time,seasonal = "multiplicative")
qcement.hw <- ets(GPI1_time,
                  model = "ZZZ")
#Model=ZZZ indicate automatic selection
autoplot(forecast(qcement.hw))
summary(qcement.hw)
checkresiduals(qcement.hw)
qcement.f1 <- forecast(qcement,
                       h = 2)
accuracy(qcement.f1, QTY_1_testing)
######################################################################################################
#Double exponential smoothing (alpha, and beta)
holt.GPI_1=holt(GPI1_time,beta=0.01,h=20)
holt.GPI_1$model$par[1]
alpha <- seq(.01, .99, by = .01)
beta <- seq(.01, .99, by = .01)

MPE=NA
df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c('Alpha', 'Beta', 'MPE')
for(i in seq_along(alpha)) {
  for(j in seq_along(beta))
  {
      df2 <- data.frame(matrix(ncol = 3, nrow = 1))
      colnames(df2) <- c('Alpha', 'Beta', 'MPE')
      fit <- holt(GPI1_time, alpha = alpha[i],beta = beta[j],h = 20)
      res=fit$residuals
      res[(length(res)-6):length(res)]
      obs=fit$x
      obs[(length(obs)-6):length(obs)]
      df2$Alpha=alpha[i]
      df2$Beta=beta[j]
      df2$MPE=sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
      df=rbind(df,df2)
  }
}

df$absMPE=abs(df$MPE)
both_min=filter(df,abs(MPE)==min(abs(MPE)))

####################################################################################################
#Function of simple exponential smoothing
simple_exponential_smoothing=function(GPII)
{
  GPI_1=GPI_Monthly %>% filter(GPI==GPII)
  GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]
  GPI_1=GPI_1$Qty
  
  alpha <- seq(.01, .99, by = .01)
  MPE <- NA
  for(i in seq_along(alpha)) {
    fit <- ses(GPI_1, alpha = alpha[i],
               h = 20)
    res=fit$residuals
    res[(length(res)-6):length(res)]
    obs=fit$x
    obs[(length(obs)-6):length(obs)]
    MPE[i] <- sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
  }
  alpha.fit <- data_frame(alpha, MPE)
  alpha.min <- filter(alpha.fit,MPE == min(MPE))
  returnlist=list(TheModel=ses(GPI_1, alpha = alpha.min$alpha,h = 20),MPE=alpha.min)
  return(returnlist)
}
#test function
aaa1=simple_exponential_smoothing("03530025000320")

#Function of double parameter exponential smoothing
double_exponential_smoothing=function(GPII)
  {
  
  GPI_1=GPI_Monthly %>% filter(GPI==GPII)
  GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]
  GPI_1=GPI_1$Qty
  
  GPI1_time=ts(GPI_1,start = c(2021,1),end=c(2022,5),frequency = 12)
  GPI1_time
  
  alpha <- seq(.01, .99, by = .01)
  beta <- seq(.01, .99, by = .01)
  
  MPE=NA
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c('Alpha', 'Beta', 'MPE')
  for(i in seq_along(alpha)) {
    for(j in seq_along(beta))
    {
      if(alpha[i]>=beta[j])
      {
        df2 <- data.frame(matrix(ncol = 3, nrow = 1))
        colnames(df2) <- c('Alpha', 'Beta', 'MPE')
        fit <- holt(GPI1_time, alpha = alpha[i],beta = beta[j],h = 20)
        res=fit$residuals
        res[(length(res)-6):length(res)]
        obs=fit$x
        obs[(length(obs)-6):length(obs)]
        df2$Alpha=alpha[i]
        df2$Beta=beta[j]
        df2$MPE=sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
        df=rbind(df,df2)
      }
    }
  }
  
  both_min=filter(df,abs(MPE)==min(abs(MPE)))
  returnlist=list(TheModel=holt(GPI1_time, alpha = both_min$Alpha,beta=both_min$Beta,h = 20),MPE=both_min)
  return(returnlist)
}
#test function
aaa2=double_exponential_smoothing("12109902260320")

########################################################################
#Build loop to create models for all GPIS
GPI_Monthly=GPI_Monthly[GPI_Monthly$Year>2020| GPI_Monthly$Month>=11,]
Allneeds=GPI_Monthly %>% group_by(GPI)%>%summarise(n=n())
Allneeds=filter(Allneeds,n==19)
#simple exponential smoothing
MPE=NA
counter=1
for (i in Allneeds$GPI)
{
  asdasd=simple_exponential_smoothing(i)
  MPE[counter]=asdasd$MPE$MPE
  counter=counter+1
}
MPE

#double exponential smoothing
MPE2=NA
counter=1
for (i in Allneeds$GPI)
{
  asdasd=double_exponential_smoothing(i)
  MPE2[counter]=asdasd$MPE$MPE
  counter=counter+1
}
mean(MPE2)
