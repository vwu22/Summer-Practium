library("readxl")
library("dplyr")
#("xlsx")
library("tidyr")
library("tidyverse")
library("fpp2")
library("zoo")


GPI_Monthly = read_excel("GPI Monthly Cleaned.xlsx")
GPI_Monthly=GPI_Monthly[GPI_Monthly$Year>2020| GPI_Monthly$Month>=11,]##Filter out data that is after 2020/11
Allneeds=GPI_Monthly %>% group_by(GPI)%>%summarise(n=n())
Allneeds=filter(Allneeds,n==19)
####################################################################################################
#Function of simple exponential smoothing
simple_exponential_smoothing=function(GPII)
{
  GPI_1=GPI_Monthly %>% filter(GPI==GPII)
  GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]
  GPI_1=GPI_1$Qty
  
  alpha <- seq(.01, .99, by = .01)
  MAPE <- NA
  MAE =NA
  for(i in seq_along(alpha)) {
    fit <- ses(GPI_1, alpha = alpha[i],
               h = 20)
    res=fit$residuals
    res=abs(res)
    res[(length(res)-6):length(res)]
    obs=fit$x
    obs[(length(obs)-6):length(obs)]
    MAPE[i] <- sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
    MAE[i]=mean(res[(length(res)-6):length(res)])
  }
  alpha.fit <- data_frame(alpha, MAPE,MAE)
  alpha.min <- filter(alpha.fit,MAPE == min(MAPE))
  returnlist=list(TheModel=ses(GPI_1, alpha = alpha.min$alpha,h = 20),MAPE=alpha.min)
  return(returnlist)
}
#test function
aaa1=simple_exponential_smoothing("03530025000320")

#Build loop to create models for all GPIS

#simple exponential smoothing
MAPE=NA
MAE=NA
counter=1
for (i in Allneeds$GPI)
{
  asdasd=simple_exponential_smoothing(i)
  MAPE[counter]=asdasd$MAPE$MAPE
  MAE[counter]=asdasd$MAPE$MAE
  counter=counter+1
}
mean(MAPE)
mean(MAE)

##########################################################################################################
#Function of double parameter exponential smoothing

double_exponential_smoothing=function(GPII)
{
  
  GPI_1=GPI_Monthly %>% filter(GPI==GPII)
  GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]
  GPI_1=GPI_1$Qty
  
  GPI1_time=ts(GPI_1,start = c(2020,11),end=c(2022,5),frequency = 12)
  GPI1_time
  
  alpha <- seq(.01, .99, by = .01)
  beta <- seq(.01, .99, by = .01)
  
  MAPE=NA
  MAE=NA
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <- c('Alpha', 'Beta', 'MAPE','MAE')
  for(i in seq_along(beta)) {
        df2 <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(df2) <- c('Alpha', 'Beta', 'MAPE','MAE')
        fit <- holt(GPI1_time,beta = beta[i],h = 20)
        res=fit$residuals
        res=abs(res)
        res[(length(res)-6):length(res)]
        obs=fit$x
        obs[(length(obs)-6):length(obs)]
        df2$Alpha=fit$model$par[1]
        df2$Beta=beta[i]
        df2$MAPE=sum(res[(length(res)-6):length(res)]/obs[(length(obs)-6):length(obs)])/7
        df2$MAE=mean(res[(length(res)-6):length(res)])
        df=rbind(df,df2)
      }
  
  both_min=filter(df,MAPE==min(MAPE))
  returnlist=list(TheModel=holt(GPI1_time, alpha = both_min$Alpha,beta=both_min$Beta,h = 20),MAPE=both_min)
  return(returnlist)
  
}

#test function
aaa2=double_exponential_smoothing("02100015132030")

#double exponential smoothing
MAPE2=NA
MAE2=NA
alpha=NA
beta=NA
predicted=NA
counter=1

Result_table = data.frame()

for (i in Allneeds$GPI)
{
  asdasd=double_exponential_smoothing(i)
  pred=forecast(asdasd$TheModel,h=1)
  predicted[counter]=pred$mean
  MAPE2[counter]=asdasd$MAPE$MAPE
  MAE2[counter]=asdasd$MAPE$MAE
  alpha[counter]=asdasd$MAPE$Alpha
  beta[counter]=asdasd$MAPE$Beta
  counter=counter+1
}
Result_table = data.frame(MAPE2,MAE2,alpha,beta,predicted)

#Result_table = data.frame(matrix(ncol = 6, nrow = 1))
#colnames(Result_table) <- c('GPI','Alpha', 'Beta', 'MAPE','MAE','Predicted')
#Result_table$GPI="02100015132030"
#Result_table$Alpha=aaa$Model_Summary$Alpha
#Result_table$Beta=aaa$Model_Summary$Beta
#Result_table$MAPE=aaa$Model_Summary$MAPE
#Result_table$MAE=aaa$Model_Summary$MAE
#Result_table$Predicted=pred$mean

mean(MAPE2)
mean(MAE2)
