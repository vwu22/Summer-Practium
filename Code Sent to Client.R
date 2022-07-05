###Double Exponential Smoothing

library("readxl")
library("dplyr")
library("tidyr")
library("tidyverse")
library("fpp2")
library("zoo")
#Use below code to install package if not installed yet
#install.packages("package_name")

##########################################################################################################
#remove GPIs with missing dispensing values in some month
GPI_Monthly = read_excel("GPI Monthly Cleaned.xlsx")
GPI_Monthly=GPI_Monthly[GPI_Monthly$Year>2020| GPI_Monthly$Month>=11,]  ##Only take data beyond Nov 2020
Allneeds=GPI_Monthly %>% group_by(GPI)%>%summarise(n=n())
Allneeds=filter(Allneeds,n==19)##Change the number, 19 indicate 19 months, as more months data go into the data set,
                               ##need to incease number. For example, in next month, we add June's data into the table, 
                               ##change it to be n==20

##########################################################################################################
#Function of double parameter exponential smoothing

double_exponential_smoothing=function(GPII)
{
  
  GPI_1=GPI_Monthly %>% filter(GPI==GPII)
  GPI_1=GPI_1[GPI_1$Year>2020| GPI_1$Month>=11,]#Same as above, only take data beyond Nov 2020
  GPI_1=GPI_1$Qty
  
  GPI1_time=ts(GPI_1,start = c(2020,11),end=c(2022,5),frequency = 12) ##select time period, this is from 11/2020 to 05/2022, 
                                                                      ##change it to whenever it should be
  
  beta <- seq(.01, .99, by = .01)
  
  MAPE=NA
  MAE=NA
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <- c('Alpha', 'Beta', 'MAPE','MAE')
  for(i in seq_along(beta)) {
    df2 <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(df2) <- c('Alpha', 'Beta', 'MAPE','MAE')
    fit <- holt(GPI1_time,beta = beta[i],h = 20)    ##########Change h=20 when more data comes in. Currently have 19, so I put 20
                                                    ##########Make sure h is greater than number of data put in to capture all data.
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
  returnlist=list(TheModel=holt(GPI1_time, alpha = both_min$Alpha,beta=both_min$Beta,h = 20),Model_Summary=both_min) ###Again, change h=20 when more data comes in
  return(returnlist)
  
}



###############################################
#How to use function:
aaa=double_exponential_smoothing("02100015132030")##put the GPI that want to be predicted into the parameter
#Use model to predict
pred=forecast(aaa$TheModel,h=1) #h=1 indicate make prediction for one month, this model can predict more months of data,
                                 #but I recommend only predict for one month since exponential smoothing need new observed 
                                 #data to make better prediction.

aaa$Model_Summary  #tells you the alpha, beta, MAPE and MAE
                   #Note:the MAPE and MAE here is calculated by the last 7 months
pred$mean  #predicted value is printed 


#########################################################################
#For loop to create models for all gpis
GPI=NA
MAPE=NA
MAE=NA
alpha=NA
beta=NA
predicted=NA
counter=1

for (i in Allneeds$GPI)
{
  asdasd=double_exponential_smoothing(i)
  pred=forecast(asdasd$TheModel,h=1)
  GPI[counter]=i
  predicted[counter]=pred$mean
  MAPE[counter]=asdasd$Model_Summary$MAPE
  MAE[counter]=asdasd$Model_Summary$MAE
  alpha[counter]=asdasd$Model_Summary$Alpha
  beta[counter]=asdasd$Model_Summary$Beta
  counter=counter+1
}

Result_table = data.frame(GPI,MAPE,MAE,alpha,beta,predicted)

write.csv(Result_table,"C:\\Users\\a2452\\Desktop\\Summer practicum\\Result_table.csv", row.names = TRUE)
#use the code to write the csv file if needed, example is shown above
#write.csv(Your DataFrame,"Path to export the DataFrame\\File Name.csv", row.names = FALSE)
