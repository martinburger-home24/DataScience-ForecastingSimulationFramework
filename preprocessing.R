

#################################################################################################
## structure of this file #######################################################################
#################################################################################################
# main components:
# 1. loading, cleaning and extrapolate data
# 2. calculate cleaned sales (outside the quantiles)
# 3. prepare variables to be used in the simulation script




# load some packages and define functions

library(plyr)
library(data.table)
library(dplyr)
library(matrixStats)

#function for creating historical information

rowShift <- function(x, shiftLen = -1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}



#################################################################################################
## 1. loading, cleaning and extrapolate data ####################################################
#################################################################################################


#loading data and preparing it

# set your working directory!

setwd("C:/Users/martin.burger/Arbeitsordner/Data_Products/Simulation")

mydat <- read.csv("data_15_16.csv", sep=";")
mydat$leadtime <- as.numeric(as.character(mydat$leadtime)) 
mydat$sales <- as.numeric(as.character(mydat$sales)) 
mydat[is.na(mydat)] <- 0
names(mydat)[names(mydat)=="ï..yearofweeknumber"] <- "yearofweeknumber"
mydat <-subset(mydat, mydat$weeknumber!=53) # removing week 53(too messy)
mydat <-mydat[,c(1:8,18,19,20)]
mydat <- mydat %>% arrange(itemno,yearofweeknumber,weeknumber)
mydat<-as.data.frame(mydat)

#removing items with a week gap equal or more than 3 weeks

infodat <- mydat %>% arrange(itemno,yearofweeknumber,weeknumber) %>% group_by(itemno) %>% mutate(infoweek=rowShift(weeknumber), difference=weeknumber-infoweek)
infodatsub <- infodat %>% group_by(itemno) %>% summarize(maxim=max(difference,na.rm=TRUE)) 
sub<-subset(infodatsub,infodatsub$maxim>3)
infor<-as.character(sub$itemno)

mydat <-subset(mydat,!(mydat$itemno %in% infor))
mydat$itemno<-droplevels(mydat$itemno)


mydat=as.data.table(mydat)
mydat[, year_fut_1 := rowShift(yearofweeknumber, 1L), by=list(itemno)]
mydat<-as.data.frame(mydat)

#creating last week of 2015 variable

mydat <-mydat %>% arrange(itemno,yearofweeknumber,weeknumber) %>% group_by(itemno) %>% mutate(last=ifelse(yearofweeknumber==2015 & year_fut_1==2016, weeknumber,0 )) %>% mutate(last1=as.numeric(max(last,na.rm=TRUE)))
mydat$yearofweeknumber<-as.numeric(mydat$yearofweeknumber)

#creating new week variable

mydat <-mydat %>% arrange(itemno,yearofweeknumber,weeknumber) %>% group_by(itemno) %>% do(mutate(.,week=ifelse(yearofweeknumber==2016,last1+weeknumber, weeknumber)))

#adding rows with missing values

padFun<-function(data,idvars,timevar){
  # Coerce ID variables to character
  data[,idvars]<-lapply(data[,idvars,drop=FALSE],as.character)
  # Create global ID variable of all individual ID vars pasted together
  globalID<-Reduce(function(...)paste(...,sep="SOMETHINGWACKY"),
                   data[,idvars,drop=FALSE])
  # Create data.frame of all possible combinations of globalIDs and times
  allTimes<-expand.grid(globalID=unique(globalID),
                        allTime=min(data[,timevar]):max(data[,timevar]),
                        stringsAsFactors=FALSE)
  # Get the original ID variables back
  allTimes2<-data.frame(allTimes$allTime,do.call(rbind,
                                                 strsplit(allTimes$globalID,"SOMETHINGWACKY")),stringsAsFactors=FALSE)
  # Convert combinations data.frame to data.table with idvars and timevar as key
  allTimesDT<-data.table(allTimes2)
  setnames(allTimesDT,1:ncol(allTimesDT),c(timevar,idvars))
  setkeyv(allTimesDT,c(idvars,timevar))
  # Convert data to data.table with same variables as key
  dataDT<-data.table(data,key=c(idvars,timevar))
  # Join the two data.tables to create padding
  res<-dataDT[allTimesDT]
  return(res)
}

padded<-padFun(data=mydat,idvars=c("itemno"),timevar="week")

#create copy of padded table

padded2=as.data.table(padded)

#remove leading and trailing na rows

DT2 = padded2[!is.na(company)]
setkey(padded2,itemno,week)
setkey(DT2,itemno,week)
tokeep = DT2[padded2,!is.na(company),roll=TRUE,rollends=FALSE]
padded2 = padded2[tokeep]

#remove columns that are not necessary

padded3<-subset(padded2, select=-c(company,MainCommodityGroup,SubCommodityGroup1,stockaivalable,unitcost,year_fut_1,last,last1))
padded3<- padded3 %>% arrange(itemno,week) %>% group_by(itemno) %>% 
  mutate(yearofweeknumber=ifelse(week<53,2015,2016))

padded3<-as.data.table(padded3)

padded3[, c('sales_hist_sales_1','sales_hist_sales_2','sales_hist_sales_3','sales_hist_sales_4','sales_hist_sales_5','sales_hist_sales_6',
            'sales_hist_sales_7','sales_hist_sales_8','sales_hist_sales_9','sales_hist_sales_10','sales_hist_sales_11','sales_hist_sales_12'
) := list(rowShift(sales, -1L),rowShift(sales, -2L),rowShift(sales, -3L),rowShift(sales, -4L),rowShift(sales, -5L),rowShift(sales, -6L),rowShift(sales, -7L),rowShift(sales, -8L),rowShift(sales, -9L),rowShift(sales, -10L),rowShift(sales, -11L),rowShift(sales, -12L)), by=list(itemno)]

padded3[, c('leadtime_hist_sales_1','leadtime_hist_sales_2','leadtime_hist_sales_3','leadtime_hist_sales_4','leadtime_hist_sales_5','leadtime_hist_sales_6'
) := list(rowShift(leadtime, -1L),rowShift(leadtime, -2L),rowShift(leadtime, -3L),rowShift(leadtime, -4L),rowShift(leadtime, -5L),rowShift(leadtime, -6L)), by=list(itemno)]


padded3[, c('price_hist_sales_1','price_hist_sales_2','price_hist_sales_3','price_hist_sales_4','price_hist_sales_5','price_hist_sales_6'
) := list(rowShift(price_reduction, -1L),rowShift(price_reduction, -2L),rowShift(price_reduction, -3L),rowShift(price_reduction, -4L),rowShift(price_reduction, -5L),rowShift(price_reduction, -6L)), by=list(itemno)]


#library(zoo)
#fill in missing values with mean of last six weeks

padded3<-as.data.frame(padded3)
padded4<- padded3 %>% arrange(itemno,week) %>% group_by(itemno) %>% rowwise %>%
  
  mutate(leadtime=ifelse(is.na(weeknumber)==TRUE, mean(c(leadtime_hist_sales_1,leadtime_hist_sales_2,leadtime_hist_sales_3,leadtime_hist_sales_4
                                                         ,leadtime_hist_sales_5,leadtime_hist_sales_6),na.rm=TRUE),leadtime),
         
         sales=ifelse(is.na(weeknumber)==TRUE,mean(c(sales_hist_sales_1,sales_hist_sales_2,sales_hist_sales_3,sales_hist_sales_4
                                                     ,sales_hist_sales_5,sales_hist_sales_6),na.rm=TRUE),sales),
         
         price_reduction=ifelse(is.na(weeknumber)==TRUE,mean(c(price_hist_sales_1,price_hist_sales_2,price_hist_sales_3,price_hist_sales_4
                                                               ,price_hist_sales_5,price_hist_sales_6),na.rm=TRUE),price_reduction)
         
  )

mydata<-as.data.frame(padded4)



#################################################################################################
## 2. creating cleaned sales ####################################################################
#################################################################################################

#wooooooo that was the first part
#now creating clean sales and other variables

mydata$quant_last8 <- rowQuantiles(as.matrix(mydata[,c("sales_hist_sales_1","sales_hist_sales_2","sales_hist_sales_3","sales_hist_sales_4","sales_hist_sales_5","sales_hist_sales_6","sales_hist_sales_7", "sales_hist_sales_8")]),probs =  0.9)
mydata$quant_last8_lower <- rowQuantiles(as.matrix(mydata[,c("sales_hist_sales_1","sales_hist_sales_2","sales_hist_sales_3","sales_hist_sales_4","sales_hist_sales_5","sales_hist_sales_6","sales_hist_sales_7", "sales_hist_sales_8")]),probs =  0.1)


DT <- data.table(mydata)

DT[, count := .N, by=list(itemno)]

maxi <<- max(as.data.frame(DT$count)$'DT$count')  


S=split(mydata, f=mydata$itemno)

f <- function (x) {
  
  output <- data.frame(1:maxi)  
  output$sales_cleaned <- NA
  output$year <- NA
  output$week <- NA
  
  for (i in 1:nrow(x)) {
    
    if (x$leadtime[i] < 14 & x$sales[i]<=x$quant_last8[i] & (x$sales[i]>=x$quant_last8_lower[i] | x$quant_last8_lower[i]<3) &
        is.na(x$quant_last8[i])==FALSE & is.na(x$quant_last8_lower[i])==FALSE) {
      
      output$sales_cleaned[i] <- x$sales[i]
      
    } 
    else if (x$sales[i]>x$quant_last8[i] & is.na(x$quant_last8[i])==FALSE & is.na(x$quant_last8_lower[i])==FALSE) {output$sales_cleaned[i] <- x$quant_last8[i] }
    else if (x$sales[i]<x$quant_last8_lower[i] &  is.na(x$quant_last8[i])==FALSE & is.na(x$quant_last8_lower[i])==FALSE) {output$sales_cleaned[i] <- x$quant_last8_lower[i] }
    
    else {
      
      output$sales_cleaned[i] <- mean(rbind(output$sales_cleaned[i-1],output$sales_cleaned[i-2],output$sales_cleaned[i-3],output$sales_cleaned[i-4],output$sales_cleaned[i-5],output$sales_cleaned[i-6]), na.rm=TRUE)
      
    }
    
    output$yearofweeknumber[i]       <- x$yearofweeknumber[i]
    output$week[i] <- x$week[i]
    
  }
  
  as.data.frame(output)
  
}

t <- ldply(S,f)
t <- plyr::rename(t, c(".id"="itemno"))

result <- merge(t,mydata, by = c('week','itemno'))
information <- mydat[,c("MainCommodityGroup","SubCommodityGroup1","itemno","unitcost","stockaivalable","week")]
result1 <-merge(result,information,by=c("week","itemno"),all.x=TRUE)

resultDT <- data.table(result1)




#################################################################################################
## 3. Adding variables for direct use in forecasting simulation function ########################
#################################################################################################

# Future Sales

resultDT[, c('fut_price_1','fut_price_2','fut_price_3','fut_price_4','fut_price_5','fut_price_6',
             'fut_price_7','fut_price_8','fut_price_9','fut_price_10','fut_price_11','fut_price_12'
) := list(rowShift(price_reduction, 1L),rowShift(price_reduction, 2L),rowShift(price_reduction, 3L),rowShift(price_reduction, 4L),rowShift(price_reduction, 5L),rowShift(price_reduction, 6L),rowShift(price_reduction, 7L),rowShift(price_reduction, 8L),rowShift(price_reduction, 9L),rowShift(price_reduction, 10L),rowShift(price_reduction, 11L),rowShift(price_reduction, 12L)), by=list(itemno)]

resultDT[, c('fut_sales_1','fut_sales_2','fut_sales_3','fut_sales_4','fut_sales_5','fut_sales_6','fut_sales_7','fut_sales_8','fut_sales_9','fut_sales_10','fut_sales_11','fut_sales_12','fut_sales_13','fut_sales_14','fut_sales_15','fut_sales_16','fut_sales_17','fut_sales_18','fut_sales_19','fut_sales_20'
) := list(rowShift(sales, 1),rowShift(sales, 2),rowShift(sales, 3),rowShift(sales, 4),rowShift(sales, 5),rowShift(sales, 6),rowShift(sales, 7),rowShift(sales, 8),rowShift(sales, 9),rowShift(sales, 10),rowShift(sales, 11),rowShift(sales, 12),rowShift(sales, 13),rowShift(sales, 14),rowShift(sales, 15),rowShift(sales_cleaned, 16),rowShift(sales, 17),rowShift(sales, 18),rowShift(sales, 19),rowShift(sales, 20)), by=list(itemno)]



resultDT[, c('csales_hist_sales_1','csales_hist_sales_2','csales_hist_sales_3','csales_hist_sales_4','csales_hist_sales_5','csales_hist_sales_6',
             'csales_hist_sales_7','csales_hist_sales_8','csales_hist_sales_9','csales_hist_sales_10','csales_hist_sales_11','csales_hist_sales_12'
) := list(rowShift(sales_cleaned, -1),rowShift(sales_cleaned, -2),rowShift(sales_cleaned, -3),rowShift(sales_cleaned, -4),rowShift(sales_cleaned, -5),rowShift(sales_cleaned, -6),rowShift(sales_cleaned, -7),rowShift(sales_cleaned, -8),rowShift(sales_cleaned, -9),rowShift(sales_cleaned, -10),rowShift(sales_cleaned, -11),rowShift(sales_cleaned, -12)), by=list(itemno)]


resultDT[, c('csales_hist_sales_1','csales_hist_sales_2','csales_hist_sales_3','csales_hist_sales_4','csales_hist_sales_5','csales_hist_sales_6',
             'csales_hist_sales_7','csales_hist_sales_8','csales_hist_sales_9','csales_hist_sales_10','csales_hist_sales_11','csales_hist_sales_12'
) := list(rowShift(sales_cleaned, -1),rowShift(sales_cleaned, -2),rowShift(sales_cleaned, -3),rowShift(sales_cleaned, -4),rowShift(sales_cleaned, -5),rowShift(sales_cleaned, -6),rowShift(sales_cleaned, -7),rowShift(sales_cleaned, -8),rowShift(sales_cleaned, -9),rowShift(sales_cleaned, -10),rowShift(sales_cleaned, -11),rowShift(sales_cleaned, -12)), by=list(itemno)]


resultDT<-resultDT %>% arrange(itemno,week) %>% group_by(itemno) %>% mutate(weeknumber=ifelse(week<53,week,week-52))
resultDT1<-resultDT[,-c(1,3,5,7)]
write.table(resultDT1, "finally_result.txt")