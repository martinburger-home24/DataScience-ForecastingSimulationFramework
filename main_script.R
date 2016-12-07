


#################################################################################################
## structure of this file #######################################################################
#################################################################################################
# main components:
# 1. load and prepare data and result table
# 2. define looping function
# 3. define forecasting formula



library(plyr)
library(lubridate)
library(data.table)
library(stringr)



#################################################################################################
## 1. load and prepare data and result table ####################################################
#################################################################################################

# set your working directory!

setwd("C:/Users/martin.burger/Arbeitsordner/Data_Products/Simulation")

data_sample <- read.csv("finally_result.txt", sep=" ")


#subsample <- data_sample[data_sample$itemno == 'M-0202-06-640-00003',]
#data_sample <- subsample


data_sample <- data_sample[order(data_sample$itemno, data_sample$yearofweeknumber.x, data_sample$weeknumber),]
data_sample$weeknumber <- str_pad(data_sample$weeknumber, 2, pad = "0")


result_dtable <- data.table(itemno=rep("Char",nrow(data_sample)), 
                 predict_year=rep(0,nrow(data_sample)),
                 predict_week=rep(0,nrow(data_sample)),
                 from_year=rep(0,nrow(data_sample)),
                 from_week=rep(0,nrow(data_sample)),
                 to_year=rep(0,nrow(data_sample)),
                 to_week=rep(0,nrow(data_sample)),
                 sales=rep(0,nrow(data_sample)),
                 prediction=rep(0,nrow(data_sample)),
                 sales_orig=rep(0,nrow(data_sample))
                 )


#################################################################################################
## 2. define looping function ###################################################################
#################################################################################################

simulate <- function(formula,from,to) {
  
  for (i in 1:nrow(data_sample)) {
    
  #for (i in 1:1) {  
    
    if (data_sample[i + to,]$itemno == data_sample[i,]$itemno) {
    
    data <<- data_sample[i,]
    prediction <<- eval(parse(text = formula))
    

    
    # add time information 
    
    from_year <- data_sample[i,]$yearofweeknumber.x
    to_year <- data_sample[i + to,]$yearofweeknumber.x
    from_week <- data_sample[i,]$weeknumber
    to_week <- data_sample[i + to,]$weeknumber
    
    string <<- "sales <- data$sales"  
    
    for(e in 1:10){
      string <<- gsub(" ","",paste(string,"+ data$fut_sales_", e))
    }
    
    eval(parse(text = string))
    
    if (length(prediction)==0) {prediction <- NA}
    if (length(sales)==0) {sales <- NA}
    
    set(result_dtable,as.integer(i),1L,data$itemno)
    set(result_dtable,as.integer(i),2L,as.numeric(data$yearofweeknumber.x))
    set(result_dtable,as.integer(i),3L,as.numeric(data$weeknumber))
    set(result_dtable,as.integer(i),4L,from_year)
    set(result_dtable,as.integer(i),5L,from_week)
    set(result_dtable,as.integer(i),6L,to_year)
    set(result_dtable,as.integer(i),7L,to_week)
    set(result_dtable,as.integer(i),8L,sales)
    set(result_dtable,as.integer(i),9L,prediction)
    set(result_dtable,as.integer(i),10L,data$sales)
    
    print(i)
    
    }
    
  }
  
}
  

#################################################################################################
## 3. define forecasting formula ################################################################
#################################################################################################

simulate('
         ((data$csales_hist_sales_1 * 0.1) 
         + (data$csales_hist_sales_2 * 0.1) 
         + (data$csales_hist_sales_3 * 0.1) 
         + (data$csales_hist_sales_4 * 0.1) 
         + (data$csales_hist_sales_5 * 0.1) 
         + (data$csales_hist_sales_6 * 0.1)
         + (data$csales_hist_sales_7 * 0.1)
         + (data$csales_hist_sales_8 * 0.1)
         + (data$csales_hist_sales_9 * 0.05)
         + (data$csales_hist_sales_10 * 0.05)
         + (data$csales_hist_sales_11 * 0.05)
         + (data$csales_hist_sales_12 * 0.05)) * 10',
        0,10)











