

#################################################################################################
## structure of this file #######################################################################
#################################################################################################
# main components:
# 1. load data
# 2. define function for iterating
# 3. define forecasting model by formula



library(plyr)
library(lubridate)
library(data.table)
library(stringr)




#################################################################################################
## 1. load data #################################################################################
#################################################################################################

# set your working directory!

setwd("C:/Users/martin.burger/Arbeitsordner/Data_Products/Simulation")

data_sample <- read.csv("finally_result.txt", sep=" ")


#subsample <- data_sample[data_sample$itemno == 'M-0202-06-640-00003',]
#data_sample <- subsample


data_sample <- data_sample[order(data_sample$itemno, data_sample$yearofweeknumber.x, data_sample$weeknumber),]
data_sample$weeknumber <- str_pad(data_sample$weeknumber, 2, pad = "0")





#################################################################################################
## 2. define function for iterating #############################################################
#################################################################################################


simulate <- function(formula,f,t) {
      
      # create new columns which are needed for prediction
  
      data_sampledt <<- as.data.table(data_sample)
      
      data_sampledt[,predict_year:= yearofweeknumber.x]
      data_sampledt[,predict_weeknumber:= weeknumber]
      data_sampledt[,weeknumber:= as.numeric(weeknumber)]
      data_sampledt[,yearofweeknumber.x:= as.numeric(yearofweeknumber.x)]
      
      exec <-paste("data_sampledt[,from_year:= shift(yearofweeknumber.x,",f,"), by = itemno]")
      eval(parse(text = exec))
      
      exec <-paste("data_sampledt[,from_weeknumber:= shift(weeknumber,",f,"), by = itemno]")
      eval(parse(text = exec))
      
      exec <-paste("data_sampledt[,to_year:= shift(yearofweeknumber.x,",t,"), by = itemno]")
      eval(parse(text = exec))
      
      exec <-paste("data_sampledt[,to_weeknumber:= shift(weeknumber,",t,"), by = itemno]")
      eval(parse(text = exec))
      
      # cummulate sales for the timespan predicted (dynamic)
      
      string <- ""
      
      for(e in f:t){
        
        string <- gsub(" ","",paste(string,"shift(sales, ", e , ") +"))
        
      }
      
      string <- substr(string, 1, nchar(string)-1)
      
      exec <- paste('data_sampledt[,sales_cum:=',string,', by = itemno]')
      eval(parse(text = exec))
      
      data_sampledt[,prediction := 0]
      
      exec <- gsub(" ","",paste("set(data_sampledt, as.integer(i),",ncol(data_sampledt),"L,prediction)"))
      
      # loop through all rows as fast as possible ;)
      
      for (i in 1:nrow(data_sampledt)) {
        
          data <- data_sampledt[i,]
          prediction <<- eval(parse(text = formula))
          eval(parse(text = exec))
          print(as.integer(i))
          
        }
        
}





#################################################################################################
## 3. define forecasting model by formula #######################################################
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
           0,9)


  write.table(data_sampledt, "test_result.txt")
