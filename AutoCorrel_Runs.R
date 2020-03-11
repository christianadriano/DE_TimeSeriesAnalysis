"
Test simulations for stationarity

"

library(stringr)
library(tseries)


root <-  "C://Users//Christian//Documents//GitHub//DE_TimeSeriesAnalysis//data//"
file_name = "avg_runtimes"
dt.set <- read.csv(str_c(root,file_name,".csv"))


add_dataframe <- function(df_tests, row, test, series.name){
  
  df_tests[row,"series.name"] = series.name
  df_tests[row,"test.name"] = test$method;
  df_tests[row,"statistic"] = test$statistic
  df_tests[row,"lag.parameter"] = test$parameter
  df_tests[row,"p.value"] = test$p.value
  
  return(df_tests)
}


test_stationary <- function(y,index,series.name){

  #--------
  #Augmented Dickey-Fuller Test
  test <- adf.test(y)
  df_tests <- add_dataframe(df_tests,index,test, series.name)
  
  #----
  #KPSS Test for Stationarity
  test <-kpss.test(y, null="Level")
  df_tests <- add_dataframe(df_tests,index+1,test,series.name)
  
  #----
  #Phillips-Perron Unit Root Test
  test <- pp.test(y)
  df_tests <- add_dataframe(df_tests,index+2,test,series.name)
  
  return (df_tests)
}


#----------------------------------------------------
#MAIN

series.names.list = colnames(dt.set)

#create the data.frame for all test statitics
#3 times because three tests per series.name
col.length = length(series.names.list) * 3;

df_tests <- data.frame(
  series.name=character(col.length),
  test.name=character(col.length),
  statistic=numeric(col.length),
  lag.parameter=integer(col.length),
  p.value=numeric(col.length),
  stringsAsFactors =FALSE
)

index = 1
for(i in c(1:length(series.names.list))){
  y <- dt.set[,i]
  y <- y[!is.na(y)]
  df_tests <- test_stationary(y,index,series.names.list[i])
  index = index + 3
}


#WRITE TO FILE
csv_file <- str_c(root,"stationarity_test_results",".csv")
write.table(df_tests, file = csv_file, sep = ",", 
            col.names = !file.exists(csv_file), 
            row.names = FALSE,
            append = F)
