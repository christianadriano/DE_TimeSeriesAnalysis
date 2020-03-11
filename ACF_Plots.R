"
Find data point after which the autocorrelations are below the 5%
of significance threshold.

"

library(stringr)
library(tseries)

#------------------------------
#LOAD DATA
root <-  "C://Users//Christian//Documents//GitHub//DE_TimeSeriesAnalysis//data//"
file_name = "avg_runtimes"
dt.set <- read.csv(str_c(root,file_name,".csv"))

#-------------------------------

#Plot time series

#wrangle the dataset
y= dt.set$Frankfurt_b[!is.na(dt.set$Frankfurt_b)]
x=c(1:length(y))
df = data.frame(x,y)


#--------------------------------

add_dataframe <- function(df_results, row, threshold, series.size,
                          model.name, series.name){
  
  df_results[row,"series.name"] = series.name
  df_results[row,"auto.correl.model"] = model.name;
  df_results[row,"threshold.point"] = threshold;
  df_results[row,"series.size"] = series.size;
  
  
  return(df_results)
}


#-------------------------------
acf_model <- function(y, series.name){
  
  "ACF model"
  lag.length = length(y)
  acf_data <- acf(y,lag.max = lag.length,
                  xlab = "lag #", ylab = str_c('ACF ',series.name),
                  main=series.name, 
                  plot = FALSE)
  
  #Plot without the first correlation value (which is One by default)
  plot(acf_data[2:length(acf_data$acf)])
  
  return(acf_data);
}

#-------------------------------
"Partial Auto-correlation model"
pacf_model <- function(df_results, series, series.name){
  
  data <- pacf(series,
              lag.max = length(series),
              xlab = "lag #",
              ylab = str_c('PACF ',series.name),
              main=series.name,
              plot = FALSE)
  
  #plot without first data point because it is always 1 and
  #it distorts the plot visualization
  plot(data[2:length(data$acf)])
  
  #Discover the lag after which all values are significant
  #remove all significant points.
  df <- data.frame(lag=data$lag,pacf=data$acf)
  df <- df[df$pacf>0.025 | df$pacf< -0.025,]
  threshold <-  max(df$lag)
  
  df_results <- add_dataframe(df_results,index,threshold, 
                              length(series), model.name,series.name)
  
  return (df_results)
  
}

plot(acf_data[acf_data<0.025 & acf_data>-0.025])

#------------------------------
#MAIN

series.names.list = colnames(dt.set)

#create the data.frame for all test statitics
#3 times because three tests per series.name
col.length = length(series.names.list) * 3;

df_results <- data.frame(
  series.name=character(col.length),
  auto.correl.model=character(col.length),
  threshold.point=integer(col.length),
  series.size=integer(col.length),
  stringsAsFactors =FALSE
)

index = 1
i=1
for(i in c(1:length(series.names.list))){
  series <- dt.set[,i]
  series <- series[!is.na(series)]
  df_results <- pacf_model(series,index,series.names.list[i])
  index = index + 3
}


#WRITE TO FILE
csv_file <- str_c(root,"stationarity_test_results",".csv")
write.table(df_tests, file = csv_file, sep = ",", 
            col.names = !file.exists(csv_file), 
            row.names = FALSE,
            append = F)
