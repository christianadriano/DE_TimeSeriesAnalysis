"
Test simulations for stationarity

"

library(stringr)
library(ggplot2)


root <-  "C://Users//Christian//Documents//GitHub//DE_TimeSeriesAnalysis//data//"
file_name = "avg_runtimes"
dt.set <- read.csv(str_c(root,file_name,".csv"))

head(dt.set)


 

library(tseries)

add_dataframe <- function(df_tests, row, test){
  
  df_tests[row,"test.name"] = test$method;
  df_tests[row,"statistic"] = test$statistic
  df_tests[row,"lag.parameter"] = test$parameter
  df_tests[row,"p.value"] = test$p.value
  
  return(df_tests)
}


test_stationary <- function(y,series.name){
  
  df_tests <- data.frame(
    test.num = numeric(3),
    test.name=character(3),
    statistic=numeric(3),
    lag.parameter=integer(3),
    p.value=numeric(3),
    series.name=character(3),
    stringsAsFactors =FALSE
  )
  
  df_tests$series.name = series.name
  
  #--------
  #Augmented Dickey-Fuller Test
  test <- adf.test(y)
  df_tests <- add_dataframe(df_tests,1,test)
  
  #----
  #KPSS Test for Stationarity
  test <-kpss.test(y, null="Level")
  df_tests <- add_dataframe(df_tests,2,test)
  
  #----
  #Phillips-Perron Unit Root Test
  pp_test <- pp.test(y)
  df_tests <- add_dataframe(df_tests,3,test)
  
  return (df_tests)
}

series.names.list = c(data.)

#Plot timeseries

#wrangle the dataset
y= dt.set$Berlin_a[!is.na(dt.set$Berlin_a)]
x=c(1:length(y))
df = data.frame(x,y)

#ggplot(data=df,aes(x,y)) +geom_point()

#-----------
"ACF model"
lag.length = length(y)
acf_data <- acf(y,lag.max = lag.length,
                xlab = "lag #", ylab = 'ACF',main='Berlin_a', 
                plot = FALSE)

#PLot without the first correlation value (which is One by default)
plot(acf_data[2:length(acf_data$acf)])


#----------
"Partial Auto-correlation model"
pacf_data <- pacf(y,lag.max = length(y),
                  xlab = "lag #", ylab = 'ACF',main='Berlin_a',
                  plot = FALSE)

plot(pacf_data[2:length(pacf_data$acf)])

plot(acf_data[acf_data<0.025 & acf_data>-0.025])

