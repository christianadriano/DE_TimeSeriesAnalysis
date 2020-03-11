"
Find data point after which the autocorrelations are below the 5%
of significance threshold.

"

library(stringr)
library(tseries)
install.packages("Rmisc")
library(Rmisc)

#------------------------------
#LOAD DATA
root <-  "C://Users//Christian//Documents//GitHub//DE_TimeSeriesAnalysis//data//"
file_name = "avg_runtimes"
dt.set <- read.csv(str_c(root,file_name,".csv"))

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
"Plot ACF model and find the highest non-significant lag"
find_threshold <- function(df_results, data, index, model.name, series.name){
  
  #plot without first data point because it is always 1 and
  #it distorts the plot visualization
  plot(data[2:length(data$acf)], main=str_c(model.name,".",series.name))
  
  #Discover the lag after which all values are significant
  #remove all significant points.
  df <- data.frame(lag=data$lag,acf=data$acf)
  df <- df[df$acf>0.025 | df$acf< -0.025,]
  threshold <-  max(df$lag)
  
  df_results <- add_dataframe(df_results,index,threshold, 
                              length(data$lag), model.name, series.name)
  
  return (df_results)
}

#plot(acf_data[acf_data<0.025 & acf_data>-0.025])

#------------------------------
#MAIN

series.names.list = colnames(dt.set)

#create the data.frame for all test statitics
#2 times because two ACF models per series.name
col.length = length(series.names.list) * 2;

df_results <- data.frame(
  series.name=character(col.length),
  auto.correl.model=character(col.length),
  threshold.point=integer(col.length),
  series.size=integer(col.length),
  stringsAsFactors =FALSE
)

plot_array <-  vector('list',length=2)

index <-  1
#i <- 1

for(i in c(1:length(series.names.list))){
  series <- dt.set[,i]
  series <- series[!is.na(series)]
  series.name <- series.names.list[i]
  
  #ACF
  model.name = "ACF"
  data <- acf(series,lag.max = length(series),
               xlab = "lag #",
               ylab = str_c(model.name),
               main=series.name, plot = FALSE)
  

  # plot_array[[index]] <- local({   
  #                           p <- plot(data[2:length(data$acf)], 
  #                                            main=str_c(model.name,".",series.name))
  #                           print(p)
  #                       })
  
  df_results <- find_threshold(df_results, data,index, 
                               model.name, series.name)
  index = index+1
 
  #PACF
  model.name = "PACF"
  data <- pacf(series, lag.max = length(series),
               xlab = "lag #",
               ylab = str_c(model.name),
               main=series.name, plot = FALSE)
  
  # plot_array[[index]]  <- plot(data[2:length(data$acf)], 
  #                                   main=str_c(model.name,".",series.name))
  #                             print(p)
  # })
  
  df_results <- find_threshold(df_results, data,index, 
                               model.name, series.name)
  
  index = index + 1
}

#multiplot(plot_array[1],plot_array[2],2)

#WRITE TO FILE
csv_file <- str_c(root,"threshold_results",".csv")
write.table(df_tests, file = csv_file, sep = ",", 
            col.names = !file.exists(csv_file), 
            row.names = FALSE,
            append = F)
