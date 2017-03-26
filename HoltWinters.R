# =====================================================================
# CSE487/587
# Author: sanjay natraj
# Email: sanjayna@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
#install.packages("forecast")
#install.packages("fpp")

# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO


#create empty vectors to append mae and filename
Listofmae = c()
compname = c()

## need to read the stocklist, and loop all files
### TO DO
filename <- list.files(path= "/gpfs/courses/cse587/spring2015/data/hw2/data", pattern="*.csv", full.names=T, recursive=FALSE)

for (files in filename)  {

# if file is not empty
if(file.info(files)[1]>0) {
  
  # read one csv file into variable (DO NOT EDIT)
  textData=read.csv(file=files, header=T)
  
  #Skip while the data file is empty
  numbofrows = nrow(textData)
  if(numbofrows < 754){
    next
  }
  #split the name of the file
  compname = c(compname, basename(files))
  
  # convert txt data to time-series data, in day unit (DO NOT EDIT)
  tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
 
  # define train data (DO NOT EDIT)
  trainData = window(tsData, end=c(2014,14))
  
  # define test data (DO NOT EDIT)
  testData = window(tsData, start=c(2014,15))
             
  # MAE row vector (DO NOT EDIT)
  MAE = matrix(NA,1,length(testData))
  
  # apply Holt Winters model 
   
  fitData = HoltWinters(trainData,beta = FALSE)
  
  #apply forecast(DO NOT EDIT)
  forecastData = forecast(fitData, h=length(testData))

  # calculate Mean Absolute Error 
  for(i in 1:length(testData))
  {
    MAE[1,i] = abs(forecastData$mean[i] - testData[i])
  }
  
  # this is the result you need for stock AAPL
  sum1 = sum(MAE[1,1:10])
  
  #append the sum of mae values in a list
  Listofmae = c(Listofmae,sum1)

}
  
}

#Sort the dataframe and ot the minimum 10
DataFrame = data.frame(compname,Listofmae)

sorted_list <- DataFrame[order(Listofmae), ]

min_val = head(sorted_list, n=10)

min_vals = na.omit(min_val)

print("HoltWinters Model")

print(min_vals)

# plot the top 10 minimum sum of MAE in 3 models respectively

jpeg(filename = "hw.jpeg", width = 1081, height = 664, units = "px", pointsize = 12, quality = 75,bg = "white")

plot(min_vals[1:10,2], main="HoltWinters model", ylab="MAE", xlab="Stock_Name")

text(min_vals[1:10,2], (sub("^([^.]*).*", "\\1", min_vals[1:10,1])),col="black", cex=1, pos=1)

lines(min_vals[1:10,2], lw = 2, col = "red")

dev.off()