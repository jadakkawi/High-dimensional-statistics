create_binVar_dataset <- function(var_name="AH", threshold=-1, 
                                  file_name="AirQuality.csv", folder="Data") {
  # Read in the CSV file
  air_quality_data <- read.table(paste(folder, '/', file_name, sep=""),
                                 dec=",", sep=";", 
                                 header=TRUE, stringsAsFactors=TRUE)
  
  # Select the 500 first samples
  air_quality_data <- air_quality_data[1:500,]
  
  # Remove column x & x.1
  air_quality_data <- subset(air_quality_data, select = -c(X, X.1))
  
  # Change missing values (-200 -> NA)
  air_quality_data[air_quality_data == -200] <- NA
  
  # Calculate the threshold value if necessary
  if (threshold == -1) {
    threshold <- min(air_quality_data[[var_name]]) + 
      (max(air_quality_data[[var_name]]) - min(air_quality_data[[var_name]]))/2
  }
  
  # Create the binary variable
  bin_var_name <- paste(var_name, "_bin", sep="")
  air_quality_data[bin_var_name] <- 0
  air_quality_data[bin_var_name][air_quality_data[var_name] >= threshold] <- 1
  
  air_quality_data <- select(air_quality_data, -!!var_name)
  
  # Save the new dataset
  write.table(air_quality_data, file=paste("Data/", bin_var_name, "_", file_name, sep=""), 
              row.names = TRUE, col.names = TRUE, sep=";", dec=".")
}

summary(air_quality_data)

create_binVar_dataset(var_name="CO.GT.", threshold=-1)
create_binVar_dataset(var_name="PT08.S1.CO.", threshold=-1)
create_binVar_dataset(var_name="NMHC.GT.", threshold=-1)
create_binVar_dataset(var_name="C6H6.GT.", threshold=-1)
create_binVar_dataset(var_name="PT08.S2.NMHC.", threshold=-1)
create_binVar_dataset(var_name="NOx.GT.", threshold=-1)
create_binVar_dataset(var_name="PT08.S3.NOx.", threshold=-1)
create_binVar_dataset(var_name="NO2.GT.", threshold=-1)
create_binVar_dataset(var_name="PT08.S4.NO2.", threshold=-1)
create_binVar_dataset(var_name="PT08.S5.O3.", threshold=-1)
create_binVar_dataset(var_name="T", threshold=-1)
create_binVar_dataset(var_name="RH", threshold=-1)
create_binVar_dataset(var_name="AH", threshold=-1)