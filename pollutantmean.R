pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
 
  total <- 0
  
  
  for(i in id){
  
      new_id <- formatC(i, flag = '0', width = 3)
      path <- paste(directory, new_id, sep = "/")
      filename <- paste(path, ".csv", sep = "")
      pol <- read.csv(filename)
  
      
      
      
      pol_vec <- pol[[pollutant]]
      good <- complete.cases( pol_vec )
      
      
     
      junzhi <- pol_vec[good]
    
      total <- c(total,junzhi)
      #print(total)
      
  }
      
      
      sum(total) / (length(total)-1)
      
}