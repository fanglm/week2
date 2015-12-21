complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        nobs <- vector("integer", length = length(id))
        j <- 1
        
        for(i in id){
                
                new_id <- formatC(i, flag = '0', width = 3)
                path <- paste(directory, new_id, sep = "/")
                filename <- paste(path, ".csv", sep = "")
                pol <- read.csv(filename)
                
                good <- complete.cases( pol)
                
                nobs[j] <- sum(good)
                #print(nobs[j])
                j <- j + 1
                
        }
        
        result <- data.frame(id, nobs)
        colnames(result) <- c("id", "nobs")
        #print(class(result))
        result
        
        
}