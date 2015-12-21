corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        
        cor_vec <- vector("numeric", length = 0)
        cor_lst <- list()
        j <- 0
        k <- 1
        
        for(i in 1:332){
                
                new_id <- formatC(i, flag = '0', width = 3)
                path <- paste(directory, new_id, sep = "/")
                filename <- paste(path, ".csv", sep = "")
                pol <- read.csv(filename)
                
                good <- complete.cases(pol)
                pol_good <- pol[good, ]
                result <- complete(directory, i)
                
                
                
                if( result$nobs > threshold){
                        
                        #print(result$nobs)
                        j <- 1
                        cor <- cor(pol_good$sulfate, pol_good$nitrate)
                        
                        cor_lst[k] <- cor
                        
                        k <- k+1
                        
                }
                
                
        }# end of for
        
        #print(j)
        if(j == 1){
                cor_vec <- as.numeric(cor_lst)
                
        }
        
        else{
                cor_vec
                
        }
        
        #cor_vec
        
}