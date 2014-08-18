## 2014-08-18
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/datasciencecoursera
## Assignment 1

## Part 3
corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## set id to a vector from 1:332
    id <- 1:332
    
    ## require that the directory exists in the current path
    if (!file.exists(directory)) { 
        stop(paste(c("invalid directory; '", directory, 
                     "' must be in the working directory")), collapse = "")
    }
    
    ## require that threshold not be less than 0
    if (threshold < 0) { stop("invalid threshold; must not be less than 0") }
    
    ## returns a string representing the filename as three characters 
    ## concatenated with .csv (ex, if id == 1 then it returns 
    ## 001.csv; if id == 22 then it returns 022.csv, and so on.)
    ## assumes that the id has no more than 3 digits
    getNameForId <- function(id) {
        paste(c(rep("0", 3 - nchar(id)), id, ".csv"), collapse = "")
    }
    
    ## create an empty vector to append to
    return_vector <- vector(mode = "double", length = 0)
    
    ## iterate over the id's and bind the rows from each file together
    for (i in id) {
        filename <- paste(c("./", directory, "/", getNameForId(i)), collapse = "")
        
        ## ensure filename has appropriate # of columns
        fieldCount <- count.fields(filename, sep = ",")
        row <- 0
        for (f in fieldCount) {
            if (!f == 4) { 
                stop(c("invalid # of fields; row: ", row, " has ", 
                       f, " fields in ", filename, 
                       " --> need 4 fields"), collapse = "") }
            row <- row + 1
        }
        
        ## get the raw data for the filename
        data <- read.csv(filename, header = TRUE, sep = ",", comment.char = "")
        complete <- complete.cases(data)
        complete_cases <- data[complete, ]
        if (NROW(complete_cases) > threshold) {
            icorr <- cor(complete_cases$sulfate, 
                         complete_cases$nitrate)
            return_vector <- c(return_vector, icorr)
        }
    }
    return_vector
}
