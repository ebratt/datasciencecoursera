## 2014-08-18
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/datasciencecoursera
## Assignment 1

## Part 2
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
    
    ## require that the directory exists in the current path
    if (!file.exists(directory)) { 
        stop(paste(c("invalid directory; '", directory, 
                     "' must be in the working directory")), collapse = "")
    }
    
    ## require that id be between 1 and 332
    if (min(id) < 1)   { stop("invalid id; floor must be greater than 0") }
    if (max(id) > 332) { stop("invalid id; ceiling must be less than 333") }
    
    ## returns a string representing the filename as three characters 
    ## concatenated with .csv (ex, if id == 1 then it returns 
    ## 001.csv; if id == 22 then it returns 022.csv, and so on.)
    ## assumes that the id has no more than 3 digits
    getNameForId <- function(id) {
        paste(c(rep("0", 3 - nchar(id)), id, ".csv"), collapse = "")
    }
    
    ## create an empty vector to store the result
    return_vector <- data.frame(
        matrix(vector(), 0, 2, dimnames=list(c(), c("id", "nobs"))))
    ## iterate over the id's and check for consistency
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
        
        ## put the summary data into a data frame
        summary_data <- data.frame(id = i, nobs = sum(complete.cases(data)))
        return_vector <- rbind(return_vector, summary_data)
    }
    return_vector
}
