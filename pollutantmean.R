## 2014-08-18
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/datasciencecoursera
## Assignment 1

## Part 1
pollutantmean <- function(directory, pollutant, id = 1:332) {    
    ## require that the directory exists in the current path
    if (!file.exists(directory)) { 
        stop(paste(c("invalid directory; '", directory, 
                     "' must be in the working directory")), collapse = "")
    }
    
    ## require that pollutant be a valid pollutant
    if (!pollutant == "sulfate" && !pollutant == "nitrate") {
        stop("invalid pollutant; must either be 'sulfate' or 'nitrate'")
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
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    data <- data.frame(
        matrix(vector(), 0, 4, 
               dimnames=list(c(), c("Date","sulfate","nitrate","ID"))))
    ## iterate over the id's and bind the rows from each file together
    for (i in id) {
        filename = paste(c("./", directory, "/", getNameForId(i)), collapse = "")
        
        ## ensure filename has appropriate # of columns
        fieldCount = count.fields(filename, sep = ",")
        row <- 0
        for (f in fieldCount) {
            if (!f == 4) { 
                stop(c("invalid # of fields; row: ", row, " has ", 
                       f, " fields in ", filename, 
                       " --> need 4 fields"), collapse = "") }
            row <- row + 1
        }
        
        ## append the data from the next file to the data frame
        data <- rbind(data, read.csv(filename, header = TRUE, sep = ",", 
                             comment.char = ""))
    }
    
    ## get rid of the incomplete cases
    complete <- complete.cases(data)
    complete_data <- data[complete, ]
    
    ## return the mean for the particular pollutant
    mean(complete_data[, pollutant])
}