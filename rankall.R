rankall <- function(outcome, num = "best") {
  ## Read outcome data
  options(warn = -1)
  raw_data <- read.csv("C:\\Users\\james\\OneDrive\\Documents\\Rworkspaces\\progAssignment3\\outcome-of-care-measures.csv", colClasses = "character") 
  dataSet1 <- as.data.frame(cbind(raw_data[,2],
                                  raw_data[,7],
                                  raw_data[,11],
                                  raw_data[,17],
                                  raw_data[,23]),
                            stringsAsFactors=FALSE)
  colnames(dataSet1) <- c("hospital", "state", "hattack", "hfailure", "pneumonia")
  dataSet1[,3:5]<-apply( dataSet1[,3:5],2,as.numeric)
  
  ## Check that state and outcome are valid
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")

  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack"){
    col = 3
    df <- subset(dataSet1, select = c(1, 2, col))
    df <- na.omit(df)
    df <- df[order(df$state, -df$hattack, df$hospital),]
  }
  else if (outcome == "heart failure"){
    col = 4
    df <- subset(dataSet1, select = c(1, 2, col))
    df <-na.omit(df)
    df <- df[order(df$state, -df$hfailure, df$hospital),]
    
  }
  else if (outcome == "pneumonia")
  {
    col = 5
    df <- subset(dataSet1, select = c(1, 2, col))
    df <-na.omit(df)
    df <- df[order(df$state, df$pneumonia, df$hospital),]
  }
  
  else stop("invalid outcome")
  
  ## Return a data frame with the hospital names and the
  df <-na.omit(df)
  View(df)
  ## (abbreviated) state name
}