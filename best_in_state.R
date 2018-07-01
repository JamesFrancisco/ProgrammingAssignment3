best <- function(state, outcome) {
  ## Read outcome data
  options(warn = -1)
  raw_data <- read.csv("C:\\Users\\james\\OneDrive\\Documents\\Rworkspaces\\progAssignment3\\outcome-of-care-measures.csv", colClasses = "character") 
  dataSet1 <- as.data.frame(cbind(raw_data[,2],
                                  raw_data[,7],
                                  raw_data[,11],
                                  raw_data[,17],
                                  raw_data[,23]),
                            stringsAsFactors=FALSE)
  colnames(dataSet1) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  dataSet1[,3:5]<-apply( dataSet1[,3:5],2,as.numeric)
  
  hist(dataSet1[,3], breaks = 100, main = "30 Day mortality rate for heart attack")
  hist(dataSet1[,4], breaks = 100, main = "30 Day mortality rate for heart failure")
  hist(dataSet1[,5], breaks = 100, main = "30 Day mortality rate for pneumonia")
  if (!state %in% dataSet1[,"state"])
    stop("invalid state")
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  else 
    subData1 <- subset(dataSet1, (state == state))
  ##subData1[,3:5]<-apply( subData1[,3:5],2,as.numeric)
  
  cleandata<-na.omit(subData1)
  if (outcome == "heart attack"){
    col = 3
  }
  else if (outcome == "heart failure"){
    col = 4
  }
  else if (outcome == "pneumonia")
    {
    col = 5
  }
  else stop("invalid outcome")
  df = cleandata[cleandata$state == state, c(1, col)]
  ## Return

  df[which.min(df[, 2]), 1]
  
  ## Rate
}