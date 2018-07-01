rankhospital <- function(state, outcome, num = "best")
{
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

  if (!state %in% dataSet1[,"state"])
    stop("invalid state")
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  
  if (outcome == "heart attack"){
    col = 3
    df = dataSet1[dataSet1$state == state, c(1, col)]
    df <- df[order(df$hattack, df$hospital),]


  }
  else if (outcome == "heart failure"){
    col = 4
    df = dataSet1[dataSet1$state == state, c(1, col)]
    df <- df[order(df$hfailure, df$hospital),]

  }
  else if (outcome == "pneumonia")
  {
    col = 5
    df = dataSet1[dataSet1$state == state, c(1, col)]
    df <- df[order(df$pneumonia, df$hospital),]
  }
  
  else stop("invalid outcome")
  
  df <-na.omit(df)
  View(df)
  max1 = as.numeric(length(df$hospital))
  if (num == "Best")
    df[which.min(df[, 2]), 1]
  else if (num == "Worst"){
    print(num)
    print(max1)
    df[max1,1]
    }
  else if (num <= max1){

    df[num,1]
  }
  else
    print("NA")
  
  
}