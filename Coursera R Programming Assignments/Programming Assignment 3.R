setwd("D:/Coursera/R/Programming Assignment 3")
best <- function(state, outcome) {
  best_csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df   <- as.data.frame(cbind(best_csv[, 2],   # hospital column
                              best_csv[, 7],   # state column
                              best_csv[, 11],  # heart attack column
                              best_csv[, 17],  # heart failure column
                              best_csv[, 23]), # pneumonia column
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% df[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    st <- which(df[, "state"] == state)
    ts <- df[st, ]
    oi <- as.numeric(ts[, eval(outcome)])
    min_value <- min(oi, na.rm = TRUE)
    result  <- ts[, "hospital"][which(oi == min_value)]
    output  <- result[order(result)]
  }
  return(output)
}
best("SC", "heart attack")  
best("NY", "pneumonia")
best("AK", "pneumonia")



rankhospital <- function(state, outcome, rank = "best"){
  rankhospital_csv <- read.csv("outcome-of-care-measures.csv", 
                                       colClasses = "character")
  df   <- as.data.frame(cbind(rankhospital_csv[, 2],  # hospital column
                              rankhospital_csv[, 7],  # state column
                              rankhospital_csv[, 11],  # heart attack column
                              rankhospital_csv[, 17],  # heart failure column
                              rankhospital_csv[, 23]), # pneumonia column
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if (!state %in% df[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    st <- which(df[, "state"] == state)
    ts <- df[st, ]            
    ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
    ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
    output <- ts[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      st <- which(df[, "state"] == state)
      ts <- df[st, ]    
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
      output <- ts[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}


rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)


rankall <- function(outcome, num = "best"){''
  rankall_csv <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
  df   <- as.data.frame(cbind(rankall_csv[, 2],  # hospital column
                              rankall_csv[, 7],  # state column
                              rankall_csv[, 11],  # heart attack column
                              rankall_csv[, 17],  # heart failure column
                              rankall_csv[, 23]), # pneumonia column
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  df[, eval(outcome)] <- as.numeric(df[, eval(outcome)])
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    by_state <- with(df, split(df, state))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(df, split(df, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(df, split(df, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
