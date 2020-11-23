
best <- function(state, outcome) {
    outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    st<-outcome[,"State"]
    c<-sapply(st, function(x)x==state)
    if(sum(c)==0){
        return(stop(geterrmessage="invalid state"))
    }
    
    des<-c("heart attack", "heart failure","pneumonia")
    c<-sapply(des, function(x)x==outcome)
    if(sum(c)==0){
        return(stop(geterrmessage="invalid outcome"))
    }
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}