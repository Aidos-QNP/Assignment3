
best <- function(state, outcome) {
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    st<-data[,"State"]
    
    if(sum(st==state)==0){
        return(stop(geterrmessage="invalid state"))
    }
    
    des<-c("heart attack"=11, "heart failure"=17,"pneumonia"=23)
    
    if(sum(names(des)==outcome)==0){
        return(stop(geterrmessage="invalid outcome"))
    }
    ##print("Good!")
    ## Check that state and outcome are valid
    
    ## d2<-tapply(data[,11], data[,7], min)
    ## as.list(d2)[[state]]
    ## hosp<-tapply(data[,2],data[,7], 
    ## subset(data[,2], data[,11]==)
    
    
    mst<-subset(data[,des[outcome]], data[,7]==state)
    hospst<-subset(data[,2], data[,7]==state)
    names(mst)<-hospst
    
    mstNum<-as.numeric(mst)
    mstNum<-mstNum[!is.na(mstNum)]
    
    hospVec<-mst[mst==min(mstNum)]
    if (length(hospVec)>1) sort(hospVec)
    names(hospVec)[1]
    ## subset(data[,2], (data[,11]==min(mst) & data[,7]==state))    
    
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}   