best <- function(state, outcome) {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    if(sum(data[,7]==state)==0){
        return(stop(geterrmessage="invalid state"))
    }
    
    des<-c("heart attack"=11, "heart failure"=17,"pneumonia"=23)
    if(sum(names(des)==outcome)==0){
        return(stop(geterrmessage="invalid outcome"))
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    mst<-subset(data[,des[outcome]], data[,7]==state)
    hospst<-subset(data[,2], data[,7]==state)
    mstNum<-as.vector(mst[mst!="Not Available"],"numeric")
    hospstCl<-hospst[mst!="Not Available"]
    names(mstNum)<-hospstCl
    hospVec<-mstNum[mstNum==min(mstNum)]
    if (length(hospVec)>1) hospVec<-hospVec[sort(names(hospVec))]
    names(hospVec)[1]
}   