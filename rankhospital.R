rankhospital <- function(state, outcome, num = "best") {
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
    if (!(num=="best" || num=="worst" || class(num)=="numeric"
          || class(num)=="integer")){
        return(stop(geterrmessage="invalid num"))
    }
    ## Return hospital name in that state with the given rank
    mst<-subset(data[,des[outcome]], data[,7]==state)
    hospst<-subset(data[,2], data[,7]==state)
    mstNum<-as.vector(mst[mst!="Not Available"],"numeric")
    hospstCl<-hospst[mst!="Not Available"]
    names(mstNum)<-hospstCl

    rnk<-mstNum[order(names(mstNum))]
    rnk<-rnk[order(rnk)]
    if (num=="best") num<-1
    if (num=="worst") num<-length(rnk)
    names(rnk[num])
}   