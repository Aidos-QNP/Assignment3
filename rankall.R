rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    des<-c("heart attack"=11, "heart failure"=17,"pneumonia"=23)
    
    if(sum(names(des)==outcome)==0){
        return(stop(geterrmessage="invalid outcome"))
    }
    if (!(num=="best" || num=="worst" || class(num)=="numeric"
          || class(num)=="integer")){
        return(stop(geterrmessage="invalid num"))
    }
    ## For each state, find the hospital of the given rank
    mAl<-data[,des[[outcome]]]
    mNum<-as.vector(mAl[mAl!="Not Available"],"numeric")
    hospCl<-data[,2][mAl!="Not Available"]
    names(mNum)<-hospCl
    
     tfunc<-function(x){
        if (num=="best") num<-1
        if (num=="worst") num<-length(x)
        rnk<-x[order(names(x))]
        rnk[order(rnk)][num]
    }
    stCl<-data[,7][mAl!="Not Available"]
    rnkLst<-tapply(mNum, stCl, tfunc, simplify = FALSE)
    
    hospN<-vector("character",0)
    for (i in seq_along(rnkLst)) {
       hospN<-c(hospN, names(rnkLst[[i]]))
   } 
    ## Return a data frame with the hospital names and the state name    
    data.frame(hospital=hospN,state=names(rnkLst))
}   