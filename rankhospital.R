rankhospital<-function(state,outcome,num="best"){
a1<- read.csv("outcome-of-care-measures.csv", colClasses="character")
a2<-a1[7]  ## State Column
s1<- c("heart attack","heart failure","pneumonia")
w1<- which(unique(a2)==state)
if(length(w1)==0)
stop("invalid state")
w2<- which(s1==outcome)
if(length(w2)==0)
stop("invalid outcome")

if(outcome=="heart attack"){
a4<-a1[11]}  ## Column for heart attack
else if(outcome=="heart failure"){
a4<- a1[17]}  ## Column for heart failure
else{
a4<-a1[23]}   ## Column for pneumonia  

cn<-a4    ## Outcome column
a3<-which(a2==state)
y1<-a2[a3,]
a5<-suppressWarnings(as.numeric(cn[a3,]))    
hospitalname<- a1[2]  ## Hospital column
h1<-hospitalname[a3,]   

t1<-data.frame(a5,h1)
rownames(t1)<-c()
t2<- t1[order(a5, h1),]
t3<-t2[complete.cases(t2),]
t8<-t3[2]

if(num==1||num=="best"){
final<-t8[1,]
}
else if (num=="worst"||num==nrow(t8)){
final<-t8[nrow(t8),]
}
else if (num>1 && num<nrow(t8)){
final<-t8[num,]
}
else{
final<-NA
}
return(as.character(final))
}




