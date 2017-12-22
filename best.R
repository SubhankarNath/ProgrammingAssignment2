best<-function(state,outcome){
r<-read.csv("outcome-of-care-measures.csv",colClasses="character")
s1<-r[7]          ## State column
s2<- c("heart attack","heart failure","pneumonia")
w1<- which(unique(s1)==state)
w2<- which(s2==outcome)
if(length(w1)==0)
stop("invalid state")
if(length(w2)==0)
stop("invalid outcome")
if(outcome=="heart attack"){
a<-r[11]     
} 
else if(outcome=="heart failure"){
a<-r[17]}
else{
a<-r[23]}
cn<-a      ## Outcome column
h1<-r[2]                ## Hospital column  
d<-which(s1==state)
m1<-suppressWarnings(as.numeric(cn[d,]))                ## derived outcome
f1<-h1[d,]                ## derived Hospital
df<-data.frame(m1,f1)
df1<-df[order(m1,f1),]
final<- as.character(df1[2][1,])
return(final)
}








