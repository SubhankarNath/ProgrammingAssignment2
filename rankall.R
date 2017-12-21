rankall<-function(outcome,num="best"){
r1<-read.csv("outcome-of-care-measures.csv",colClasses="character")
r2<-r1[,2]   ## Hospital column
r3<-r1[,7]   ## State column
r4<- c("heart attack","heart failure","pneumonia")
r5<- which(r4==outcome)
if(length(r5)==0)
stop("invalid outcome")
if(outcome=="heart attack"){
r6<-r1[,11]}
else if(outcome=="heart failure"){
r6<-r1[,17]}
else{
r6<-r1[,23]}
r7<-suppressWarnings(as.numeric(r6))
df<-data.frame(r2,r7,r3)
df1<-df[order(r3,r7,r2),]
df1<-df1[complete.cases(df1),]
df1[,2]<-NULL
Lev<-levels(df1[,2])

FUN<-function(x,num="best"){
if(num=="best"){
final<-x[1]
}
else if (num=="worst"){
final<-x[length(x)]
}
else if (num>length(x)||num<1){
final<-NA
}
else{
final<-x[num]
}
}
subh<-NULL
d<-NULL
for(i in 1:length(Lev)){
subh<-subset(df1, df1[,2] == Lev[i])
subh<-subh[,1]
m111<- FUN(subh,num)
m111<-as.character(m111)
d[i]<-m111
}
amiperechi<- data.frame(d,Lev)
colnames(amiperechi)<-c("hospital","state")
return(amiperechi)
}




