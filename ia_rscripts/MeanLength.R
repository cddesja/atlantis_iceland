tmp<-read.table('../Data/tusk.alkeys.igfs', header=FALSE, skip=1)
colnames(tmp)<-c('yr','step','area','age','label','no')
head(tmp)
agg<-read.table(paste(mpath,'/Aggfiles/len.agg', sep=''))
colnames(agg)<-c('label','mid','max')
age<-read.table(paste(mpath,'/Aggfiles/age.agg', sep=''))
colnames(age)<-c('age','age2')
tmp<-merge(tmp,agg)
tmp<-merge(tmp,age)
head(tmp)
kk<-tapply(tmp$no*tmp$max,tmp$age2, sum)/tapply(tmp$no, tmp$age2,sum)
no<-tapply(tmp$no,tmp$age2, sum)
dat<-na.omit(data.frame(age=as.numeric(names(kk)),le=kk))
vb.nls<-nls(le~Linf*(1-exp(-K*(age-t0))), data=dat, start=list(Linf=200,K=0.1,t0=0))

####
#Check the sd
########
for(i in unique(sort(tmp$age2))){
  k<-tmp[tmp$age2==i,]
  kk<-sd(rep(k$max, k$no))
  cat(i, 'sd=',kk,'\n')
}



ml<-data.frame(age=paste('age',1:9,sep=''),no=ifelse(no<2,2,no),  le=round(predict(vb.nls, data.frame(age=1:9)),2))
#ml$no<-ifelse(no<2,2,no)
ml$sd=round(ml$le*0.15,3)
ml

par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(c(0,9), c(0,180), type='n',xlab='Age (Years)', ylab='Length (cm)', main= )
lines(1:9, ml$le, lwd=3)
lines(1:9, ml$le+ml$sd*1.96, lty=8, lwd=3)
lines(1:9, ml$le-ml$sd*1.96, lty=8, lwd=3)
points(dat$age, dat$le, pch=16, cex=1.1)


ml.sur<-NULL
for(yr in 2005:2011){
  le.row<-dim(ml)[[1]]
  k<-data.frame(year=rep(yr, le.row),step=rep(1, le.row), area=rep('area1',le.row))
  k<-cbind(k, ml)
  ml.sur<-rbind(ml.sur,k)
}
write.table(ml.sur, 'SurveyML.dat', sep='\t', col.names=FALSE, row.names=FALSE, quote=FALSE)

######################
#Commercial catches
######################

tmp<-read.table('../Data/tusk.alkeys.comm', header=FALSE, skip=1)
colnames(tmp)<-c('yr','step','area','age','label','no')
head(tmp)
agg<-read.table(paste(mpath,'/Aggfiles/len.agg', sep=''))
colnames(agg)<-c('label','mid','max')
age<-read.table(paste(mpath,'/Aggfiles/age.agg', sep=''))
colnames(age)<-c('age','age2')
tmp<-merge(tmp,agg)
tmp<-merge(tmp,age)
head(tmp)
kk<-tapply(tmp$no*tmp$max,tmp$age2, sum)/tapply(tmp$no, tmp$age2,sum)
no<-tapply(tmp$no,tmp$age2, sum)
dat<-na.omit(data.frame(age=as.numeric(names(kk)),le=kk))
vb.nlsc<-nls(le~Linf*(1-exp(-K*(age-t0))), data=dat, start=list(Linf=200,K=0.1,t0=0))


ml<-data.frame(age=paste('age',1:9,sep=''),no=ifelse(no<2,2,no),  le=round(predict(vb.nls, data.frame(age=1:9)),2))
#ml$no<-ifelse(no<2,2,no)
ml$sd=round(ml$le*0.15,3)
ml

#par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(c(0,9), c(0,180), type='n',xlab='Age (Years)', ylab='Length (cm)')
lines(1:9, ml$le, lwd=3)
lines(1:9, ml$le+ml$sd*1.96, lty=8, lwd=3)
lines(1:9, ml$le-ml$sd*1.96, lty=8, lwd=3)
points(dat$age, dat$le, pch=16, cex=1.1)

ml.comm<-NULL
for(yr in 1980:2011){
  le.row<-dim(ml)[[1]]
  k<-data.frame(year=rep(yr, le.row),step=rep(1, le.row), area=rep('area1',le.row))
  k<-cbind(k, ml)
  ml.comm<-rbind(ml.comm,k)
}
write.table(ml.comm, 'CommML.dat', sep='\t', col.names=FALSE, row.names=FALSE, quote=FALSE)
