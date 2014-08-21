#st<-stodvar[stodvar$synaflokkur==30 & stodvar$ar==2012,]
#kv<-lesa.kvarnir(st$synis.id, teg=2,
#                 col.names = c("synis.id", "aldur", "lengd",'kyn','kynthroski'), oracle = FALSE)
#dimnames(kv)<-list(1:nrow(kv),c('id','len','age', 'sex','maturity'))
#write.table(na.omit(kv), 'had.dat', sep='\t', row.names=FALSE)


had<-read.table('had.dat', header=TRUE)
head(had)
boxplot(len~age, had)
####################
# the easy way
####################
had.nls<-nls(len~Linf*(1-exp(-K*(age-t0))),
             data=had, start=list(Linf=80, K=0.2, t0=-1))
had.nls
# the hard way
##################
vonB.function<-function(para, data){
  age<-data$age
  len<-data$len
  Linf<-para[1]
  K<-para[2]
  t0<-para[3]
  lhat<-Linf*(1-exp(-K*(age-t0)))
  ss<-sum((len-lhat)^2,na.rm=TRUE)
  return(ss)
}
para<-c(80,0.2,-1)
vonB.function(para,had)
had.nlm<-nlm(vonB.function,para, had)
had.nlm


#####################3
# Adding a line to the plot
#########################
para<- had.nlm$estimate
vonBpred.function<-function(age, para){
  Linf<-para[1]
  K<-para[2]
  t0<-para[3]
  lhat<-Linf*(1-exp(-K*(age-t0)))
  return(lhat)
}
vonBpred.function(age=1:15, para=c(80,0.2,-1))

plot(c(0,15), c(0,100), type='n')
boxplot(len~age, had, add=TRUE)
lhat<-vonBpred.function(age=1:15, para=c(80,0.2,-1))
lines(1:15,lhat)

nls.estimate<- str(summary(had.nls)$para[1:3])
nls.predict<-vonBpred.function(age=1:15, para=nls.estimate)
lines(1:15,nls.predict, col='red',lwd=3)

###################################3
## Maturity
#####################
tmp<-had[had$maturity<=4,]
tmp$mat<-ifelse(tmp$maturity==1,0,1)
plot(tmp$age, tmp$mat)

init<-tapply(tmp$mat, list(tmp$age, tmp$mat), length)
hadMA.dat<-data.frame(age=as.numeric(dimnames(init)[[1]]), init)
colnames(hadMA.dat)<-c('age','imat','mat')
hadMA.dat[is.na(hadMA.dat)]<-0
hadMA.dat$pmat<-hadMA.dat$mat/(hadMA.dat$mat+hadMA.dat$imat)
hadMA.dat
plot(hadMA.dat$age, hadMA.dat$pmat, xlim=c(0,15), xaxt='n')
axis(side=1, labels=TRUE, at=0:15, cex.axis=0.5, mgp=c(0,0.2,0))
mat.glm<-glm(mat~age, tmp, family='binomial')
ag<-seq(0,15,0.1)
lines(ag, predict(mat.glm, data.frame(age=ag), type='response'))





mat.glm2<-glm(pmat~age, hadMA.dat, family='binomial')
lines(ag, predict(mat.glm2, data.frame(age=ag), type='response'), col='red')


gomp.min<-function(p,x){
	Asymp<-p[1]; lambda<-p[2]; K<-p[3]
	mat <-x$mat; total<-x$imat+x$mat; size<-x$age
	e <- Asymp*exp(-(lambda/K)*exp(-(K*size)))
	likk<-(-sum(mat * log(e) + (total - mat) * log(1 - e)))
        return(likk)
	
}

p2<-c(0.99,0.359,0.05)

gomp.min(p2,hadMA.dat)
gomp.est<-optim(p2,gomp.min,x=hadMA.dat)
p<-gomp.est$par
lines(ag,p[1]*exp(-(p[2]/p[3])*exp(-(p[3]*ag)))
      
 
