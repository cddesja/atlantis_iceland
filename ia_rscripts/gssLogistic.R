gss<-read.table('gss.txt', header=TRUE, sep='\t')
head(gss)
plot(gss$yr,gss$le, xlab='year', ylab='length')
exp(3.1487)
plot(gss$yr, gss$age, xlab='year', ylab='length')
lw<-lm(log(le)~log(age),gss)
summary(gss)
plot(gss$age, gss$le, xlab='lenght', ylab='age', lty=8)
lines(1:66,
      exp(3.1487)*(1:66)^0.2356, lwd=3, col='red')

gss.nls<-nls(le~Linf*(1-exp(-K*(age-t0))),
             data=gss, start=list(Linf=67, K=0.2, t0=-1))
gss.nls
boxplot(le~age, gss, add=TRUE)

para<-gss.nls$estimate
VonBpred.function<-function(age, para){
  Linf<-para[1]
  K<-para[2]
  t0<-para[3]
  lhat<-Linf*(1-exp(-K*(age-t0)))
  return(lhat)
}

lhat<-VonBpred.function(age=1:65,para=c(54.32816,0.06886,-9.50048))
lines(1:65,lhat, col="blue", lwd=3)
min(gss$age)

nls.estimate<-summary(gss.nls)$para[1:3]
nls.predict<-VonBpred.function(age=1:65, para=nls.estimate)
lines(1:65,nls.predict, col='green',lwd=3)

head(gss)
table(gss$maturity)

tmp<-gss[gss$maturity<=4,]
head(tmp)
tmp$mat<-ifelse(tmp$maturity==1,0,1) #if 0 then not mature, if 1 then mature
plot(tmp$age, tmp$mat)

init<-tapply(tmp$mat, list(tmp$age, tmp$mat), length)
gssMA.dat<-data.frame(age=as.numeric(dimnames(init)[[1]]), init)
colnames(gssMA.dat)<-c('age','imat','mat')
gssMA.dat[is.na(gssMA.dat)]<-0
gssMA.dat$pmat<-gssMA.dat$mat/(gssMA.dat$mat+gssMA.dat$imat)
gssMA.dat
plot(gssMA.dat$age, gssMA.dat$pmat, xlim=c(0,max(gssMA.dat$age)), xaxt='n')
axis(side=1, labels=TRUE, at=0:max(gssMA.dat$age), cex.axis=0.5, mgp=c(0,0.2,0))
mat.glm<-glm(mat~age, tmp, family='binomial')
ag<-seq(0,max(gssMA.dat$age),0.1)
lines(ag, predict(mat.glm, data.frame(age=ag), type='response'))

mat.glm2<-glm(pmat~age, gssMA.dat, family='binomial')
lines(ag, predict(mat.glm2, data.frame(age=ag), type='response'), col='red')


gomp.min<-function(p,x){
  Asymp<-p[1]; lambda<-p[2]; K<-p[3]
  mat <-x$mat; total<-x$imat+x$mat; size<-x$age
  e <- Asymp*exp(-(lambda/K)*exp(-(K*size)))
  likk<-(-sum(mat * log(e) + (total - mat) * log(1 - e)))
  return(likk)
  
}

p2<-c(0.99,0.359,0.05)

gomp.min(p2,gssMA.dat)
gomp.est<-optim(p2,gomp.min,x=gssMA.dat)
p<-gomp.est$par
lines(ag,p[1]*exp(-(p[2]/p[3])*exp(-(p[3]*ag))))

###############
# maturity

gss$mat<-ifelse(gss$maturity==1,0,1) 
fit<-glm(mat ~ le,data=gss, family='binomial')
pred<-predict(fit,data.frame(le=1:65), type='response')
le.pred<-tapply(gss$mat,gss$le,mean)
plot(1:65,pred, xlab='length', ylab='maturity')
lines(1:65,pred)
l_50 <- max(which(pred < 0.5))
l_50
abline(h=0.5)
abline(v=33)
