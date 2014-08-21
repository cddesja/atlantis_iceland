tmp<-read.table("np.txt", header=TRUE)
f <- tmp[which(tmp$s=='F'),]
tmp <- f
#tmp$FL  <- as.numeric(gsub(',','.',tmp$FL))

tmp$mat<-ifelse(tmp$m<=4,0,1)

j<-tmp[!is.na(tmp$m),]

k<-tapply(j$mat, list(j$l,j$mat),length)
k[is.na(k)]<-0
k<-data.frame(le=as.numeric(dimnames(k)[[1]]),k)
dimnames(k)<-list(1:nrow(k),c('le','imat','mat'))
k$pmat<-k$mat/(k$mat+k$imat)
f.glm<-glm(mat~l, data=tmp, family="binomial")
l50f<-f.glm$coefficients[1]/f.glm$coefficients[2]*(-1)

par(mfcol=c(1,1), mar=c(4,4,1,1))
#plot(c(0,10), c(0,1), type='n', xlab="", ylab="")
#points(k$le, k$pmat, pch=10)
#points(l50,0.5, col='red', pch=16)
#le<-seq(0,10,0.01)
#lines(le, predict(m.glm,data.frame(FL=le),type='response'))
#axis(side=1, labels=TRUE)
#mtext(side=1, 'age', line=2)
############
#par(new=TRUE)
plot(c(0,120), c(0,1), type='n', ylab="Proportion of mature fish", 
     xlab='Length (cm)',lwd=5, lty=9)
points(k$l, k$pmat, pch=16, col='black')
le<-seq(0,120,1)
lines(le, predict(m.glm,data.frame(l=le),type='response'), lwd=3,col='black' )
#points(l50f,0.5, col='red', pch=18)
axis(side=1, labels=TRUE)
axis(side=2, labels=TRUE)



#############################################
tmp<-read.table("np.txt", header=TRUE)
m <- tmp[which(tmp$s=='M'),]
tmp <- m
#tmp$FL  <- as.numeric(gsub(',','.',tmp$FL))

tmp$mat<-ifelse(tmp$m<=3,0,1)

j<-tmp[!is.na(tmp$m),]

k<-tapply(j$mat, list(j$l,j$mat),length)
k[is.na(k)]<-0
k<-data.frame(le=as.numeric(dimnames(k)[[1]]),k)
dimnames(k)<-list(1:nrow(k),c('le','imat','mat'))
k$pmat<-k$mat/(k$mat+k$imat)
m.glm<-glm(mat~l, data=tmp, family="binomial")
l50m<-m.glm$coefficients[1]/m.glm$coefficients[2]*(-1)

#par(mfcol=c(1,1))
#plot(c(0,10), c(0,1), type='n', xlab="", ylab="")
#points(k$le, k$pmat, pch=10)
#points(l50,0.5, col='red', pch=16)
#le<-seq(0,10,0.01)
#lines(le, predict(m.glm,data.frame(FL=le),type='response'))
#axis(side=1, labels=TRUE)
#mtext(side=1, 'age', line=2)
############
#par(new=TRUE)
#plot(c(0,120), c(0,1), type='n',yaxt='n', ylab="proportion of mature fish", 
     #xaxt='n',lwd=5, lty=9)
points(k$l, k$pmat, pch=21)

le<-seq(0,120,1)
lines(le, predict(m.glm,data.frame(l=le),type='response'),lty=8, lwd=3,col='black' )
#points(l50m,0.5, col='red', pch=18)

abline(v=61.33717,lty=3)
abline(v=70.37096,lty=3)
abline(h=0.5,lty=3)
legend('topleft', c("Males","Females"),lty=c(8,1),
       lwd=c(3,3),bty="n")




