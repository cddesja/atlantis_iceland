#
# Stock-Production Model
#

ssefcn <- function(beta, yrs, Y, I, plot.it = F){
	betatmp <- exp(beta)
	nyrs <- length(yrs)
	K <- betatmp[1]
  B0 <- betatmp[2]
	#B0 <- K
	r <- betatmp[3]
	q <- betatmp[4]
	B <- B0
	Btraj <- rep(NA, nyrs)
	Ihat <- rep(NA, nyrs)
	for(y in 1:nyrs){
		SY <- r*B*(1-B/K)
		Btraj[y] <- B
		Ihat[y] <- q*B
		B1 <- B + SY - Y[y]
		B <- ifelse(B1 > 0, B1, 0.001*B)
	}
	#print(Ihat)
	#print(I)
	#print(Btraj)	
	SSE <- sum((log(I) - log(Ihat))^2)
	if(plot.it){
		plot(yrs, I, ylim = c(0, max(I)))
		lines(yrs, Ihat, col = "blue")
	}
	#cat("SSE = ", SSE, "\n")
	return(SSE)
}
