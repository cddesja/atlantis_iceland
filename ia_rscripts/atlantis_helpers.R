# 
# - Convert tons to Mg N / x -
# unit could be area, volume, number of individuals
# 

tons_mgN <- function(tons, unit){
  mgN <- (tons * 1e9) / 20 / 5.7 / unit
  return(mgN)
}

#
# - Length Distributions from Hafro -
# Patched by Höski
#

skala_med_toldum <- function (lengdir, numer, tegund) 
{
  if (missing(numer)) 
    numer <- lesa.numer(synis.id = unique(lengdir$synis.id), 
                        tegund = tegund)
  numer$rat <- 1 + numer$fj.talid/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 1
  i <- match("rat", names(lengdir))
  if (!is.na(i)) 
    lengdir <- lengdir[, -i]
  lengdir$index <- paste(lengdir$synis.id,lengdir$tegund)
  numer$index <- paste(numer$synis.id,numer$tegund)
  l1 <- fjolst:::join(lengdir, numer[, c("index", "rat")], 
                      "index")
  i <- is.na(l1$rat)
  if (any(i)) 
    l1$rat[i] <- 1
  lengdir$fj.alls <- l1$fjoldi * l1$rat
  i <- match("index",names(lengdir))
  lengdir <- lengdir[,-i]
  return(lengdir)
}

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