#' 
#' Convert tons to mg N / x 
#' 
#' The funtion converts tons (kg) to mg N per unit could be area, volume, number of individuals
#' 
#' @param tons metric tons
#' @param unit unit, should be area, volume, or number of individuals
#' 
tons_mgN <- function(tons, unit){
  mgN <- (tons * 1e9) / 20 / 5.7 / unit
  return(mgN)
}

#'
#'Length Distributions from Hafro 
#'Patched by Höski
#'
#'@param lengdir length distribution
#'@param numer number per length
#'@param tegund species ID from Háfro
#'
#'
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

#'
#'Stock-Production Model
#'
#' The function runs a stock-production model
#' 
#'@param beta vector of starting values for K, B0, r, and q
#'@param yrs number of years
#'@param I abundance index
#'@param plot.it Whether a plot should be generate (default = FALSE).
#'
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

#'
#'Initial conditions data template
#' 
#'Fill in initial conditions data for Atlantis
#'
#' @param nbox Number of boxes in the model.
#' @param data Matrix or data.frame with two columns. First column corresponds to the box ID and the second to the data to be input.
#' @param numlayer Number of layers for 3D tracer.
#' @param varname Specify tracer name. Defaults to NULL. 
#' 
init_cond_data <- function(nbox, data, numlayer, varname = NULL){
  tmp <- matrix(nrow = nbox, ncol = numlayer)
  tmp[data[,1],1] <- data[,2]
  tmp[is.na(tmp)] <- "_"
  if(numlayer == 1){
    tmp <- paste(apply(tmp, 1, paste, collapse=", "), collapse=", ")
  } else tmp <- paste(apply(tmp, 1, paste, collapse=", "), collapse=",\n")
  if(is.null(varname)){
    cat(tmp, " ;", sep ="")
    } else cat(varname, " =\n", tmp, " ;", sep ="")
}        

#'
#' Beverton-Holt Equation
#'
#'@param beta a vector containing starting values for alpha and K. 
#'@param S Spawning stock biomass (tons)
#'@param R Recruitment (tons)
ssefcn<-function(beta,S,R){
alpha<-beta[1]
K<-beta[2]
Rhat<-alpha*S/(1+S/K)             # For B-H
SSE<-sum((log(R)-log(Rhat))^2)
return(SSE)
}


#'
#' Atlantis Beverton-Holt
#' 
#' This function returns alpha and beta from the Beverton-Holt recruitment model in Atlantis units
#' 
#' @param R Recruitment (tons)
#' @param SSB Spawning stock biomass
#' @param recrwt Weight of a recruit (grams)
#' @details Only requirement is that R and SSB must be in the same units. 
#' 
bh_atl <- function(R, S, recrwt, plot.it = TRUE){
  K0<-min(S)/2
  alpha0<-mean(R)/K0
  fit<-nlm(ssefcn,c(alpha0,K0),S,R)
  alpha<-fit$estimate[1]
  K<-fit$estimate[2]
  atlantis_alpha = alpha*K*1e6/recrwt
  atlantis_beta = tons_mgN(K,1)
  if(plot.it){
    plot(S,R,xlim=c(0,max(S)),ylim=c(0,max(R)),xlab="SSB",ylab="Recruitment")
    Srange<-(0:120)*30
    Rrange<-alpha0*Srange/(1+Srange/K0) # For B-H
    lines(Srange,Rrange,lwd=2, col='green') 
    Rrange <- alpha*Srange/(1+Srange/K) # For B-H
    lines(Srange,Rrange,lwd=2,col="red")
    }
  params <- data.frame(alpha = alpha, K = K, atl_alpha = atlantis_alpha, atl_beta = atlantis_beta)
  return(params)
}