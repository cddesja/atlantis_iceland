#
# Length Distributions from Hafro
#

Skala.med.toldum <- function (lengdir, numer, tegund) 
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