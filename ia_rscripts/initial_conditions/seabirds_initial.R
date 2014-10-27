library("dplyr")

#
# Seabirds - initial conditions and biology parameters
#

com_mur <- 2590 * 1e3 *  1040 / 1e6
tb_mur <- 1512 * 1e3 * 1280 / 1e6
raz_bill <- 988 * 1e3 * 632 / 1e6
puf <- 7342 * 1e3 * 510 / 1e6
kit <- 1363 * 1e3 * 341 / 1e6
ful <- 4352 * 1e3 * 787 / 1e6

birds <- c("puffin", "thick-billed murre", "common murre", "razorbill", "kittiwake", "northern fulmar")
species <- c(puf, tb_mur, com_mur, raz_bill, kit, ful)
bird_init <- data.frame(birds, species)
cbind(birds,bird_init$species/sum(bird_init$species))

wts <- c(1040, 1280, 632, 510, 341, 787)
east <- c(31, 6, 1, 931, 83, 218)
south <- c(243, 9, 5, 4522, 142, 1088)
west <- c(787, 313, 606, 1064, 174, 1523)
northwest <- c(1190, 1067, 191, 160, 453, 870)
northeast <- c(339, 117, 139, 665, 511, 653)

#
# Biological parameters
#
birds_wl <- read.csv("~/Dropbox/hi/atlantis/hafro_data/bird_data.csv", header = T)

birds_ab <- birds_wl %>%
  group_by(species) %>% 
  do(model = tryCatch(lm(log(weight) ~ log(length), data = .),
                      error = function(e) NULL)) %>%
  filter(model != "NULL") %>%
  group_by(species) %>% 
  do(data.frame(var = c("a","b"),
                coef = coef(.$model[[1]]))) %>%
  dcast(., species~var, value.var = "coef")
birds_ab$a <- exp(birds_ab$a)
write.csv(birds_ab, file = "birds.csv")

#
# vb
#
birds_wl %>%
  group_by(species) %>%
  do(plot(table(.$weight)))

# puffins
puf <- filter(birds_wl, species == "puffin")
hist(puf$weight)

puf$quants <- findInterval(puf$weight, quantile(puf$weight, probs = seq(0,.9,.1)))
puf[14,] <- c("puffin", 291.7308, NA, 1/12)
puf[15,] <- c("puffin", 437.5961, NA, 2/12)
puf[16,] <- c("puffin", 510.5288, NA, 3/12)
nls(weight~Winf*(1-exp(-K*(quants - t0))),
    data= puf, start=list(Winf=676, K=0.2, t0=3))

# brunnichs
brun <- filter(birds_wl, species == "brunnich")
hist(brun$weight)

brun$quants <- findInterval(brun$weight, quantile(brun$weight, probs = seq(0,.9,.1)))
brun[14,] <- c("brunnich", 505.3333, NA, 1/12)
brun[15,] <- c("brunnich", 758, NA, 2/12)
brun[16,] <- c("brunnich", 884.3333, NA, 3/12)
nls(weight~Winf*(1-exp(-K*(quants - t0))),
    data= brun, start=list(Winf=676, K=0.2, t0=3))

(wt1 <- mean(brun$weight) / 2)
(wt2 <- (mean(brun$weight) + wt1) / 2)
(wt3 <- (mean(brun$weight) + wt2) / 2)
