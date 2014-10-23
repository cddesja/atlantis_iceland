library("car")
library("dplyr")

#
# WTO
#

## Harbour porpoises
N <- 1e5
bm <- 52.73 * N / 1000  # 52.73 taken as average wt from EOL

## Dolphins
# white-sided
N <- 5e4
bm <- 208 * (N / 2) / 1000

# white-beaked
bm <- 186.63 * (N / 2) / 1000

#
# WMW
#

N <- 5e4
bm <- 5587.09 * N / 1000

#
# WHT
#

## Sperm
N <- 7e3
bm <- 14540.96 * N / 1000

## Northern Bottle nose
N <- 2e3
bm <- 3393.36 * (N / 2) / 1000

## Beaked whale
bm <- (3400 + 2300 + 4775) / 3 * (N / 2) / 1000

## pilot whale
N <- 3e4
bm <- 800 * N / 1000

## orca
N <- 5e3
bm <- 5628.76 * N / 1000

#
# WHB
#

## sei whale
N <- 3e3
bm <- 22106.25 * N / 1000

## fin whale
N <- 15000
(bm <- 47506.01 * N / 1000)

## blue whale
N <- 1500
(bm <- 154321.3 * N / 1000)

## hump whale
N <- 15000
(bm <- 30000.0 * N / 1000)

0.3376*0.125 + 0.0113*0.375 + 0.10*(.56 + 0.0115) + .10*0.0796 # num of offspring / individual

#
# PIN
#

# grey
N <- 5000
(bm <- 197.57 * N / 1000)

# har/com
N <- 50000
(bm <- 87.32 * N / 1000)

(87.32*0.8155 + 197.57 * 0.1845)  ## ave wt

linf <- (279.2 + 164.1) / 2 * 0.1845 + (104 + 83.2) / 2 * 0.8155
a <- (.116 + .332) / 2 * 0.1845 + (.21 + .26) / 2 * 0.8155
b <- (.716 + .849) / 2 * 0.1845 + (.3 + .31) / 2 * 0.8155

t0 <- -.59 * .1845 + .64*0.8155
ages <- seq(4, 40, by = 4)
W <- linf * (1 - exp(-a * (ages)))^b * 1e3
W_rec <- linf * (1 - exp(-a * (28/365)))^b * 1e3 

#
# Distribution
#
bgm <- readLines("~/Dropbox/hi/atlantis/iceland_atlantis/atlantis_L93.bgm")
area <- grep("area",bgm)
bgm_area <- bgm[area]
bgm_area <- strsplit(bgm_area, "\t")
area <- sapply(bgm_area,`[`,2)
area <- as.numeric(as.character(area))
boxes <- rep(-999, 53)
boundaries <- c(42, 41, 28, 27, 26, 3, 2, 0, 50, 49, 48, 47, 46, 45, 44, 19, 52)
boxes[boundaries + 1] <- 0

## MWM
dense_1 <-  c(5, 13, 14, 15, 16, 20, 21, 31, 35, 36, 43)
boxes[dense_1 + 1] <- 1
 
dense_05 <- c(17, 18, 19, 22, 34, 37)
boxes[dense_05 + 1] <- .5 

dense_01 <- c(1, 4, 29, 40)
boxes[dense_01 + 1] <- .1

boxes[boxes == - 999] <- .3

area_wt <- boxes * area
wts <- area_wt / sum(area_wt)
write.csv(wts, file = "wts.csv")

#
# WTO
#
boxes_hp <- rep(-999, 53)
boundaries <- c(42, 41, 28, 27, 26, 3, 2, 0, 50, 49, 48, 47, 46, 45, 44, 19, 52)
boxes_hp[boundaries + 1] <- 0
boxes_dol <- boxes_hp

dense_1 <- c(13:22, 31, 34:37)
boxes_hp[dense_1 + 1] <- 1 * 0.3483
dense_05 <- 51
boxes_hp[dense_05 + 1] <- .5 * 0.3483
boxes_hp[boxes_hp == -999] <- .3 * 0.3483
boxes_dol[boxes_dol == -999] <- 0.6517

# Sum these
wto_wts <- boxes_hp + boxes_dol
area_wt <- wto_wts * area
wts <- area_wt / sum(area_wt)
write.csv(wts, file = "wts.csv")

#
# WHT
#

boxes_wht <- rep(-999, 53)
boundaries <- c(42, 41, 28, 27, 26, 3, 2, 0, 50, 49, 48, 47, 46, 45, 44, 19, 52)
boxes_wht[boundaries + 1] <- 0

# sperm
boxes_sperm <- boxes_wht
sperm_not_in <- c(13:22, 31, 34:37)
boxes_sperm[boxes_sperm == -999] <- 0.3376
boxes_sperm[sperm_not_in + 1] <- 0

# north and beaked
boxes_botbeak <- boxes_wht
botbeak_not_in <- sperm_not_in
boxes_botbeak[boxes_botbeak == -999] <- 0.0113 + 0.0116
boxes_botbeak[botbeak_not_in + 1] <- 0

# killer
boxes_killer <- boxes_wht
boxes_killer[boxes_killer == -999] <- 0.5600

# pilot
boxes_pilot <- boxes_wht
pilot_not_in <- c(13:22, 31, 34:37, 52, 11, 12, 44:49)
dense_03 <- 51
boxes_pilot[dense_03 + 1] <- .3 * 0.0796
boxes_pilot[boxes_pilot == -999] <-  0.0796
boxes_pilot[pilot_not_in + 1] <- 0

# Combine these
boxes_wht <- boxes_sperm + boxes_botbeak + boxes_killer + boxes_pilot
area_wt <- boxes_wht * area
wts <- area_wt / sum(area_wt)
write.csv(wts, file = "wts.csv")

#
# WHB
#
boxes_whb <- rep(-999, 53)
boundaries <- c(42, 41, 28, 27, 26, 3, 2, 0, 50, 49, 48, 47, 46, 45, 44, 19, 52)
boxes_whb[boundaries + 1] <- 0

# sei
boxes_sei <- boxes_whb
boxes_sei[c(29, 30, 40) + 1] <- .0454
boxes_sei[boxes_sei == -999] <- 0

# fin/blue
boxes_fb <- boxes_whb
not_in <- c(13:22, 31, 34:37, 52)
dense_1 <- c(29, 39, 40)
boxes_fb[dense_1 + 1] <- (0.4879 + 0.1585)
dense_01 <- 51
boxes_fb[dense_01 + 1] <- (0.4879 + 0.1585)*.1
boxes_fb[boxes_fb == -999] <- (0.4879 + 0.1585)*.5

# hump
boxes_hump <- boxes_whb
dense_1 <- c(10:18, 32, 39)
boxes_hump[dense_1 + 1] <- 0.3081
dense_05 <- c(40, 43)
boxes_hump[dense_05 + 1] <- 0.3081*.5
boxes_hump[boxes_hump == -999] <- 0.3081*.1

boxes_whb <- boxes_sei + boxes_fb + boxes_hump
area_wt <- boxes_whb * area
wts <- area_wt / sum(area_wt)
write.csv(wts, file = "wts.csv")

#
# PIN
#
boxes_pin <- rep(0, 53)
b_har <- boxes_pin 
b_har[c(35, 36) + 1] <- 0.8155
b_har[c(15, 16, 18, 5, 22, 21, 20, 30, 31, 32, 34) + 1] <- 0.8155*.5

b_grey <- boxes_pin
b_grey[c(31, 32, 34, 35, 20, 21, 30, 5) + 1] <- 0.1845
b_grey[c(22, 18, 16, 15, 36)] <- .5*0.1845

boxes_pin <- b_har + b_grey
area_wt <- boxes_pin * area
wts <- area_wt / sum(area_wt)
write.csv(wts, file = "wts.csv")

#
# MigIOBox whale flags
#
boxes_whb[boxes_whb > 0] <- .9  ## WHB
boxes[boxes > 0] <- .9 ## WMW


