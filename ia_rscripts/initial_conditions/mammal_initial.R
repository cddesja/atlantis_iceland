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

linf <- (279.2 + 164.1) / 2 * 0.1845 + (104 + 83.2) / 2 * 0.8155
a <- (.116 + .332) / 2 * 0.1845 + (.21 + .26) / 2 * 0.8155
b <- (.716 + .849) / 2 * 0.1845 + (.3 + .31) / 2 * 0.8155

t0 <- -.59 * .1845 + .64*0.8155
ages <- seq(4, 40, by = 4)
W <- linf * (1 - exp(-a * (ages)))^b * 1e3
W_rec <- linf * (1 - exp(-a * (28/365)))^b * 1e3 

#
#
#



  