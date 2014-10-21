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
N <- 4000
(bm <- 197.57 * N / 1000)

# har/com
N <- 35000
(bm <- 87.32 * N / 1000)


linf <- (242.9 + 200.1) / 2
a <- (.124 + .252) / 2
b <- (.283 + .286) / 2
t <- 0:40
t0 <- -.59
L <- linf * (1 - exp(-a*(t - t0)))^b
L_b0 <- 221.5 * (1 - exp(-a*(t - t0)))
plot(L, type = "l")
lines(L_b0, col = "red")

linf * (1 - exp(-a *( 10 - t0)))^b / linf * (1 - exp(-a *( 10 - t0)))

