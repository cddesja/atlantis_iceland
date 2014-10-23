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