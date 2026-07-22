library(sf)
library(terra)
library(data.table)
source("../functions/extract.g2g.grid.R")

new_g2g_sites <- fread("../data/new_g2g_sites.csv")

ac_grid <- read.ascii("../data/for_cdata/accuk_nffs.asc")

new_g2g_sites[, ac_catchmentsize := Inf] # index new column

for (i in 1:nrow(new_g2g_sites)){
    xy <- new_g2g_sites[i, .(x,y)]
    new_g2g_sites[i, "ac_catchmentsize"] <-  extract.at.points(ac_grid, xy$x, xy$y)
}
