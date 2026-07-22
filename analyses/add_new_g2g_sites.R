library(sf)
library(terra)
library(data.table)
source("../functions/extract.g2g.grid.R")

new_g2g_sites <- fread("../data/new_g2g_sites.csv")

ac_grid <- read.ascii("../data/for_cdata/accuk_nffs.asc")

new_g2g_sites[, ac_catchmentsize := Inf] # index new column
new_g2g_sites[, G2G.Easting := Inf] # index new column
new_g2g_sites[, G2G.Northing := Inf] # index new column


for (i in 1:nrow(new_g2g_sites)){
    xy <- new_g2g_sites[i, .(x,y)]
    xy <- sub("\\d{3}$", "500", as.character(xy))
    x <- as.numeric(xy[1])
    y <- as.numeric(xy[2])
    new_g2g_sites[i, "ac_catchmentsize"]  <- extract.at.points(ac_grid, x, y)
    new_g2g_sites[i, "G2G.Easting"] <-  x
    new_g2g_sites[i, "G2G.Northing"] <-  y
}



## manually change some due to issues
new_g2g_sites[Location == "Brecon Promenade", G2G.Easting := 303500]
new_g2g_sites[Location == "Crickhowell", G2G.Northing := 217500]
