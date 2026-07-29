library(sf)
library(terra)
library(data.table)
source("../functions/extract.g2g.grid.R")

new_g2g_sites <- fread("../data/new_g2g_sites.csv")
summary_ifs <- fread(
  "S:/data/hmf/data/data_processing/FFC/FFC_flows_2022update/investigation_summary_csvs/summary__IMFS_non898_COMMENTED_V2.csv",
  fill = TRUE,
  quote = ""
)

summary_ifs_filtered <- summary_ifs[, .(SITE, IMFS.Name, G2G.ID, X, Y, G2G.Easting, G2G.Northing, BasicSpatialLocation
)]
summary_ifs_filtered[, Location := IMFS.Name]

matched_imfs_new_g2g <- merge(new_g2g_sites, summary_ifs_filtered, by = "Location", all.x = TRUE)


ac_grid <- read.ascii("../data/for_cdata/accuk_nffs.asc")

matched_imfs_new_g2g[, ac_catchmentsize := Inf] # index new column
# matched_imfs_new_g2g[, G2G.Easting := Inf] # index new column
# matched_imfs_new_g2g[, G2G.Northing := Inf] # index new column
imfs_metadata_2021 <- fread("../data/imfs_id_name.csv")
j <- 0
for (i in 1:nrow(matched_imfs_new_g2g)){
    xy <- matched_imfs_new_g2g[i, .(x,y)]
    xy <- sub("\\d{3}$", "500", as.character(xy))
    x <- as.numeric(xy[1])
    y <- as.numeric(xy[2])
    matched_imfs_new_g2g[i, "ac_catchmentsize"]  <- extract.at.points(ac_grid, x, y)
    if(is.na(as.logical(matched_imfs_new_g2g[i, "G2G.Easting"]))){
        matched_imfs_new_g2g[i, "G2G.Easting"] <-  x
    }
    if(is.na(as.logical(matched_imfs_new_g2g[i, "G2G.Northing"]))){
        matched_imfs_new_g2g[i, "G2G.Northing"] <-  y
    }

    if (matched_imfs_new_g2g[i, .(G2G.ID)] == "XNA" | is.na(matched_imfs_new_g2g[i, .(G2G.ID)])  ){
        matched_imfs_new_g2g[i, "G2G.ID"] <- paste0("X", matched_imfs_new_g2g[i, .(SITE)])
        if (matched_imfs_new_g2g[i, .(G2G.ID)] == "XNA" | is.na(matched_imfs_new_g2g[i, .(G2G.ID)])) {
            loco <- matched_imfs_new_g2g[i, .(Location)]
            tryCatch({matched_imfs_new_g2g[i, "G2G.ID"] <- imfs_metadata_2021[IMFS.Name == loco]$IMFS.ID}, error = function(e) "XNA")
            if (matched_imfs_new_g2g[i, .(G2G.ID)] == "XNA"){
                j <- j+1
                matched_imfs_new_g2g[i, "G2G.ID"]  <- sprintf("XNRW_site_%03d", j)
            }
        }
    }
}


setcolorder(matched_imfs_new_g2g, "CATCHMENTSIZE", "ac_catchmentsize")


## manually change some due to issues
matched_imfs_new_g2g[Location == "Brecon Promenade", G2G.Easting := 303500]
matched_imfs_new_g2g[Location == "Crickhowell", G2G.Northing := 217500]
matched_imfs_new_g2g[Location == "Esgair Carnau", G2G.Easting := 297500]
matched_imfs_new_g2g[Location == "Nant Peris", `:=` (G2G.Easting = 261500, G2G.Northing = 357500)]
matched_imfs_new_g2g[Location == "New Tredegar", `:=` (G2G.Easting = 314500, G2G.Northing = 203500)]
matched_imfs_new_g2g[Location == "Olway Inn", `:=` (G2G.Northing = 200500, G2G.Easting = 338500)]
matched_imfs_new_g2g[Location == "Pont y Cambwll", G2G.Easting := 307500]
# matched_imfs_new_g2g[Location == "Pont y Gaer", G2G.Northing := 300500]
matched_imfs_new_g2g[Location == "Taibach", G2G.Easting := 278500]
matched_imfs_new_g2g[Location == "Treffgarne", G2G.Northing := 225500]



matched_imfs_new_g2g[, ac_catchmentsize_g2g := Inf] # index new column

## now check if the areas are correct
for (i in 1:nrow(matched_imfs_new_g2g)){
    # xy <- matched_imfs_new_g2g[i, .(x,y)]
    # xy <- sub("\\d{3}$", "500", as.character(xy))
    x <- matched_imfs_new_g2g[i, "G2G.Easting"]
    y <- matched_imfs_new_g2g[i, "G2G.Northing"]
    # x <- as.numeric(xy[1])
    # y <- as.numeric(xy[2])
    matched_imfs_new_g2g[i, "ac_catchmentsize_g2g"]  <- extract.at.points(ac_grid, x, y)
}

cleaned_new_g2g_sites <- matched_imfs_new_g2g[, .(Location, x, y, Flow_vs_level, Threshold, SITE, G2G.ID, G2G.Easting, G2G.Northing, ac_catchmentsize_g2g)]
# cleaned_new_g2g_sites <- matched_imfs_new_g2g[, .(Location, x, y, Flow_vs_level, Threshold, SITE, G2G.ID, G2G.Easting, G2G.Northing)]


cleaned_new_g2g_sites <- cleaned_new_g2g_sites[Location != "Bala Weir X" ] ## remove this one
cleaned_new_g2g_sites[,G2G.ID := sub("^X", "", G2G.ID) ] ## remove X in front


catchments <- fread("../data/cdata/catchment_cdata_EA-NRW.csv", fill = Inf)
## filter by wales
wales <- st_read("../data/SENC_MAY_2026_WA_BFC_-1059615406868623242/SENC_MAY_2026_WA_BFC.shp")
wales <- st_transform(wales, 27700)

catchments_sf <- st_as_sf(
  catchments,
  coords = c("WISKI.EASTING", "WISKI.NORTHING"),
  crs = 27700,
  remove = FALSE
)
wales_poly <- st_union(wales)
sf_g2g_ids <- unique(c(catchments_sf[st_intersects(catchments_sf, wales_poly, sparse = FALSE), ]$G2G.ID, catchments[Region. == "Wales"]$G2G.ID)) ## use both ids coded as Wales, plus anything inside the polygon
wales_cdata <- catchments[G2G.ID %in% sf_g2g_ids]## filter by wales


cleaned_new_g2g_sites <- cleaned_new_g2g_sites[
    !tolower(Location) %in% tolower(wales_cdata$Site.Name.)
] ## remove anything we already have


cleaned_new_g2g_sites <- cleaned_new_g2g_sites[
    !wales_cdata,
    on = .(G2G.Easting, G2G.Northing)
]




fwrite(cleaned_new_g2g_sites,"../data/new_g2g_sites_g2g_coords.csv")

cleaned_new_g2g_sites <- fread("../data/new_g2g_sites_g2g_coords.csv")



any(cleaned_new_g2g_sites$G2G.ID %in% original_ids)

if (!dir.exists("../data/conf")){
    dir.create("../data/conf")
}

path <- "../data/conf"

write.conf <- function(sites){
    
    nffs_config <- c(
    nrow(sites),
    apply(
        sites,
        1,
        function(x)
        paste(
            x["G2G.Easting"], x["G2G.Northing"],
            x["G2G.Easting"], x["G2G.Northing"],  # repeated coordinates
            x["ac_catchmentsize_g2g"],
            x["G2G.ID"],
            sep = ","
        )
    )
    )

    writeLines(nffs_config, file.path(path, "NFFS_config.conf"))



    hydronometric_region <- c(
    "[GRID2GRID VERSION]",
    "Version= 1.0",
    "",
    "[Hydrometric Area]",
    paste0("number=", 1),
    sites[, paste0(G2G.ID, " =", "WA")]
    )

    writeLines(hydronometric_region, file.path(path, "g2g_hydrometric_region.conf"))

    flow_params <- c(
    "[GRID2GRID VERSION]",
    "Version= 1.0",
    "",
    "[FORECAST ACTION]",
    sites[, paste0(G2G.ID, " =", "-1")]
    )

    writeLines(flow_params, file.path(path, "flow_parameters_other.conf"))

    state_upd <- c(
    "[GRID2GRID VERSION]",
    "Version= 1.0",
    "",
    "[STATE UPDATING]",
    sites[, paste0(G2G.ID, " =", "0")]
    )
    writeLines(state_upd, file.path(path, "ffc_state_update.conf"))

}