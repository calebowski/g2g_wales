plot_directory <- "../data/figures/hydrographs"
if (!dir.exists(plot_directory)){
  dir.create(plot_directory, recursive = TRUE)
}
source("../functions/load.g2g.runs.R")
source("../functions/extract.g2g.grid.R")
# source("../functions/qt.exceed.R")
source("../functions/plot.wales.R")
source("../functions/obs.mod.stats.R")
source("../functions/make.hydrograph.R")
library(data.table)
library(ggplot2)
library(lubridate)
library(gtools)
library(sf)


g2g_output_2018_2022 <- load.g2g.data(dates = c(2018, 2021), sufi = TRUE)
obs_ts <- g2g_output_2018_2022$obs
mod_ts <- g2g_output_2018_2022$mod
## read in the events
events <- fread("../data/notable_events.csv")
events_pre_2022 <- events[Year <= 2021,]
## read in the catchment metadata
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


## make list for each event, with sublist for mod & obs
events_list <- make.events.list(mod_ts, obs_ts, events_pre_2022, cdata = wales_cdata, region.classifier =  NULL, exclude.non.notable.sites = FALSE)

qt_grid_paths <- mixedsort(sort(file.path("../data/qt_grids", list.files("../data/qt_grids")))) ## create relative paths
qt_val <- sub("_.*", "", mixedsort(sort(list.files("../data/qt_grids"))))
qt_grid_list <- lapply(qt_grid_paths, read.ascii)
names(qt_grid_list) <- qt_val ## name by qt

qt_dt <- make.qt.csv(qt_grid_list, cdata = wales_cdata)


sufi_data <- fread("../data/sites_list_final.csv")
sufi_data <- sufi_data[,.(G2G.ID, SU, FI)] ## extract cols of use

events_list_pivoted <- list()
for (event in names(events_list[[1]])) {
  sim <- events_list$sim[[event]]$mod
  sufi <- events_list$sufi[[event]]$mod
  obs <- events_list$sufi[[event]]$obs
  events_list_pivoted[[event]] <- list(sim = sim, sufi = sufi, obs = obs)
}


rf_dat <- fread("../data/catchment_average_rg_precip.csv")

rf_event_dat <- list()
for (storm in names(events_list[[1]])){
  event_period <- c(min(events_list$sufi[[storm]]$mod$DATE_TIME), max(events_list$sufi[[storm]]$mod$DATE_TIME))
  ids_to_keep <- gsub("_Mod", "", names(events_list$sim[[storm]]$mod))[-1] ## remove DATE_TIME column
  rf_event_dat[[storm]] <- rf_dat[date >= event_period[1] & date <= event_period[2],  c("date",ids_to_keep), with = FALSE]
  setnames(rf_event_dat[[storm]], "date", "DATE_TIME") ## set it to same name as g2g output
}










for (event in names(events_list_pivoted)) {

  cat("Beginning event plotting:", event,"...\n")
  event_data <- events_list_pivoted[[event]]
  g2g_ids <- gsub("_Mod", "", colnames(event_data$sim)[2:length(colnames(event_data$sim))])
  ord_catchment_size_river <- wales_cdata[G2G.ID %in% g2g_ids][
    order(as.numeric(CATCHMENTSIZE)),
    .SD,
    by = River.Name
  ]
  g2g_ids <- ord_catchment_size_river$G2G.ID

  for (g2g_id in g2g_ids) {
    tryCatch({
      event_plot <- make.hydrograph.sim.sufi(
        event.data = event_data,
        rain.data = rf_event_dat,
        g2g.id = g2g_id,
        storm.name = event,
        cdata = wales_cdata,
        qt.data = qt_dt,
        sufi =sufi_data
      )
      ggsave(
        filename = file.path(plot_directory, paste0(event, "__", g2g_id, ".png")),
        plot = event_plot,
        width = 8,
        height = 6,
        units = "in",
        dpi = 300
      )
    }, error = function(e) {
      cat("Plot failed for storm:", event, "G2G.id:", g2g_id)
    })
  }
}  

