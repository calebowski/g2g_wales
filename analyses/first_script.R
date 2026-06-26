library(data.table)
library(ggplot2)
library(lubridate)
library(gtools)
source("../functions/utility.R")
source("../functions/extract.g2g.qt.R")
source("../functions/qt.comparison.R")
run_dates <- list.dirs("../data/g2g_data/Sim_g2g_run/", full.names = FALSE, recursive = FALSE)
tsfiles <- paste0(run_dates, "/base_.dat_WA")
tsfiles_filtered <- tsfiles[19:22] ## starts at 2018
tspath <- file.path("..", "data", "g2g_data","Sim_g2g_run")



mod_ts <- read.time.series(tsfiles_filtered,tspath, output = "model")
obs_ts <- read.time.series(tsfiles_filtered,tspath, output = "observed")

events <- fread("../data/notable_events.csv")
events_pre_2022 <- events[Year <= 2021,]


catchments <- fread("../data/cdata/catchment_cdata_EA-NRW.csv", fill = Inf)

wales_cdata <- catchments[Region. == "Wales", ]


region_classifier <- list(north_wales = wales_cdata$WISKI.NORTHING >= 300000, 
                          mid_wales = wales_cdata$WISKI.NORTHING >= 230000 & wales_cdata$WISKI.NORTHING <= 300000, 
                          south_wales= wales_cdata$WISKI.NORTHING <= 230000)

events_list <- make.events.list(events_pre_2022, cdata = wales_cdata, region.classifier =  region_classifier, exclude.non.notable.sites = FALSE)


qt_grid_paths <- mixedsort(sort(file.path("../data/qt_grids", list.files("../data/qt_grids")))) ## create relative paths
qt_val <- sub("_.*", "", mixedsort(sort(list.files("../data/qt_grids"))))
qt_grid_list <- lapply(qt_grid_paths, read.ascii)
names(qt_grid_list) <- qt_val ## name by qt

qt_dt <- make.qt.csv(qt_grid_list, cdata = wales_cdata)


filtered_qt <- lapply(events_list, filter.qt, qt = qt_dt)

max_discharge_qt <- list()
for (t_val in qt_val){
    max_discharge_qt[[t_val]] <- Map(function(event_mod_obs, event_qt){
        extract.peak.discharge(event_mod_obs, event_qt,  t_val)
    }, events_list, filtered_qt)
}


qt_exceeded <- lapply(max_discharge_qt,lapply, compare.qt)

qt_exceeded_dt_list <- lapply(qt_exceeded, function(qt){
    dt_exceed <- data.table(
    path     = names(unlist(qt)),
    Exceeded = unlist(qt)
    )
    dt_exceed[, c("Storm", "Data_Type", "G2G.ID") := tstrsplit(path, "\\.", keep = 1:3)]
    dt_exceed[, G2G.ID := gsub("_Obs|_Mod", "", G2G.ID)]
    dt_exceed[, path := NULL]
    setcolorder(dt_exceed, c("Storm", "Data_Type", "G2G.ID", "Exceeded"))
    return(dt_exceed)
})


merged_dat <- lapply(qt_exceeded_dt_list, merge, qt_dt, by = "G2G.ID")

wales <- st_read("W:/hymod/Hydro-JULES/HJ Internships/2026 - NRW G2G/SENC_MAY_2026_WA_BFC_-1059615406868623242/SENC_MAY_2026_WA_BFC.shp")# Read Wales constituency polygons
wales <- st_transform(wales, 27700) # Ensure CRS is BNG (should already be, but safe to enforce)


p <- plot.wales(merged_dat)

### test
## bronagh
## qmed

# events_list$Bronagh[]










# get.stats(mod_ts[[1]], obs_ts[[1]])










## remember to unit test

# tsdatX <- lapply(tsfiles_filtered,read.simple.file,path=tspath)



# ret=data.frame(DATE_TIME= (ISOdatetime((tsdatX[[1]])$Year,(tsdatX[[1]])$Month,(tsdatX[[1]])$Day, 0,0,0,tz="GMT") +60*(tsdatX[[1]])$"Time"))
# model_only=TRUE




# for(i in 1:length(tsdatX)){
#     names(tsdatX[[i]])=gsub("^X", "",  names(tsdatX[[i]]))
#     DT = (ISOdatetime((tsdatX[[i]])$Year,(tsdatX[[i]])$Month,(tsdatX[[i]])$Day, 0,0,0,tz="GMT") +60*(tsdatX[[i]])$Time)
#     if(model_only){
#       tsdatX[[i]]=tsdatX[[i]][,c(1,grep("_Mod",names(tsdatX[[i]])))]
#       names(tsdatX[[i]])=gsub("_Mod$", "",  names(tsdatX[[i]]) )
#     }
#     tsdatX[[i]][,"DATE_TIME"]=DT
    
#     #ret=merge(ret,   subset(tsdatX[[i]] , select = -c(Step,Year, Month, Day, Time, En)),by="DATE_TIME")
#     ret=merge(ret,    tsdatX[[i]][, !names(tsdatX[[i]]) %in% c("Step","Year", "Month", "Day", "Time", "En") ]   ,by="DATE_TIME")
# }
#     ret=merge(ret,    tsdatX[[i]][, !names(tsdatX[[i]]) %in% c("Step","Year", "Month", "Day", "Time", "En") ]   ,by="DATE_TIME")










# ## date range we need is 2018 to 2021


