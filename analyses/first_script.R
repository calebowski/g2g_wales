source("../functions/utility.R")
run_dates <- list.dirs("../data/g2g_data/Sim_g2g_run/", full.names = FALSE, recursive = FALSE)
tsfiles <- paste0(run_dates, "/base_.dat_WA")
tsfiles_filtered <- tsfiles[19:20] ## starts at 2018
tspath <- "../data/g2g_data/Sim_g2g_run/"

obs_mod_ts <- read.time.series(tsfiles_filtered,tspath)

events <- read.csv("../data/notable_events.csv")


catchments <- read.csv("../data/cdata/catchment_cdata_EA-NRW.csv")


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


