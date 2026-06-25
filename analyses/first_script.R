library(data.table)
library(ggplot2)
library(lubridate)
source("../functions/utility.R")
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



events_list <- list()
j  <- 0
north_wales_classifier <- wales_cdata$WISKI.NORTHING >= 300000
mid_wales_classifier <- wales_cdata$WISKI.NORTHING >= 230000 & wales_cdata$WISKI.NORTHING <= 300000
south_wales_classifier <- wales_cdata$WISKI.NORTHING <= 230000


region_classifier <- list(north_wales = wales_cdata$WISKI.NORTHING >= 300000, mid_walesr = wales_cdata$WISKI.NORTHING >= 230000 & wales_cdata$WISKI.NORTHING <= 300000, south_wales= wales_cdata$WISKI.NORTHING <= 230000 )


for (i in 1:nrow(events_pre_2022)) {
    # events_list[[i]] <- data.table(DATE_TIME=, notable_site = )
    event <- events[i, ]
    storm_name <- NULL
    if (!is.na(event[, Storm_name])){
        storm_name <- event[, Storm_name]
    } else {
        j <- j + 1
        storm_name <- paste0("Unnamed_storm_" , j) 
    }

    if (!is.na(event[, Notable_catchment])) {
        river <- event[, Notable_catchment]
        g2g_notable_sites <- wales_cdata[grepl(river, wales_cdata$River.Name, ignore.case = TRUE ),] 
    }
    ## get time period of event

    start_date <- as.POSIXct(paste(event$Year, event$Month, event$Start_day, sep="-"), tz="GMT")
    end_date <- as.POSIXct(paste(event$Year, event$Month, event$End_day, sep="-"), tz="GMT")
    pstart <- start_date - days(7)
    pend <- end_date + days(7)

    ## get geographic regions of event
    g2g_ids <- c()
    if (event$North) {
        g2g_ids <- c(g2g_ids, wales_cdata[north_wales_classifier | Area. == "Northern [CY]" , ]$G2G.ID)
    }
    if (event$Mid) {
        g2g_ids <- c(g2g_ids, wales_cdata[mid_wales_classifier, ]$G2G.ID)
    }
    if (event$South) {
        g2g_ids <- c(g2g_ids, wales_cdata[(south_wales_classifier | Area. == "South East [CY]") | Area. == "South West[CY]" , ]$G2G.ID)
    }


    ## filter ts

    event_mod <- mod_ts[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(mod_ts) %in% c("DATE_TIME", paste0(g2g_ids, "_Mod")), with = FALSE] 
    
    ## print the sites missing
    expected_cols <- paste0(g2g_ids, "_Mod")

    missing_cols <- setdiff(expected_cols, colnames(mod_ts))

    cat("The following sites are missing from g2g model output, but present in catchment data:",missing_cols, "\n")


    event_obs <- obs_ts[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(obs_ts) %in% c("DATE_TIME", paste0(g2g_ids, "_Obs")), with = FALSE] 
    events_list[[storm_name]] <- merge(event_mod, event_obs)
}


storm <- events_list[[2]]

# source("../Functions/get.stats.R")
source("../Functions/make.hydrograph.R")


p <- make.hydrograph(storm, catchment = "060007_TG_9103")









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


