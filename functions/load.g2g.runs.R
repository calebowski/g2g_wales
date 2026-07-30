


read.time.series <- function(tsfiles, tspath, output = c("model", "observed")) {
  output <- match.arg(output)
  
  suffix <- if(output == "model") "_Mod" else "_Obs"
  
  processed_list <- lapply(tsfiles, function(file) {
    
    dt <- fread(file.path(tspath, file))
    dt[dt < - 999] <- NA
    
    setnames(dt, gsub("^X", "", names(dt)))
    
 
    dt[, DATE_TIME := as.POSIXct(paste(Year, Month, Day, sep="-"), tz="GMT") + (60 * Time)]
    
    target_cols <- grep(suffix, names(dt), value = TRUE)
    keep_cols <- c("DATE_TIME", target_cols)
    

    dt <- dt[, keep_cols, with = FALSE]
    
    return(dt)
  })
  ret <- data.table::rbindlist(processed_list, fill = TRUE)  
  return(ret)
}

read.simple.file<-function(path,filename,sep=",",exclude=NULL,...) {
fname<-paste(path,filename,sep="")
tdata<- fread(fname,header=T,sep=sep,...)
tdata[tdata< -999]<-NA
if (!is.null(exclude))
{
tdata<-exclude_data(tdata,exclude)
}
return(tdata)
}




# events should be a dataframe of events (see data)
# notable sites will be a column in events
make.events.list <- function(mod, obs, events,cdata, region.classifier, exclude.non.notable.sites = FALSE, time.span = lubridate::days(7)) {

  make.events.helper <- function(mod, obs, events,cdata, region.classifier, exclude.non.notable.sites, time.span) {

    events_list <- list()
    events_with_notable_sites <- c()
    j  <- 0
    for (i in 1:nrow(events)) {
        # events_list[[i]] <- data.table(DATE_TIME=, notable_site = )
        event <- events[i, ]
        storm_name <- NULL
        if (!is.na(event[, Storm_name])){
            storm_name <- event[, Storm_name]
        } else {
            j <- j + 1
            storm_name <- paste0("Unnamed_storm_", j) ## assign unnamed storms a name for reference to csv
        }

        # if (!is.na(event[, Notable_catchment])) {
        #   river_string <- event$Notable_catchment
        #   rivers_vec <- strsplit(river_string, "\\s*,\\s*")[[1]]
        #   river_pattern <- paste(rivers_vec, collapse = "|")
        #   g2g_notable_sites <- wales_cdata[grepl(river_pattern, River.Name, ignore.case = TRUE), ]
        # }
        ## get time period of event

        start_date <- as.POSIXct(paste(event$Year, event$Month, event$Start_day, sep="-"), tz="GMT")
        end_date <- as.POSIXct(paste(event$Year, event$Month, event$End_day, sep="-"), tz="GMT")
        pstart <- start_date - time.span ## period starts 7 days before 
        pend <- end_date + time.span ## period ends 7 days after

        ## get geographic regions of event, using region classifer list (NOT USING THIS ANYMORE BUT LEFT IN IN CASE USEFUL, mostly redundant)
        if ((!exclude.non.notable.sites || is.na(event[, Notable_catchment])) && !is.null(region.classifier)) {
          g2g_ids <- c()
          if (event$North) {
              g2g_ids <- c(g2g_ids, cdata[region.classifier$north_wales | Area. == "Northern [CY]" , ]$G2G.ID)
          }
          if (event$Mid) {
              g2g_ids <- c(g2g_ids, cdata[region.classifier$mid_wales, ]$G2G.ID)
          }
          if (event$South) {
              g2g_ids <- c(g2g_ids, cdata[(region.classifier$south_wales | Area. == "South East [CY]") | Area. == "South West[CY]" , ]$G2G.ID)
          }
        } else if (exclude.non.notable.sites && !is.na(event[, Notable_catchment])) {
          river_string <- event$Notable_catchment
          rivers_vec <- strsplit(river_string, "\\s*,\\s*")[[1]]
          river_pattern <- paste(rivers_vec, collapse = "|")
          g2g_notable_sites <- cdata[grepl(river_pattern, River.Name, ignore.case = TRUE), ]
          g2g_ids <- g2g_notable_sites$G2G.ID
          events_with_notable_sites <- c(events_with_notable_sites, storm_name)
        } else if (is.null(region.classifier)){
          g2g_ids <- cdata$G2G.ID
        }

        ## FILTER by period start and end datae
        event_mod <- mod[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(mod) %in% c("DATE_TIME", paste0(g2g_ids, "_Mod")), with = FALSE] 
        
        ## print the sites missing from metadata
        expected_cols <- paste0(g2g_ids, "_Mod")

        missing_cols <- setdiff(expected_cols, colnames(mod)) 

        cat("The following sites are missing from g2g model output, but present in catchment data:",missing_cols, "\n")

        event_obs <- obs[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(obs) %in% c("DATE_TIME", paste0(g2g_ids, "_Obs")), with = FALSE] 
        events_list[[storm_name]] <- list(mod = event_mod, obs = event_obs)
    }

    if (exclude.non.notable.sites) {
      events_list <- events_list[events_with_notable_sites]
    }

    return(events_list)
  }

  if (inherits(mod, "list") && all(names(mod) %in% c("sufi", "sim"))) {
    sim_events <- make.events.helper(mod = mod$sim, obs, events,cdata, region.classifier, exclude.non.notable.sites, time.span)
    sufi_events <- make.events.helper(mod = mod$sufi, obs, events,cdata, region.classifier, exclude.non.notable.sites, time.span)
    events_list_master <- list(sim = sim_events, sufi = sufi_events)
  } else {
    events_list_master <- make.events.helper(mod = mod, obs, events,cdata, region.classifier, exclude.non.notable.sites, time.span)
  }
  return(events_list_master)
}


remove.suffix <- function(g2g_id, suffix = c("_Obs", "_Mod")) {
  g2g_id_suffix_removed <- gsub("_Obs", "", g2g_id)
}



## note that this is for the runs up to 2021, dates need to be changed otherwise
## this loads the original run NOT NEW DATA.
load.g2g.data <- function(dates, sufi = TRUE){
  if (any(!dir.exists(c("../data/g2g_data/Sim_g2g_run/", "../data/g2g_data/SUFI_g2g_run/")))){
    stop("Paths to g2g data don't exist..\n")
  }

  if (!inherits(dates, "numeric") || length(dates) != 2){
    stop("`Dates` must be a vector of 2 years...\n")
  }

  years <- seq(from = 2000, to = 2021, by = 1) ## needs changing when new runs added
  year_index <- which(years == dates) ## get the positions in vector that match dates we want
  sim_path <- file.path("..", "data", "g2g_data","Sim_g2g_run")
  sufi_path <- file.path("..", "data", "g2g_data","SUFI_g2g_run")
  sim_runs <- list.dirs(sim_path, full.names = FALSE, recursive = FALSE)
  sufi_runs <- list.dirs(sufi_path, full.names = FALSE, recursive = FALSE)
  
  mi_files_sim  <- paste0(sim_runs, "/base_.dat_MI")
  wa_files_sim  <- paste0(sim_runs, "/base_.dat_WA")
  mi_ts_sim <- read.time.series(mi_files_sim[year_index[1]:year_index[2]], sim_path, output = "model")
  wa_t_sim <- read.time.series(wa_files_sim[year_index[1]:year_index[2]], sim_path, output = "model")
  combined_ts_sim <- merge(mi_ts_sim, wa_t_sim, by = "DATE_TIME", all = TRUE)

  mi_files_sufi  <- paste0(sufi_runs, "/base_.dat_MI")
  wa_files_sufi  <- paste0(sufi_runs, "/base_.dat_WA")
  mi_ts_sufi <- read.time.series(mi_files_sufi[year_index[1]:year_index[2]], sufi_path, output = "model")
  wa_ts_sufi <- read.time.series(wa_files_sufi[year_index[1]:year_index[2]], sufi_path, output = "model")
  combined_ts_sufi <- merge(mi_ts_sufi, wa_ts_sufi, by = "DATE_TIME", all = TRUE)

  mi_ts_obs <- read.time.series(mi_files_sufi[year_index[1]:year_index[2]], sufi_path, output = "observed")
  wa_ts_obs <- read.time.series(wa_files_sufi[year_index[1]:year_index[2]], sufi_path, output = "observed")
  combined_ts_obs <- merge(mi_ts_obs, wa_ts_obs, by = "DATE_TIME", all = TRUE)

  return(list(obs = combined_ts_obs, mod = list(sim = combined_ts_sim, sufi = combined_ts_sufi)))
}
