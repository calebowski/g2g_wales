


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
  ret <- do.call(rbind, processed_list)

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
make.events.list <- function(mod, obs, events,cdata, region.classifier, exclude.non.notable.sites = FALSE) {

  events_list <- list()
  j  <- 0
  for (i in 1:nrow(events_pre_2022)) {
      # events_list[[i]] <- data.table(DATE_TIME=, notable_site = )
      event <- events[i, ]
      storm_name <- NULL
      if (!is.na(event[, Storm_name])){
          storm_name <- event[, Storm_name]
      } else {
          j <- j + 1
          storm_name <- paste0("Unnamed_storm_", j) 
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
      pstart <- start_date - lubridate::days(7)
      pend <- end_date + lubridate::days(7)

      ## get geographic regions of event
      if (!exclude.non.notable.sites || is.na(event[, Notable_catchment])) {
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
        g2g_notable_sites <- wales_cdata[grepl(river_pattern, River.Name, ignore.case = TRUE), ]
        g2g_ids <- g2g_notable_sites$G2G.ID
      }
      ## filter ts

      event_mod <- mod[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(mod) %in% c("DATE_TIME", paste0(g2g_ids, "_Mod")), with = FALSE] 
      
      ## print the sites missing
      expected_cols <- paste0(g2g_ids, "_Mod")

      missing_cols <- setdiff(expected_cols, colnames(mod))

      cat("The following sites are missing from g2g model output, but present in catchment data:",missing_cols, "\n")

      event_obs <- obs[DATE_TIME >=pstart &  DATE_TIME <=pend ,  colnames(obs) %in% c("DATE_TIME", paste0(g2g_ids, "_Obs")), with = FALSE] 
      events_list[[storm_name]] <- list(mod = event_mod, obs = event_obs)
  }

  return(events_list)
}


remove.suffix <- function(g2g_id, suffix = c("_Obs", "_Mod")) {
  g2g_id_suffix_removed <- gsub("_Obs", "", g2g_id)
}

