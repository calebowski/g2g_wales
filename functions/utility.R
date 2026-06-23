







read.time.series <- function(tsfiles, tspath, model_only = TRUE) {
  
  tsdatX <- lapply(tsfiles, read.simple.file, path = tspath) ## read files into list
  processed_list <- list()
  
  for(i in 1:length(tsdatX)) {
    ts_df <- tsdatX[[i]]
    
    # Clean up column names (remove leading 'X' if R added it)
    names(ts_df) <- gsub("^X", "", names(ts_df))
    
    # DATE_TIME column added
    DT <- ISOdatetime(ts_df$Year, ts_df$Month, ts_df$Day, 0, 0, 0, tz = "GMT") + (60 * ts_df$Time)
    ts_df$DATE_TIME <- DT

    # if (!length(unique(ts_df$Year)) == 1){
    #   stop("File spans > 1 year...\n")
    # } else {
    #   year <- ts_df$Year[1]
    # }

    ## this was in old function, not sure if is needed but leaving in here in case    
    # if (model_only) {
    #   # Keep DATE_TIME and any column ending in _Mod
    #   mod_cols <- grep("_Mod", names(ts_df), value = TRUE)
    #   ts_df <- ts_df[, c("DATE_TIME", mod_cols), drop = FALSE]
      
    #   # Strip the "_Mod" suffix from the names
    #   names(ts_df) <- gsub("_Mod$", "", names(ts_df))
    # }
    mod_cols <- grep("_Mod", names(ts_df), value = TRUE)
    obs_cols <- grep("_Obs", names(ts_df), value = TRUE)
    ## check that obs_cols matches mod_cols
    if (!length(mod_cols) == length(obs_cols)){
      stop("Not same amount of mod and obs cols...\n")
    }
    ts_df <- ts_df[, c("DATE_TIME", mod_cols, obs_cols), drop = FALSE]
    
    ## probably redundant code since previous line will remove these cols but leaving in case
    cols_to_remove <- c("Step", "Year", "Month", "Day", "Time", "En")
    ts_df <- ts_df[, !names(ts_df) %in% cols_to_remove, drop = FALSE]
    
    ## optional, could maybe find use of this by renaming catchments by the year they are in? might be of use later
    # if (length(tsfiles) > 1) {
    #   catchment_ids <- names(ts_df) != "DATE_TIME"
    #   names(ts_df)[catchment_ids] <- paste0(names(ts_df)[catchment_ids], "_", year)
    # }
    
    processed_list[[i]] <- ts_df
  }

  ## use `Reduce` to compile df's at end of for loop
  ret <- Reduce(function(x, y) merge(x, y, by = "DATE_TIME", all = TRUE), processed_list)
  
  return(ret)
}

## unit test it


# read.time.series <- function(tsfiles, tspath){
#    tsdatX <- lapply(tsfiles,read.simple.file,path=tspath)
#     ret <- data.frame(DATE_TIME= (ISOdatetime((tsdatX[[1]])$"Year",(tsdatX[[1]])$"Month",(tsdatX[[1]])$"Day", 0,0,0,tz="GMT") +60*(tsdatX[[1]])$"Time"))


# }

read.simple.file<-function(path,filename,sep=",",exclude=NULL,...) {
fname<-paste(path,filename,sep="")
tdata<-read.csv(fname,header=T,sep=sep,...)
tdata[tdata< -999]<-NA
if (!is.null(exclude))
{
tdata<-exclude_data(tdata,exclude)
}
return(tdata)
}