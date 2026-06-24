


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


# read.time.series <- function(tsfiles, tspath, output = c("model", "observed")) {
  
#   tsdatX <- lapply(tsfiles, read.simple.file, path = tspath) ## read files into list
#   processed_list <- list()
  
#   for(i in 1:length(tsdatX)) {
#     ts_df <- tsdatX[[i]]

    
#     names(ts_df) <- gsub("^X", "", names(ts_df))
    
#     # DATE_TIME column added
#     DT <- ISOdatetime(ts_df$Year, ts_df$Month, ts_df$Day, 0, 0, 0, tz = "GMT") + (60 * ts_df$Time)
#     ts_df$DATE_TIME <- DT

#     if (output == "model"){
#       mod_cols <- grep("_Mod", names(ts_df), value = TRUE)
#       ts_df <- ts_df[, c("DATE_TIME", mod_cols), drop = FALSE]
#     } else if (output == "observed"){
#       obs_cols <- grep("_Obs", names(ts_df), value = TRUE)
#       ts_df <- ts_df[, c("DATE_TIME", obs_cols), drop = FALSE]
#     }
#     ## check that obs_cols matches mod_cols
#     # if (!length(mod_cols) == length(obs_cols)){
#     #   stop("Not same amount of mod and obs cols...\n")
#     # }    
#     ## probably redundant code since previous line will remove these cols but leaving in case
#     # cols_to_remove <- c("Step", "Year", "Month", "Day", "Time", "En")
#     # ts_df <- ts_df[, !names(ts_df) %in% cols_to_remove, drop = FALSE]
    
#     ## optional, could maybe find use of this by renaming catchments by the year they are in? might be of use later
#     # if (length(tsfiles) > 1) {
#     #   catchment_ids <- names(ts_df) != "DATE_TIME"
#     #   names(ts_df)[catchment_ids] <- paste0(names(ts_df)[catchment_ids], "_", year)
#     # }
    
#     processed_list[[i]] <- ts_df
#   }

#   ## use `Reduce` to compile df's at end of for loop
#   ret <- Reduce(function(x, y) merge(x, y, by = "DATE_TIME", all = TRUE), processed_list)
  
#   return(ret)
# }

# ## unit test it


# # read.time.series <- function(tsfiles, tspath){
# #    tsdatX <- lapply(tsfiles,read.simple.file,path=tspath)
# #     ret <- data.frame(DATE_TIME= (ISOdatetime((tsdatX[[1]])$"Year",(tsdatX[[1]])$"Month",(tsdatX[[1]])$"Day", 0,0,0,tz="GMT") +60*(tsdatX[[1]])$"Time"))


# # }

# read.simple.file<-function(path,filename,sep=",",exclude=NULL,...) {
# fname<-paste(path,filename,sep="")
# tdata<-read.csv(fname,header=T,sep=sep,...)
# tdata[tdata< -999]<-NA
# if (!is.null(exclude))
# {
# tdata<-exclude_data(tdata,exclude)
# }
# return(tdata)
# }