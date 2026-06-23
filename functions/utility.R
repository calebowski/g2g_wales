read_timeseries <- function(tsfiles, tspath, model_only = TRUE) {
  
  # 1. Read all files into a temporary list
  tsdatX <- lapply(tsfiles, read.simple.file, path = tspath)
  processed_list <- list()
  
  for(i in 1:length(tsdatX)) {
    df <- tsdatX[[i]]
    
    # Clean up column names (remove leading 'X' if R added it)
    names(df) <- gsub("^X", "", names(df))
    
    # Calculate the proper DATE_TIME column
    DT <- ISOdatetime(df$Year, df$Month, df$Day, 0, 0, 0, tz = "GMT") + (60 * df$Time)
    df$DATE_TIME <- DT
    
    # Filter for model columns if requested
    if (model_only) {
      # Keep DATE_TIME and any column ending in _Mod
      mod_cols <- grep("_Mod", names(df), value = TRUE)
      df <- df[, c("DATE_TIME", mod_cols), drop = FALSE]
      
      # Strip the "_Mod" suffix from the names
      names(df) <- gsub("_Mod$", "", names(df))
    }
    
    # Remove unwanted metadata columns safely (Note the correct comma placement here)
    cols_to_remove <- c("Step", "Year", "Month", "Day", "Time", "En")
    df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
    
    # OPTIONAL FIX FOR NAME CLASHES: 
    # If importing multiple files, append the filename as a suffix to the variables
    if (length(tsfiles) > 1) {
      file_id <- gsub("\\.dat.*$", "", tsfiles[i]) # Strips file extension
      var_idx <- names(df) != "DATE_TIME"
      names(df)[var_idx] <- paste0(names(df)[var_idx], "_", file_id)
    }
    
    processed_list[[i]] <- df
  }
  
  # 2. Elegantly merge all processed data frames by DATE_TIME all at once
  # (all = TRUE ensures no time-steps are accidentally dropped if data is missing)
  ret <- Reduce(function(x, y) merge(x, y, by = "DATE_TIME", all = TRUE), processed_list)
  
  return(ret)
}

## unit test it


read.time.series <- function(tsfiles, tspath){
   tsdatX <- lapply(tsfiles,read.simple.file,path=tspath)
    ret <- data.frame(DATE_TIME= (ISOdatetime((tsdatX[[1]])$"Year",(tsdatX[[1]])$"Month",(tsdatX[[1]])$"Day", 0,0,0,tz="GMT") +60*(tsdatX[[1]])$"Time"))


}

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