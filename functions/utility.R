read_timeseries<-function(tsfiles,tspath,model_only=T){
  tsdatX=lapply(tsfiles,read.simple.file,path=tspath)
  ret=data.frame(DATE_TIME= (ISOdatetime((tsdatX[[1]])$"Year",(tsdatX[[1]])$"Month",(tsdatX[[1]])$"Day", 0,0,0,tz="GMT") +60*(tsdatX[[1]])$"Time"))
  for(i in 1:length(tsdatX)){
    names(tsdatX[[i]])=gsub("^X", "",  names(tsdatX[[i]]))
    DT = (ISOdatetime((tsdatX[[i]])$"Year",(tsdatX[[i]])$"Month",(tsdatX[[i]])$"Day", 0,0,0,tz="GMT") +60*(tsdatX[[i]])$"Time")
    if(model_only){
      tsdatX[[i]]=tsdatX[[i]][,c(1,grep("_Mod",names(tsdatX[[i]])))]
      names(tsdatX[[i]])=gsub("_Mod$", "",  names(tsdatX[[i]]) )
    }
    tsdatX[[i]][,"DATE_TIME"]=DT
    
    #ret=merge(ret,   subset(tsdatX[[i]] , select = -c(Step,Year, Month, Day, Time, En)),by="DATE_TIME")
    ret=merge(ret,    tsdatX[[i]][, !names(tsdatX[[i]]) %in% c("Step","Year", "Month", "Day", "Time", "En") ]   ,by="DATE_TIME")
    
  }
  return(ret)
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