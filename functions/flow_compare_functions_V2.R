
print("SOURCED flow_compare_functions_V2.R")  


#Cband radar locations From http://artefacts.ceda.ac.uk/badc_datadocs/nimrod/factsheet15.pdf (Shannon & Jersey have no BNG listed => removed) Check dates (locations not matching little map, eg hole head(?) )
#https://www.metoffice.gov.uk/binaries/content/assets/metofficegovuk/pdf/research/library-and-archive/library/publications/factsheets/factsheet_15-weather-radar.pdf
# full info is at https://catalogue.ceda.ac.uk/uuid/82adec1f896af6169112d09cc1174499
#coments based on whats available on https://catalogue.ceda.ac.uk/uuid/82adec1f896af6169112d09cc1174499
#Corse Hill 259830 646445  - "Holehead Weather Radar ... will replace Corse Hill (NS 59913 46446) due to the new wind farm there." https://www.geograph.org.uk/photo/568104
#https://www.metoffice.gov.uk/services/business-industry/energy/safeguarding
cBandRadarDF = data.frame(matrix(ncol = 3, nrow = 0))
names(cBandRadarDF) =c("radarName","Easting","Northing")                     # dates with data in https://catalogue.ceda.ac.uk/uuid/82adec1f896af6169112d09cc1174499
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Hameldon Hill", 381060, 428740 ) # single 2005-2014, dual 2014-  . Rain rate also.
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("High Moorsley", 433873, 545572 ) # dual 2016- 
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Holehead",      261790, 682835 ) # single 2012-2015, dual 2018-
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Castor Bay",    119112, 520302 ) # single 2011-2013, dual 2014-  
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Ingham",        496045, 382975 ) # single 2014     , dual 2018-  
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Clee Hill",     359485, 277940 ) # single 2008-2018, dual 2018-  . Rain rate also.
cBandRadarDF[nrow(cBandRadarDF)+1,] = list("Munduff Hill",  318820, 703225 ) # single 2008-2017, dual 2014-  .



#define colours
colsLRain=list(c(102,51,0),c(204,153,102),c(240,240,240),c(153,153,255),c(0,0,153)) #brown-white-blue
#colsBias=list(c(200,0,0),c(240,240,0),c(240,240,240),c(0,240,240),c(0,0,120))#red yellow white blue dark-blue
#colsBias=list(c(200,0,0),c(240,240,0),c(240,240,240),c(0,240,240),c(0,0,120))# blue-grey-red colors
colsBias=list(c(200,0,0),c(240,240,240),c(0,0,200))# red-grey-blue colors

colGreenRed = list( c(0,235,0), c(255,48,48))
colRedGreen = list( c(255,48,48), c(0,235,0) )

colRedWhiteGreen = list( c(255,48,48),c(255,255,255), c(0,235,0) )
colGreenWhiteRed = list( c(0,235,0) , c(255,255,255), c(255,48,48) )

colRedGreenBlue = list( c(255,48,48), c(0,235,0), c(0,0,235) )
colContrast = list(c(0,0,0),c(255,48,48),c(255,255,255) , c(0,235,0), c(0,0,235) ) #back red white green blue
#https://venngage.com/tools/accessible-color-palette-generator
colOrangeBlue = lapply(c("#c44601","#f57600","#8babf1","#0073e6","#054fb9"), function(z) as.vector( col2rgb(z) ) )  
colOrangeWhiteBlue = colOrangeBlue;  colOrangeWhiteBlue[[3]] = c(255,255,255)

#https://davidmathlogic.com/colorblind/#%23D81B60-%231E88E5-%23FFC107-%23004D40
colRebBlueCB = list( c(220,50,32), c(0,90,181) )
#colRebWhiteBlueCB = list( c(220,50,32), c(255,255,255), c(0,90,181) )
colRebWhiteBlueCB = list( c(180,0,0), c(255,255,255), c(0,0,180) )

colWhiteRebBrown = list( c(255,255,255), c(255,230,128), c(255,0,0) , c(80,60,20))



#take df1,df2, of same dimensions, interleave them together.
interleaveDFs<-function(df1,df2){
  if(!nrow(df)==nrow(df2))stop("rows")
  if(!all(names(df)==names(df2)))stop("names")
  DF = data.frame(matrix(ncol = ncol(df), nrow = 2*nrow(df)))
  names(DF)=names(df)
  for( i in seq(from=1,to=2*nrow(df))){#i=1
    if(i%%2==1){#i is odd
      rownames(DF)[i]=rownames(df)[(i+1)/2]
      DF[i,]=df[(i+1)/2,]
    }else{#i is even
      rownames(DF)[i]=rownames(df2)[i/2]
      DF[i,]=df2[i/2,]
    }
  }
  return(DF)
}


#Input : a data.frame with a "DATE_TIME" field (labelling end of accumulation period)
#Output : hourly accumulations labelled by final time (incomplete hours at start/end removed)
getHourlyAccumulations<-function(df,unitConvFact=1){
  #make "hr" as the category to aggregate on.
  hourFromStart = as.numeric(difftime(df$DATE_TIME, as.POSIXct(date(df$DATE_TIME[1])), units='mins'))/60
  dt = df[hourFromStart%%1==0, "DATE_TIME"]
  df$hr = ceiling(hourFromStart)
  
  #remove if not group only partial.
  if(!all(df$hr[1:4]==df$hr[1]))df=df[df$hr!=df$hr[1],]
  if(!all(df$hr[(nrow(df)-4):nrow(df)] == df$hr[nrow(df)]))df=df[df$hr!=df$hr[nrow(df)],]
  
  #get the date-time from the end of the hour
  hourFromStart = as.numeric(difftime(df$DATE_TIME, as.POSIXct(date(df$DATE_TIME[1])), units='mins'))/60
  dt = df[hourFromStart%%1==0, "DATE_TIME"]
  
  #head(df[,])
  #tail(df[,])
  #use "aggregate to get accumulations, plus add the date-time from the end of the hour
  hrlyAccumulations = unitConvFact*aggregate(df[,!names(df) %in% c("DATE_TIME","hr")], by=list(hr=df$hr), FUN=sum)
  return(cbind(data.frame(DATE_TIME=dt), hrlyAccumulations))
}

#accumulate precip or flows- outputting the entire TS (and zeroing at radarTiltTimes)
#mm_units="flow" or "precip" is used to convert to dephs (mm)
cumSumTS<-function(DF,radarTiltTimes,excludeDF,mm_units="no"){
  nam = names(DF)[names(DF)!="DATE_TIME"]
  cond = DF$DATE_TIME>=radarTiltTimes[1]  & DF$DATE_TIME<=radarTiltTimes[length(radarTiltTimes)] 
  DF = DF[cond,]
  q=DF[,nam]
  cond=q<0 | is.na(q)
  if(sum(cond)>0)printp("cumSumTS: there where a this many NA in the TS:",sum(cond))
  q[cond]=0
  cond = as.Date(DF$DATE_TIME) %in% excludeDF[excludeDF$exclude,"date"]
  q[cond]=0
  DF[,nam]= cumsum(q)
  #row.names(DF)=DF$DATE_TIME
  for(i in 2:(length(radarTiltTimes)-1)  ){
    qCumTilt = DF[DF$DATE_TIME == radarTiltTimes[i],nam]
    DF[DF$DATE_TIME > radarTiltTimes[i],nam] = DF[DF$DATE_TIME > radarTiltTimes[i],nam] - qCumTilt
  }
  
  if(mm_units=="flow"){
    DF[,nam]=(15*60)/(cdata[cdata$G2G.ID == site,"Found.area"]*1000) * DF[,nam]
  }
  
  if(mm_units=="precip"){
    DF[,nam]=   0.25* DF[,nam] 
  }
  #head(DF)
  #tail(DF)
  return(DF)
}



#output a linetype integer from the QPE name, e.g. QPE = "xband1_V3" 
lineType<-function(QPE){
  ver =strsplit(QPE,"_")[[1]]
  if(length(ver)==1){
    return(1)
  }else{
    ver=ver[length(ver)]
    if(ver=="V3"){
      return(3)
    }else if(ver=="V3b"){
      return(2) 
    }else if(ver=="V4"){
      return(1)
    }else{
      return(4) 
    }
  }
}


blendMeColoursHelper<-function(z1,colsL,ymin=0,ymax=1,maxColorValue=255,naCol=rgb(0,1,0)){
  nn = length(colsL)-1
  if(is.na(z1))return(naCol)
  
  if(z1<ymax){
    m=floor(z1*nn )
    x=z1*nn - m
    r = (colsL[[m+1]]*(1-x) + colsL[[m+2]]*x)
  }else{
    r = colsL[[nn+1]]
  }
  return(rgb(r[1],r[2],r[3],maxColorValue=maxColorValue))
}



blendMeColours<-function(z,colsL,ymin=0,ymax=1,maxColorValue=255){
  ran = range(z, na.rm = T)
  if(ran[1]<ymin | ran[2]>ymax){
    print("limiting range in blendMeColours")
    z=sapply( z, function(z1) max(min(z1,ymax),ymin) )
  }
  if(ymin==(-Inf)) ymin = min(z)
  if(ymax==(Inf))  ymax = max(z)
  zT = (z-ymin)/(ymax-ymin)
  sapply(zT, function(z1)blendMeColoursHelper(z1,colsL,maxColorValue=maxColorValue) )
}


#"multiply" two lists of strings to get list of all combinations
strProd<-function(l1,l2,sep="-",propend="",append=""){
  ret=c()
  for(s1 in l1){
    for(s2 in l2){
      ret=c(ret,paste0(propend,paste(s1,s2,sep=sep),append))    
    }
  }
  return(ret)
}



readCatAvFiles <- function(FN){
  dat = read.csv(FN,check.names=F)
  names(dat)[names(dat)=="date"]="DATE_TIME"
  if( length(unique(names(dat))) != length(names(dat)) ){
    stop("readCatAvFiles: non-unique names")
  }
  dat$DATE_TIME = as.POSIXct(dat$DATE_TIME, format= "%Y-%m-%d %H:%M:00")
  tmp = dat[,names(dat)!="DATE_TIME"]
  tmp[tmp<0]=NA
  dat[,names(dat)!="DATE_TIME"]=tmp
  return(dat)
}


# this is for accumulating over the full periods, not a running total as a ts. 
accumulateTSDF<-function(dat,radarTiltTimes,excludeDFHere=NA,exclField ="exclude" ){ 
  if( is.data.frame(excludeDFHere)){
    printp("**excluding days using",exclField," **")
    excludeCond = as.Date(dat$DATE_TIME) %in% excludeDFHere[excludeDFHere[,exclField],"date"]
    dat[excludeCond,names(dat)!="DATE_TIME"]=NA
  }else{
    print("**NOT excluding the days**")
  }
  sites = names(dat)[names(dat)!="DATE_TIME"]
  accumCatAvDf <- data.frame(matrix(ncol = length(sites)+2, nrow = length(radarTiltTimes)))
  names(accumCatAvDf)=c("Period","nTimes",sites)
  accumCatAvDf$Period = as.numeric(row.names(accumCatAvDf))-1
  for(periodNow in 0:nrow(accumCatAvDf)){#periodNow = 0 
    if(periodNow==0){
      tmp = dat[dat$DATE_TIME >= radarTiltTimes[1] & dat$DATE_TIME <= radarTiltTimes[length(radarTiltTimes)],names(dat)!="DATE_TIME" ]
    }else{
      tmp = dat[dat$DATE_TIME >= radarTiltTimes[periodNow] & dat$DATE_TIME <= radarTiltTimes[periodNow+1],names(dat)!="DATE_TIME" ]
    }
    accums = colSums(tmp,na.rm =T)
    if(any(names(accums)!=names(accumCatAvDf)[   3:ncol(accumCatAvDf)  ] ))stop("accumCatAv - names!")
    accumCatAvDf[accumCatAvDf$Period==periodNow, 3:ncol(accumCatAvDf) ] = accums
    nTimes = colSums(!is.na(tmp)) 
    accumCatAvDf[accumCatAvDf$Period==periodNow, "nTimes" ] =  max(nTimes)
    if(length(unique(nTimes))>1){ printp("accumCatAv:Some sites have more NA than others, period",periodNow); cat(unique(nTimes),"\n")}
  }
  return(accumCatAvDf)
}



mkBestDF <-function(use_Stat,use_flows,statsDFListAll,cols,densityNA,colNA,TINY=1e-6){
  Orient="pos"                                       #bigger is better 
  if(use_Stat %in% c("PODF","FAR") )Orient="neg"     #smaller is better
  if(use_Stat=="perc_bias")         Orient="negmod"  #smaller modulus is better
  
  statsDF=statsDFListAll[names(statsDFListAll) %in% use_flows]
  
  ## a DF with stateing the best flow acourding to the stat = "MaxName" (should be "BestName" realy) and also "density" to plot (NA unless stat does *not* exist) and the color "col" from cols in preamble to this script ##
  maxDF=data.frame(G2G.ID=statsDF[[1]]$G2G.ID,stat=use_Stat)
  #maxDF$MaxName="NA"
  if(Orient!="neg"){
    maxDF$MaxVal=-Inf
  }else{
    maxDF$MaxVal=Inf
  }
  #maxDF$density=NA
  for(name in names(statsDF)){
    cStats = statsDF[[name]][,use_Stat]
    con=NULL
    if(Orient=="pos")    con=(cStats>maxDF$MaxVal & !is.na(cStats))
    if(Orient=="neg")    con=(cStats<maxDF$MaxVal & !is.na(cStats))
    if(Orient=="negmod") con=(abs(cStats)<abs(maxDF$MaxVal) & !is.na(cStats))
    maxDF[con,"MaxVal"]=cStats[con]
    #maxDF[con,"MaxName"]=name
  }
  #maxDF[,"col"]=sapply(maxDF$MaxName,function(x)cols[x])
  #maxDF[is.na(maxDF$col),c("density","col")]=list(densityNA,colNA)
  
  maxDF[names(statsDF)]=""
  for(name in names(statsDF)){
    cStats = statsDF[[name]][,use_Stat]
    maxDF[name]=abs(maxDF$MaxVal -  cStats) < TINY
  }
  maxDF["number_best"] = rowSums(maxDF[names(statsDF)] )
  
  if(any(maxDF["number_best"]>2)){
    print(head(maxDF))
    stop("greater than 2 best")
  }
  
  maxDF$best1 = ""
  maxDF$best2 = ""
  maxDF$col1  = rgb(0,0,0)
  maxDF$col2  = rgb(0,0,0)
  for(k in 1:nrow(maxDF)){
    rw=maxDF[k,use_flows]
    bests = names(rw)[which( (as.logical(rw) ) )]
    maxDF[k,"best1"] = bests[1]
    maxDF[k,"col1"] = cols[ bests[1]]
    if(length(bests)>1){
      maxDF[k,"best2"] = bests[2]
      maxDF[k,"col2"] = cols[ bests[2]]
    }
  }
  
  print(paste("**** STAT =",use_Stat,"****"))
  for(flw in use_flows){
    printp( flw, "best for", sum( maxDF[,flw] ) , "of",nrow(maxDF)  )
  }
  printp("Joint best = ",sum( maxDF$number_best!=1) )
  
  return(maxDF)
}



colGoodBad<-function(x,rMin,rMax,rev=F){
  x=min(x,rMax)
  x=max(x,rMin)
  cMax=c(0,235,0)
  cMin=c(255,48,48)
  if(rev){
    tmp=cMax
    cMax=cMin
    cMin=tmp
  }
  xr=(x-rMin)/(rMax-rMin)
  xr=xr^.75
  cUse=(1-xr)*cMin+xr*cMax
  return(rgb(cUse[1],cUse[2],cUse[3],maxColorValue = 255))
}

#sapply( dd[1], function(x)colBias(x,rMin=-rr,rMax=rr) )

colBias<-function(x,rMin,rMax,rev=F){
  x=min(x,rMax)
  x=max(x,rMin)
  #cMin=c(0,191,255)
  cMax=c(0,0,240)
  cMin=c(240,0,0)
  cMid=c(240,240,240)
  if(rev){
    tmp=cMax
    cMax=cMin
    cMin=tmp
  }
  rMid=(rMin+rMax)/2
  if(x<rMid){
    xr   = (x-rMin)/(rMid-rMin)
    cUse = (1-xr)*cMin+xr*cMid
  }
  if(x>=rMid){
    xr   = (x-rMid)/(rMax-rMid)
    cUse = (1-xr)*cMid+xr*cMax
  }
  return(rgb(cUse[1],cUse[2],cUse[3],maxColorValue = 255))
}


#get names from file path
namFunc<-function(fn){
  f = strsplit(fn,"/",fixed=T)[[1]]
  f = ifelse( ! f[length(f)-1]%in%c("EA","SEPA"),f[length(f)-1],f[length(f)])
  if(grepl("Nimrod",f))f     = "nimrod"
  if(grepl("RGdata_run",f))f = "RG"
  return(f)
}

#checks if path fn contains tsfiles, if not checks most resent dir in fn does
getLatestFlowPath<-function(fn,tsfiles) {
  fo = list.files(fn)
  if(all(tsfiles %in% fo)){
    return(paste0(fn,"/")) 
  }else{
    fo = fo[order(fo)]
    ret = paste(fn,fo[length(fo)],sep="/") 
    if(!all(tsfiles%in%list.files(ret)))stop("check timeserries files exist")
    return(paste0(ret,"/") )  
  }
}

make_boxplot<-function(statsDFlist,stats,ylabs,ylims,x,xlabs,col=NULL){
  statsBox=statsDFlist[x]
  par(mfrow=c(length(stats)+1,1)) 
  #par(mgp=c(1.9,.6,0))
  for(i in  1:length(stats) ){
    dat= lapply(   statsBox,function(x) x[,stats[[i]] ]   )
    cat("*** ",stats[[i]]," ***\n")
    print(lapply(dat,median))
    if(i!=length(stats)){
      par(mar=c(1, 4.5, .1, .1))#bottom, left, top, right
      names=rep("",length(xlabs))
      xaxt="n"
    }else{
      par(mar=c(1, 4.5, .1, .1))#bottom, left, top, right
      names=xlabs
      xaxt="s"
    }
    par(las=2)
    boxplot(dat ,ylim=ylims[[i]],ylab=ylabs[[i]],names=names ,xaxt=xaxt , col=col)
    if(stats[[i]]=="perc_bias") lines(c(-20,20),c(0,0),col="black",lty=2)
  }
  par(mfrow=c(1,1)) 
}

#prints out nicely formatted summary of the threshold statistics
display_thresholdStats<-function(thresholdStats){
  sf=3
  fmt<-function(x,sf=3){
    x=as.character(signif(x,sf))
    return(str_pad(x, 14, side = c( "both"), pad = " "))
  }
  
  AT=sum(thresholdStats$A)
  BT=sum(thresholdStats$B)
  CT=sum(thresholdStats$C)
  DT=sum(thresholdStats$D)
  NT=AT+BT+CT+DT
  PODT=AT/(AT+CT)
  PODFT=BT/(BT+DT) 
  FART=BT/(AT+BT)
  CSIT=AT/(AT+BT+CT)
  
  {cat("total threshold crossings (obs) :",sum(thresholdStats$OB_CROSSINGS),"\n")
    cat("total threshold crossings (mod) :",sum(thresholdStats$MOD_CROSSINGS),"\n")
    cat("\n")
    cat("Contingency table (all sites pooled) :\n")
    cat("       ~~~~~~~~~~~ OBS ~~~~~~~~~\n")
    cat( "Mod |",  fmt(AT/NT,sf) ,"|", fmt(BT/NT,sf)  ,"\n")
    cat( "Mod |",  fmt(CT/NT,sf) ,"|", fmt(DT/NT,sf)  ,"\n")
    cat("\n")
    cat("Statistic (all sites pooled) :\n")
    cat("POD      = ",signif(PODT,sf),"\n")
    cat("PODF (F) = ",signif(PODFT,sf),"\n")
    cat("FAR      = ",signif(FART,sf),"\n")
    cat("CSI      = ",signif(CSIT,sf),"\n")
    cat("\n")
    cat("Statistic (medians by site, na.rm=T) :\n")
    cat("POD      = ",signif(median(thresholdStats$POD,na.rm=T),sf),"\n")
    cat("PODF (F) = ",signif(median(thresholdStats$PODF,na.rm=T),sf),"\n")
    cat("FAR      = ",signif(median(thresholdStats$FAR,na.rm=T),sf),"\n")
    cat("CSI      = ",signif(median(thresholdStats$CSI,na.rm=T),sf),"\n")
    cat("\n")
  }
}

#returns binDat = binary 0/1 dataframe to hold upwards crossings.thresholds_vec it a DF with atleast a column for thresh and G2G.IDs as rows. 
get_upward_crossings<-function(tsdat,thresholds_vec,thresh="QMED/2"){
  sites=names(tsdat)
  sites=sites[sites!="DATE_TIME"]
  
  binDat=tsdat
  binDat[,sites]=0
  
  for(site in sites){
    threshSite=thresholds_vec[site,thresh]
    #vector of 0/1 for where threshold is exceeded
    exceed=as.integer(tsdat[,site]>threshSite)
    diffs=c(0,diff(exceed))
    # enter 1 for upward crossing, else keep as 0
    binDat[diffs>0 &!is.na(diffs) ,site]=1
    binDat[ is.na(diffs) ,site]=NA
  }
  return(binDat)
}

#takes upwards crossings from get_upward_crossings and cacluates a DF of the stats
calc_threshold_stats<-function(binDatSim,binDatObs,sstart=ISOdatetime(1066,1,1,0,0,0) , send=  ISOdatetime(2999,1,1,0,0,0),window=96,QC_sites=c(),plotdir=NA){
  #take subset of mod and obs with in sstart and ssend and overlaping
  sstart=max(min(binDatObs$DATE_TIME),min(binDatSim$DATE_TIME),sstart)
  send=min(max(binDatObs$DATE_TIME),max(binDatSim$DATE_TIME),send)
  binDatObs=binDatObs[binDatObs$DATE_TIME>=sstart & binDatObs$DATE_TIME<=send,]
  binDatSim=binDatSim[binDatSim$DATE_TIME>=sstart & binDatSim$DATE_TIME<=send,]
  if(dim(binDatObs)[1]!=dim(binDatSim)[1])Stop("number of timesteps in mod not equal to obs")
  
  sites=intersect(names(binDatSim),names(binDatObs)) 
  sites=sites[sites!="DATE_TIME"]
  
  if(length(QC_sites)!=0){
    cat("removing QC'd sites :",QC_sites,"\n")
    sites=sites[!sites %in% QC_sites]
    
  }
  
  statsDF=data.frame(G2G.ID=character(0),A=numeric(0),B=numeric(0),C=numeric(0),D=numeric(0),POD=numeric(0),PODF=numeric(0),FAR=numeric(0),CSI=numeric(0), OB_CROSSINGS=numeric(0),MOD_CROSSINGS=numeric(0)  )
  
  for(site in sites){
    ob=binDatObs[,site]
    OB_CROSSINGS=sum(ob)
    mod=binDatSim[,site]
    MOD_CROSSINGS=sum(mod)
    
    ob=rollmean(ob,window)
    
    ob[ob>0]=1
    mod=rollmean(mod,window)
    mod[mod>0]=1
    
    #AA: forecast = yes, observation = yes
    AA = sum(mod*ob,na.rm=T)
    #BB: forecast = yes, observation = no
    BB = sum(mod*(1-ob),na.rm=T)
    #CC: forecast = no,  observation = yes
    CC = sum((1-mod)*ob,na.rm=T)
    #DD: forecast = yes, observation = no
    DD = sum((1-mod)*(1-ob),na.rm=T)
    
    POD=AA/(AA+CC)
    PODF=BB/(BB+DD) 
    FAR=BB/(AA+BB)
    CSI=AA/(AA+BB+CC)
    
    if( !is.na(plotdir)   ){
      #check threshold!
      cat(site, " drawn threshols is QMED/2 \n")
      thr=thresholds_vec_mod[site,"QMED/2"]
      MM=1
      NN = length(obs[obs$DATE_TIME >= sstart & obs$DATE_TIME <= send  ,site])
      NN=4000
      
      png(paste(plotdir,"/podfarcsi_",site,"_.png",sep=""),   width=60,height=25, units="cm",res=300)
      plot(obs[obs$DATE_TIME >= sstart & obs$DATE_TIME <= send  ,site][MM:NN],lwd=1)
      points(tslist[[1]][tslist[[1]]$DATE_TIME >= sstart & tslist[[1]]$DATE_TIME <= send,site][MM:NN] ,col="red")
      lines(c(0,10^6),c(thr,thr))
      
      points(thr*binDatObs[,site],pch=15,lwd=6,col="grey")
      points(0.99*thr*binDatSim[,site],pch=2,lwd=4,col="pink")
      
      points(.9*thr*ob,pch=15,lwd=1,col="grey")
      points(.85*thr*mod,pch=15,lwd=1,col="pink")
      dev.off()
    }
    
    statsDF[site,c("G2G.ID","A","B","C","D","POD","PODF","FAR","CSI","OB_CROSSINGS","MOD_CROSSINGS")]=list(site,AA,BB,CC,DD,POD,PODF,FAR,CSI,OB_CROSSINGS,MOD_CROSSINGS)
  }
  
  return(statsDF)
}


#9Jan2023 - added excludeDF into get_stats 
get_stats<-function(tsdat,obs,sstart=ISOdatetime(1066,1,1,0,0,0) ,send=  ISOdatetime(2999,1,1,0,0,0),bias_correct=F,removeStart=NA,removeEnd=NA, excludeDF=NULL,exclField="exclude"){
  
  #take subset of mod and obs with in sstart and ssend and overlaping
  sstart=max(min(obs$DATE_TIME),min(tsdat$DATE_TIME),sstart)
  send=min(max(obs$DATE_TIME),max(tsdat$DATE_TIME),send)
  
  obs=obs[obs$DATE_TIME>=sstart & obs$DATE_TIME<=send,]
  tsdat=tsdat[tsdat$DATE_TIME>=sstart & tsdat$DATE_TIME<=send,]
  
  if((!is.na(removeStart)) & (!is.na(removeEnd)) ){
    obs=obs[obs$DATE_TIME<=removeStart | obs$DATE_TIME>=removeEnd,]
    tsdat=tsdat[tsdat$DATE_TIME<=removeStart | tsdat$DATE_TIME>=removeEnd,]
  }
  
  if(!is.null(excludeDF)){
    printp("subsetting times with excludeDF field",exclField)
    if( !(nrow(obs)==nrow(tsdat) & all(obs$DATE_TIME == tsdat$DATE_TIME )) )stop("get_stats: check times here - excludeDF")
    dateTimes = as.Date(obs$DATE_TIME)
    m_excludeDF =  rep(F,nrow(obs)) #This will become list of times to exclude based on excludeDF
    
    for(j in 1:nrow(excludeDF)){
      if( excludeDF[j,exclField] ){
        m_excludeDF = m_excludeDF | (dateTimes==excludeDF[j,"date"])
      }
    }
  }else{
    m_excludeDF = rep(F,nrow(obs))
  }
  
  if(dim(obs)[1]!=dim(tsdat)[1])Stop("number of timesteps in mod not equal to obs")
  sites=intersect(names(obs),names(tsdat))
  sites=sites[!sites %in%c("DATE_TIME","hr")]
  statsDF=data.frame(G2G.ID=character(0),perc_bias=numeric(0),r=numeric(0),R2=numeric(0),KGE=numeric(0),KGE_sqrt=numeric(0),MSE=numeric(0),RMSE=numeric(0),MAPE=numeric(0),n=numeric(0))
  
  for(site in sites ){
    #print(site)
    ob=obs[,site]
    mod=tsdat[,site]
    
    #eliminate NA or < 0 flows
    mod[mod<0]=NA
    ob[ob<0]=NA
    m<-is.na(ob)|is.na(mod)|m_excludeDF
    ob=ob[!m]
    mod=mod[!m]
    if(length(ob)!=length(mod))Stop("unequal")
    
    #perc_bias 
    mean_ob=mean(ob)
    mean_mod=mean(mod)
    bias<- mean_mod-mean_ob
    perc_bias<-(bias/mean_ob)*100
    
    #bias correct mod?
    if(bias_correct){
      print("warning - funny bias correct")
      mod=mod-mean_mod+mean_ob
      mean_mod=mean(mod)
    }
    
    #correlation coeff
    r = cor(ob,mod)
    
    #R2 stat
    r2<- NA
    n_sq_sigma_ob=sum((ob-mean_ob)^2)
    SE = sum((ob-mod)^2)
    if ( n_sq_sigma_ob>0) r2= 1 -  SE/ n_sq_sigma_ob 
    
    #calc '*MODIFIED* Kling-Gupta efficiency', c.f Gupta Journal of Hydrology 377 (2009) 80-91 jrw 26/06/2018 Kling 2012
    sd_ob=sd(ob)
    sd_mod=sd(mod)
    KGE=(r-1)^2+((sd_mod/mean_mod)/(sd_ob/mean_ob)-1)^2+(mean_mod/mean_ob-1)^2
    KGE=1-sqrt(KGE)
    
    #calc 'Kling-Gupta efficiency' of square-routed flows jrw 28/06/2018
    mod_sqrt=sqrt(mod)#already elimanated flow<0
    ob_sqrt=sqrt(ob)
    
    r_sqrt = cor(ob_sqrt,mod_sqrt)
    sd_ob_sqrt=sd(ob_sqrt)
    sd_mod_sqrt=sd(mod_sqrt)
    mean_ob_sqrt=mean(ob_sqrt)
    mean_mod_sqrt=mean(mod_sqrt)
    
    KGE_sqrt=(r_sqrt-1)^2+((sd_mod_sqrt/mean_mod_sqrt)/(sd_ob_sqrt/mean_ob_sqrt)-1)^2+(mean_mod_sqrt/mean_ob_sqrt-1)^2
    KGE_sqrt=1-sqrt(KGE_sqrt)
    
    #MSE
    MSE = SE/length(ob)
    
    #MAPE
    MAPE = 100*mean(abs(mod - ob))/mean_ob
    
    #n
    nn=length(ob)
    
    statsDF[site,]=list(site,perc_bias,r,r2,KGE,KGE_sqrt,MSE,sqrt(MSE),MAPE,nn)
  }
  
  return(statsDF)
}


Xget_stats<-function(tsdat,obs,sstart=ISOdatetime(1066,1,1,0,0,0) ,send=  ISOdatetime(2999,1,1,0,0,0),bias_correct=F,removeStart=NA,removeEnd=NA){
  
  #take subset of mod and obs with in sstart and ssend and overlaping
  sstart=max(min(obs$DATE_TIME),min(tsdat$DATE_TIME),sstart)
  send=min(max(obs$DATE_TIME),max(tsdat$DATE_TIME),send)
  
  obs=obs[obs$DATE_TIME>=sstart & obs$DATE_TIME<=send,]
  tsdat=tsdat[tsdat$DATE_TIME>=sstart & tsdat$DATE_TIME<=send,]
  
  if((!is.na(removeStart)) & (!is.na(removeEnd)) ){
    obs=obs[obs$DATE_TIME<=removeStart | obs$DATE_TIME>=removeEnd,]
    tsdat=tsdat[tsdat$DATE_TIME<=removeStart | tsdat$DATE_TIME>=removeEnd,]
  }
  
  
  if(dim(obs)[1]!=dim(tsdat)[1])Stop("number of timesteps in mod not equal to obs")
  
  sites=intersect(names(obs),names(tsdat))
  sites=sites[!sites %in%c("DATE_TIME","hr")]
  statsDF=data.frame(G2G.ID=character(0),perc_bias=numeric(0),r=numeric(0),R2=numeric(0),KGE=numeric(0),KGE_sqrt=numeric(0),MSE=numeric(0))
  
  for(site in sites ){
    ob=obs[,site]
    mod=tsdat[,site]
    
    #eliminate NA or < 0 flows
    mod[mod<0]=NA
    ob[ob<0]=NA
    m<-is.na(ob)|is.na(mod)
    ob=ob[!m]
    mod=mod[!m]
    if(length(ob)!=length(mod))Stop("unequal")
    
    #perc_bias 
    mean_ob=mean(ob)
    mean_mod=mean(mod)
    bias<- mean_mod-mean_ob
    perc_bias<-(bias/mean_ob)*100
    
    #bias correct mod?
    if(bias_correct){
      mod=mod-mean_mod+mean_ob
      mean_mod=mean(mod)
    }
    
    #correlation coeff
    r = cor(ob,mod)
    
    #R2 stat
    r2<- NA
    n_sq_sigma_ob=sum((ob-mean_ob)^2)
    SE = sum((ob-mod)^2)
    if ( n_sq_sigma_ob>0) r2= 1 -  SE/ n_sq_sigma_ob 
    
    #calc '*MODIFIED* Kling-Gupta efficiency', c.f Gupta Journal of Hydrology 377 (2009) 80-91 jrw 26/06/2018 Kling 2012
    sd_ob=sd(ob)
    sd_mod=sd(mod)
    KGE=(r-1)^2+((sd_mod/mean_mod)/(sd_ob/mean_ob)-1)^2+(mean_mod/mean_ob-1)^2
    KGE=1-sqrt(KGE)
    
    #calc 'Kling-Gupta efficiency' of square-routed flows jrw 28/06/2018
    mod_sqrt=sqrt(mod)#already elimanated flow<0
    ob_sqrt=sqrt(ob)
    
    r_sqrt = cor(ob_sqrt,mod_sqrt)
    sd_ob_sqrt=sd(ob_sqrt)
    sd_mod_sqrt=sd(mod_sqrt)
    mean_ob_sqrt=mean(ob_sqrt)
    mean_mod_sqrt=mean(mod_sqrt)
    
    KGE_sqrt=(r_sqrt-1)^2+((sd_mod_sqrt/mean_mod_sqrt)/(sd_ob_sqrt/mean_ob_sqrt)-1)^2+(mean_mod_sqrt/mean_ob_sqrt-1)^2
    KGE_sqrt=1-sqrt(KGE_sqrt)
    
    #MSE
    MSE = SE/length(ob)
    
    statsDF[site,]=list(site,perc_bias,r,r2,KGE,KGE_sqrt,MSE)
  }
  
  return(statsDF)
}


#input: dataframe with columns DATE_TIME  14915  14916, .. with snow and without snow.
#output: T/F (at each timestep, but same midnight to midnight) of whether abs(FloSno[,site]-FloNoSno[,site]) > qFrac*FloSno[,site] found on that day or 2 preceeding/following days.
calc_Snoindex<-function(FloSno,FloNoSno,qFrac=0.20){
  
  #Data frame of T/F for each catchment and timestep to return
  Snoindex=FloSno
  Snoindex[,names(Snoindex)!="DATE_TIME"]=NA
  
  #check equatity of time
  DT=FloSno$DATE_TIME
  DT2=FloNoSno$DATE_TIME
  if(length(DT)!=length(DT2))Stop("TIME LENGTHS NOT EQUAL!")
  if(!all(DT==DT2))Stop("TIMEs NOT EQUAL!")
  
  #dEnd "dayEnd" lists the index of 23:45 timesteps (or first or last timestep in the data)
  #This is a complicated but much quicker way to loop over days.
  stpsday=round(24*60/as.numeric(DT[2]-DT[1], units="mins"))
  de1=max(which(as.Date(DT[1:(stpsday+1)])==as.Date(DT[1])))
  dEnd=seq(from=de1,to=length(DT),by=stpsday)
  if(dEnd[length(dEnd)]!=length(DT)) dEnd=c(dEnd,length(DT))
  dEnd=c(0,dEnd)
  
  #loop on sites population Snoindex[...,site]
  for(site in names(Snoindex)[names(Snoindex)!="DATE_TIME"]){
    #qmed=median(FloSno[,site])
    snowy=abs(FloSno[,site]-FloNoSno[,site]) > qFrac*FloSno[,site]
    Sindex=rep(NA,length(snowy))
    
    #make Sindex T if any snowy over that day 
    for(i in 1:(length(dEnd)-1)){
      iS=dEnd[i]+1;iE=dEnd[i+1]
      Sindex[iS:iE]=any(snowy[iS:iE])
    }
    #make Snoindex for site T if Sindex is T on that day, or preceeding/postceeding by upto 2 day
    len=length(Sindex)
    for(i in 1:(length(dEnd)-1)){
      iS=dEnd[i]+1 ;iE=min(dEnd[i+1], length(Sindex)-1 )
      Snoindex[iS:iE,site] =  Sindex[max(iS-1-stpsday,1)] | Sindex[max(iS-1,1)] | Sindex[iS]| Sindex[min(iE+1,len)] | Sindex[min(iE+1+stpsday,len)]
    }
  }
  
  return(Snoindex)
}

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

get_obs<-function(path,toread){  
  #function to read in observed flow .csv files for a number of years
  print(paste("reading flows from", path))
  flows=NULL
  for (f in toread){
    print(paste("reading file ",f))
    if (is.null(flows)){
      flows=read.simple.file(path,f)
    }else{
      f1=read.simple.file(path,f)
      #deal with different sites in each file
      new_diff=setdiff(names(flows), names(f1))
      old_diff=setdiff(names(f1), names(flows))
      f1[new_diff] <- NA
      flows[old_diff] <- NA
      if (length(new_diff)>0){print(join("Warning! site ",new_diff," added to data from ",f))}
      if (length(old_diff)>0){print(join("Warning! site ",new_diff," added to data from ",f))}                                            
      flows=rbind(flows,f1)
    } 
  }
  
  DATE_TIME<-ISOdatetime(flows$year,flows$month,flows$day,  flows$hours,flows$minutes,0,tz="GMT")
  flows=cbind(DATE_TIME,flows)
  flows=flows[,! names(flows) %in% c("year","month","day","hours","minutes")]
  #set <0 to NA
  flows[flows<0]=NA
  names(flows)=gsub("^X", "",  names(flows))
  #names(flows)=sapply(strsplit(names(flows),"X"),function(x)x=x[[length(x)]])   
  return(flows)
}

hydrograph<-function(flowDat,ylab="",main="",pcolors=NA,ylim=NA,lwd=1.5,leg=NA,maxfact=1,excludeDF=NA){
  #get ylim
  if(is.na(ylim)){
    maxy=0
    for(i in 2:dim(flowDat)[2]){
      maxy=max(maxy,flowDat[,i],na.rm = T)
    }
  }
  
  # if(length(pcolors)==1 && is.na(pcolors)){
  #  pcolors=c(rgb(0,0,0,max=255),rgb(255,0,0,max=255),rgb(0,0,255,max=255),rgb(128,255,106,max=255),rgb(0,95,24,max=255),rgb(255,205,106,max=255),rgb(234,110,0,max=255))[1:(length(flowDat)-1)]
  #  pcolors=c(pcolors[1],rev(pcolors[2:length(pcolors)]))
  # }
  useLwd=rep(lwd,(length(flowDat)-1))
  useLwd[  names(flowDat)[2:length(flowDat) ]=="Obs"    ] =3*lwd/1.5
  
  #plot(flowDat$DATE_TIME,rep(0,length(flowDat$DATE_TIME)),ylab=ylab,xlab="",ylim=c(0,maxfact*maxy),col="white",xaxs="i",main=main,yaxs="i")
  plot(flowDat$DATE_TIME,rep(0,length(flowDat$DATE_TIME)),ylab=ylab,xlab="",ylim=c(-0.04*maxy,maxfact*maxy),col="white",xaxs="i",main=main,yaxs="i")
  
  lines(flowDat$DATE_TIME,rep(0,length(flowDat$DATE_TIME)))
  
  #greyOut=c("2016-05-1","2016-06-1")
  #rect(as.POSIXct(greyOut[1]), -0.04*maxy, as.POSIXct(greyOut[2]),maxfact*maxy, col =rgb(0, 0, 0, .13) ,border = NA)
  if(class(excludeDF)=="data.frame"){
    excDate = excludeDF[excludeDF$exclude,"date"]
    for(i in 1:length(excDate)){
      dateNow = excDate[i]
      rect(as.POSIXct(dateNow), -0.04*maxy, as.POSIXct(dateNow+1),maxfact*maxy, col =rgb(0, 0, 0, .13) ,border = NA)
    }
  }
  
  
  print(names(flowDat))
  for(i in 1:(length(flowDat)-1)){
    lines(flowDat$DATE_TIME,flowDat[,i+1],lwd=useLwd[i],col=pcolors[i] )#first entry=DATE_TIME
  }
  
  if(is.na(leg[1]))leg=names( pcolors)
  if(leg[1]!="none")legend("topright",leg,col=pcolors, lwd=useLwd,bty="n" )
  
}


# mk hydrograph using "hydrograph" but input tslist & obs instead
hydrographList<-function(site,tslist,obs,pstart,pend,ylab=ylab,main=main,pcolors=NA,ylim=NA,lwd=2,leg=NA,maxfact=1.1,excludeDF=NA){
  #make flowDat for just site and time period of interest (inc obs, can name columns for use in legend)
  if(exists("flowDat"))rm(flowDat)
  for(i in 1:length(tslist)){
    con=tslist[[i]]$DATE_TIME>=pstart &  tslist[[i]]$DATE_TIME<=pend
    if(!exists("flowDat")){
      tmp=tslist[[i]][con,c("DATE_TIME",site)]
      names(tmp)[2]=names(tslist )[i]
      flowDat=tmp
    }else{
      tmp=tslist[[i]][con,c("DATE_TIME",site)]
      names(tmp)[2]=names(tslist )[i]
      flowDat=merge(flowDat,tmp)
    }
  }
  if (site %in% names(obs)){
    con=obs$DATE_TIME>=pstart &  obs$DATE_TIME<=pend
    tmp=obs[con,c("DATE_TIME",site)]
    names(tmp)[2]="Obs"
    flowDat = merge(tmp,flowDat)
  }
  
  hydrograph(flowDat,ylab=ylab,main=main,pcolors=pcolors,ylim=ylim,lwd=lwd,leg=leg,maxfact=maxfact,excludeDF=excludeDF)
}


######################################
#code to get map outlines in bng
# Change from lat-long to national grid
# from SRA (original: S:\projects\ensemble_verification\Phase 2\Rcode\backup\20190807)
change.to.natgrid<-function(llgrid){
  names(llgrid)<-c("lon","lat")
  coordinates(llgrid)<-cbind(llgrid$lon,llgrid$lat)
  ukgrid = "+init=epsg:27700"
  latlong = "+init=epsg:4326"
  llgrid@proj4string <- CRS(latlong)
  out<-spTransform(llgrid, CRS(ukgrid))
  return(coordinates(out))
}

#split vectors by NA values
splitbyna <- function( x ){
  idx <- 1 + cumsum( is.na( x ) )
  not.na <- ! is.na( x )
  return(split( x[not.na], idx[not.na] ))
}

get_map_outline_bng<-function(mapobj){
  llgrid<-data.frame(cbind(mapobj$x,mapobj$y))
  names(llgrid)<-c("lon","lat")
  lon_lst<-splitbyna(llgrid$lon)
  lat_lst<-splitbyna(llgrid$lat)
  bnggrid<-NULL
  for (i in 1:length(lon_lst)){
    llgridfr<-data.frame(cbind(lon_lst[[i]],lat_lst[[i]]))
    names(llgridfr)<-c("lon","lat")
    if (is.null(bnggrid)){
      bnggrid<-change.to.natgrid(llgridfr)
    }else{
      bnggrid<-rbind(bnggrid,change.to.natgrid(llgridfr))
    }
    bnggrid<-rbind(bnggrid,c(NA,NA))
  }
  bnggrid<-data.frame(bnggrid)
  names(bnggrid)<-c("east","north")
  return(bnggrid)
}



