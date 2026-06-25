#This is code originally from RAINS but now adapted to RAINE.
#V5 is the first version using the full EA flow data.
#v6 9jan23 - added excludeDF into get_stats
#v7 15dec23 - set up for run with V6 of outputs
#v8 jun24 - removed rgdal dependency due to enforced upgrade of R

rm(list=ls()) # Clears the list of variables #library(snowfall)
#library(rgdal)  # install.packages("rgdal")
library(mapdata)# install.packages("mapdata")
library(lubridate)#install.packages("lubridate")
library(zoo)
library(stringr)#install.packages("stringr")
library("sf")#install.packages("sf")
library("PerformanceAnalytics")#install.packages("snowfall")

rm(list=ls()) # Clears the list of variables
options(max.print=900)
options(stringsAsFactors = FALSE)
Sys.setenv(TZ="GMT")
#memory.limit(size=25000)
printp<-function(...)cat(...,"\n")
join = paste0 
#VER = "V3b"  #for xband products X5, X10, X14
#VER = "V4"  #for xband products X5, X10, X14
# VER = 
VERS =c("V6a")
exampleCats=c()
doSEPA = TRUE
doRuntimePlots = FALSE
#1 = R(Z); 5 = R(ZC); 10=R(Dual-Pol); 14=R(Zm)
namQPE<-function(str){
  spt = strsplit(str,"_")[[1]] 
  if(length(spt)==2){
    prod=spt[1]
    ver =spt[2]
  }else{
    prod=spt[1]
    ver =""
  }
  if(prod=="nimrod"){
    prod="R(C-band)"
  }else if(prod=="xband1"){
    prod="R(Z)"
  }else if(prod=="xband14"){
    prod="R(Zm)"
  }else if(prod=="xband5"){
    prod="R(ZC)"
  }else if(prod=="xband10"){
    prod="R(D-P)"
  }else if(prod=="xband15"){
    prod="R(D-P)"
  }else if(prod=="xband16"){
    prod="R(C-band)"
  }else if(prod=="xband17"){
    prod="R(merged)"
  }
  
  return(paste(prod,ver))
}







function(llgrid){
  names(llgrid)<-c("lon","lat")
  st_coordinates(llgrid)<-cbind(llgrid$lon,llgrid$lat)
  ukgrid = "+init=epsg:27700"
  latlong = "+init=epsg:4326"
  llgrid@proj4string <- CRS(latlong)
  out<-st_transform(llgrid, CRS(ukgrid))
  return(coordinates(out))
}



#CHECK
r2leg0= c("750806" ,"750605","733020","761103","UGLYDB1","761104","Bellingham","COWGRN5","NTCLGH1","NUNNYKIRK","HEUGHM1","MITFRD1","STHCH02","SOWRBY1","BURNHL1","WOOLSN1","Gosforth","690206","BRIGHS2","CRAGHL1","TEAMVL1","RIPON01","JSDARL5","PRESTL5", "690207","BRADBY5","RIPPND1")#these have R2<0 in RG flows
r2leg0= c()

QC_obs=c()#exclude these from obs : steppy hydrograph   CHECK
QC_obs_thresh=c()#additionally, exlude these sitesfrom threshold stats slight noise in obs can result in fake "upwards threshold crossing" during recessions


system=c("SectionLaptop","PersonalLaptop","Windows","Linux")[3]


#first and last xband images, 
XBstart=ISOdatetime(2018,11,1,0,0,0)
XBend=ISOdatetime(2020,12,18,11,30,0)
#XBend=ISOdatetime(2019,3,1,0,0,0) 


#aSummarySummaryFN = "S:/projects/hydroJULES/RAINE/timeseries/EA/aSummarySummary.csv"  #needs updating here
# NXPOL
#aSummarySummaryFN = "/data/hmf/projects/hydroJULES/RAINE/timeseries/EA/Xband15_V6/aSummarySummary.csv"  #needs updating here
aSummarySummaryFN = "S:/projects/hydroJULES/RAINE/timeseries/EA/Xband15_V6/aSummarySummary.csv"  #needs updating here


maxNimrodUsed = 288/3  #Nimrod is every 5min, hence max 288 (+1) images read in a day (288/3 => at most 1/3rd of day).
excludeSubsequentDay=2 #2 => If monday is unacceptable should we kill tuesday, wednesday?



############# Make excludeDF a dataframe of days to exclude from analysis #############
mkExcludeDF = T
if(mkExcludeDF){
  excludeDF = read.csv(aSummarySummaryFN)
  excludeDF = excludeDF[excludeDF$date!="TOTAL",]
  excludeDF$date = as.Date(as.character(excludeDF$date),format = "%Y%m%d")
  excludeDF$exclude =  (excludeDF$infillImages > maxNimrodUsed)
  excludeDF$exclude0 = excludeDF$exclude
  if( excludeSubsequentDay>0 ){
    for(i in 1:excludeSubsequentDay){
      excludeDF$exclude[2:nrow(excludeDF)] = (excludeDF$exclude[2:nrow(excludeDF)] | excludeDF$exclude[1:(nrow(excludeDF)-1)])
    }
  }
  cat("Total days         : ", nrow(excludeDF),"\n")
  cat("Good days          : ", sum(!excludeDF$exclude),"\n")
  cat("Excluded days      : ", sum(excludeDF$exclude),"\n")
  cat("missingInfillImages: ", sum(excludeDF$missingInfillImages),"\n")
  
}
head(excludeDF,20)


defineCols=T;vividCols=T
if(defineCols){
  cols=c( 
    rgb(166,206,227,maxColorValue=255),#RG (NS)
    rgb(31,120,180,maxColorValue=255),#RG 
    rgb(244,109,67,maxColorValue=255),#Nimrod
    rgb(64,0,75,maxColorValue=255), rgb(118,42,131,maxColorValue=255),rgb(153,112,171,maxColorValue=255),rgb(194,165,207,maxColorValue=255),
    rgb(231,212,232,maxColorValue=255),rgb(217,240,211,maxColorValue=255),rgb(166,219,160,maxColorValue=255),rgb(90,174,97,maxColorValue=255),
    rgb(27,120,55,maxColorValue=255),rgb(0,68,27,maxColorValue=255))#X1,...,X10
  
  cFunc<-function(n){
    a1=c(118,42,131)
    a4=0.5*(c(231,212,232)+220*c(1,1,1))
    b1=0.5*(c(217,240,211)+220*c(1,1,1))
    b4=c(27,120,55)
    # b4=c(7,140,25)
    if(n<=5){
      t=a1+(n-1)*(a4-a1)/5
      return(rgb(t[1],t[2],t[3] ,maxColorValue = 255))  
    }else{
      t=b1+(n-1-5)*(b4-b1)/5
      return(rgb(t[1],t[2],t[3] ,maxColorValue = 255))  
    }
  }
  
  plot(rep(0,10),col=sapply(1:10 , cFunc),pch=1,lwd=40)
  
  cols=c( 
    rgb(166,206,227,maxColorValue=255),#RG (NS)
    rgb(31,120,180,maxColorValue=255),#RG 
    #rgb(178,223,138,maxColorValue=255),#Nimrod
    rgb(244,109,67,maxColorValue=255),#Nimrod
    sapply(1:10 , cFunc)#xband 1, ...., 10
  ) 
  ##names(cols)=plot_flows  CHECK
  
  
  if(vividCols){
    cols=c(
      rgb(128,0,0,maxColorValue=255),#RG (NS)
      rgb(108,83,83,maxColorValue=255),#RG 
      rgb(172,147,147,maxColorValue=255),#Nimrod
      rgb(225,225,95,maxColorValue=255),rgb(255,204,0,maxColorValue=255),rgb(255,179,128,maxColorValue=255),rgb(255,102,0,maxColorValue=255),rgb(147,12,11,maxColorValue=255),rgb(90,11,138,maxColorValue=255),rgb(141,95,211,maxColorValue=255),rgb(55,113,200,maxColorValue=255),rgb(167,202,177,maxColorValue=255),rgb(21,113,69,maxColorValue=255)
    )
    ##names(cols)=plot_flows  CHECK
    plot(rep(0,length(cols)),col=cols,pch=1,lwd=40)
  }
  
  if(vividCols){
    cols=c(
      #rgb(108,83,83,maxColorValue=255),#RG
      rgb(0,255,255,maxColorValue=255),#RG 
      rgb(172,147,147,maxColorValue=255),#Nimrod
      rgb(225,225,95,maxColorValue=255),#X1
      rgb(141,95,211,maxColorValue=255),#X14
      rgb(147,12,11,maxColorValue=255),#X5
      rgb(21,113,69,maxColorValue=255),#X10
      
      
      rgb(141,95,211,maxColorValue=255),#X14 v4
      rgb(147,12,11,maxColorValue=255),#X5  v4
      rgb(21,113,69,maxColorValue=255)#X10 v4
      
    )
    names(cols)=c("RG" , "nimrod","xband1_V3","xband14_V3b","xband5_V3b","xband10_V3b","xband15_V6a","xband16_V6a","xband17_V6a" )
    plot(rep(0,length(cols)),col=cols,pch=1,lwd=40)
  }
}


get_cdata_etal=T; writeCdataOut=F
if(get_cdata_etal){
  
  
  Windows= {
    ##print(obspathEA   <- "S:/data/data_processing/FFC/FFC_flows_2019update/output_flows/")
    print(obspathEA   <- "S:/data/data_processing/FFC/FFC_flows_2022update/flows_qc_no_issues/")
    print(obspathSEPA <- "S:/data/flows/sepa/Update_Flows_2022/")
    print(projDir<-"S:/projects/hydroJULES/RAINE/")
    #print(base_path<-"S:/projects/grid2grid/R/")
    print(spath<-"S:/projects/grid2grid/R/code/setup_code_current/version_1_7_2/")
    print(code_path<-"S:/projects/hydroJULES/RAINE/R/")
    linux=FALSE
    print(dataDir <-"S:/projects/hydroJULES/RAINE/")
  }#,
  
  
  #obsfilesEA   = c("flows_2019_1src_no_qc.csv","flows_2020_1src_no_qc.csv","flows_2021_1src_no_qc.csv")#WATER yr    CHECK
  obsfilesEA   = c("flows_2019_qc.csv","flows_2020_qc.csv","flows_2021_qc.csv")#WATER yr    CHECK
  obsfilesSEPA = c("flow_sepa_2019.csv","flow_sepa_2020.csv","flow_sepa_2021.csv" )     #water yr
  
  plot_dir<-paste0(projDir,"figs_",paste0(VERS,collapse= ''),"/")
  
  source(paste0(code_path,"/flow_compare_functions_V2.R"))
  
  join=paste0
  source_functions<-c("roughness.r","ajr_quick_rcode_pjh.r","upstream_downstream_functions_pjh.r","newperf_summary_extras.r","geol_perc.r","roughness.r","reservoirs.r","newffc_plots_pjh.r","littlefunctions.r","ajr_update_basic_plots_pjh_new.r","ajr_update_r2_comp_pjh.r","newpodfar_pjh.r")
  for (i_fun in 1:length(source_functions))   {
    if(system == "SectionLaptop" & (source_functions[i_fun] %in% c("littlefunctions.r"))) next
    print(join("sourcing ",spath,source_functions[i_fun]))
    source(join(spath,source_functions[i_fun]))
  }
  
  #Source SEPA cdata:
  if(doSEPA){
    source(join(spath,"create_cdata_sepa_v_1_7.r"))
    
    #write.csv(cdata,file="C:/Users/johwal/Documents/R_data_extras/cdata_SEPA_All.csv",quote = T,row.names = F)# arma params are comma separated!? need quote=T
    #nrow(cdata)
    cdata<-subset(cdata,!is.na(cdata$"current_sites")) 
    #write.csv(cdata,file="C:/Users/johwal/Documents/R_data_extras/cdata_SEPA_Current.csv",quote = T,row.names = F)
    #nrow(cdata)
    cdataSepa = cdata
    cdataSepa$Country = "SEPA"
    rm(cdata)
  }
  
  #Source EA-NRW cdata:
  source(paste0(spath,"create_cdata_v_1_7.r"))
  #write.csv(cdata,file="C:/Users/johwal/Documents/R_data_extras/cdata_EA_All.csv",quote = T,row.names = F) # arma params are comma separated!? need quote=T
  #nrow(cdata)
  cdata<-subset(cdata,!is.na(cdata$"current_sites")) 
  #write.csv(cdata,file="C:/Users/johwal/Documents/R_data_extras/cdata_EA_Current.csv",quote = T,row.names = F)
  #nrow(cdata)
  cdataEA = cdata
  cdataEA$Country = "EA-NRW"
  rm(cdata)
  
  
  
  #join EA-NRW & SEPA  cdata
  if(doSEPA){
    colsNow = intersect(names(cdataEA), names(cdataSepa))
    cdata = rbind( cdataEA[,colsNow], cdataSepa[,colsNow])
    head(cdata)
  }else{
    cdata<-cdataEA
  }
  
  if(writeCdataOut){
    
    country=ifelse(SEPA,"SEPA","EA-NRW")
    FN = paste0(projDir,"Catchments/","catchment_cdata_",country,".csv")
    dta = cdata[,c("G2G.ID", "Site.No.", "Region.", "Area.", "Sub.Area.", "NFFS.ID", "NRFA.ID", "Site.Name.", "River.Name", "LETTERCODE", "EASTING", "NORTHING", "CATCHMENTSIZE", "WISKI.EASTING", "WISKI.NORTHING", "NRFA.area", "G2G.Easting", "G2G.Northing", "Found.area") ]
    write.table(dta,file = FN, quote = F, row.names =  F, sep = ",")
  }
}
head(cdata,2)

#read files of catchment mean LUE, mean distance (puts into CatStatDat) 
mk_CatStatDat=T
if(mk_CatStatDat){
  
  ## define data srcs ##  
  cat_stat_dat_dir=join(projDir,"Catchments/catStatDatDir/")
  eaDistFN = "eaDist4.csv"
  eaLueFN = "eaLue.csv"
  sepaDistFN = "sepaDist4.csv"
  sepaLueFN = "sepaLue.csv"
  
  ## read EA-NRW LUE stats ##
  eaDistDat=read.table(paste0(cat_stat_dat_dir,eaDistFN),sep=",",header = T)
  eaDistDat = eaDistDat[,c("name","AREA","MIN","MAX","MEAN")]
  eaDistDat[,c("MIN","MAX","MEAN")] = eaDistDat[,c("MIN","MAX","MEAN")]/1000
  eaDistDat$AREA = eaDistDat$AREA/1e6
  names(eaDistDat)=c("Site.No.","Area_Dist_calc","Min_Dist","Max_Dist","Mean_Dist")
  head(eaDistDat)
  
  ## read EA-NRW LUE stats ##
  eaLueDat=read.table(paste0(cat_stat_dat_dir,eaLueFN),sep=",",header = T)
  eaLueDat = eaLueDat[,c("name","AREA","MIN","MAX","MEAN")]
  eaLueDat[,c("MIN","MAX","MEAN")] = eaLueDat[,c("MIN","MAX","MEAN")]/1000
  eaLueDat$AREA = eaLueDat$AREA/1e6
  names(eaLueDat)=c("Site.No.","Area_LUE_calc","Min_LUE","Max_LUE","Mean_LUE")
  head(eaLueDat)
  
  #Merge EA-NRW data ##
  eaDat = merge(eaLueDat,eaDistDat,by="Site.No.")
  eaDat =eaDat[ eaDat$Max_Dist <150 ,]
  printp("max area excluded as NA in LUE calc (km2): ", max(abs(eaDat$Area_LUE_calc-eaDat$Area_Dist_calc)) )
  rm(eaDistDat,eaLueDat)
  head(eaDat)
  
  #select only those eaDat in cdata, and replace name 'Site.No.' with G2G.ID 
  if(length(cdata$Site.No.) - length(unique(cdata$Site.No.)) != 0) stop("EA-NRW/Sepa merge created non-unique site numbers")
  if(length(cdata$G2G.ID) - length(unique(cdata$G2G.ID)) != 0)     stop("EA-NRW/Sepa merge created non-unique site numbers (2)")
  eaDat = eaDat[eaDat[,"Site.No."] %in% cdata[,"Site.No."] ,]
  for(i in 1:nrow(eaDat)){#i=3
    g2gID = cdata[ cdata$Site.No. == eaDat[i,"Site.No."] , "G2G.ID"]
    eaDat[i,"Site.No."]=g2gID
  }
  names(eaDat)[1]="G2G.ID"
  head(eaDat)
  
  if(doSEPA){
    ## read SEPA LUE stats ##
    sepaDistDat=read.table(paste0(cat_stat_dat_dir,sepaDistFN),sep=",",header = T)
    sepaDistDat = sepaDistDat[,c("g2g_id","AREA","MIN","MAX","MEAN")]
    sepaDistDat[,c("MIN","MAX","MEAN")] = sepaDistDat[,c("MIN","MAX","MEAN")]/1000
    sepaDistDat$AREA = sepaDistDat$AREA/1e6
    names(sepaDistDat)=c("G2G.ID","Area_Dist_calc","Min_Dist","Max_Dist","Mean_Dist")
    head(sepaDistDat)
    dim(sepaDistDat)
    
    ## read SEPA LUE stats ##
    sepaLueDat=read.table(paste0(cat_stat_dat_dir,sepaLueFN),sep=",",header = T)
    sepaLueDat = sepaLueDat[,c("g2g_id","AREA","MIN","MAX","MEAN")]
    sepaLueDat[,c("MIN","MAX","MEAN")] = sepaLueDat[,c("MIN","MAX","MEAN")]/1000
    sepaLueDat$AREA = sepaLueDat$AREA/1e6
    names(sepaLueDat)=c("G2G.ID","Area_LUE_calc","Min_LUE","Max_LUE","Mean_LUE")
    head(sepaLueDat)
    dim(sepaLueDat)
    
    #Merge SEPA data ##
    sepaDat = merge(sepaLueDat,sepaDistDat,by="G2G.ID")
    sepaDat = sepaDat[ sepaDat$Max_Dist <150 ,]
    printp("max area excluded as NA in LUE calc (km2): ", max(abs(sepaDat$Area_LUE_calc-sepaDat$Area_Dist_calc)) )
    rm(sepaDistDat,sepaLueDat)
    
    plot(sepaDat$Area_LUE_calc,sepaDat$Area_Dist_calc)
    sepaDat[sepaDat$Area_LUE_calc!=sepaDat$Area_Dist_calc,]
  }#end sEPA bit
  
  #Merge EA-NRW & SEPA data ##
  eaDat$Country = "EA-NRW"
  if(doSEPA){
    sepaDat$Country = "SEPA"
    CatStatDat = rbind(eaDat,sepaDat)
  }else{CatStatDat<-eaDat}
  CatStatDat = CatStatDat[,names(CatStatDat)!="Area_LUE_calc"]
  CatStatDat[CatStatDat$Country =="EA-NRW","Col"]="red"
  CatStatDat[CatStatDat$Country =="SEPA","Col"]="blue"
  
  
  
  #rm(eaDat,sepaDat)
  if(doRuntimePlots){
    plot(CatStatDat$Mean_Dist,CatStatDat$Mean_LUE,col=CatStatDat$Col,pch=1,lwd=2,main="EA-NRW (red), SEPA (blue)")
  }
  if(length(CatStatDat$G2G.ID) - length(unique(CatStatDat$G2G.ID)) != 0) stop("EA-NRW/Sepa merge created non-unique site numbers (3)")
  
  #merge with rows from cdata - and check the merge.
  nrow1 = nrow(CatStatDat)
  CatStatDat = merge(CatStatDat,cdata[,c("G2G.ID","Region.","NRFA.ID", "Site.Name.","River.Name","Found.area")],by = "G2G.ID")
  if( nrow(CatStatDat) - nrow1 !=0)stop("rows eliminated in merge with cdata")
  
  
  dif = CatStatDat[,"Area_Dist_calc"] - CatStatDat[,"Found.area"]
  cond = (sum(dif!=0)>4 ) | max(abs(dif))>33
  if(sum(dif!=0)==0 ){
    print("Merge - area's agree")
  }else if(!cond){
    print("Caution :Merge -   some rows have different area compared cdata")
    print(CatStatDat[dif!=0,c("G2G.ID",  "Area_Dist_calc",  "Country"  ,    "Region.", "NRFA.ID" ,"River.Name", "Found.area")])
  }else{
    stop("Merge -  lots of rows have different area comared cdata")
  }
  
  print("CatStatDat catchments not in main 4 regions :")
  print(CatStatDat[! CatStatDat[,"Region."] %in% c("North West","North East","South West","South East"),])
  
}
CatStatDat[1:5,]
nrow(CatStatDat)


#read obs and simulated flows. QC_obs done here - takes a while!
get_flows=T
if(get_flows){
  #GET Observations - and merge into "obs"
  obsEA=get_obs(obspathEA,obsfilesEA)       #EA
  if(doSEPA){
    obsSEPA=get_obs(obspathSEPA,obsfilesSEPA) #SEPA
    
    #check no overlapping names
    if( any(names(obsEA)[names(obsEA)!="DATE_TIME"] %in% names(obsSEPA)[names(obsSEPA)!="DATE_TIME"]) ){
      stop("SEPA - EA NAME OVERLAP") 
    }
    
    obs = merge(obsEA,obsSEPA,by = "DATE_TIME")
  }else{
    obs<-obsEA
  }
  #rm(obsSEPA,obsEA)
  
  obs[1:5,c(1:5,ncol(obs))]
  dim(obs)
  
  #remove any QC'd sites:
  if(length(QC_obs)!=0){
    cat("removing the following QC_obs :",QC_obs,"\n")
    obs=obs[,! names(obs) %in% QC_obs]
  }
  
  #two sites with missing obs! (missing in the .csv)
  #head(obs[,c("234321","234326")]) in rains that is
  #tail(obs[,c("234321","234326")])
  
  
  ### EA simulation files ###
  tsfilesEA = c("base_.dat_NE","base_.dat_NW")
  tspathEA=c(
    paste0(dataDir,"timeseries/EA/20210913_164321_RGdata_run"),     #rg_precip
    # paste0(dataDir,"timeseries/EA/Nimrod")     , #Nimrod
    #  paste0(dataDir,"timeseries/EA/xband1"     ),  #X1  v3
    #  paste0(dataDir,"timeseries/EA/Xband14_V3b" ), #X14 v3b
    #  paste0(dataDir,"timeseries/EA/Xband5_V3b"  ), #X5  v3b 
    #  paste0(dataDir,"timeseries/EA/Xband10_V3b" ), #X10 v3b
    #  paste0(dataDir,"timeseries/EA/Xband14_V4"  ), #X14 v4
    #  paste0(dataDir,"timeseries/EA/Xband5_V4"   ), #X5  v4
    #  paste0(dataDir,"timeseries/EA/Xband10_V4"  ),  #X10 v4
    #
    #  paste0(dataDir,"timeseries/EA/Xband5_V4b"  ),  #X5 v4b
    #  paste0(dataDir,"timeseries/EA/Xband10_V4b"  ), #X10 v4b
    #  paste0(dataDir,"timeseries/EA/Xband14_V4b"  ),  #X5 v4b
    paste0(dataDir,"timeseries/EA/Xband15_V6a"  ),   #NXPOL V6a
    paste0(dataDir,"timeseries/EA/Xband16_V6a"  ),   # UKMO V6a
    paste0(dataDir,"timeseries/EA/Xband17_V6a"  )   # MERGED V6a
  )
  tspathEA = sapply(tspathEA,getLatestFlowPath,tsfilesEA, USE.NAMES = FALSE)
  tslistEA=vector("list", length(tspathEA))
  names(tslistEA) = sapply(tspathEA, namFunc, USE.NAMES = FALSE)
  names(tslistEA)[names(tslistEA)=="xband1"]="xband1_V3"#transform names to match SEPA
  names(tslistEA) = sapply(names(tslistEA),function(x) ifelse(substring(x,1,1)=="X",paste0("x", substring(x,2,999)),x), USE.NAMES =F)
  
  ### SEPA simulation files ###
  if(doSEPA){
    tsfilesSEPA = c("base_.dat_SW","base_.dat_SE")
    tspathSEPA=c(
      paste0(dataDir,"timeseries/SEPA/20220405_101636_RGdata_run"),  #rg_precip
      # paste0(dataDir,"timeseries/SEPA/20220405_152347_Nimrod"),      #Nimrod
      #  paste0(dataDir,"timeseries/SEPA/xband1_V3"   ),   #X1  v3
      #  paste0(dataDir,"timeseries/SEPA/xband14_V3b" ),   #X14 v3b
      #  paste0(dataDir,"timeseries/SEPA/xband5_V3b"  ),   #X5  v3b 
      #  paste0(dataDir,"timeseries/SEPA/xband10_V3b" ),   #X10 v3b
      #  paste0(dataDir,"timeseries/SEPA/xband14_V4" ) ,   #X14 v4
      #  paste0(dataDir,"timeseries/SEPA/xband5_V4"  ) ,   #X5  v4
      #  paste0(dataDir,"timeseries/SEPA/xband10_V4" ) ,   #X10 v4
      #
      # paste0(dataDir,"timeseries/SEPA/xband5_V4b" ) ,   #X5  v4b
      #  paste0(dataDir,"timeseries/SEPA/xband10_V4b" ),   #X10 v4b
      #  paste0(dataDir,"timeseries/SEPA/xband14_V4b" )    #X14 v4b
      paste0(dataDir,"timeseries/SEPA/xband15_V6a" ) ,   #X5  v4b
      paste0(dataDir,"timeseries/SEPA/xband16_V6a" ),   #X10 v4b
      paste0(dataDir,"timeseries/SEPA/xband17_V6a" )    #X14 v4b
    )
    tspathSEPA = sapply(tspathSEPA,getLatestFlowPath,tsfilesSEPA, USE.NAMES = FALSE)
    tslistSEPA=vector("list", length(tspathSEPA))
    names(tslistSEPA) = sapply(tspathSEPA, namFunc, USE.NAMES = FALSE)
  }
  #  T0=Sys.time()
  #  x = read.csv("E:/RAINE_data/timeseries/SEPA/xband14_V4/20220407_102018/base_.dat_NW")
  # Sys.time()-T0
  
  #  T0=Sys.time()
  #  y = read.csv("S:/projects/hydroJULES/RAINE/timeseries/SEPA/xband14_V4/20220407_102018/base_.dat_NW")
  #  Sys.time()-T0
  
  ## Load EA ## 
  # for(i in 1SEPA ## 
  #   for(i in 1:length(tspathSEPA)){
  #     cat("** reading ",tspathSEPA[i]," **\n")
  #     tslistSEPA[[i]]= (tsfilesSEPA,tspathSEPA[i],model_only=T)
  #   }
  # }:length(tspathEA)){
  #   cat("** reading ",tspathEA[i]," **\n")
  #   tslistEA[[i]]=read_timeseries(tsfilesEA,tspathEA[i],model_only=T)
  # }
  # if(doSEPA){
  #   ## Load 
  
  if(doSEPA){
    ## Merge EA - SEPA ##
    
    if( length(tslistEA) !=length(tslistSEPA) |  any(names(tslistEA)!=names(tslistSEPA)) ) stop("CHECK model time series !!!!!!!!!!!!!!!!!!!!! ")
    tslist=vector("list", length(tslistSEPA))
    names(tslist) = names(tslistSEPA)
  }else{
    tslist=vector("list", length(tslistEA))
    names(tslist) = names(tslistEA)
  }
  
  if(doSEPA){
    if(sum(names(tslistEA[[1]]) %in% names(tslistSEPA[[1]]) ) !=1)stop("CHECK Matching EA - SEPA names (other than DATE_TIME)")
    
    
    #head(tslistEA[[1]],2)
    #head(tslistSEPA[[1]],2)
    for(i in 1:length(tslist)){
      tslist[[i]] = merge(tslistEA[[i]],tslistSEPA[[i]],by = "DATE_TIME")
    }
    rm(tslistEA,tslistSEPA)
  }else{
    tslist = tslistEA
  }
  
  plot_flows = names(tslist)
  
  plot_labs        = sapply( plot_flows, function(s)namQPE(s))
  names(plot_labs) = plot_flows
  (plot_labs)
}
head(tslist[[1]],2) # 133060 is the first sepa station.
names(tslist)


#this was checking that the full period flows where reasonable (following the inclusion of the uptodate obs. flows) DELETE WHEN DONE
check_why_bias_changed=F 
if(check_why_bias_changed){
  obsEA0=get_obs("S:/data/data_processing/FFC/FFC_flows_2019update/output_flows/" ,"flow_NFFS_2019_qc.csv")
  obsEA1=get_obs(obspathEA,obsfilesEA)       #EA
  
  
  namBoth = intersect(names(obsEA0), names(obsEA1) )
  c0=names(obsEA0)%in%namBoth
  c1=names(obsEA1)%in%namBoth
  
  dim(obsEA0[,c0])
  dim(obsEA1[,c1])
  
  XBstart=ISOdatetime(2018,10,26,0,0,0)
  XBend=ISOdatetime(2020,12,18,11,30,0)
  #XBend=ISOdatetime(2019,3,1,0,0,0) 
  #XBend=ISOdatetime(2019,1,1,0,0,0) 
  names(tslist)
  
  #par(mfrow=c(1,2))
  s = get_stats(tslist[["RG"]],obsEA0[,c0],sstart=sstart  ,send=ISOdatetime(2019,1,1,0,0,0)  ,bias_correct=F )
  s=merge(s, CatStatDat)
  s[!is.na(s$R2) & s$R2<0 ,"R2"]=0
  s0=s[order(s$Mean_LUE),]
  
  s = get_stats(tslist[["RG"]],obsEA1[,c1],sstart=sstart  ,send=XBend ,bias_correct=F )
  s=merge(s, CatStatDat)
  s[!is.na(s$R2) & s$R2<0 ,"R2"]=0
  s1=s[order(s$Mean_LUE),]
  
  plot(s0$R2,s1$R2)
  lines(c(-10,10),c(-10,10),col="green")
  median(s0$R2,na.rm=T)#old
  median(s1$R2)#new
  
  site = s2$G2G.ID[1]; ppp=(1*30*96):(15*30*96)
  plot(obsEA1$DATE_TIME[ppp], obsEA1[ppp,names(obsEA1)==site])
  lines(obsEA0$DATE_TIME[ppp], obsEA0[ppp,names(obsEA0)==site],col="blue",lwd=2)
  
  
  s = get_stats(tslist[["xband10_V4"]],obsEA0[,c0],sstart=sstart  ,send=ISOdatetime(2019,1,1,0,0,0) ,bias_correct=F )
  s=merge(s, CatStatDat)
  s[!is.na(s$R2) & s$R2<0 ,"R2"]=0
  s2=s[order(s$Mean_LUE),]
  
  
  s = get_stats(tslist[["xband10_V4"]],obsEA1[,c1],sstart=sstart  ,send=ISOdatetime(2019,1,1,0,0,0) ,bias_correct=F )
  s=merge(s, CatStatDat)
  s[!is.na(s$R2) & s$R2<0 ,"R2"]=0
  s2p=s[order(s$Mean_LUE),]
  
  
  s = get_stats(tslist[["xband10_V4"]],obsEA1[,c1],sstart=sstart  ,send=XBend,bias_correct=F )
  s=merge(s, CatStatDat)
  s[!is.na(s$R2) & s$R2<0 ,"R2"]=0
  s3=s[order(s$Mean_LUE),]
  
  
  {par(mfrow=c(2,3))
    
    cond=s2$Mean_LUE<=3
    printp("num sites:",sum(cond))
    
    stat="R2"
    lim=range(c(s2[cond,stat],s2p[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s2p[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-10,10),c(-10,10),col="green")
    
    stat="r"
    lim=range(c(s2[cond,stat],s2p[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s2p[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-10,10),c(-10,10),col="green")
    
    stat="perc_bias"
    lim=range(c(s2[cond,stat],s2p[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s2p[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-100,100),c(-100,100),col="green")
    lines(c(-100,100),c(0,0))
    lines(c(0,0),c(-100,100))
    
    
    
    stat="R2"
    lim=range(c(s2[cond,stat],s3[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s3[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-10,10),c(-10,10),col="green")
    
    stat="r"
    lim=range(c(s2[cond,stat],s3[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s3[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-10,10),c(-10,10),col="green")
    
    stat="perc_bias"
    lim=range(c(s2[cond,stat],s3[cond,stat]),na.rm=T)
    plot(s2[cond,stat],s3[cond,stat],xlim=lim,ylim=lim,main=stat)
    lines(c(-100,100),c(-100,100),col="green")
    lines(c(-100,100),c(0,0))
    lines(c(0,0),c(-100,100))
    
    par(mfrow=c(1,1))}
  
  
  
  
  
  s2$G2G.ID==s3$G2G.ID
  g0=s2$R2>0 & s3$R2>0 &!is.na(s2$R2)
  median(s2$R2[g0])#old
  median(s3$R2[g0])#new
  lc=s2$Mean_LUE<=3 &!is.na(s2$R2)
  median(s2$R2[lc])#old
  median(s3$R2[lc])#new
  
  median(s0$R2[lc])#old
  median(s1$R2[lc])#new
  
  
  cbind(s2[,c(1:5,8,11,13)],s3[,c(1:5,8,11,13)])
  
  cbind(s2[,c(1,2,4)],s3[,c(2,4,14)])[1:10,]
  
  par(mfrow=c(1,1))
  s3[,1:10]
  plot(s3$R2[1:10],pch=16,col=rgb(0,0,0,0.3))
  points(s2$R2[1:10],col="blue",pch=4)
  
  site = s2$G2G.ID[6]
  site= "Allen_Mill_Bridge"; ppp=1:(5*30*96)
  plot(obsEA0$DATE_TIME[ppp], obsEA0[ppp,names(obsEA0)==site],col="blue")
  lines(obsEA1$DATE_TIME[ppp], obsEA1[ppp,names(obsEA1)==site],lwd=2)
  
  s0[s0$G2G.ID== site,1:6]
  s1[s1$G2G.ID== site,1:6]
}



#remove unsuitable sites
limit_to_all_dat=T; kill_r2leg0=F;maxMeanDist = Inf;
if(limit_to_all_dat){
  
  printp("Limiting to radar distance of ",maxMeanDist)
  CatStatDat = CatStatDat[CatStatDat$Mean_Dist <=maxMeanDist,]
  
  namCatStat = CatStatDat$G2G.ID;                                length(namCatStat)
  namCdata = cdata$G2G.ID;                                       length(namCdata)
  namObs = names(obs)[names(obs)!="DATE_TIME"];                  length(namObs)
  namSim = names(tslist[[1]])[names(tslist[[1]])!="DATE_TIME"];  length(namSim)
  
  if(! all(namCatStat %in% namCdata))stop("names?")
  printp( sum(!namSim%in%namObs) ," simulations don't have obs." )
  printp( sum(!namCatStat %in% namSim) ," CatStatDat don't have simulations" )
  
  nam = intersect( intersect(namCatStat,namCdata) , intersect(namObs,namSim) )
  printp("Number of catchments with all data = ",length(nam))
  
  
  if(kill_r2leg0){
    nam = nam[!(nam %in% r2leg0 )] #these have R2<0 in RG flows
    printp(length(r2leg0)," sites killed due to R2<0 in RG flows")
  }
  
  #check for completely missing obs
  compVec=sapply(nam,function(site)sum(!is.na(obs[,site])))
  nam=nam[compVec!=0]
  cat("no obs for sites: ", nam[compVec==0],"\n")
  
  
  #limit to remaining sites
  printp("sites remaining = ",length(nam))
  sitesFullDat=nam
  CatStatDat=CatStatDat[CatStatDat$G2G.ID %in% nam,]
  obs=obs[ , names(obs) %in% nam | names(obs)=="DATE_TIME"]
  for(i in 1:length(tslist)){
    tslist[[i]]=tslist[[i]][ , names(tslist[[i]]) %in% nam | names(tslist[[i]])=="DATE_TIME"]
  }
  cdata_sites=cdata[cdata$G2G.ID %in% nam, ]
  
  
  if(nrow(cdata_sites)!=length(nam)){
    stop("problem making cdata_sites")
    cdata_sites=NA
  }
  
}

names(tslist)
#get_stats(tsdat,obs,sstart=ISOdatetime(1066,1,1,0,0,0) ,send=  ISOdatetime(2999,1,1,0,0,0),bias_correct=F,removeStart=NA,removeEnd=NA, excludeDF=NULL,exclField="exclude")

obs[1:5,1:5]
dim(obs)
#calculate performance stats between XBstart and XBend
mk_statsDFlist=T
if(mk_statsDFlist){
  sstart=XBstart ;send=  XBend
  statsDFlist = vector(mode = "list", length = length(tslist))
  for(i in 1:length(tslist)){
    cat(i,names(tslist)[i],"\n")
    statsDFlist[[i]]=get_stats(tslist[[i]],obs,sstart=sstart  ,send=send,bias_correct=F,excludeDF = excludeDF )
  }
  names(statsDFlist)=names(tslist)
}
head(statsDFlist[[1]])
##### testing get_stats w excludeDF ####
#s0 = get_stats(tslist[["RG"]],obs,sstart=sstart  ,send=send )
#s1 = get_stats(tslist[["RG"]],obs,sstart=ISOdatetime(2020,1,1,0,0,0)  ,send=send )
#excludeDFTMP = excludeDF
#excludeDFTMP$exclude = sapply(excludeDFTMP$date, function(dd) ifelse(dd<ISOdatetime(2020,1,1,0,0,0),T,F))
#s2 = get_stats(tslist[["RG"]],obs,sstart=ISOdatetime(2020,1,1,0,0,0)  ,send=send ,excludeDF =excludeDFTMP )
#head(s0);head(s1);head(s2)

mergeDF=T
#merge in to statsDFListAll : statsDFlist, thresholdStatsList, CatStatDat (when existant)
if(mergeDF & exists("statsDFlist") & exists("CatStatDat")){ 
  if(exists("statsDFListAll"))rm(statsDFListAll)
  
  #merge in thresholdStatsList if exists
  if(exists("statsDFlist") & exists("thresholdStatsList")){ 
    if(all( names(statsDFlist) == names(thresholdStatsList) ) ){
      
      statsDFListAll=vector(mode = "list", length = length(statsDFlist)  )
      for(i in 1:length(statsDFlist)){
        statsDFListAll[[i]]=merge(statsDFlist[[i]],thresholdStatsList[[i]],all=T)
      }
      names(statsDFListAll)=names(statsDFlist)
    }else{
      stop("name err")
    }
  }else{
    statsDFListAll=statsDFlist
  }
  
  for(ii in 1:length(statsDFListAll)){
    if(!(all(statsDFListAll[[ii]]$G2G.ID %in% CatStatDat$G2G.ID) &  dim(statsDFListAll[[ii]])[1] == dim(CatStatDat)[1]   ))stop("name err 2")
    statsDFListAll[[ii]]=merge(statsDFListAll[[ii]], CatStatDat)
    statsDFListAll[[ii]]=statsDFListAll[[ii]][order(statsDFListAll[[ii]]$Mean_LUE),]
    rownames(statsDFListAll[[ii]])=NULL
  }
  
}else if (mergeDF & !(exists("statsDFlist") & exists("CatStatDat"))){
  stop("cant merge: statsDFlist or CatStatDat missing")
}
nrow(statsDFlist[[1]])
nrow(statsDFListAll[[1]])

names(statsDFListAll)
head(statsDFListAll[["RG"]],3)

R2=sapply(statsDFListAll[["RG"]]$R2 , function(z)max(z,0))
area = statsDFListAll[["RG"]]$Found.area
plot(log10(area), R2)

KGE=sapply(statsDFListAll[["RG"]]$KGE , function(z)max(z,0))
plot(log10(area), KGE)


make_plots=T
if (make_plots){
  
  do_plot_hydrograph=T
  runs = c('15','16','17','all')
  if (do_plot_hydrograph){
    for(run in runs){
      if(run=='15'){
        showNames<-c("RG","xband15_V6a")
        use_plot_dir=paste0(plot_dir,"timeseries_15/")
      } else if(run=='16'){
        showNames<-c("RG","xband16_V6a")
        use_plot_dir=paste0(plot_dir,"timeseries_16/")
      }else if(run=='17'){
        showNames<-c("RG","xband17_V6a")
        use_plot_dir=paste0(plot_dir,"timeseries_17/")
      }else{#all
        showNames=c("RG","xband15_V6a","xband16_V6a","xband17_V6a")
        use_plot_dir=paste0(plot_dir,"timeseries_all/")
      }
      
      # use_plot_dir=paste0(plot_dir,"timeseries_all/")
      
      pstart= XBstart
      pend =  XBend#ISOdatetime(2018,11,15,0,0,0) 
      #pend =  ISOdatetime(2019,3,1,0,0,0) 
      
      #showNames=c("RG","xband15_V6","xband16_V6","xband17_V6")
      useCols= c(rgb(0,0,0),cols[names(cols) %in% showNames])
      
      names(useCols) = c("Obs.",plot_labs[showNames]  )
      tmp = CatStatDat[CatStatDat$Mean_Dist<=100,]
      (tmp = tmp[order(-tmp$Mean_LUE),1:11][1:9,])
      sites=c("744312",#nearest to radar
              "743509",#near to radar
              "740102",#fairly near to radar
              "PARKBG1"
      )  
      CatStatDat[1:4,]
      sites = CatStatDat[order(CatStatDat$Mean_LUE),"G2G.ID"] 
      
      i=0
      for(site in sites){
        i=i+1
        cat("site:",which(sites==site), "of", length(sites), ", G2G.ID = " ,site,"\n")
        names(statsDFListAll)
        lue =        round(statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"Mean_LUE"] ,3)
        dist =       round(statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"Mean_Dist"] ,3)
        R2_RG =      round(statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"R2"] ,3)
        R2_X =       round(statsDFListAll[[showNames[2]]][statsDFListAll[[1]]$G2G.ID ==site,"R2"] ,3)
        Found.area = round(statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"Found.area"] ,3)
        coun =             statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"Country"]
        river =            statsDFListAll[["RG"]][statsDFListAll[[1]]$G2G.ID ==site,"River.Name"]
        
        
        main=paste0(site ," (",river,"): LUE=",lue, " d=",dist," A=", Found.area, " R2(RG)=",R2_RG," R2(",showNames[2],")=", R2_X)
        ylab <- bquote(.(paste0("Flow (")) ~ m^3/s ~ .(")") )
        
        png(paste0(use_plot_dir,"timeseries_",i,"_",site,"_LUE=",lue,"_",coun,".png" ),  width=30,height=12, units="cm",res=300)
        #par(mgp=c(1.9,.6,0))
        par(mar=c(2, 4.5, 1.5, .1))#bottom, left, top, right
        use_leg=NA
        hydrographList(site,tslist[names(tslist) %in% showNames],obs,pstart,pend,ylab=ylab,main=main,pcolors=useCols,ylim=NA,lwd=2,leg=use_leg ,maxfact=1.05,excludeDF=excludeDF)
        dev.off()
      }
    }# loop over runs
  }
  
  
  
  "
REMOVE R2LE0 THEN RECHECK!
timeseries_23_770302_LUE=1.252_EA-NRW - portion of obs to qc: very high but all in a period of downtime (R2=-0.1 w RG).
timeseries_25_133157_LUE=1.267_SEPA   - very unnatural (square) at low flows   (R2=-0.8 w RG).
timeseries_133_STOCKB2_LUE=3.232_EA-NRW  - some unnaturalness in the flow: there its entirely flat at low flow - but probably ok (R2=0.79 w RG).

"
  
  #PAPER FIGURE 5 
  make_stats_box_plot=T
  if(make_stats_box_plot){
    #manually changing the periods
    #statsDFlist_SAVE=statsDFlist
    #statsDFlist=statsDFlist_p1
    #statsDFlist=statsDFlist_p2
    #statsDFlist=statsDFlist_SAVE
    
    use_plot_dir=paste0(plot_dir,"boxplots/")
    stats=c("R2", "r",  "KGE_sqrt","perc_bias" )
    ylabs=list( bquote( R^2 ~ .(" Efficiency") ),"r", "KGE'[sqrt]","Bias")
    ylims=list(c(0,1),c(0.5,1),c(0,1),c(-100,100))
    
    use_plot_flows = plot_flows
    use_plot_labs  = plot_labs
    use_cols=cols
    statsDFlistTmp = statsDFListAll
    
    #png(paste(plot_dir,"/boxplot.png",sep=""),   width=8,height=12, units="cm",res=600) 
    #make_boxplot(statsDFlistTmp,stats,ylabs,ylims,plot_flows,plot_labs,col=cols)
    #dev.off()
    
    for(MAXLUE in c( 2,3,4,5,Inf)){
      UseSites=CatStatDat[CatStatDat$Mean_LUE<=MAXLUE,"G2G.ID"]
      cat("reduce sites from ", dim(CatStatDat)[1], " to ", length(UseSites),"\n")
      UsestatsDFlist=vector(mode = "list", length = length(statsDFlistTmp))
      names(UsestatsDFlist)= names(statsDFlistTmp)
      for(ii in 1:length(UsestatsDFlist)){
        UsestatsDFlist[[ii]]=statsDFlistTmp[[ii]][statsDFlistTmp[[ii]]$G2G.ID %in% UseSites,]
      }
      UsestatsDFlist
      png(paste(use_plot_dir,"/boxplot_MAXLUE=",MAXLUE,"km_nsites=", length(UseSites),".png",sep=""),   width=8,height=12, units="cm",res=600) 
      make_boxplot(UsestatsDFlist,stats,ylabs,ylims,use_plot_flows,use_plot_labs,col=use_cols)
      dev.off()
    }
    rm(MAXLUE)
    
    bands=list()
    # bands=list(c(0,2),c(1,3),c(2,4),c(3,5),c(4,6),c(6,Inf))
    for(band in bands){
      UseSites=CatStatDat[ CatStatDat$Mean_LUE>=band[1] & CatStatDat$Mean_LUE<band[2],"G2G.ID"]
      cat("reduce sites from ", dim(CatStatDat)[1], " to ", length(UseSites),"\n")
      UsestatsDFlist=vector(mode = "list", length = length(statsDFlistTmp))
      names(UsestatsDFlist)= names(statsDFlistTmp)
      for(ii in 1:length(UsestatsDFlist)){
        UsestatsDFlist[[ii]]=statsDFlistTmp[[ii]][statsDFlistTmp[[ii]]$G2G.ID %in% UseSites,]
      }
      UsestatsDFlist=UsestatsDFlist[names(UsestatsDFlist)!="rg_precip_no_snow"]
      png(paste(use_plot_dir,"boxplot_LUErange=",band[1],"-",band[2],"km_nsites=", length(UseSites),"_",periodNow,".png",sep=""),   width=8,height=12, units="cm",res=600) 
      make_boxplot(UsestatsDFlist,stats,ylabs,ylims,use_plot_flows,use_plot_labs,col=use_cols)
      dev.off()
    }
    rm(statsDFlistTmp)
  }
  
  
  sorted_stat_plot=T
  if(sorted_stat_plot){
    use_plot_dir=paste0(plot_dir,"plot_stats_sorted/")
    maxD=Inf #cut sortBy at this value
    maxLUE=Inf#2.5
    #(precips=names(statsDFListAll))# ie only plot for these precipitation types
    
    #precips=c("RG","nimrod","xband1_V3", "xband14_V4","xband5_V4" ,"xband10_V4" )
    precips=c("RG","xband15_V6a","xband16_V6a","xband17_V6a")
    
    #sortBy="mean_rad_dist"
    sortBy="Mean_LUE"
    
    #pchs=c(3,1,15,17,18,16)
    pchs=c(3,1,15,17)
    names(pchs)=precips
    
    cexs=rep(1.5,length(precips))
    cexs[1]=1.8;cexs[2]=1.8
    names(cexs)=precips
    
    for(stat in c("perc_bias","r","R2")){ #stat ="perc_bias"
      if(stat=="R2"){
        minStat=0#limit low values
        maxStat=1
        #legOff=1.55#legend offset
        lpy = 1.15
      }
      if(stat=="r"){
        #legOff=1.27
        maxStat=1
        minStat=0.5#limit low values
        lpy = 1.075
      }
      if(stat=="perc_bias"){
        # legOff=155
        maxStat=100
        minStat=-100#limit low values
        lpy = 130
      }
      
      useXlab=""
      if(sortBy=="mean_rad_dist")useXlab="Catchment (ordered by radar distance)"
      if(sortBy=="Mean_LUE")useXlab="Catchment (ordered by LUE)"
      yLabs=stat
      if(stat=="R2")yLabs=bquote( R^2 ~ .(" Efficiency") )
      if(stat=="perc_bias")yLabs="Bias"
      
      tmp=statsDFListAll[[1]]
      X=1:length(tmp[tmp$Mean_Dist <maxD & tmp$Mean_LUE <maxLUE, stat])
      
      png(paste0(use_plot_dir,"sorted_stats_",stat,"_maxD=",maxD,"_maxLUE=",maxLUE,"_sortBy=",sortBy,".png" ),  width=30,height=15, units="cm",res=500)
      par(xpd=TRUE)
      par(mar=c(5.1, 4.6, 2.1, .1))#c(bottom, left, top, right)
      plot(X,ylim=c(minStat,maxStat),col="white",xlab=useXlab,ylab=yLabs)
      
      for(precip in precips){
        #get order based on statsDFListAll (extra stats not merged into statsDFlist_p1/p2)
        tmp=statsDFListAll[[precip]]
        tmp=tmp[tmp$Mean_Dist <maxD & tmp$Mean_LUE <maxLUE ,]
        orderTmp=order(tmp[,sortBy])
        
        tmp=statsDFListAll[[precip]]
        #tmp=tmp[tmp$Mean_Dist <maxD,]
        tmp=tmp[orderTmp,stat]
        tmp=sapply(tmp,function(z)max(z,minStat))
        tmp=sapply(tmp,function(z)min(z,maxStat))
        points(X,tmp,pch=pchs[precip],col=cols[precip],cex=cexs[precip], lwd=3)
      }#
      if(stat=="perc_bias")lines(c(-0,300),c(0,0))
      legend(x=10,y=lpy,plot_labs[precips],col=cols[precips] ,cex=1.2,pch=pchs[precips],bty="n" ,ncol=6)
      dev.off()
    }
  }
  warnings()
  head(statsDFListAll[["RG" ]])
  names(statsDFListAll)
  
  
  mk_correlations_plot=T
  if(mk_correlations_plot){
    
    use_plot_dir=paste0(plot_dir,"correlations")
    maxD=Inf #cut sortBy at this value
    maxLUE=3#2.5
    selDF = statsDFListAll[["RG" ]][,c("G2G.ID","Mean_LUE","Mean_Dist")]
    cond = (selDF$Mean_LUE < maxLUE & selDF$Mean_Dist < maxD)
    
    for(stat in c("R2", "perc_bias","r")){
      corDF = selDF[cond,]
      for(nam in names(statsDFListAll)){
        corDF =merge(corDF,statsDFListAll[[nam ]][,c("G2G.ID",stat)], by = "G2G.ID")
        names(corDF)[names(corDF)==stat]=nam
      }
      head(corDF)
      tail(corDF)
      #tmpDF = corDF[, c("RG", "nimrod", "Mean_LUE", "Mean_Dist", "xband1_V3","xband14_V4","xband5_V4","xband10_V4" ) ]
      tmpDF = corDF[,c("RG","xband15_V6a","xband16_V6a","xband17_V6a")]
      png(paste(use_plot_dir,"/correlations_stat=",stat,"_maxLUE=",maxLUE,"_maxD=",maxD,".png",sep=""),   width=18,height=18, units="cm",res=600) 
      chart.Correlation(tmpDF, histogram=TRUE, pch=19)
      dev.off()
      
      head(corDF)
      x="xband15_V6a"
      png(paste(use_plot_dir,"/xband_stat=",stat,"_maxLUE=",maxLUE,"_maxD=",maxD,".png",sep=""),   width=18,height=6, units="cm",res=600) 
      par(mfrow=c(1,3) )
      for(y in c("xband15_V6a","xband16_V6a","xband17_V6a")){#y="ncas_xband_1"
        rangeNow =   range( c(corDF[,x],corDF[,y]) )
        plot(corDF[,x],corDF[,y],xlab=plot_labs[x],ylab=plot_labs[y],main=stat, xlim= rangeNow )
        lines(200*c(-1,1),200*c(-1,1),col="green")
      }
      dev.off()
    }
    
  }
  warnings()
  
  #PAPER FIGURES 4
  LUE_vs_distance_model_2=T
  if(LUE_vs_distance_model_2){
    plot_dirNow=paste0(plot_dir,"correlations_model/")
    flows  = c( "xband15_V6a","xband16_V6a","xband17_V6a")
    addLegend=F
    eVar="Mean_LUE";#eVar="mean_rad_dist"
    xlabUse=eVar
    if(eVar=="Mean_LUE") xlabUse="Mean LUE (km)"
    
    for (stat in c( "r","R2","perc_bias"  ) ){#stat="perc_bias"
      print(stat)
      ylabUse=stat
      if(stat=="R2" )ylabUse=bquote( R^2 ~ .(" Efficiency") )
      if(stat=="perc_bias")ylabUse="Bias (%)"
      
      #scatter plot with model fit
      mk_s_plot=T
      fitDF=data.frame(flow=character(0),Intercept=numeric(0),lue=numeric(0),radDist=numeric(0),lueSD=numeric(0),radDistSD=numeric(0));n=1
      for(flow in flows){#flow="xband10_V4"
        mergedStats=statsDFListAll[[flow]]
        lue=mergedStats[,"Mean_LUE"]
        radDist=mergedStats[,"Mean_Dist"]
        
        statsVec=mergedStats[,stat]
        if(stat=="R2") statsVec = sapply( statsVec ,function(x) max(x,0) )
        mergedStats[1:3,]
        
        res=lm(statsVec~lue+radDist)
        print(res)
        fitDF[n,"flow"]=flow
        resVec=c(res[["coefficients"]]["lue"] ,res[["coefficients"]]["radDist"]    )
        resVec=c(resVec,  resVec*c( sd( lue ),   sd( radDist) )   )
        
        fitDF[n,"Intercept"]=res[["coefficients"]]["(Intercept)"] 
        fitDF[n,c("lue","radDist","lueSD","radDistSD")]=resVec
        n=n+1
        if(mk_s_plot & flow=="xband15_V6a"){
          png(paste(plot_dirNow,"/scatter_",stat,"_",eVar,"_",flow,".png",sep=""),   width=9,height=9, units="cm",res=600)# width=14,height=14, units="cm",res=600)
          par(mar=c(4. ,4.5,.1,.1)) #sets the bottom, left, top and right 
          ee=mergedStats[,eVar]
          res2=lm(statsVec~ee)
          statsVecRGprecip = statsDFListAll[["RG"]][,stat]
          res3=lm(statsVecRGprecip~ee)
          rangeNow=range(statsVec,statsVecRGprecip)
          if(stat=="perc_bias")rangeNow = max(abs(rangeNow))*c(-1,1)
          x=0:120
          plot(ee , statsVecRGprecip ,ylab=ylabUse,xlab=xlabUse,pch=1,col=rgb(108/255,83/255,83/255,alpha=1),ylim=rangeNow) #rgb(0,0,0,alpha=.4) #main=plot_labs[flow],
          if(stat!="R2")lines(x,res3$coefficients[1]+res3$coefficients[2]*x,lwd=2,lty=3,col=cols["RG"])#trend line for RG
          points(ee , statsVec ,ylab=ylabUse,xlab=xlabUse,pch=16,col=cols["xband15_V6a"])#rgb(0,0,1,alpha=.7)
          
          if(stat!="R2")lines(x,res2$coefficients[1]+res2$coefficients[2]*x,lwd=2)#trend line for NXPol
          if(addLegend)legend("topleft",paste("cor =", round(cor(statsVec,ee),digits = 4) ))
          dev.off()
        }
      }
      write.csv(fitDF, paste0(plot_dirNow,"fitDF_",stat,"_",eVar,".csv" ) )
    }
    summary(res)
  }
  
  names(statsDFListAll)
  #flow="RG" #"RG" "nimrod" "xband1_V3" "xband14_V3b" "xband5_V3b" "xband10_V3b" "xband14_V4" "xband5_V4" "xband10_V4" 
  #plot(statsDFListAll[[flow]][,c("Mean_LUE","perc_bias")],col=statsDFListAll[[flow]]$Col,lwd=2)
  
  
  export_statsDFListAll=T
  if(export_statsDFListAll){
    for(nam in names(statsDFListAll)){
      write.csv( statsDFListAll[[nam]], paste0(plot_dir,"statsDFListAll__",nam,".csv"))
    }
  }
  
  
  
  #PAPER FIGURES 1, 2, 7, 8
  map_best_performance=T
  if(map_best_performance){
    
    ###### OPTIONS ######
    use_plot_dir=paste0(plot_dir,"plot_maps/")
    #
    stats=c("LUE", "R2.xband15_V6a","R2.xband16_V6a","R2.xband17_V6a", "r.xband15_V6a", "r.xband16_V6a","r.xband17_V6a","perc_bias.xband15_V6a","perc_bias.xband16_V6a","perc_bias.xband17_V6a","r","R2","perc_bias")
    
    
    #use_flows=c( "nimrod","xband10_V4")#plot_flows for use with statsSimp
    use_flows=c( "xband15_V6a","xband16_V6a","xband17_V6a")
    
    addLegend = T
    addExampleCatchments=F
    statsSimp=c("R2","r","KGE_sqrt","perc_bias")
    
    colNA="black";densityNA=25#shaded lines if "NA" for any stats (e.g CSI). Color underneath shows through!
    
    radarLoc=c(295549, 514791)
    Radius0=150 ;Radius1=100 ;Radius2=50 ;theta=seq(0,2*pi,2*pi/10^4)
    
    ptCex=2.5 #poinn size for radar locations
    ptLwd=2.5
    
    FNcatShapeSEPA = "S:/projects/hydroJULES/catchment_stats/catchments/g2g_catchments_for_NCAS_20191204"
    layerSEPA      = "fullcats_g2g_sepa_v1_7_2_loc"
    FNcatShapeEA   = "S:/projects/hydroJULES/RAINE/Catchments/g2g_shape_files"                           
    layerEA        = "AllCatchments"
    
    
    ###### Load catchment shape files ######
    #catShapeSEPA <- readOGR(dsn = FNcatShapeSEPA , layer = layerSEPA) #no trailing "/" in FN, no ".shp" in layer.
    #catShapeEA   <- readOGR(dsn = FNcatShapeEA   , layer = layerEA  )   
    catShapeSEPA <- read_sf(dsn = FNcatShapeSEPA , layer = layerSEPA) #no trailing "/" in FN, no ".shp" in layer.
    catShapeEA   <- read_sf(dsn = FNcatShapeEA   , layer = layerEA  )   
    
    # "name" in catShapeEA appears to be "Site.No." -> replace with G2G.ID first
    row.names(cdata) = cdata$Site.No.
    
    
    
    #catShapeEA@data$name = cdata[catShapeEA@data$name,"G2G.ID"]
    catShapeEA$name = cdata[catShapeEA$name,"G2G.ID"]
    row.names(cdata) = NULL
    
    
    
    #names(catShapeEA@data) = c("GRIDCODE", "G2G_KM2", "g2g_id") 
    names(catShapeEA) = c("GRIDCODE", "G2G_KM2", "g2g_id","geometry") 
    catShapeEA   = catShapeEA[  catShapeEA$g2g_id   %in% statsDFListAll[[1]]$"G2G.ID", ]
    catShapeSEPA = catShapeSEPA[catShapeSEPA$g2g_id %in% statsDFListAll[[1]]$"G2G.ID", ]
    
    #rbind EA SEPA to catShape
    #catShapeSEPA@data = catShapeSEPA@data[,c("GRIDCODE", "G2G_KM2", "g2g_id")]
    catShapeSEPA = catShapeSEPA[,c("GRIDCODE", "G2G_KM2", "g2g_id","geometry")]
    catShape = rbind(catShapeEA,catShapeSEPA)
    #catShape<-catShapeEA
    #plot(catShapeEA,col="red")
    #plot(catShapeSEPA,col="blue",add=T)
    #plot(catShape,add=T,lwd=4)
    
    #There are some catchments that are "divided in two" (a single square connects only at a point)
    #They have G2G_KM2=1. Hence add area of full catchment and determine @plotOrder based on that. 
    rownames(cdata)=cdata$G2G.ID
    #head(catShape@data)
    rownames(cdata)=cdata$G2G.ID
  #  catShape@data[,"areaCdata"]= cdata[catShape@data[,"g2g_id"],"Found.area"]
  #  catShape@plotOrder = order(-catShape@data[,"areaCdata"])
    ctmp = cdata[,c("G2G.ID","Found.area")]
    names(ctmp) = c("g2g_id","areaCdata")
    catShape= merge(catShape,ctmp,by="g2g_id",all.y=FALSE,all.x=TRUE)
    
    #catShape[,"areaCdata"]= cdata[catShape[,"g2g_id"],"Found.area"]
    catShape$plotOrder = order(-catShape$areaCdata)
    rownames(cdata)=NULL
    
    #tutourial= https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
    #also seonaid S:\projects\ensemble_verification\Phase 2\Rcode\backup\20190807
    #http://keithnewman.co.uk/r/maps-in-r.html?msclkid=8318abffbfeb11eca0483455859a47b2
    mapobj<-map(database="worldHires",c("UK:Scotland","UK:Great Britain", 'Isle of Man','Isle of Wight','Wales:Anglesey'),fill=TRUE)
    ukcoast<-get_map_outline_bng(mapobj)
    names(ukcoast)<-c("east","north")
    
    #for( use_Stat in stats){
    # mapping(use_Stat) 
    #}
    ###### Loop on stats  ###### 
    for( use_Stat in stats){ #use_Stat="r" #use_Stat="R2.xband10_V4.xband10_V3b"
      cat("plotting ",use_Stat,"\n")
      HashFlag=F#tells if re-ploting is needed for hashing 
      
      if(use_Stat %in% statsSimp )  {
        pltName = "map_best"
        maxDF=mkBestDF(use_Stat,use_flows,statsDFListAll,cols,densityNA,colNA)
        use_cols    = sapply( catShape$g2g_id, function(x)maxDF[maxDF$G2G.ID==x,"col1"]  )
        use_cols2   = sapply( catShape$g2g_id, function(x)maxDF[maxDF$G2G.ID==x,"col2"]  )
        hashCon     = sapply( catShape$g2g_id, function(x)maxDF[maxDF$G2G.ID==x,"number_best"]>1  )
        HashFlag = any(maxDF$number_best>1)
        if(HashFlag)print("Hashing of equal bests to be used")
        #head(maxDF)
        #head(statsDFListAll$nimrod[,1:8])
        #head(statsDFListAll$xband10_V4[,1:8])
        
      }else{#for stat==LUE,"R2.xband10_V4","R2.xband10_V4.xband10_V3b",  etc
        
        if(use_Stat=="LUE"){
          pltName = "map_LUE"
          mLUE=sapply( catShape$g2g_id, function(x)CatStatDat[CatStatDat$G2G.ID==x,"Mean_LUE"]  )
          rMin=0#floor(min(CatStatDat$Mean_LUE))
          rMax=5#ceiling(max(CatStatDat$Mean_LUE))
          colFuncMp = colGoodBad
          use_cols=sapply( mLUE, function(x)colFuncMp(x,rMin,rMax,rev=T) )
          
        }
        
        #e.g. for use_Stat="R2.xband10_V4", use_Stat="R2.xband10_V4.xband10_V3b"
        splts = strsplit(use_Stat,split = ".",fixed=T )[[1]]
        if( length(splts)%in%c(2,3) ){  
          statNow   = splts[1]
          flw1      = splts[2]
          tmpDF1    = statsDFListAll[[ flw1 ]]
          plotStat1 = sapply( catShape$g2g_id, function(x)tmpDF1[tmpDF1$G2G.ID==x,statNow]  )
          
          if( length(splts)==3 ){
            flw2      = splts[3]
            tmpDF2    = statsDFListAll[[ flw2 ]]
            plotStat2 = sapply( catShape$g2g_id, function(x)tmpDF2[tmpDF2$G2G.ID==x,statNow]  )  
          }
          
          if( length(splts)==2 ){
            pltName = "map_stat"
            ##Options for statistics: 
            if(       statNow == "R2"){
              rMin=0 
              rMax=1
              colFuncMp = colGoodBad
            }else if (statNow == "r" ){
              rMin=0.5
              rMax=1
              colFuncMp = colGoodBad
            }else if (statNow == "perc_bias" ){
              rMin=-100
              rMax=100
              colFuncMp = colBias #use blue-grey-red colors
            }
            ##make use_cols:
            plotStat1[plotStat1<rMin]=rMin
            plotStat1[plotStat1>rMax]=rMax
            use_cols=sapply( plotStat1, function(x)colFuncMp(x,rMin,rMax) )
            
          }else if(length(splts)==3 ){
            pltName = "map_2stats"
            dd = plotStat1 - plotStat2
            rr = ceiling( 10*min(abs( range(dd) )))/10
            if(statNow=="perc_bias" )rr=100
            rMin=-rr;rMax=rr
            colFuncMp = colBias
            use_cols=sapply( dd, function(x)colFuncMp(x,rMin,rMax) )
          }
        }
      }
      
      
      ####### Make the plot  ####### 
      { 
        png(paste0(use_plot_dir,pltName,"__",use_Stat,".png"),height=7,width=6,units="in",res=800,pointsize=10)
        par(fig = c(0,1,0,6/7))           #(x1, x2, y1, y2)
        par(mai = c(0, 0, 0, 0) , xpd=NA) #(B , L , T , R)
        
        xl = radarLoc[1] + (140*10^3)*c(-1,1)
        yl = radarLoc[2] + (140*10^3)*c(-1,1)
        
        if(!HashFlag){
          print("No hashing")
          plot(catShape[,] ,xlim=xl, ylim=yl, col=use_cols )
        }else{
          print("Yes hashing")
          plot(catShape[       ,], xlim=xl, ylim=yl, col=use_cols )
          #plot(catShape[hashCon,], xlim=xl, ylim=yl, col=use_cols[hashCon] , add=TRUE )
          plot(catShape[hashCon,], xlim=xl, ylim=yl, col=use_cols2[hashCon] ,density=densityNA, add=TRUE)
          
        }
        
        if(addExampleCatchments){
          plot(catShape[catShape$g2g_id %in% exampleCats  ,]  ,col=rgb(1,1,1,alpha=0) ,add=T ,lwd=3)
        }
        
        
        #plot coast
        lines(ukcoast$east,ukcoast$north,col="GREY60",lwd=2) 
        #radar point
        points(radarLoc[1],radarLoc[2],pch=16,col="red",cex=ptCex)
        points(radarLoc[1],radarLoc[2],pch=1,col="black",lwd=ptLwd,cex=ptCex)
        #add cBand radar locations
        for(radNam   in cBandRadarDF$radarName ){
          EN=cBandRadarDF[cBandRadarDF==radNam,c("Easting", "Northing")]
          points(EN[1],EN[2],col="blue",pch=16,lwd=ptLwd,cex=ptCex)
          points(EN[1],EN[2],pch=1,col="black",lwd=ptLwd,cex=ptCex)
        }
        lines(Radius0*10^3*cos(theta)+radarLoc[1],Radius0*10^3*sin(theta)+radarLoc[2])
        lines(Radius1*10^3*cos(theta)+radarLoc[1],Radius1*10^3*sin(theta)+radarLoc[2],lty=2)
        lines(Radius2*10^3*cos(theta)+radarLoc[1],Radius2*10^3*sin(theta)+radarLoc[2],lty=2)
        
        
        
        if(addLegend){
          
          #add legend statsSimp
          if( use_Stat%in% statsSimp ){
            legend("bottomleft",legend=plot_labs[use_flows],inset=c(0.1 ,.2),pch=15,col=cols[use_flows],pt.cex = 2.5,cex=1.5,bty="n",ncol=2,pt.bg ="red")
          }
          
          #add legend not statsSimp
          if(! use_Stat %in% statsSimp ){
            par(fig = c(0.15,0.57, 0.18, .24), new = T)  #L,R,B,T
            map_2stats=(length(strsplit(use_Stat,split = ".",fixed=T )[[1]])==3)
            if(use_Stat=="LUE"){
              dx=1
            }else if(map_2stats){
              dx=(rMax-rMin)/100
            }else{
              dx=.1
            }
            x <- seq(rMin,rMax,dx)
            z=matrix(x)
            theseCols= sapply(x,function(w) colFuncMp(w,rMin,rMax,rev=(use_Stat=="LUE")))
            image(x,1,z,col=theseCols,xlab="",ylab="",yaxt='n',cex=2,xaxt="n",cex.main=2)
            
            if(use_Stat=="LUE") tit = "LUE (km)"
            if(length(splts)==2){
              if(splts[1]=="R2")        tit = "R2-efficiency"
              if(splts[1]=="r")         tit = "r"
              if(splts[1]=="perc_bias") tit = "Bias (%)"
            }
            if(length(splts)==3){
              if(splts[1]=="R2")        tit = "R2-efficiency change"
              if(splts[1]=="r")         tit = "r change"
              if(splts[1]=="perc_bias") tit = "Bias change (%)"
            }
            title(tit, line = .5,cex.main=2)
            if(length(x)>8){
              atNow = c(rMin,(rMax+rMin)/2,rMax)
            }else{
              atNow = x
            }
            axis(1,at=atNow,cex.axis=2)
          }
        }
        dev.off()
      }
    }
  }
  
  
  test_R2_components = F
  if(test_R2_components){
    plot_labs
    site="133167"
    tmp = statsDFListAll[["xband10_V3b"]]
    row.names(tmp)=tmp$G2G.ID
    tmp[c(site),1:9]
    
    tmp = statsDFListAll[["xband10_V4"]]
    row.names(tmp)=tmp$G2G.ID
    tmp[c(site),1:9]
    
    
    
    obs1=obs[,c("DATE_TIME",site)]
    mod1=tslist[["xband10_V3b"]][,c("DATE_TIME",site)] 
    mod2=tslist[["xband10_V4"]][,c("DATE_TIME",site)]
    
    obs1= obs1[ obs1$DATE_TIME>XBstart & obs1$DATE_TIME<XBend,site]
    mod1= mod1[ mod1$DATE_TIME>XBstart & mod1$DATE_TIME<XBend,site]
    mod2= mod2[ mod2$DATE_TIME>XBstart & mod2$DATE_TIME<XBend,site]
    head(obs1)
    head(mod1)
    head(mod2)
    
    any(is.na(obs1))
    any(is.na(mod1))
    any(is.na(mod2))
    
    
    #https://www.sciencedirect.com/science/article/pii/S0022169409004843?via%3Dihub
    plot(obs1,lwd=2,type="l")
    lines(mod1,col="red")
    lines(mod2,col="darkgreen")
    #install.packages('Metrics')
    
    1 - sum((mod1-obs1)^2)/sum(( obs1 - mean(obs1) )^2)
    1 - sum((mod2-obs1)^2)/sum(( obs1 - mean(obs1) )^2)
    
    (alpha = sd(mod1)/sd(obs1))
    (alpha2 = sd(mod2)/sd(obs1))
    (beta  = (mean(mod1)-mean(obs1))/sd(obs1))
    (beta2  = (mean(mod2)-mean(obs1))/sd(obs1))
    (r     = cor(mod1,obs1))
    (r2     = cor(mod2,obs1))
    
    
    2*alpha*r - alpha^2 - beta^2 
    2*alpha2*r2 - alpha2^2 - beta2^2 
    
    
    A  = cor(mod1,obs1)^2
    A2 = cor(mod2,obs1)^2
    B  = (cor(mod1,obs1) -  sd(mod1)/sd(obs1) )^2
    B2 = (cor(mod2,obs1) -  sd(mod2)/sd(obs1) )^2
    C  = ((mean(mod1)-mean(obs1))/sd(obs1))^2
    C2 = ((mean(mod2)-mean(obs1))/sd(obs1))^2
    
    A -B -C
    A2-B2-C2
    
    2*alpha*r - alpha^2 - beta^2 
    
  }
  
  
  
  pstart= XBstart
  pend =  XBend#ISOdatetime(2018,11,15,0,0,0) 
  
  
  #funny obs in first 72:
  #750605
  #133157
  #UGLYDB1
  
  
  compare_versions =F
  if(compare_versions){
    LUEmax=3
    evar= "Mean_LUE"
    use_plot_dir = paste0(plot_dir, "compare_V3b_V4/")
    
    for(precip in c("ncas_xband_1","ncas_xband_5","ncas_xband_10", "ncas_xband_14")){ #precip = "ncas_xband_10"
      
      statsDF1 = read.csv(paste0("S:/projects/hydroJULES/RAINE//figs_V3b/","statsDFListAll__",precip,".csv"))
      statsDF2 = read.csv(paste0("S:/projects/hydroJULES/RAINE//figs_V4/","statsDFListAll__",precip,".csv"))
      all(statsDF1$G2G.ID==statsDF2$G2G.ID)
      
      mergeDF = merge(statsDF1[,c(2:7,10)],statsDF2[,c(2:7,10)],by="G2G.ID")
      mergeDF=mergeDF[order(mergeDF$Mean_LUE.x),]
      mergeDF[1:10,]
      
      head(statsDF1)
      
      
      
      for(stat in c("R2","r","perc_bias")){#  stat= "R2"
        
        x1 = statsDF1[statsDF1$Mean_LUE<= LUEmax,evar] 
        x2 = statsDF2[statsDF2$Mean_LUE<= LUEmax,evar] 
        
        y1 = statsDF1[statsDF1$Mean_LUE<= LUEmax,stat]
        y2 = statsDF2[statsDF2$Mean_LUE<= LUEmax,stat] 
        
        if(stat=="R2"){
          y1=sapply(y1,function(x)max(x,0)) 
          y2=sapply(y2,function(x)max(x,0))
        }
        
        png(paste0(use_plot_dir,"compare_",precip,"_",stat,"LUEmax=",LUEmax,"_sortBy=",sortBy,".png" ),  width=12,height=24, units="cm",res=600)
        par(mfrow=c(2,1))
        
        plot(   y1,y2,pch=16,col=rgb(0,0,0,.5), xlab="V3b",ylab="V4" , main = paste(stat,": med(V3b) =",round(median(y1),4),"med(V4) =",round(median(y2),4) )   )
        lines(200*c(-1,1),200*c(-1,1),col="green")
        
        
        
        plot(   x1,y1,pch=16,col=rgb(0,0,0,.5) ,xlab="LUE (km)",ylab=stat,main="V3b (grey) compared to V4 (red)")
        points( x2,y2,pch=16,col=rgb(0.8,0,0,.5) )
        dev.off()
      }
    }
    
  }
  ###############################################################################
  
  #statsDFListAll_ALLSITES = statsDFListAll
  do_extras<-FALSE
  if(do_extras){
    tmp = statsDFListAll$rg_precip
    tmp[tmp$R2<0,1:8]
    
    tmp[tmp$R2<0,"G2G.ID"]
    
    tmp[tmp$R2<0.2,1:8]
    #    G2G.ID   perc_bias           r            R2          KGE    KGE_sqrt  Min_LUE Max_LUE
    #14  765850   61.289942  0.89521294  1.964059e-01   0.37319601  0.69961066 0.698087 1.29007
    #190 690207   33.02947   0.97398758 -4.156580e-02   0.48896574  0.52322762 4.812980 9.08148
    names(tslist)
    head(tslist$"rg_precip",2) # 133060 is the first sepa station.
    
    head(obs,2)
    
    mod[,"765850" ]
    #site = "765850" 
    site ="690207"
    if(doRuntimePlots){
      mod = tslist$"rg_precip"
      plot(obs$DATE_TIME,obs[,site],ylim=c(0,15))
      lines( mod$DATE_TIME , mod[,site  ] , col="red")
    }
    tmp = merge( obs[,c("DATE_TIME",site) ] , mod[,c("DATE_TIME",site)  ],by="DATE_TIME"    )
    ob=tmp[,"690207.x"]
    mod=tmp[,"690207.y"]
    #eliminate NA or < 0 flows
    mod[mod<0]=NA
    ob[ob<0]=NA
    m<-is.na(ob)|is.na(mod)
    ob=ob[!m]
    mod=mod[!m]
    
    #perc_bias 
    mean_ob=mean(ob)
    mean_mod=mean(mod)
    bias<- mean_mod-mean_ob
    perc_bias<-(bias/mean_ob)*100
    1-mean_mod/mean_ob
    
    #correlation coeff
    r = cor(ob,mod)
    
    #R2 stat
    r2<- NA
    n_sq_sigma_ob=sum((ob-mean_ob)^2)
    if ( n_sq_sigma_ob>0) r2= 1 -  sum((ob-mod)^2)/ n_sq_sigma_ob 
    
    if(doRuntimePlots){
      plot(ob,ylim=c(-0,15))
      lines(mod,col="red")
      lines(mod-ob,col="green")
      
      plot(ob,mod)
    }
  }
  mk_thresholdStatsList=F;mk_thresh_boxplots=T
  if(mk_thresholdStatsList){
    thresh="QMED/2"
    thres_obs="g2g"
    sstart = XBstart ;send = XBend
    
    #create DF of various threshold crossings (prob only need QMED/2 & QMED)
    thresholds_vec_mod<-data.frame(  QMEDby2=0.5*cdata_sites$qmed_g2g, QMED=cdata_sites$qmed_g2g,QT5=cdata_sites$QT_FAC_05*cdata_sites$qmed_g2g     )
    row.names(thresholds_vec_mod)=cdata_sites$G2G.ID
    names(thresholds_vec_mod)[names(thresholds_vec_mod)=="QMEDby2"]="QMED/2"
    
    #separate thesholds for the observation can be used 
    thresholds_vec_obs<-data.frame(  QMEDby2=0.5*cdata_sites$QT_MED, QMED=cdata_sites$QT_MED,QT5=cdata_sites$QT_05    )
    row.names(thresholds_vec_obs)=cdata_sites$G2G.ID
    names(thresholds_vec_obs)[names(thresholds_vec_obs)=="QMEDby2"]="QMED/2"
    #cbind(thresholds_vec_mod,thresholds_vec_obs)
    
    if(exists("use_thresholds_vec_obs"))rm(use_thresholds_vec_obs)
    if(thres_obs=="obs")use_thresholds_vec_obs=thresholds_vec_obs
    if(thres_obs=="g2g")use_thresholds_vec_obs=thresholds_vec_mod
    
    thresholdStatsList = vector(mode = "list", length = length(tslist))
    for(i in 1:length(tslist)){
      cat(i,names(tslist)[i],"\n")
      UseTsdat=tslist[[i]]
      UseObs=obs
      
      if( (!is.na(removeStart)) & (!is.na(removeEnd)) ){
        sites=names(UseTsdat)[names(UseTsdat)!="DATE_TIME"]
        UseTsdat[ (UseTsdat$DATE_TIME > removeStart) & (UseTsdat$DATE_TIME < removeEnd),sites]=NA
        
        sites=names(UseObs)[names(UseObs)!="DATE_TIME"]
        UseObs[ (UseObs$DATE_TIME > removeStart) & (UseObs$DATE_TIME < removeEnd),sites]=NA
      }
      
      
      #calc binary 0/1 DF for upward threshold crossings
      binDatSim=get_upward_crossings(UseTsdat,thresholds_vec_mod,thresh=thresh)
      binDatObs=get_upward_crossings(UseObs,use_thresholds_vec_obs,thresh=thresh)
      #use this to obtain various statistics
      thresholdStatsList[[i]]=calc_threshold_stats(binDatSim,binDatObs,sstart=sstart,send=send,QC_sites=QC_obs_thresh)
    }
    names(thresholdStatsList)=names(tslist)
    i=1
    #print the statistics nicely for each flow in tslist
    for(i in 1:length(tslist)){
      cat("**********************************\n")
      cat("    STATS FOR",names(tslist)[[i]],"\n")
      cat("**********************************\n")
      display_thresholdStats(thresholdStatsList[[i]])
    }
    
    stats=c("POD", "PODF", "FAR", "CSI")
    ylabs=stats
    ylims=list(c(0,1),c(0,1),c(0,1),c(0,1))
    x=c("rg_precip_no_snow", "rg_precip","nimrod_precip")
    xlabs=c("RG (NS)","RG","Nimrod")
    
    if(mk_thresh_boxplots){
      png(paste(plot_dir,"/boxplot_thresholds_thres_obs=",thres_obs,".png",sep=""),   width=8,height=12, units="cm",res=600) 
      make_boxplot(thresholdStatsList,stats,ylabs,ylims,x,xlabs)
      dev.off()
    }
  }
  
  
  examine_ordered_stats=F
  if(examine_ordered_stats){
    dat=statsDFListAll[[ "ncas_xband_10" ]]
    DFo=dat[order(dat$Mean_LUE),c("G2G.ID","Mean_LUE" )]
    for(nam in names(statsDFListAll)){
      dat=statsDFListAll[[nam]]
      #dat=dat[order(dat$Mean_LUE)[1:4],c("G2G.ID" ,"perc_bias")]
      DFo[  ,nam ]=dat[order(dat$Mean_LUE), "perc_bias" ]
    }
    DFo
    write.csv(DFo,file=paste0(projDir,"radar_g2g_paper/bias_stats.csv"))
  }
  
  
  #PAPER FIGURE 3? 
  do_plot_hydrograph_paper=F
  if (do_plot_hydrograph_paper){
    use_plot_dir=paste0(plot_dir,"exampleFlows/")
    
    pstart = XBstart
    pend = XBend
    #pstart=ISOdatetime(2016,4,3,0,0,0)  # pend=  ISOdatetime(2016,4,13,0,0,0)
    # showNames=names(tslist)[names(tslist)!="rg_precip_no_snow"]
    #showNames=c("rg_precip" ,"nimrod_precip","ncas_xband_1" ,"ncas_xband_10"  )
    showNames=c("ncas_xband_10" , "rg_precip"    )
    #useCols= c(rgb(0,0,0),cols[names(cols) %in% showNames])
    
    useCols= c(rgb(0,0,0),rgb(0,0,0.9),cols[names(cols) %in% "ncas_xband_10"])
    
    useCols= c(rgb(0,0,0),rgb(0,1,1),cols[names(cols) %in% "ncas_xband_10"])
    
    names(useCols)[1]="Obs"
    
    sites=c("234307","371579","234161","234319")  
    
    i=0
    for(site in sites){
      i=i+1
      cat("site:",which(sites==site), "of", length(sites), ", G2G.ID = " ,site,"\n")
      main=site
      ylab <- bquote(.(paste0("Flow (")) ~ m^3/s ~ .(")") )
      c("Obs",plot_labs[showNames]  )
      png(paste0(use_plot_dir,"timeseries_",i,"_",site,".png" ),  width=20,height=6, units="cm",res=500)
      #par(mgp=c(1.9,.6,0))
      par(mar=c(2, 4.5, 1.5, .1))#bottom, left, top, right
      use_leg="none"
      hydrographList(site,tslist[names(tslist) %in% showNames],obs,pstart,pend,ylab=ylab,main=main,pcolors=useCols,ylim=NA,lwd=3,leg=use_leg ,maxfact=1.05)
      dev.off()
    }
  }
  
  
  
  
  examine_correlations_in_performance=F
  if(examine_correlations_in_performance){
    use_plot_dir=paste0(plot_dir,"plots_correlations2/")
    check_plots=F#print to screen individual plot of vars 
    #Delta= flow1 - flow0
    #flow0="rg_precip"
    flow0="nimrod_precip" 
    flow1="ncas_xband_10"
    stats1=c("R2","r","KGE_sqrt","perc_bias")#stats from statsDFlist
    stats2=c("POD", "PODF","FAR", "CSI")#stats from thresholdStatsList
    #stats=c(stats1,stats2)
    
    stats=c("R2","r","perc_bias")
    set_leq0_to_0=T#set any stats that are <0 to 0 EXCEPT for perc_bias
    stat="r"
    for(stat in stats){
      rm(statList)
      if(stat %in% stats1){
        statList=statsDFlist
      }else if(stat %in% stats2){
        statList=thresholdStatsList
      }
      
      DF0=statList[[flow0]][,c("G2G.ID",stat)]
      DF1=statList[[flow1]][,c("G2G.ID",stat)]
      
      #check ordering of G2G.ID's
      if(length(DF0$"G2G.ID")==length(DF1$"G2G.ID")){
        if(!all(DF0$"G2G.ID"==DF1$"G2G.ID")){
          stop("check G2G ids")
          DF0=NULL
        }
      }else{
        stop("check G2G ids")
        DF0=NULL
      }
      
      
      if(set_leq0_to_0 & stat!="perc_bias"){
        DF0[ DF0[,stat]<0 & !is.na(DF0[,stat]),stat]=0
        DF1[ DF1[,stat]<0 & !is.na(DF1[,stat]),stat]=0
      }
      
      
      DeltaDF=DF0
      DeltaDF[,stat]=DF1[,stat]-DF0[,stat]
      names(DeltaDF)=c("G2G.ID", "Delta")
      DeltaDF=merge(DeltaDF,CatStatDat,by="G2G.ID")
      DeltaDF[,"log_area"]=log(DeltaDF$Area_RG_calc)
      DeltaDF=merge(DeltaDF,DF0)
      names(DeltaDF)[names(DeltaDF)==stat]=paste0(stat,"_0")
      DeltaDF=merge(DeltaDF,DF1)
      names(DeltaDF)[names(DeltaDF)==stat]=paste0(stat,"_1")
      DeltaDF=DeltaDF[, c(paste0(stat,"_0"),paste0(stat,"_1"),"Delta",  "RG_den_km2", "log_area", "mean_cat_elev", "Mean_LUE","mean_rad_dist","perc_snow_aff")]
      
      mk_lm=F
      if(mk_lm){
        fit0=lm(DeltaDF[,paste0(stat,"_0")]~DeltaDF$"RG_den_km2"+DeltaDF$"log_area"+DeltaDF$"mean_cat_elev"+ DeltaDF$"Mean_LUE"+DeltaDF$"mean_rad_dist"+DeltaDF$"perc_snow_aff")
        fit1=lm(DeltaDF[,paste0(stat,"_1")]~DeltaDF$"RG_den_km2"+DeltaDF$"log_area"+DeltaDF$"mean_cat_elev"+ DeltaDF$"Mean_LUE"+DeltaDF$"mean_rad_dist"+DeltaDF$"perc_snow_aff")
        fitDel=lm(DeltaDF$"Delta"~DeltaDF$"RG_den_km2"+DeltaDF$"log_area"+DeltaDF$"mean_cat_elev"+ DeltaDF$"Mean_LUE"+DeltaDF$"mean_rad_dist"+DeltaDF$"perc_snow_aff")
        summary(fit0)
        summary(fit1)
        summary(fitDel)
        
        
        
        fit0=lm(DeltaDF[,paste0(stat,"_0")]~DeltaDF$"RG_den_km2"+DeltaDF$"log_area"+DeltaDF$"perc_snow_aff")
        fit1=lm(DeltaDF[,paste0(stat,"_1")]~DeltaDF$"RG_den_km2"+DeltaDF$"log_area"+ DeltaDF$"perc_snow_aff")
        fitDel=lm(DeltaDF$"Delta"~DeltaDF$"RG_den_km2"+DeltaDF$"log_area" +DeltaDF$"perc_snow_aff")
        summary(fit0)
        summary(fit1)
        summary(fitDel)
        
        median(DeltaDF$"Delta")
        mean(DeltaDF$"Delta")
      }
      
      if(check_plots){ 
        for(col in names(DeltaDF)){
          plot(DeltaDF[,col],main=col)
          Sys.sleep(2)
        }
      }
      
      png(paste(use_plot_dir,"/correlations_stat=",stat,"_Delta=",flow1,"-",flow0,"_.png",sep=""),   width=14,height=14, units="cm",res=600) 
      chart.Correlation(DeltaDF[,], histogram=TRUE, pch=19)
      dev.off()
    }
    
    
  }
  
  
  
  
  ########### Make single scatter plot ##############
  LUE_vs_distance_model=F
  if(LUE_vs_distance_model){
    
    #stat="R2"      
    #stat="perc_bias"      
    stat="r"
    eVar="Mean_LUE"
    #eVar="mean_rad_dist"
    #flow="ncas_xband_10"
    
    ylabUse=stat
    if(stat=="R2" )ylabUse=bquote( R^2 ~ .(" Efficiency") )
    if(stat=="perc_bias")ylabUse="Bias"
    
    #scatter plot with model fit
    mk_s_plot=T
    fitDF=data.frame(flow=character(0),Intercept=numeric(0),lue=numeric(0),radDist=numeric(0),lueSD=numeric(0),radDistSD=numeric(0));n=1
    for(flow in names(statsDFListAll)[4:13]){
      mergedStats=statsDFListAll[[flow]]
      lue=mergedStats[,"Mean_LUE"]
      radDist=mergedStats[,"mean_rad_dist"]
      (res=lm(mergedStats[,stat]~lue+radDist))
      
      fitDF[n,"flow"]=flow
      resVec=c(res[["coefficients"]]["lue"] ,res[["coefficients"]]["radDist"]    )
      resVec=c(resVec,  resVec*c( sd(mergedStats[,"Mean_LUE"]),   sd(mergedStats[,"mean_rad_dist"]) )   )
      
      fitDF[n,"Intercept"]=res[["coefficients"]]["(Intercept)"] 
      fitDF[n,c("lue","radDist","lueSD","radDistSD")]=resVec
      n=n+1
      if(mk_s_plot){
        png(paste(plot_dir,"plot_corr3/scatter_",stat,"_",eVar,"_",flow,".png",sep=""),   width=14,height=14, units="cm",res=600)
        ee=mergedStats[,eVar]
        res2=lm(mergedStats[,stat]~ee)
        plot(mergedStats[,eVar],sapply(mergedStats[,stat],function(x) x ),ylab=ylabUse,xlab=eVar,main=plot_labs[flow],pch=16,col=rgb(0,0,0,alpha=.5))
        x=0:10
        lines(x,res2$coefficients[1]+res2$coefficients[2]*x,lwd=2)
        dev.off()
      }
    }
    write.csv(fitDF, paste0(plot_dir,"plot_corr3/","fitDF_",stat,"_",eVar,".csv" ) )
    summary(res)
    
    #show limited differences between ncas_xband_10 and ncas_xband_9
    dat1=statsDFListAll[[ "ncas_xband_10"]]
    dat1=dat1[order(dat1$Mean_LUE),]
    
    dat2=statsDFListAll[[ "ncas_xband_9"]]
    dat2=dat2[order(dat2$Mean_LUE),]
    
    plot(dat1$perc_bias,dat2$perc_bias)
    lines( c(-100,100), c(-100,100) )
    
    
    # simple scater plots 
    plot(mergedStats$Mean_LUE,mergedStats$perc_bias)
    grid()
    
    plot(mergedStats$Mean_LUE,mergedStats$r)
    grid() 
    
    #Calculate R2 stat for the model and compare to what lm reports
    Model= fitDF[fitDF$flow==flow,"Intercept"]+ fitDF[fitDF$flow==flow,"lue"]*mergedStats$Mean_LUE + fitDF[fitDF$flow==flow,"radDist"]*mergedStats$mean_rad_dist
    1-sd(mergedStats$r - Model)^2/sd(mergedStats$r )^2
    summary(res)
    
  }
  
  
  choose_case_studies=F#choose potential case studies based on obs crossing threshold.
  if(choose_case_studies){
    use_plot_dir=paste0(plot_dir,"plots_highpeaks/new/")
    hoursPrior=5*24
    hoursPost=5*24
    thresh="QT5"; thres_obs="g2g" #g2g or obs
    thres_obs="obs";thresh="QMED" #22 june 2020
    # sstart=ISOdatetime(2016,1,22,9,0,0) ;send=  ISOdatetime(2016,9,1,9,0,0)
    sstart=XBstart ;send=XBend
    
    
    #create DF of various threshold crossings (prob only need QMED/2 & QMED)
    thresholds_vec_mod<-data.frame(  QMEDby2=0.5*cdata_sites$qmed_g2g, QMED=cdata_sites$qmed_g2g, QT5=cdata_sites$QT_FAC_05*cdata_sites$qmed_g2g, QT10=cdata_sites$QT_FAC_10*cdata_sites$qmed_g2g  , QT25=cdata_sites$QT_FAC_25*cdata_sites$qmed_g2g  , QT50=cdata_sites$QT_FAC_50*cdata_sites$qmed_g2g, QT100=cdata_sites$QT_FAC_100*cdata_sites$qmed_g2g )
    row.names(thresholds_vec_mod)=cdata_sites$G2G.ID
    names(thresholds_vec_mod)[names(thresholds_vec_mod)=="QMEDby2"]="QMED/2"
    
    #separate thesholds for the observation can be used 
    thresholds_vec_obs<-data.frame(  QMEDby2=0.5*cdata_sites$QT_MED, QMED=cdata_sites$QT_MED,QT5=cdata_sites$QT_05 ,QT10=cdata_sites$QT_10 ,QT25=cdata_sites$QT_25 ,QT50=cdata_sites$QT_50 ,QT100=cdata_sites$QT_100 )
    row.names(thresholds_vec_obs)=cdata_sites$G2G.ID
    names(thresholds_vec_obs)[names(thresholds_vec_obs)=="QMEDby2"]="QMED/2"
    #cbind(thresholds_vec_mod,thresholds_vec_obs)
    
    if(exists("use_thresholds_vec_obs"))rm(use_thresholds_vec_obs)
    if(thres_obs=="obs")use_thresholds_vec_obs=thresholds_vec_obs
    if(thres_obs=="g2g")use_thresholds_vec_obs=thresholds_vec_mod
    
    binDatObs=get_upward_crossings(obs,use_thresholds_vec_obs,thresh=thresh)
    
    
    con=binDatObs[,"DATE_TIME"]>=sstart & binDatObs[,"DATE_TIME"]<=send
    use_binDatObs=binDatObs[con,]
    for(site in sitesFullDat){
      if(sum(use_binDatObs[,site])==0)next()
      print(site)
      crossingTimes=use_binDatObs[use_binDatObs[,site]==1,"DATE_TIME"]
      for(i in 1:length(crossingTimes)){
        
        tim=crossingTimes[i]
        pstart=tim-hoursPrior*3600
        pend=tim+hoursPost*3600
        png(paste0(use_plot_dir,"timeseries_",site,".png" ),  width=30,height=15, units="cm",res=500)
        #par(mgp=c(1.9,.6,0))
        par(mar=c(3, 5, 3.5, 2))#bottom, left, top, right
        #hydrographList(site,tslist,obs,pstart,pend,ylab="flow (m3/s)",main=site,pcolors=NA,ylim=NA,lwd=2,leg=NA,maxfact=1.1)
        cond=obs$DATE_TIME>=pstart & obs$DATE_TIME<=pend
        plot(obs[cond,"DATE_TIME"], obs[cond,site],main = site,type="l" ,ylab="flow (m3/s)",xlab="Date")
        
        tmp=tslist$rg_precip
        cond=tmp$DATE_TIME>=pstart & tmp$DATE_TIME<=pend
        lines(tmp[cond,"DATE_TIME"], tmp[cond,site],col="blue")
        tmp=tslist$ncas_xband_10
        cond=tmp$DATE_TIME>=pstart & tmp$DATE_TIME<=pend
        lines(tmp[cond,"DATE_TIME"], tmp[cond,site],col="green")
        
        
        sapply( use_thresholds_vec_obs[site,] ,function(x)lines(c(pstart,pend),c(1,1)*x ,lty=2) )
        dev.off()
      }
    }
    
  }
  
  
  #PAPER FIGURE 1
  mk_elevation_map=F # must have evaluated map_best_performance first.
  if(mk_elevation_map){
    use_plot_dir=paste0(plot_dir,"plot_maps_FINAL/")
    
    addExampleCatchments=F
    library(raster)
    library(RColorBrewer)
    
    meanelevfile="C:/Users/johwal/Documents/hydroJULES/catchment_stats/meanelev_sepa.asc"
    meanelev_grid<-raster(meanelevfile)/10     #/10 to convert to m from dm
    meanelev_grid[meanelev_grid<0]<-NA
    
    zrange=seq(0,1000,200)
    use_colours<-rev(  colorRampPalette(brewer.pal(9,"YlOrBr"))(length(zrange)) )
    
    png(paste0(use_plot_dir,"Elevation_plot.png"),height=6,width=6,units="in",res=800,pointsize=10)
    par(bty="n")
    par(mar=c(0,0,0,0))#bottom, left, top, right
    plot( meanelev_grid,xlim=radarLoc[1]+c(-160*10^3 ,150*10^3),ylim=radarLoc[2] +c(-130*10^3,166*10^3),col=use_colours,breaks=zrange,zlim=c(min(zrange),max(zrange)),axes=FALSE,bty="n"  , legend=F)
    
    if(addExampleCatchments){
      plot(catShape[catShape$g2g_id %in% exampleCats  ,]  ,col=rgb(0,1,0),density=30,add=T )
      plot(catShape[catShape$g2g_id %in% exampleCats  ,]  ,col=rgb(1,1,1,alpha=0) ,add=T ,lwd=3)
    }
    plot(catShape[,]  ,col=rgb(1,1,1,alpha=0) ,add=T ,lwd=2)
    
    #plot(meanelev_grid,xlim=radarLoc[1]+c(-160*10^3 ,160*10^3),ylim=radarLoc[2]+20*10^3+c(-160*10^3,160*10^3),axes=FALSE,bty="n" )
    par(bty="o")
    #lines(ukcoast$east,ukcoast$north,col="GREY60") #plot coast
    points(radarLoc[1],radarLoc[2],pch=16,col="red",cex=ptCex)
    points(radarLoc[1],radarLoc[2],pch=1,col="black",lwd=ptLwd,cex=ptCex)
    
    lines(Radius0*10^3*cos(theta)+radarLoc[1],Radius0*10^3*sin(theta)+radarLoc[2])
    lines(Radius1*10^3*cos(theta)+radarLoc[1],Radius1*10^3*sin(theta)+radarLoc[2],lty=2)
    lines(Radius2*10^3*cos(theta)+radarLoc[1],Radius2*10^3*sin(theta)+radarLoc[2],lty=2)
    
    #add cBand radar locations
    for(radNam   in cBandRadarDF$radarName ){
      EN=cBandRadarDF[cBandRadarDF==radNam,c("Easting", "Northing")]
      points(EN[1],EN[2],col="blue",pch=16,lwd=ptLwd,cex=ptCex)
      points(EN[1],EN[2],pch=1,col="black",lwd=ptLwd,cex=ptCex)
    }
    
    par(fig = c(0.15,0.75, 0.81, .845), new = T )  #L,R,B,T
    
    x <- zrange
    z=matrix(x)
    theseCols= use_colours
    image(x,1,z,col=theseCols,xlab="",ylab="",yaxt='n',cex=2,xaxt="n",cex.main=2,bg="red")
    title("Elevation (m)", line = .5,cex.main=2)
    axis(1,at=zrange,cex.axis=2)
    dev.off()
    
  }    
  
  
  
}


