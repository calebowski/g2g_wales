#This code reproduces the original  E:\svn_jrw\G2G\Flow_quality_control\Data checking\RatingCurves_check.py. However it is updated to:
# 1) read the IMFS formatted xml (or csv data)
# 2) select default ratings only (and winter only for the few sites with separate winter/summer ratings)
# 3) output a csv of sites waranting further investigation.
# 4)	output a csv of the min/max (non-doubtful) and min/max doubtful flows so that they can be added to the hydrographs (done for non-898 sites only). Output as: “investigation_summary_csvs/ratings_data.csv”
# Step throught the code carefully if re-ran with new ratings (e.g. winter/summer ratings need picking by hand)

#All power ratings were re-formatted for the IMRD database from NFFS config as the IMFS rating power equation is Q=c(h-a)b  and NFFS rating power equation was Q=a(h+b)c. Ratings are supplied in IMFS format.

# JRW Jul23

#Output codes are:
#Rdsc - significant discontinuity between sections of the rating   (checkDisc)
#Rmf  - flow significantly below lower doutfull bound (checkMinFlow)
#Rhf (checkHighPrefac)
#Rhp  (checkHighPow)

### Results are ####

#For curent 898 sites:
##check discontinuities
#8 sites with discontinuity between sections of a power rating were checked. These were generally either fine due to either the discontinuity being reasonably small and/or occuring at flows above those actually achieved at the station. For one station, IMFSID = S27575_FW, it was noticed that the ratings at high flow (see peak in CY2004) looked possibly suspicious (raise and fall from 50m3/s to 150m3/s in 2hrs)  (Boscastle Flood and so no further action was taken).
##check high minimum flow:
#checked 9 sites with either min (posibly "doughtful") flow greater than 10m3/s or 10% of the range to the non-doughtfull flow.
# 1100TH 1800TH 2200TH -all merged flows in G2G-22 with R-IMFS unsuitable
# 5290TH - caused flow to be missing (almost always)
##high prefactor : 
#15 sites with ratings prefactor C>300 (occurring over a reasonable flow range) were checked. These were generally fine excepting IMFS_ID = 2175 which has several dubious spikes in its flow.
## high power:  
#9 sites with a ratings power of greater than 12 (occurring over a reasonable flow range) were checked. The flows all appeared reasonable, sometimes with the high power occurring at flows above those actually achieved at the station.


#For non-898 sites: 30 sites flagged in total
##check discontinuities
#9 sites checked. These were generally either fine, with site 4191 the exception (already noted) 
##check high minimum flow:
#checked 11 sites with either min (posibly "doughtful") flow greater than 10m3/s or 10% of the range to the non-doughtfull flow.
# most sites fine, but some mising substanital portion of low flow record
# 3090TH - caused flow to be almost always missing (already noted). Obs is available (short record)
##high prefactor : 
#7 sites with ratings prefactor C>300 (occurring over a reasonable flow range) were checked. 
#These were generally fine excepting IMFS_ID = 2202 already noted unsuitable as different from NRFA.
## high power:  
#4 sites with a ratings power of greater than 12 (occurring over a reasonable flow range) were checked
#The flows all appeared reasonable, excepting site 652411002 (already noted)


rm(list=ls()) # Clears the list of variables
options(max.print=500)
options(stringsAsFactors = FALSE)
Sys.setenv(TZ="GMT")
library("stringr")      
library(lubridate)
library(mapdata)
library(rgdal)  # install.packages("rgdal")
#library(mapproj )#install.packages("mapproj")
printp=function(...)print(paste0(...))
source("S:/data/data_processing/FFC/FFC_flows_2022update/code/flowProcFunctions.R")

################# functions to read XML formatted data #################  
#get the bit of string between the ">" and "<"
splitter=function(str){
  z = strsplit(str,">", fixed = T)[[1]][2]
  z =  strsplit(z,"<", fixed = T)[[1]][1]
  return(z)
}
splitter("<locationId>021041</locationId>")

#return named numeric vector of fields from the "<powerEquation" line
getNumbers<-function(str){
  vars = c("minStage", "maxStage", "alpha", "beta", "cr", "flag")
  ret  = rep(NA,length(vars)); names(ret) = vars
  str = gsub("= ","=",str,fixed = T )#just in case 
  str = gsub("= ","=",str,fixed = T )
  str = gsub("= ","=",str,fixed = T )
  str = gsub("<powerEquation ","",str,fixed = T )
  str = gsub("/>","",str,fixed = T )
  str = strsplit(str," ")[[1]]
  for( varN in  vars){
    z = gsub(paste0(varN,"=") , "",str[grepl(paste0(varN,"="),str)])
    ret[varN] = as.numeric(gsub("\"" , "",z))
  }
  return(ret)
}
getNumbers("<powerEquation minStage= \"0.352\" maxStage=\"3.7\" alpha=\"-0.35207\" beta=\"1.7505\" cr=\"28.395\" flag=\"0\"/>")
#####################################################################################


##### OPTIONS #####
updateDir = "S:/data/data_processing/FFC/FFC_flows_2022update/" 
fewsConfig="S:/data/incoming_files/FFC/toCEH 29-06-2022/FEWS SA/Config/"
NFFS_G2G<-read.csv(paste0(fewsConfig,"MapLayerFiles/NFFS_G2G.csv"))  #  NFFS_ID  OldID
cDF = read.csv(paste0(updateDir,"/investigation_summary_csvs/summary__cDF__ALL_SITES_V3.csv") )
use_xml=T
if(use_xml){
  pw_FN =paste0(updateDir,"RatingCurves.xml") #this was found in S:\data\incoming_files\FFC\toCEH 29-06-2022\FEWS SA\Config\RegionConfigFiles\RatingCurves.xml also there is MapLayerFiles/Hydrology/RatingType.csv
  defaultRat_FN = paste0(updateDir,"RatingType.csv")
}else{
  pw_FN =paste0(updateDir,"G2G_IMFSMetadata_Oct2021-PowerRatings.csv") #this was taken from a sheet in the supplied "G2G_IMFSMetadata_Oct2021.xlxs" - it is only for the 898 sites  
  defaultRat_FN = ""
}

### read ratings ###  => produces pwDF
readRatings=T
if(readRatings){
  nl=nchar(pw_FN)
  if(substr(pw_FN,nl-2,nl) =="csv"){# if read from csv table 
    pwDF = read.csv(pw_FN)
    pwDF=pwDF[,!names(pwDF) %in% c("RatingID","RatingRowID","RatingType")]
    pwDF[,c("IMFS_ID","IMFS_Name", "RatingCurveID", "LowerLevel", "UpperLevel","A","B","C","Doubtful")]
    #field "n" -> increments for each new RatingCurveID
    n = 0;RatingCurveID ="X"
    for( i in 1:nrow(pwDF)){
      if(pwDF[i,"RatingCurveID"] != RatingCurveID){
        n = n+1
        RatingCurveID = pwDF[i,"RatingCurveID"]
      }
      pwDF[i,"n"] = n
    }
    
  }else if(substr(pw_FN,nl-2,nl) =="xml"){ # if read from a particularly formated xml
    rcFile = file(description = pw_FN, open="r")
    rcLines = readLines(rcFile) 
    close(rcFile)
    pwDF = data.frame(n=numeric(0), IMFS_ID=character(0),IMFS_Name=character(0),qualifierId=character(0),dischargeUnit=character(0),LowerLevel=numeric(0), UpperLevel=numeric(0),A=numeric(0),B=numeric(0),C=numeric(0),Doubtful=numeric(0), QL=numeric(0),QU=numeric(0))
    gotMetaNames = c("IMFS_ID","IMFS_Name","qualifierId","dischargeUnit"); gotMeta = rep(F,length(gotMetaNames)); names(gotMeta) = gotMetaNames
    ## loop on xml file lines reading (i) gotMetaNames and (ii) fields specified in funtion "getNumbers" for line starting "<powerEquation"
    station=F; n=0
    for(lin in rcLines) {
      if(grepl("<ratingCurve>",lin,fixed=T)){
        station = T
        n=n+1
        gotMeta[gotMetaNames]=F
      }
      if(grepl("<locationId>",lin,fixed=T))    {IMFS_ID=splitter(lin);gotMeta["IMFS_ID"]=T}
      if(grepl("<stationName>",lin,fixed=T))   {IMFS_Name=splitter(lin);gotMeta["IMFS_Name"]=T}
      if(grepl("<qualifierId>",lin,fixed=T))   {qualifierId=splitter(lin);gotMeta["qualifierId"]=T}
      if(grepl("<dischargeUnit>",lin,fixed=T)) {dischargeUnit=splitter(lin);gotMeta["dischargeUnit"]=T}
      
      if(grepl("<powerEquation",lin,fixed=T)){#e.g. <powerEquation minStage="0.352" maxStage="3.7" alpha="-0.35207" beta="1.7505" cr="28.395" flag="0"/>
        if((!station) | any(!gotMeta) )stop("powerEquation: check here")
        pwDF[nrow(pwDF)+1,"n"]=n
        pwDF[nrow(pwDF),c("IMFS_ID","IMFS_Name","qualifierId","dischargeUnit")] = c(IMFS_ID,IMFS_Name,qualifierId,dischargeUnit)
        pwDF[nrow(pwDF),c("LowerLevel", "UpperLevel", "A" ,"B" , "C","Doubtful")] = getNumbers(lin)
      }
      
      if(grepl("<\ratingCurve>",lin,fixed=T))station = F
    }
    if(any(pwDF$dischargeUnit != "m3/s"))stop("non-m3/s units") 
    if(pwDF[1,"n"]>1){
      printp("Subtracting ",pwDF[1,"n"]," from n - presumed table or other type ratings before power ratings begin")
      pwDF[,"n"] = pwDF[,"n"] -  pwDF[1,"n"] +1
    }
    if( !  all((pwDF[2:(nrow(pwDF)),"n"] - pwDF[1:(nrow(pwDF)-1),"n"]) %in% c(0,1))) print("Presumed other non-power ratings scattered amoung the power ratings due to discontinuous n")
    
    
  }else{
   stop("pw_FN" )
  }
}

### add following to pwDF: 
#QL/QU (min and max flows for that section)
#dQ (jump of flow from previous sectio of ratting)
#currentSite (is in the current sites? T/F)
#RatID 
add_to_pwDF=T
if(add_to_pwDF){
  ####convert levels to flow using IMFS rating power equation :  Q=c(h-a)b
  pwDF$row=1:nrow(pwDF)
  pwDF$QL = pwDF$C*(pwDF$LowerLevel - pwDF$A)^pwDF$B
  pwDF$QU = pwDF$C*(pwDF$UpperLevel - pwDF$A)^pwDF$B
  pwDF$QL[pwDF$LowerLevel < pwDF$A] = 0
  pwDF$QU[pwDF$UpperLevel < pwDF$A] = 0
  
  ####add jump in ratings between parts of the curve. Also "newRat" (=1 to mark start of new rating)
   n =-999; pwDF$dQ = NA; pwDF$newRat = 0
  for( i in 1:nrow(pwDF)){
    if(pwDF[i,"n"] != n){
      n =pwDF[i,"n"]
      pwDF[i,"newRat"] = 1 
    }else{
      pwDF[i,"dQ"] = pwDF[i,"QL"] - pwDF[i-1,"QU"]
    }
    pwDF[i,"n"] = n
  }
   
   
  #### add to pwDF
  pwDF$currentSite = (pwDF$IMFS_ID%in%NFFS_G2G$NFFS_ID)
  printp("got power rating for ",length(unique(pwDF[pwDF$currentSite,"IMFS_ID"]))," current, and ", length(unique(pwDF[!pwDF$currentSite,"IMFS_ID"])), " extra sites")

  #### add RatID
  pwDF$RatID = sapply(1:nrow(pwDF) ,function(m)paste(pwDF[m,"IMFS_ID"],pwDF[m,"qualifierId" ],sep="*"),  USE.NAMES = F)
}

### get table telling about default ratings ###  => defRatDF
if(defaultRat_FN != ""){
  defRatDF = read.csv(defaultRat_FN)
  defRatDF$Default = as.logical( defRatDF$Default)
  defRatDF$EA      = as.logical( defRatDF$EA)
  defRatDF$G2G    = as.logical( defRatDF$G2G)
  all(pwDF$IMFS_ID %in% defRatDF$FFFS_ID)
  
  #add RatID field
  defRatDF$RatID = sapply(1:nrow(defRatDF) ,function(m)paste(defRatDF[m,"FFFS_ID"],defRatDF[m,"Qualifier" ],sep="*"),  USE.NAMES = F)
  cc = (!defRatDF$Default) & (defRatDF$G2G)
  defRatDF[cc,"RatID"] =  sapply( defRatDF[cc,"RatID"]  ,function(z) gsub("*","*G2G_",z,fixed=T),  USE.NAMES = F)
}

if(defaultRat_FN =="")stop("Thou shalt not pass!") # need to knwo the default rating beyond here


### check ratings ### 
#and limit to winter ratings for following sites with summer/winter ratings: "4082"   "4095"   "694748"; 4082 is a 898 site
check_defualt_ratings = T
if(check_defualt_ratings){
  
  #only G2G ratings have no match
  unique(  (pwDF[!pwDF$RatID %in% defRatDF$RatID,"RatID"]))
  x=unique(  (pwDF[!pwDF$RatID %in% defRatDF$RatID,"RatID"]))
  x[!grepl("G2G_",x)]
  
  #The non-G2G sites with no matching ID - but don't care because the Defaults have matches.
  pwDF[pwDF$IMFS_ID =="021033",]
  defRatDF[defRatDF$FFFS_ID =="021033",]
  pwDF[pwDF$IMFS_ID =="029021",]
  defRatDF[defRatDF$FFFS_ID =="029021",]
  pwDF[pwDF$IMFS_ID =="2270TH",]
  defRatDF[defRatDF$FFFS_ID =="2270TH",]
   
  # There are separate summer/winter ratings (lines <startMonthDay>,<endMonthDay>)
  getNonUnique(pwDF[pwDF$newRat ==1,"RatID"])
  defRatDF[defRatDF$FFFS_ID%in%c("4082","4095", "694748"),] 
  
  getNonUnique(defRatDF$RatID)
  
  #There are a bunch of sites with no default - most are not power ratings
  #Exceptions are :
  #1. Tst.ChBltn.Tot_UG_C042034 - "Test UG Chilbolton"   
  #2. 2270TH - "Shaw USGS" This is a current site. RatingsMetadata of G2G_IMFSMetadata_Oct2021.xlxs gives default=0 for it. It has no flows in the data.
  defRatDF$numDefaults = NA
  for(ii in 1:nrow(defRatDF)){
    site= defRatDF[ii,"FFFS_ID"]
    defRatDF[ii,"numDefaults"] = sum(defRatDF[defRatDF$FFFS_ID==site,"Default"])
  }
  defRatDF[defRatDF$numDefaults != 1,]
  pwDF[pwDF$IMFS_ID %in% c("Tst.ChBltn.Tot_UG_C042034","2270TH"),]
  
  #merge Default into pwDF => it is the Default==1 sites we want
  pwDF=merge(pwDF,defRatDF[,c("RatID","Default", "EA") ],by = "RatID",all.x=T, all.y=F)
  pwDF=pwDF[order(pwDF$row),]
  
  #Check non-matches : they are either:
  #1) G2G only
  #2) have a default as other row 
  #3) are  2270TH  which has no flows in the data.
  pwDF[(pwDF$IMFS_ID %in% unique(pwDF[is.na(pwDF$Default),"IMFS_ID"])  ) &(pwDF$newRat ),c("RatID", "IMFS_Name", "currentSite", "Default","EA"  )]
  pwDF[is.na(pwDF$Default),"Default" ]= -999 #use -999 for there default to save dealing wiht NA.
  
  
  ## Use winter ratings only where multiple exist!
  setSummerToNotDefaultN = c(615,624,924)
  sitesNow = unique(pwDF[pwDF$n %in% setSummerToNotDefaultN,"IMFS_ID"]) # "4082"   "4095"   "694748"; 4082 is a 898 site
  #pwDF[pwDF$n %in% setSummerToNotDefaultN,c("IMFS_ID","currentSite") ]
  if( sitesNow %in% names(getNonUnique(pwDF[pwDF$newRat==1 & pwDF$Default==1,"IMFS_ID"])) %>% all  ){
    printp("Using winter rating only for sites:",paste(sitesNow,collapse = ", "))
    pwDF[pwDF$n %in%setSummerToNotDefaultN,"Default"]=0
  }else{
   stop("check summer rating row numbers to set to non-default") 
  }
    
}

####make  pwDFn (with only single row per rating) ###   => pwDFn
mk_pwDFn=T
if(mk_pwDFn){
  pwDFn=pwDF[pwDF$newRat==1,]
  pwDFn = pwDFn[,!names(pwDFn)%in% c("QL","QU", "iRat", "dQ","RatingRowID","RatingType","RatingID","Doubtful","newRat","LowerLevel", "UpperLevel","A","B","C" )]
  #and populate with summary info
  pwDFn$QminDoubtful = -999;pwDFn$Qmin = -999;pwDFn$Qmax = -999;pwDFn$QmaxDoubtful = -999;pwDFn$maxJump=-999;pwDFn$maxJumpPerc=-999
  for(i in 1:nrow(pwDFn)){#i=1
    n    = pwDFn[i,"n"]
    rows = pwDF[pwDF$"n"==n,]
    pwDFn[i,"QminDoubtful"] = min(rows[,"QL"])
    pwDFn[i,"Qmin"]         = min(rows[rows$Doubtful==0,"QL"])
    pwDFn[i,"Qmax"]         = max(rows[rows$Doubtful==0,"QU"])
    pwDFn[i,"QmaxDoubtful"] = max(rows[,"QU"])
    pwDFn[i,"maxJump"]      = max(rows[,"dQ"],na.rm=T)
    pwDFn[i,"maxJumpPerc"] = 100* max( rows[,"dQ"]/rows[,"QL"],na.rm=T)
  }
  warnings()
  
  #check get at most 1 Default
  z=pwDFn[pwDFn$Default ==1 ,"IMFS_ID"]
  getNonUnique(z) 
  if(length(unique(z)) != length(z))stop("check non-unique default ratings")
}


pwDF[pwDF$IMFS_ID=="035010",]

### examine various possible ratings issues to make a list of sites requiring further investigation ###   => checkRatDF
mk_checkRatDF =T
if(mk_checkRatDF){
  currentSit = F #898 (T) or non-898 (F) sites?  - look at them separately
  
  #check discontinuities
  qExamine     = 0.5 #m3/s
  qpercExamine = 5 #%
  ccn=(pwDFn$Default ==1 &   abs(pwDFn$maxJump)>qExamine & abs(pwDFn$maxJumpPerc)>qpercExamine) & (pwDFn$currentSite == currentSit)
  pwDFn[ccn,]
  pwDF[pwDF$RatID %in% pwDFn[ccn,"RatID"][8],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel")]
  checkDisc = pwDFn[ccn,"IMFS_ID"]

  
  #check high minimum flow
  ccn =    ( (pwDFn$QminDoubtful)/(pwDFn$Qmax) > .1  |    pwDFn$QminDoubtful >10) & (pwDFn$currentSite == currentSit) & pwDFn$Default ==1 
  pwDFn[ccn,!names(pwDFn)%in% c("qualifierId", "dischargeUnit")]
  pwDFn[ccn,!names(pwDFn)%in% c("qualifierId", "dischargeUnit")]
  pwDF[pwDF$RatID %in% pwDFn[ccn,"RatID"],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel", "A", "B", "C")]
  checkMinFlow = pwDFn[ccn & !is.na(ccn),"IMFS_ID"]

  
  #high prefactor
  minQ=0.5
  maxQ = 500
  minRangeQ = 1
  cRange =  pwDF$QU > minQ  & pwDF$QL < maxQ  & (pwDF$QU-pwDF$QL) > minRangeQ & pwDF$Default ==1
  ccn = pwDFn$RatID %in% unique( pwDF[pwDF$C > 300   & cRange, "RatID"]) & (pwDFn$currentSite == currentSit) #IMFS rating power equation is Q=c(h-a)b 
  sum(ccn)
  pwDFn[ccn,!names(pwDFn)%in% c("qualifierId", "dischargeUnit")]
  pwDF[pwDF$RatID==pwDFn[ccn,"RatID"][20],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel")]
  pwDF[pwDF$RatID %in% pwDFn[ccn,"RatID"],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel", "A", "B", "C")]
  checkHighPrefac = pwDFn[ccn,"IMFS_ID"]


  # high power
  ccn = (pwDFn$RatID %in% unique( pwDF[pwDF$B > 12 & cRange   , "RatID"])) & (pwDFn$currentSite == currentSit) & pwDFn$Default ==1 #IMFS rating power equation is Q=c(h-a)b 
  sum(ccn)
  pwDFn[ccn,!names(pwDFn)%in% c("qualifierId", "dischargeUnit")]
  pwDF[pwDF$RatID==pwDFn[ccn,"RatID"][1],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel")]
  pwDF[pwDF$RatID %in% pwDFn[ccn,"RatID"],!names(pwDF) %in% c("qualifierId", "dischargeUnit", "LowerLevel", "UpperLevel")]
  checkHighPow = pwDFn[ccn,"IMFS_ID"]



  #make checkRatDF 
  checkRatDF = data.frame(IMFS_ID = unique(c(checkDisc,checkMinFlow,checkHighPrefac,checkHighPow)),Comment_code="")
  for(i in 1:nrow(checkRatDF)){
    site = checkRatDF[i,"IMFS_ID"]
    if(site %in% checkDisc)       checkRatDF[i,"Comment_code"] = paste(checkRatDF[i,"Comment_code"],"Rdsc",sep=";")
    if(site %in% checkMinFlow)    checkRatDF[i,"Comment_code"] = paste(checkRatDF[i,"Comment_code"],"Rmf",sep=";")
    if(site %in% checkHighPrefac) checkRatDF[i,"Comment_code"] = paste(checkRatDF[i,"Comment_code"],"Rhf",sep=";")
    if(site %in% checkHighPow)    checkRatDF[i,"Comment_code"] = paste(checkRatDF[i,"Comment_code"],"Rhp",sep=";")
  }
  checkRatDF$Comment_code=sub("^;","",checkRatDF$Comment_code)
  
  writecheckRatDF=F
  if(writecheckRatDF){
    x = ifelse(currentSit,"","non-")
    write.csv(checkRatDF, paste0(updateDir,"investigation_summary_csvs/summary__checkRatings_",x,"898.csv") ,row.names = F,quote = T)
  }
  
  #### export ####
  do_write = F
  if(do_write){
    write.csv(pwDFn, paste0(updateDir,"investigation_summary_csvs/ratings_data.csv") ,row.names = F,quote = T)
  }
}
