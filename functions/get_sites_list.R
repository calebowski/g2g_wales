"
~~~~~ G2G_sites_list_w_areas.csv ~~~~~
Final list of 882 sites for G2G. Fields are:
  
SITE: IMFS ID
srcName: prefered source of flow data used with FFC (described below)
IMFS.Name: location name
G2G.ID: G2G ID
G2G.Easting/G2G.Northing: Location in G2G
X/Y: True location
Added_Site: whether site was added in the current G2G release
SU/FI: whether State Updating or Flow Insertion is on.
Hydrometric_Area: Which of the 8 G2G hydrometric areas the site sits in (WA is Wales) - this is distinct from whether the site is actually in Wales (see inWales)
inWales: TRUE, FALSE, or  on.border (see png)


srcName - 
O-IMFS: Current Obs flow is used.
R-IMFS/R-IMFS.1: The current default rated flow is used. (R-IMFS.1 indicated that the site appeared twice in the supplied csv file)
R-LEVEL: Indicates I needed to calculate flows from levels and current default rating (or flows for Shaw downloaded from the Hydrology Data Explorer - current default rating). 
R-G2G: the G2G rated flow is used.
M-G2G: the old Merged G2G source is used.

"

rm(list=ls()) 
options(max.print=600)
options(stringsAsFactors = FALSE)
Sys.setenv(TZ="GMT")
#library(dplyr)
printp=function(...)print(paste0(...))
library(lubridate)
library(sf)

#############################################################
############ Check available gauging stations   #############
#############################################################

##Final list of 882 sites in G2G FFC 2.0
sitesDF =  read.csv("S:/data/data_processing/FFC/FFC_flows_2022update/investigation_summary_csvs/sites_list_final.csv")
nrow(sitesDF)


## add Hydrometric_Area
regionsDF = read.table("P:/NEC04218_Ungauged/releases/ffc/releases/grid2grid/version_2_0_0_ffc/cshma/conf/g2g/g2g_hydrometric_region.conf",sep="=",skip=5)
names(regionsDF)=c("G2G.ID","Hydrometric_Area")
regionsDF$G2G.ID =trimws(regionsDF$G2G.ID)
head(regionsDF)
head(sitesDF)

sitesDF = merge(sitesDF,regionsDF,by="G2G.ID",all.x=T,all.y=F)
nrow(sitesDF)
all(nchar(sitesDF$Hydrometric_Area)==2)



## add "is it in Wales" field - but actually its better to look at  "WMArea" from the FFC22 delivery
wales <- st_read("W:/hymod/Hydro-JULES/HJ Internships/2026 - NRW G2G/SENC_MAY_2026_WA_BFC_-1059615406868623242/SENC_MAY_2026_WA_BFC.shp")# Read Wales constituency polygons
wales <- st_transform(wales, 27700) # Ensure CRS is BNG (should already be, but safe to enforce)
sites_sf <- st_as_sf(sitesDF, coords = c("X", "Y"), crs = 27700) # Convert sites to sf
in_wales <- st_within(sites_sf, wales, sparse = FALSE) # Test if each site falls within ANY Wales polygon
sitesDF$inWales <- apply(in_wales, 1, any)# Reduce (rows × polygons → single TRUE/FALSE per site)

onBorder =c("067015_TG_132","2038","2175","2639","2107","055023_TG_322") #examination in QGIS
sitesDF[sitesDF$G2G.ID %in% onBorder,"inWales"] = "on.border"


head(sitesDF)
table(sitesDF$inWales)
nrow(sitesDF)

write.csv(sitesDF,"W:/hymod/Hydro-JULES/HJ Internships/2026 - NRW G2G/G2G_sites_list_w_areas.csv",row.names = F)


###### All sites with data, including level only  

#these should be were have any data (inc flow only) -can check with e.g.S:\data\incoming_files\FFC\toCEH 29-06-2022\IMFS_export_v1_20220629\2015
df1 = read.csv("S:/data/data_processing/FFC/FFC_flows_2022update/G2G_IMFSMetadata-G2GLocationDetails.csv")
df2 = read.csv("S:/data/data_processing/FFC/FFC_flows_2022update/G2G_IMFSMetadata-OtherIMFSGauges.csv")
head(df1)
head(df2)
names(df2)[1:2]=names(df1)[1:2]
allDF = rbind(df1,df2)
head(allDF,1)

#these should be were have flow data (inc from a rating) 
DF1 =read.csv("S:/data/data_processing/FFC/FFC_flows_2022update/investigation_summary_csvs/summary__cDF__ALL_SITES_V5.csv")
DF2 =read.csv("S:/data/data_processing/FFC/FFC_flows_2022update/investigation_summary_csvs/summary__IMFS_non898_COMMENTED_V2.csv")
head(DF1$SITE )
length(unique(DF1$SITE))#898
length(unique(DF2$SITE))#638

sum(DF2$SITE %in% DF1$SITE)
DF2$SITE[DF2$SITE %in% DF1$SITE]#520575_FW -> check this site.

flowSites= unique(c(DF1$SITE,DF2$SITE))
length(flowSites)#1535



c1= allDF$WMArea=="NRW"
c2= (allDF$IMFS.ID %in% flowSites)

## map current ("green") and non-current ("darkgreen") sites in wales
plot(wales$geometry)
points(allDF[c1,c("X","Y")],pch=16,col="darkgreen")
points(allDF[c1 &  c2 ,c("X","Y")],pch=16,col="green")
points(allDF[,c("X","Y")])
points(allDF[ allDF$IMFS.ID %in% sitesDF$SITE ,c("X","Y")],col="red")


sum(c1)#259 
sum(c1 & c2)#139
sum(sitesDF$inWales !="FALSE")#110



all(sitesDF[sitesDF$inWales =="TRUE","SITE"] %in% allDF[c1&c2,"IMFS.ID"]) #True
any(sitesDF[sitesDF$inWales =="FALSE","SITE"] %in% allDF[c1&c2,"IMFS.ID"]) #False
sitesDF[sitesDF$inWales =="on.border","SITE"] %in% allDF[c1&c2,"IMFS.ID"] #3 True, 3 False
sitesDF[sitesDF$inWales =="TRUE",]



#############################################################
############ Check RG info source               #############
#############################################################
#check number of RGs in FFC22 delivery (1026 at all stages checked)
Rg_yr <- "S:/data/incoming_files/FFC/toCEH 29-06-2022/IMFS_export_v1_20220629/2021/2022-01-01 0000_Rainfall_IMFS.csv"
line1 <- readLines(Rg_yr, n = 1)
lengths(regmatches(line1, gregexpr(",", line1)))#1026

Rg_yr <- "S:/data/incoming_files/FFC/toCEH 29-06-2022/IMFS_export_v1_20220629/2000/2001-01-01 0000_Rainfall_IMFS.csv"
line1 <- readLines(Rg_yr, n = 1)
lengths(regmatches(line1, gregexpr(",", line1)))#1026

rgdat=read.csv("S:/data/data_processing/FFC/rg_qc/2022_update/rg_info_2022.csv")
head(rgdat)
nrow(rgdat)#1026

#alternative 
fn <- "W:/hymod/Hydro-JULES/HJ Internships/2026 - NRW G2G/rgs_tmp.txt"
vec <- readLines(fn)
vec[1:3]
length(vec)#1005




sum(!vec%in%rgdat$Raingauge )
sum(!rgdat$Raingauge%in%vec)
rgdat[!rgdat$Raingauge%in%vec,]

