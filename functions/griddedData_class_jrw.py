import numpy as np
import matplotlib.pyplot as plt 
import netCDF4 as nc
import datetime 
import sys
from ctypes  import *
import rasterio as rio
from shapely.geometry import Polygon
import time 


def maskGridInplace(grd,msk,noVal):
    """Set grd to noVal if msk is. Return number of masked cells"""
    cnt = 0
    if grd.shape != msk.shape:
        print(("unequal shapes: ",grd.shape, msk.shape))
        return(-1)
    for iy in range( grd.shape[0] ):
        for ix in range( grd.shape[1] ):
            if (msk[iy,ix] == noVal) and (grd[iy,ix] != noVal) :
                grd[iy,ix] = noVal
                cnt = cnt+1
    return(cnt)
    
def defaultWithMaskInplace(grd,msk,noVal,default,cntOnly=False):
    """Set grd to default if grd is noVal but msk isn't. Return number of masked cells"""
    cnt = 0
    if grd.shape != msk.shape:
        print(("unequal shapes: ",grd.shape, msk.shape))
        return(-1)
    for iy in range( grd.shape[0] ):
        for ix in range( grd.shape[1] ):
            if (msk[iy,ix] != noVal) and (grd[iy,ix] == noVal) :
                if not cntOnly:
                    grd[iy,ix] = default
                cnt = cnt+1
    return(cnt)
    
    
#From catchment descriptors "FARL" project
#ADD TO CLASS - JUNE23 JRW
# def decreaseResolution(datIn,resIn,BndsIn,resOut,BndsOut,padWith=0):
    # """Make the resolution of a grid courser - by summing
    # Input: datIn, resIn = [ResX,ResY] (also ResX=ResY), BndsIn = bounds [L,R,B,T]
           # resOut = resolution for final grind (must divide resIn) , BndsOut = bounds [L,R,B,T] (may want to match a certain grid)
    # Output: returns courser grid (accumulated using sum)"""
    # print("** changeResolution **")    
    # nDiv = round(resOut[0]/resIn[0]) 
    # if resIn[0]!=resIn[1] or resOut[0]!=resOut[1]:
        # sys.exit("decreaseResolution: need square grids")
    # if resOut[0]<=resIn[0]:
        # sys.exit("decreaseResolution: can only move to courser grid")
    # if abs(nDiv - resOut[0]/resIn[0]) :
        # sys.exit("decreaseResolution: grids must be integer multiples of each other")
    # nx = (BndsOut[1]-BndsOut[0])/resIn[0]
    # ny = (BndsOut[3]-BndsOut[2])/resIn[1]
    # if (not nx.is_integer()) | (not ny.is_integer()):
        # sys.exit("decreaseResolution: BndsOut not multiple of resolution")
    # Nx=round( (BndsOut[1]-BndsOut[0])/resOut[0] )
    # Ny=round(( BndsOut[3]-BndsOut[2])/resOut[1] )
    ##crop to BndsOut    
    # datIn = crop2RefGrid(BndsIn,datIn,BndsOut,resIn,padWith=padWith)[1]    
    # datOut = np.zeros( [Nx,Ny] , dtype=datIn.dtype) #x,y??
    # iX =0;iY=0
    # for iX in range( datOut.shape[0] ):
        # for iY in range( datOut.shape[1] ):
            # tmp = datIn[ (nDiv*iX):(nDiv*iX + nDiv) , (nDiv*iY):(nDiv*iY + nDiv)  ]
            # datOut[iX,iY] = np.sum(tmp)    
    # return(datOut)



# Formally "changeResolution" of catchment descriptors "FARL" project
# ADD TO CLASS - JUNE23 JRW
# ADDED Jul23 JRW
# def increaseResolution(datIn,resIn,Bnds,resOut):
    # """Make the resolution of a grid finner
    # Input: datIn, resIn = [ResX,ResY] (also ResX=ResY), Bnds = bounds [L,R,B,T]
           # resOut = resolution for final grind (must divide resIn) 
    # Output: returns finner grid"""
    # print("** changeResolution **")
    # nDiv = round(resIn[0]/resOut[0])
    # if resIn[0]!=resIn[1] or resOut[0]!=resOut[1]:
        # sys.exit("changeResolution: need square grids")
    # if resOut[0]>=resIn[0]:
        # sys.exit("changeResolution: can only move to finer grid")
    # if abs(nDiv - resIn[0]/resOut[0]) :
        # sys.exit("changeResolution: grids must be integer multiples of each other")
    # datOut = np.zeros( [nDiv*x for x in datIn.shape] ,dtype=datIn.dtype)
    # for iX in range( datOut.shape[0] ):
        # for iY in range( datOut.shape[1] ):
            # datOut[iX,iY] = datIn[ int(iX/nDiv) , int(iY/nDiv)  ]
    # return(datOut)    


#Adapted from catchment descriptors "FARL" project 
#JRW Jul23
def sumGridOverUpstreamArea(datGrid,outfDat,seaCell,maxCnt = float("inf"),dType = np.int64,flowDirectionDic=None):
    """ Inputs:     datGrid = the grid you wish to sum, must be on the same grid as self.ccarDat and self.outfDat
                    seaCell = stop accumulations when you reach the "sea" - a logical grid with same dimensions as ccar & outf grids.
                    maxCnt = max number of steps wish to sum in the upstream direction (keep as float("inf") except for quick tests)
                    dType = np.uint32 for urbext, np.int64 for saar?
        Outputs:    returns a grid which is the sum of datGrid at each point over its upstream area.
        Method:     Use the flow direction grid to repeatedly propogate datGrid downstream (uses urb0 and urb1) while adding it to urbSum.  int64 are used for the accumulation. This method becomes inefficient when it is down to the last few cells."""        
    print("********** sumGridOverUpstreamArea **********")
    if flowDirectionDic is None:
        ##for [0,0] being NW corner (ie not flip transposed)
        flowDirectionDic={1:[0,1], 2:[1,1], 4:[1,0], 8:[1,-1], 16:[0,-1], 32:[-1,-1], 64:[-1,0], 128:[-1,1],0:[0,0],255:[0,0],-1:[0,0],-999:[0,0],-9999:[0,0]}
        ##for [0,0] being SW corner (ie flip transposed)
        #flowDirectionDic={1:[1,0], 2:[1,-1], 4:[0,-1], 8:[-1,-1], 16:[-1,0], 32:[-1,1], 64:[0,1], 128:[1,1],0:[0,0],255:[0,0],-1:[0,0]}
    urb0   = (datGrid.copy()).astype(dType)
    urb0[seaCell]=0
    urb1   = np.zeros(datGrid.shape,dtype=dType)
    urbSum = (datGrid.copy()).astype(dType)#for accumulation
    ixC=int(urb0.shape[1]/2);iyC=int(urb0.shape[0]/2)#central X or Y coord - used as a quicker test in the while loop
    ts = time.time()
    cnt=0
    #repeatedly propogate datGrid downstream ...
    while (cnt <= maxCnt) & ( (urb0[ixC,:]>0).any() or (urb0[:,iyC]>0).any()  or (urb0>0).any() )  :    
        urbCoords = np.where(urb0>0)
        cnt=cnt+1;print(("* loop ",cnt," non-zero cells",len(urbCoords[0]),". tim: ",time.time()-ts," *")); ts = time.time()
        for iy, ix in zip(urbCoords[0],urbCoords[1]):
            fd=flowDirectionDic[outfDat[iy,ix]]
            ixN=ix+fd[1];iyN=iy+fd[0]
            urb1[iyN, ixN]   = urb1[iyN, ixN]   + urb0[iy, ix]
            # ... suming it up ...
            urbSum[iyN, ixN] = urbSum[iyN, ixN] + urb0[iy, ix]
        if cnt%40==0 or cnt<20:  #... and set to zero when reach sea  
            urb1[seaCell]=0
            urbSum[seaCell]=0
        urb0   = urb1#don't need to copy as overwrite urb1
        urb1   = np.zeros(datGrid.shape,dtype=dType)
    print(( "* np.max(urbSum), np.max(datGrid) =",np.max(urbSum),", ",np.max(datGrid)  ))
    del urb0, urb1, seaCell
    return(urbSum)


class griddedData:
    """ A functions to load data from various formats and then write them back out again in other formats:
        - setVars(self,fileName="NA",nX=float("nan"),nY=float("nan"),...)
        - printFileInfo(self)
        - plotMap(self,dmin="relative")      
        - getBnds(self, x0y0nxnygridSize=None)
        - getx0y0nxnygridSize(self, bnds,setSelf=True)
        - crop2RefGrid(self,refBnds,padWith=0)  (does padding also)
        - decreaseResolution(self,resOut,BndsOut,padWith=0,outDataType=None):
        - readTif(self,File,flip = True):
        - writeGridToTif(self,dat,FN,coorSysFN,CRS="fromFile"):
        - varsNetCdf(self,fileName=NA,var=False)
        - readNetCdf(self,fileName,var,x="x",y="y",timeOpt="none",demask=False)
        - readAscii(self,fileName)
        - writeAscii(self,fileName)
        - sidbRead(self,rapperFile, fileName, source_id, source_type, dt_maj, dt_min, nX, nY, year, month ,day ,time, leadtime=0)
        - writeSIDB(self,rapperFile, fileName, source_id, source_type, dt_maj, dt_min, year, month ,day ,time, leadtime=0 ,proj=3)
        - mk_a_mask_for_catav(self, shape,limitMaskToOne=True,FN="",plotMask=False)        
     """
    def __init__(self,fileName="NA"):
        self.setVars(fileName=fileName)

    def setVars(self,fileName="NA",nX=float("nan"),nY=float("nan"),x0=float("nan"),y0=float("nan"),gridSize=float("nan"),data=float("nan"),noVal=float("nan"),CenterOrCorner="NA",dataNc = float("nan"),imgTime = float("nan")):
        """setVars(self,fileName="NA",nX=float("nan"),nY=float("nan"),x0=float("nan"),y0=float("nan"),gridSize=float("nan"),data=float("nan"),noVal=float("nan"),CenterOrCorner="NA",dataNc = float("nan"),imgTime = float("nan"))
        set a load of self variables at once"""

        self.fileName=fileName
        self.nX=nX; self.nY=nY
        self.x0=x0; self.y0=y0
        self.gridSize=gridSize
        self.data=data
        self.noVal=noVal
        self.CenterOrCorner=CenterOrCorner#is the data at the "center" of the cell or "corner"

        self.dataNc = dataNc
        self.imgTime = dataNc

    def printFileInfo(self):
        """print all standard self.vars"""
        print("~~~~~~~~~~~~ FileInfo: ~~~~~~~~~~~~") 
        print("* fileName       : ",self.fileName)
        print("* nX             : ",self.nX)
        print("* nY             : ",self.nY)
        print("* x0             : ",self.x0)
        print("* y0             : ",self.y0)
        print("* CenterOrCorner : ",self.CenterOrCorner)
        print("* gridSize       : ",self.gridSize)
        print("* noVal          : ",self.noVal)  
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        
    def extract_at_points (self,X,Y):
        """input:  X and Y coords (or a list of them)
           Output: value(s) of self.dat at those points""" 
        if not isinstance(X, list):
            X=[X]
            Y=[Y]
        nPts = len(X)
        if nPts!=len(Y):
            sys.exit("extract_at_points: unequal X and Y lenghts")
        if self.CenterOrCorner!="center":
            sys.exit("extract_at_points: this function was written assuming x0,y0 are for center of cell - adapt by adding/subracting 0.5 in XT,YT (?)")                
        ny,nx = self.data.shape
        iX =           np.floor( np.subtract(X,self.x0)/self.gridSize + 0.5 ).astype(int)
        iY = (self.nY - 1) - np.floor( np.subtract(Y,self.y0)/self.gridSize + 0.5 ).astype(int) #arrays flip y
        ret = [-1.0 for i in range(nPts)]
        for k in range(nPts):#k=0
            ret[k] = self.data[iY[k],iX[k]]
        return ret         

    def intify(self):
        """Make integers of nX, nY, x0, y0, gridSize """
        self.nX=int(round(self.nX)); self.nY=int(round(self.nY))
        self.x0=int(round(self.x0)); self.y0=int(round(self.y0))
        self.gridSize=int(round(self.gridSize))


    def plotMap(self,pdata=[],dmin="relative"):
        """plot self.data (or pdata, if supplied) as 2D color map"""  
        if(len(pdata))==0:
            pdata = self.data            
        if dmin=="relative" :
            dmin=np.amax(pdata)*(-0.2)
        plt.imshow(pdata,vmin=dmin,interpolation='none')# kept 0th = north, so no origin='lower' line here
        plt.colorbar()
        plt.show()

    def getBnds(self, x0y0nxnygridSize=None):
        """Convert  x0,y0,nx,ny and gridSize of either "self" or provides as an argument (in that order) to bnds = bounds [L,R,B,T]"""
        if x0y0nxnygridSize is None:    
            x0,y0,nx,ny,gridSize = self.x0,self.y0,self.nX,self.nY,self.gridSize
        elif len(x0y0nxnygridSize) == 5:   
            x0,y0,nx,ny,gridSize = x0y0nxnygridSize
        else:
            print("getBnds: WARNING x0y0nxnygridSize inappropriate")
            return(-1) 
        if any( [np.isnan(x) for x in (x0,y0,nx,ny,gridSize)] ) : 
            print("getBnds: WARNING nan in x0y0nxnygridSize")
        bnds=[x0, x0 + nx*gridSize, y0, y0 + ny*gridSize]  #[L,R,B,T]  
        return(bnds)
       
    def getx0y0nxnygridSize(self, bnds,setSelf=True):
        """Turn bnds = bounds [L,R,B,T] into x0y0nxnygridSize"""
        x0y0nxnygridSize = [ bnds[0] , bnds[2] , (bnds[1]-bnds[0])/self.gridSize  , (bnds[3]-bnds[2])/self.gridSize , self.gridSize]
        if setSelf:
            self.x0 = x0y0nxnygridSize[0]
            self.y0 = x0y0nxnygridSize[1]
            self.nX = x0y0nxnygridSize[2]
            self.nY = x0y0nxnygridSize[3]
        else:
            return(x0y0nxnygridSize) 

    def crop2RefGrid(self,refBnds,padWith=0): 
        """ take crop/pad self.dat to the bounds given by refBnds=[L,R,B,T]. "self.getBnds" can be used to get those bounds from x0y0nxnygridSize. "self.getx0y0nxnygridSize" is used to set x0 y0 nx ny & gridSize """
        bnds = self.getBnds()
        dimDiff=[1,-1,1,-1]*(np.subtract(refBnds, bnds)/self.gridSize)
        #
        if sum(abs(dimDiff.astype("int")-dimDiff))>0.01 :
            print("Warning: grids off by non-integer multiple of the resolution!")
            print(("dimDiff =", dimDiff)) 
        if sum(abs(abs(dimDiff.astype("int")-dimDiff) - [0.5,0.5,0.5,0.5]))<0.01 :
            print("Inconsistent corner VS centre convention for grids - dat grid will be shifted up and right by half a grid spacing to match.")
            dimDiff = dimDiff + 0.5*np.array([1,-1,1,-1])
        dimDiff=np.round(dimDiff).astype("int") #[L,R,B,T]
        cropBy=[max(0,x) for x in dimDiff]
        #self.data = self.data[cropBy[0]:( self.data.shape[0] - cropBy[1] ), cropBy[2]:(self.data.shape[1] - cropBy[3]) ]
        #NB:  in farlCalc_class the array wasn't fliped-transposed hence here is rearanged!
        self.data = self.data[cropBy[3]:( self.data.shape[0] - cropBy[2] ), cropBy[0]:(self.data.shape[1] - cropBy[1]) ]
        padBy=[max(0,-x) for x in dimDiff]
        #self.data = np.pad(self.data, ((padBy[0], padBy[1]), (padBy[2], padBy[3])), mode='constant',constant_values=padWith) 
        #NB:  in farlCalc_class the array wasn't fliped-transposed hence here is rearanged!
        self.data = np.pad(self.data, ((padBy[3], padBy[2]), (padBy[0], padBy[1])), mode='constant',constant_values=padWith) 
        self.getx0y0nxnygridSize(refBnds,setSelf=True)     
        
        
        #From catchment descriptors "FARL" project - swaped x,y (no flip-transpose here) and added to class.
    def decreaseResolution(self,resOut,BndsOut,padWith=0,outDataType=None):
        """Make the resolution of a grid courser - by summing
        Input: resOut = resolution for final grind (must divide resIn) , BndsOut = bounds [L,R,B,T] (may want to match a certain grid)
        Output: returns courser grid (accumulated using sum)"""
        print("** changeResolution **")        
        nDiv = int(round(resOut/self.gridSize)) 
        if resOut<=self.gridSize:
            sys.exit("decreaseResolution: can only move to courser grid")
        if abs(nDiv - resOut/self.gridSize) :
            sys.exit("decreaseResolution: grids must be integer multiples of each other")
        nx = (BndsOut[1]-BndsOut[0])/self.gridSize
        ny = (BndsOut[3]-BndsOut[2])/self.gridSize
        if (not nx.is_integer()) | (not ny.is_integer()):
            sys.exit("decreaseResolution: BndsOut not multiple of resolution")
        Nx=int(round( (BndsOut[1]-BndsOut[0])/resOut ))
        Ny=int(round(( BndsOut[3]-BndsOut[2])/resOut ))
        #crop to BndsOut    
        self.crop2RefGrid(BndsOut,padWith=padWith)   
        if outDataType is None:
            outDataType = self.data.dtype 
        datOut = np.zeros( [Ny,Nx] , dtype=outDataType) 
        for iX in range( datOut.shape[1] ):
            for iY in range( datOut.shape[0] ):
                tmp = self.data[ (nDiv*iY):(nDiv*iY + nDiv),(nDiv*iX):(nDiv*iX + nDiv)  ]
                datOut[iY,iX] = np.sum(tmp)    
        #update self
        self.data = datOut
        self.gridSize = resOut
        self.getx0y0nxnygridSize(BndsOut)
        
    def readTif(self,File,flip = True):
        """ Read a tiff file - Will need to set noVal (the NA value) and CenterOrCorner ("center" of the cell or "corner") by hand """
        print(("Reading file:",File))
        oFile=rio.open(File)
        self.data=oFile.read(1) #numpy array
        if flip : #for sanities sake we want dat[x,y] where dat[0,0] is bottom left
            self.data=np.transpose(np.flipud(self.data))
        res = oFile.res   #resolution in meters
        tmp=oFile.bounds ;  bnds = [tmp.left, tmp.right , tmp.bottom , tmp.top ] #coords (in meters of corners)    
        print(("resolution: ",res,", bounds:",bnds ))
        oFile.close()    
        self.fileName = File    
        self.nX = self.data.shape[1]    
        self.nY = self.data.shape[0]    
        self.x0 = float(bnds[0])
        self.y0 = float(bnds[2])  
        self.gridSize = float(res[0])
        #do simple checks : 
        if res[0]!=res[1]:
            sys.exit("differing x and y resolutions!")
        c1 = round((tmp.right - tmp.left  )/self.gridSize) == self.nX        
        c2 = round((tmp.top   - tmp.bottom)/self.gridSize) == self.nY
        if( not(c1 and c2) ):
            sys.exit("claimed bounds/resolution do not match data size - (check flip argument?)"+str(c1)+str(c2))       

        
        #self.noVal=
        #self.CenterOrCorner=
    

    def writeGridToTif(self,dat,FN,coorSysFN,CRS="fromFile"):
        """Write dat grid as a Tif 
           Input: dat = the gridded data to write out (in the appropriate data-type)
                  FN  = file name of tif to write to
                  coorSysFN = the file name of another tiff with the appropriate coord system (e.g. if data came from tif and want the same coord system)     
        """
        #Compression - can massively reduce file size (losslessly), at least for integer data types (remove compress option to leave uncompressed).
        #https://rasterio.readthedocs.io/en/latest/topics/writing.html 
        #https://feed.terramonitor.com/practical-geotiff-compression-comparison/
        #CRSnow the coord system, eg 'epsg:27700' for GB or 'epsg:29902' for Northern Ireland
        print(("* writeGridToTif *\nWriting to ",FN,"..."))
        oFile=rio.open(coorSysFN) #for transform
        if CRS== "fromFile":
            CRS=oFile.crs
        Dataset = rio.open(FN,'w',driver='GTiff',height=dat.shape[1],width=dat.shape[0],count=1,dtype=dat.dtype,crs=CRS,transform=oFile.transform,compress='lzw')
        Dataset.write(np.flipud(np.transpose(dat)),1)#we did np.transpose(np.flipud(dat))
        Dataset.close()
        oFile.close()
        print("... written")        
        
        
        

    def varsNetCdf(self,fileName="NA",var=False):
        """varsNetCdf(self,fileName="NA",var=False):
        Prints dataNc.variables.keys(), dataNc.variables, and, for var!=False dataNc.variables[var] .
        Also sets self.dataNc"""
        if fileName=="NA": 
            filename =self.fileName 
        else:
            self.fileName=fileName

        dataNc = nc.Dataset(self.fileName,'r')
        self.dataNc= dataNc
        print("~~~~~~~~~~ dataNc.variables.keys() ~~~~~~~~~~")
        print(list(dataNc.variables.keys()))
        if var==False :
            print("~~~~~~~~~~ dataNc.variables ~~~~~~~~~~")
            print(dataNc.variables)
        else :
            print("~~~~~~~~~~ dataNc.variables[",var,"] ~~~~~~~~~~")
            print(dataNc.variables[var])
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")


    def readNetCdf(self,var,fileName="NA",x="x",y="y",timeOpt="none",demask=False):  
        """***  readNetCdf(self,fileName,var,x="x",y="y",timeOpt="none")***
        var=name of var to extract (e.g "Rainfall_ZC_LUE" for xBand, "rainfall_amount" for CEH-GEAR-1hr). To find this out, try using varsNetCdf() function or, e.g. external program like panoply from Y-drive. 
        x,y name of 1D varibles in the NetCdf.
        timeOpt= "none","xBand" or "CEH-GEAR-1hr" : controls extraction of time variable (populates self.imgTime). "none"=dont, "xBand"=single time (works with NCAS xband example), "CEH-GEAR-1hr"=list of times (only image for first one is returned in self.data)
        demask=True to replace masked values with zero
        """
        if fileName!="NA": self.fileName=fileName 
            
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")      
        print("readNetCdf: fileName = ", self.fileName)
        dataNc = nc.Dataset(self.fileName,'r')   
        self.dataNc= dataNc
        print("data units = ", dataNc.variables[var].units)
        print("data long_name = ", dataNc.variables[var].long_name)
        print("data shape = ", dataNc.variables[var].shape, " (only last 2D selected:",str(dataNc.variables[var]).splitlines()[1],")")
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        dims=len(dataNc.variables[var].shape)
        if not demask :
            if dims==2 :
                self.data = np.array(dataNc.variables[var])
            elif dims==3 :
                self.data = np.array(dataNc.variables[var][0])
            elif dims==4 :
                self.data = np.array(dataNc.variables[var][0][0])
            else:
                print("ERROR1: check what's in your NetCdf (e.g, use self.varsNetCdf() from class or \"approved software\" Panoply)")
                return -1
        else :
            if dims==2 :
                self.data = dataNc.variables[var].filled(0.0)
            elif dims==3 :
                self.data = dataNc.variables[var][0].filled(0.0)
            elif dims==4 :
                self.data = dataNc.variables[var][0][0].filled(0.0)
            else:
                print("ERROR2: check what's in your NetCdf (e.g, use self.varsNetCdf() from class or \"approved software\" Panoply)")
                return -1
    
        #check units (turn km into m) and get nx, ny, x0, y0  
        arrayX = np.array(dataNc.variables[x])
        arrayY = np.array(dataNc.variables[y])
        xUnits=dataNc.variables[x].units
        yUnits=dataNc.variables[y].units

        if xUnits!=yUnits: sys.exit(x + " and " + y + "units inconsistent in NetCdf!" ) 
        if xUnits=="km":
            print("converting km units to m for "+x+ " and "+y)
            arrayX=1000.0*arrayX; arrayY=1000.0*arrayY

        self.nX=len(arrayX)
        self.nY=len(arrayY)
        if( arrayX[0]<arrayX[-1]) :
            self.x0=arrayX[0]
        else:
            sys.exit("data x axis appears to be flipped: check this!")    
        
        self.y0=min(arrayY)# for y-axis expect  arrayX[-1] to be south most but it might not be (e.g ncas Xband) => np.flipud(self.data) below
 
        #check the regularity and squareness of the grid, then set gridSize:
        tmpX=arrayX[1:(-1)]-arrayX[0:(-2)]; minX=min(tmpX); maxX=max(tmpX)
        tmpY=arrayY[0:(-2)]-arrayY[1:(-1)]; minY=min(tmpY); maxY=max(tmpY)

       # import code;code.interact(local=locals())
        if (maxX-minX)==0.0 and (maxY-minY)==0.0 :
            if minX==maxY  : 
                self.gridSize=minX
            elif minX==-maxY :
                self.gridSize=minX
                self.data=np.flipud(self.data)
            else :
                print("ERROR: x and y spacing not equal in NetCdf")
                return -1                
        else:
            print("ERROR: irregular grid in NetCdf")
            return -1

        #set noVal:
        self.noVal=dataNc.variables[var]._FillValue

        ###   This stuff is to populate time variable for NCAS Xband  ###
        if timeOpt=="xBand" :
            tim=dataNc.variables["start_time"].comment  #just get the time from the comment
            self.imgTime=datetime.datetime.strptime(tim , "%Y-%m-%dT%H:%M:%SZ")
 
        #This stuff is for CEH-GEAR-1hr    ###
        if timeOpt=="CEH-GEAR-1hr" :
            #get the times. We examine "data.variables['time'].units" incase the t=0 time changes
            tu=dataNc.variables['time'].units
            tUnit=tu.split(" ")[0]
            sTime=datetime.datetime.strptime(" ".join(tu.split(" ")[2:4]) , "%Y-%m-%d %H:%M:%S")
            datTime=[sTime+datetime.timedelta( **{tUnit:int(t)}  ) for t in dataNc.variables['time']]
            print("* data start time", sTime)
            self.imgTime = datTime


    def readAscii(self,fileName):
        nheaderlines=6
        rFile = open(fileName, "r")
        headList=[]
        print("*opening ascii: ", fileName)
        for i in range(nheaderlines):
            headList=headList+[rFile.readline().split()]
        self.fileName=fileName
        self.nX=float(headList[0][1]); self.nY=float(headList[1][1])
        self.x0=float(headList[2][1]); self.y0=float(headList[3][1])
        self.gridSize=float(headList[4][1])
        self.noVal=float(headList[5][1])
        if(headList[2][0].lower().find("corner")!=-1 and headList[3][0].lower().find("corner")!=-1):
            self.CenterOrCorner="corner"
        elif(headList[2][0].lower().find("center")!=-1 and headList[3][0].lower().find("center")!=-1):
            self.CenterOrCorner="center"
        else:
            print("WARNING: unable to determine CenterOrCorner for origin of ascii file.")
            self.CenterOrCorner="NA"
        self.data=np.loadtxt(rFile)
        

    def writeAscii(self,fileName):
        str_tup=(self.nX,self.nY,self.x0,self.y0,self.gridSize,self.noVal)
        if( self.CenterOrCorner.lower()=="corner"):
            header_str=  "ncols         %s\nnrows         %s\nxllcorner     %s\nyllcorner     %s\ncellsize      %s\nNODATA_value  %s" %str_tup
        elif( self.CenterOrCorner.lower()=="center"):
            header_str=  "ncols         %s\nnrows         %s\nxllcenter     %s\nyllcenter     %s\ncellsize      %s\nNODATA_value  %s" %str_tup
        else :
            print("ERROR: writeAscii: is it corner or center?")
            return -1
        #print "here in  writeAscii",fileName
        np.savetxt(fileName,self.data,header=header_str,comments='',fmt='%6.6g')


    def sidbRead(self,rapperFile, fileName, source_id, source_type, dt_maj, dt_min, nX, nY, year, month ,day ,time, leadtime=0):
        """
            Transfer image from SIDB to python array
            rapperFile: e.g., "./sidb_rw_python_v2.so" 
            fileName: dir with sources3.conf  
            source_id, source_type, dt_maj, dt_min : sidB data parameters 
            nX, nX : x and y dimensions 
            x0, y0 : origin of x and y coords
            year, month ,day ,time, leadtime=0 : time is min after midnight , leadtime=0 except forecasts
            proj : 3 = Integer GB National Grid
        """
        sidbLib= cdll.LoadLibrary(rapperFile)
        self.fileName=fileName
        self.nX=nX; self.nY=nY 
        plength=len(self.fileName)
        arrayData = np.empty((self.nX,self.nY),dtype=c_float)
        isFound = c_int(0)
        err = c_int(-1)
        xo = c_float(0); yo = c_float(0) 
        ix = c_int(0);   iy = c_int(0)
        gx = c_float(1); gy = c_float(1)
        #print "load this -",time,day,month,year,self.fileName
        #print "sum",np.sum(arrayData)
        RA = sidbLib.sidbreadin_(byref(c_int(self.nX)),byref(c_int(self.nY)),
		    byref(c_int(time)),byref(c_int(day)),byref(c_int(month)),byref(c_int(year)),byref(c_int(leadtime)),
		    byref(c_int(source_id)),byref(c_int(source_type)),byref(c_int(dt_maj)),
		    byref(c_int(dt_min)),arrayData.ctypes.data_as(POINTER(c_float)),
		    byref(c_int(plength)),(c_char_p(self.fileName.encode('utf-8'))),byref(err), #(c_char_p(self.fileName.encode('utf-8'))) or (c_char_p(self.fileName))?
		    byref(xo),byref(yo),byref(ix),byref(iy),byref(gx),byref(gy),
		    byref(c_int(0)),byref(c_char_p(0)),byref(isFound))
        #print "sum",np.sum(arrayData)
        if isFound :
            self.x0=xo.value; self.y0=yo.value 
            self.gridSize=gx.value
            if gx.value != gy.value: print("WARNINING : sidb grid irregular!")
            self.data =  np.flipud(np.reshape(arrayData,(self.nY,self.nX))) 
            self.noVal=float("nan")
            self.CenterOrCorner="NA"#probably "corner"
        else :
            print("WARNING: sidb not found (sidbRead - python)")
            print("try: (i)deleting the sidb cache file, (ii) dos2unix datatypes2.conf sidbpaths.conf sources2.conf, (iii) check source/data-type ids ")
       # return [arrayData_shaped,xo.value,yo.value,ix.value,iy.value,gx.value,gy.value,isFound.value]


    def writeSIDB(self,rapperFile, fileName, source_id, source_type, dt_maj, dt_min, year, month ,day ,time, leadtime=0 ,proj=3):
        """
            Transfer image from  python array to SIDB
            rapperFile: e.g., "./sidb_rw_python_v2.so" 
            fileName: dir with sources3.conf  
            source_id, source_type, dt_maj, dt_min : sidB data parameters 
            nX, nX : x and y dimensions 
            x0, y0 : origin of x and y coords
            year, month ,day ,time, leadtime=0 : time is min after midnight , leadtime=0 except forecasts
            proj : 3 = Integer GB National Grid
        """
        sidbLib= cdll.LoadLibrary(rapperFile)

        #arrayData2= np.flipud(np.reshape(arrayData,(self.array_y,self.array_x)))

        # check that size of data array is the same as that defined by array_x, array_y
        #if  not all([j==[self.array_y, self.array_x][i] for i,j in enumerate(arrayData.shape)]):
         #   print "Array size does not match that of SIDB: array_x = "+str(self.array_x)+', array_y = '+str(self.array_y)
          #  return 1

        #Arr = np.empty(self.data.shape,dtype=c_float)
        #Arr[:,:] = self.data[:,:]
        #Arr=np.copy(self.data)
        Arr=np.copy(np.flipud(self.data))
        #Arr=np.flipud(self.data)
        Arr_p=Arr.ctypes.data_as(POINTER(c_float))
        self.fileName=fileName
        plength=len(self.fileName)

        err = 0
        #copied example in S:\projects\grid2grid\ffc\update2023\pseudo_warm_start_state JRW Jul25
        # also see S:\projects\grid2grid\python\griddedData_class\conversion_class_py3\SIDB_rw\SIDB_rw_py_v3.py
        sidbLib.sidbwriteout_(byref(c_int(self.nX)),byref(c_int(self.nY)),byref(c_int(time)),
		    byref(c_int(day)),byref(c_int(month)),byref(c_int(year)),byref(c_int(leadtime)),            
	   	    byref(c_int(source_id)),byref(c_int(source_type)),byref(c_int(dt_maj)),  byref(c_int(dt_min)), 
            Arr_p, byref(c_int(plength)),
            (c_char_p(self.fileName.encode('utf-8'))), byref(c_float(self.x0)),byref(c_float(self.y0)),
            byref(c_float(self.gridSize)),byref(c_float(self.gridSize)),
            byref(c_int(proj)),byref(c_int(0)),byref(c_int(err)))
            
            ###############################
 
        # sidbwriter version2:
        # sidbLib.sidbwriteout_(byref(c_int(self.nX)),byref(c_int(self.nY)),byref(c_int(time)),
		    # byref(c_int(day)),byref(c_int(month)),byref(c_int(year)),byref(c_int(leadtime)),            
	   	    # byref(c_int(source_id)),byref(c_int(source_type)),byref(c_int(dt_maj)),  byref(c_int(dt_min)), 

            # byref(c_float(self.x0)),byref(c_float(self.y0)),
            # byref(c_float(self.gridSize)),byref(c_float(self.gridSize)),(Arr_p),byref(c_int(plength)),
            # (c_char_p(self.fileName)),byref(c_int(proj)),byref(c_int(0)),byref(c_int(err)))        

        return err


    def readNimrod(self,fileName="NA"):
        """
        readNimrod(self,fileName):
        uses nimrod_functions_read_write (of seodey & scole 2017) to read a nimrod file. Automatically multiplies by 32 if integer data to account for the usual units of mm/hr/32. Also populated self.imgTime based on 1st 6 header items
        """
        import nimrod_functions_read_write as nrd
        if fileName!="NA" : self.fileName=fileName
        infile=open(self.fileName,"rb")
        testRead=nrd.read_nimrod(infile)
        infile.close()
        header=testRead["header"]
        print("******* header ***********")
        print(header) 
        print("**************************")
        nData=testRead["data"]

        nType=header[12]  # 0=real,1=int

        self.fileName=fileName
        self.nX=header[17]
        self.nY=header[16]
        self.gridSize=header[35]
        if header[35]!=header[37] : print(sys.exit("ERROR: readNimrod uneven grid. "+str(header[35])+" "+str(header[37]))) 
        self.x0=header[36]
        self.y0 = header[34] -header[16]*self.gridSize   

        self.noVal=header[38]
        self.CenterOrCorner="center"#is the data at the "center" of the cell or "corner"

        self.imgTime =  datetime.datetime(header[1],header[2],header[3],header[4],header[5],header[6])
        
        print("Nimrod units are :", header[105])   

        if nType==1 : 
            nData=nData/32.0
            nData[nData<0]=-9999.0
            self.noVal=-9999.0
            print("nimrod header[12] =",header[12],"i.e integer type. Multiplying data by 1/32.") 

        self.data=nData.reshape((self.nY,self.nX))

    def writeNimrod(self, fileName, year, month ,day ,hour, minute,writeAs="int"):
        """
        writeNimrod(self, fileName, year, month ,day ,hour, minute,writeAs="int"):
        uses nimrod_functions_read_write (of seodey & scole 2017) to write a nimrod file. Automatically multiplies by 32 if want integer data.
        """
        #load the functions from nimrod_functions_read_write.py
        import nimrod_functions_read_write as nrd
        
        #creat a data header: 31 integers, 73 reals, string of length 8, two strings of length 24, 51 integers
        header=[None]+[-32767]*31+[-32767.0]*73+["a"*8]+["b"*24]+["c"*24]+[-32767]*51
        
        #I*2:
        #Validity Time (VT) of image 
        header[1]= year; header[2]= month; header[3]= day; header[4]= hour; header[5]= minute; header[6]= 0

        #Data Time (DT) of image (= VT for radar data)
        header[7]=  year; header[8]= month; header[9]= day; header[10]= hour; header[11]= minute
        
        header[12]=  1 if writeAs=="int" else 0          # real/floating point data, 0=real,1=int
        header[13]=  2 if writeAs=="int" else 4          #number of bytes
        
        header[15]=  0			#From image metadata. Projection of data. Set to 6 = "other" (see header entry 150)#jrw edited, was  6
        header[16]=  self.nY        #from image metadata nrows #jrw edited, was  637
        header[17]=  self.nX        #from image metadata ncols  #jrw edited, was 637 
        header[18]=  2          #default
        header[20]=  5          #default - not manadatory
        header[22]=  0          #default - not manadatory
        header[23]=  0          #default - not manadatory
        header[24]=  1          #bottom left (from metadata) SET FOR ASCII DATA
        header[25]=  -1         #default - not mandatory
        
        #R*4: 
        header[32]=  9999.00    #default
        header[34]=  1.0*self.y0+header[16]*self.gridSize  #ascii_header["yllcorner"] #Northing from image metadata (of the first row of data!) 
        header[35]=  1.0*self.gridSize    #from image metadata
        header[36]=  1.0*self.x0   #ascii_header["xllcorner"] #Easting from image metadata 
        header[37]=  1.0*self.gridSize    #from image metadata
        header[38]=  1.0*self.noVal   #missing data (from data)
        header[39]=  0.000000   #default
        header[40]=  0.000000   #default

        #Character header entries
        header[105]=  "mm/hr/32" if writeAs=="int" else "mm/hr   "   #required
        header[106]=  "                        "    #example - can be used to identify data source
        header[107]=  "                        "    #example - can be used to identify product
        header[108]=  0    #0 for composite - not mandatory

        #flatten and, perhaps, intify data
        writedat=ascii_data.flatten()
        if writeAs=="int":
            writedat=32*writedat
            writedat[writedat<0]=-9999
            writedat=writedat.astype(int)

        #create image for writing to nimrod format
        image={'data':writedat,'header': header}

        #write the image
        outfile=open(fileName,"wb")
        nrd.write_nimrod(outfile,image)
        outfile.close()
        
        
    def mk_a_mask_for_catav(self, cat,limitMaskToOne=True,FN="",plotMask=False):
        """ Inputs: A Polygon or MultiPolygon (plus the grid defined x0, y0, ny, ny, and the gridSize of self - assumed to be on consistent grid)
        Output: A grid of weights for catav. This will be 0 for cells not with in cat, 1 for cells entirely with in cat, or something in between for cells partially contained in cat.   
        limitMaskToOne - the weights from overlapping polygons in a multiploygon may sum to more than 1. Limit them to 1?
        FN!=\"\" an asc written so that it can be checked (probably a good idea!)
        plotMask - or plot the mask with matplotlib 
        """       
        if cat.geom_type   == 'MultiPolygon':
            geoms = list(cat.geoms)
        elif cat.geom_type  == 'Polygon': 
            geoms = [cat]
        else :
            sys.exit("Check that shape file !")    
        if self.CenterOrCorner!="center":
            sys.exit("mk_a_mask: this function was written assuming x0,y0 are for center of cell - adapt by adding/subracting 0.5 in XT,YT (?)")
        #create a mask (shapeMask)
        ny,nx = self.data.shape
        shapeMask=np.zeros((ny,nx)) 
        for geom in geoms: 
            #transform geometry in to gridcell indicies 
            X,Y = geom.exterior.xy 
            XT=np.subtract(X,self.x0)/self.gridSize + 0.5 
            YT=np.subtract(Y,self.y0)/self.gridSize + 0.5  #xo, yo are LLcentre
            geomT=Polygon(list(zip(XT,YT)))  # shape in the transfromed space (i.e. grid space)
            bnds=geomT.bounds
            bnds=[int(np.floor(bnds[0]))-1,int(np.floor(bnds[1]))-1,int(np.ceil(bnds[2]))+1,int(np.ceil(bnds[3]))+1]
            #loop over all points in shapeMask testing it that point is within geomT
            for iy in range(ny):
                for ix in range(nx):
                    if (ix>=bnds[0]) and (iy>=bnds[1]) and (ix<=bnds[2]) and (iy<=bnds[3]):
                        cell = Polygon( [[ix , iy], [ix + 1 , iy ], [ix + 1 , iy + 1 ], [ix , iy +1 ], [ix , iy ]] )
                        shapeMask[iy,ix]=shapeMask[iy,ix]+(geomT.intersection(cell)).area
        shapeMask = np.flipud(shapeMask) # for y-axis expect  arrayX[-1] to be south most  
        #For the case of overlapping multiploygons (not a "proper" solution but this doesn't occur for our cats)
        if limitMaskToOne:
            shapeMask = np.minimum(shapeMask,1.0)
        #write out a asci of the mask so it can be checked
        if FN !="":
            dataTMP  = self.data
            self.data = shapeMask
            self.writeAscii(FN)  
            self.data = dataTMP   
        if plotMask:  
            self.plotMap(shapeMask)
        return(shapeMask)
            
 