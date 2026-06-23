#quick plotting functions



read.simple.file<-function(path,filename,sep=",",exclude=NULL,...)
{

fname<-paste(path,filename,sep="")
tdata<-read.csv(fname,header=T,sep=sep,...)
tdata[tdata< -999]<-NA
if (!is.null(exclude))
{
tdata<-exclude_data(tdata,exclude)
}
return(tdata)
}

#-----------------------------------------------
write.summary.table<-function(tmp,filename,exclude=NULL)
{
#assumes first column is site name
if (!is.null(exclude))
{
	tmp<-exclude_sites(tmp,exclude)
}
write.table(summary(tmp),filename,sep=",",append=T,quote=F,row.names=F)
tmp[tmp<0]<-0
write.table(summary(tmp),filename,sep=",",append=T,quote=F,row.names=F)
}

#-----------------------------------------------------------------------------------
exclude_data<-function(testdata,exclude)
{
i<-0: ((dim(testdata)[2]-6)/3 - 1)
testsites<-substr(names(testdata)[3*i+6],1,nchar(names(testdata)[3*i+6])-4)
tmatch<-is.na(match(tolower(testsites),tolower(exclude)))& substr(testsites,1,1)=="X"
testsites[tmatch]<-substr(testsites[tmatch],2,nchar(testsites[tmatch]))
tmatch<-i[!is.na(match(tolower(testsites),tolower(cdata$G2G.ID)))]
return(testdata[,sort(c(1:6,6+tmatch*3,6+tmatch*3+1,6+tmatch*3+2,dim(testdata)[2]))])
}
#-----------------------------------------------------------------------------------
exclude_sites<-function(outdata,exclude,sitecol=1)
{
return(outdata[is.na(match(outdata[,sitecol],exclude)),])
}


plot.simple.file<-function(path,filename,fdata,tdata,ncats,range,siteconf)
{
filen<-join(path,substr(filename,1,nchar(filename)-7),substr(filename,nchar(filename)-2,nchar(filename)))
print(filen)
fname<-paste(path,filen,sep="")
pdf(join(fname,".pdf"),height=30,width=45)

par(mfrow=c(9,1),mar=c(0.7,2,0,0),cex=1,tck=0.01, mgp=c(3,0.5,0))
par(cex=2)
for (i in range)
{
do.simple.plot(i,fdata,tdata,ncats,siteconf)
mtext(filename,outer=T, line=-3, side=3)
}


dev.off()

}


no.na<-function(x)
{
return(x[!is.na(x)])
}

#-------------------------------------------------------------------------------------------

plot.base.file<-function(path,filename,tdata,fordata,cdata,bdata,range,o=NULL,fl=0,od=0,num=3,trange=NULL,qual="",is_temp=F)
{

fname<-join(path,substr(filename,1,nchar(filename)-4),substr(filename,nchar(filename)-2,nchar(filename)))
#print(fname)


pdf(join(fname,".pdf"),height=30,width=45)

par(mfrow=c(9,1),mar=c(0.7,2,0,0),cex=1,tck=0.01, mgp=c(0.5,0.5,0))
par(cex=2)
res<-do.setof.plots(tdata,fordata,cdata,bdata,range,o,fl=fl,od=od,num=num,trange=trange,qual=qual,is_temp=is_temp)
mtext(filename,outer=T, line=-1, side=3,cex=1)



dev.off()
return(res)
}








#-----------------------------------------------------------------------------------------------------

do.simple.plot<-function(i,fdata,tdata,ncats,siteconf)
{
obs<-fdata[,5+i]
mod<-tdata[,i]
plot(obs,lty=1,type="l",ylim=c(0,max(c(obs,mod),na.rm=T)) )
lines(mod,col=2)
lines(obs,col=4)


m<-is.na(obs)|is.na(mod)

r2<-1 -  sum((obs[!m]-mod[!m])^2)/sum((obs[!m]-mean(obs[!m]))^2)
r2<-floor(r2*100)/100
logr2<-1 -  sum((log(obs[!m])-log(mod[!m]))^2)/sum((log(obs[!m])-mean(log(obs[!m])))^2)
logr2<-floor(logr2*100)/100
rmse<-sqrt(sum((obs[!m]-mod[!m])^2))/length(obs[!m])
rmse<-round(rmse,2)
logrmse<-sqrt(sum((log(obs[!m])-log(mod[!m]))^2))/length(obs[!m])
logrmse<-round(logrmse,2)
bias<-sum(mod[!m]-obs[!m])/length(obs[!m])
perc_bias<-bias/mean(obs[!m])*100
bias<-round(bias,2)
perc_bias<-round(perc_bias,0)

ptext(0.01,0.9,siteconf$name[i],adj=0)
ptext(0.01,0.8,join("Area: ",siteconf$area[i]),adj=0)
ptext(0.01,0.65,join("Location: ",siteconf$xg2g[i],"-",siteconf$yg2g[i]),adj=0)

#ptext(0.01,0.7,join("Miss: ",length(tdata[is.na(tdata[,5+i]),5+i])),adj=0,col=6)
ptext(0.5,0.9,join("R2: ", r2, " (", logr2, ")"), col=6,adj=0)
ptext(0.5,0.74,join("Bias: ", bias, " (", perc_bias, ")"), col=6,adj=0)
}


#plot.simple.file(path,filename,ncats,siteconf)

#---------------------------------------------------------------------------

do.stats<-function(i,tdata)
{
obs<-tdata[,5+i]
mod<-tdata[,5+ncats+i]
base<-tdata[,5+ncats*2+i]
m<-is.na(obs)|is.na(mod)


r2<-1 -  sum((obs[!m]-mod[!m])^2)/sum((obs[!m]-mean(obs[!m]))^2)
r2<-max(0,r2)
bias<-sum(mod[!m]-obs[!m])/length(obs[!m])
bias2<-sum(mod[!m]-obs[!m]+base[!m])/length(obs[!m])

perc_bias<-bias/mean(obs[!m])*100
bias<-round(bias,2)
perc_bias2<-bias2/mean(obs[!m])*100
#perc_bias=min(max(perc_bias,-50),50)
return(c(r2,perc_bias))
}

#---------------------------------------------------------------------
 do.comp<-function(tdata,i,ncats,doplot=FALSE)
{
mod<-tdata[,i+ncats+5]
obs<-tdata[,i+5]
mod<-getday(mod)
obs<-getday(obs)

if (length(mod[!is.na(mod)])>0 && length(obs[!is.na(obs)])>0)
{
if (doplot) 
{
plot(log(obs),col=3,type="l")
title(i)
lines(log(mod),col=4)
}
m1<-get_slope(mod,doplot)
m2<-get_slope(obs,doplot)


print(c(i,m1,m2,m1/m2))
return(c(i, m1,m2))
}
else
{
return(c(i,NA,NA))
}
}
#----------------------------------------------------------------------------------
get_slope<-function(x,doplot)
{
sm<-ksmooth(x = (1:length(x))[!is.na(x)], y = x[!is.na(x)],bandwidth=5,n.points=length(x[!is.na(x)]))

x[!is.na(x)]<-sm$y
lx<-log(x)
use<-rep(1,length(x))
        use[is.na(x)]<-0
use[lx>mean(lx,na.rm=T)]<-0


for (i in length(x):2)   
{

if (!is.na(x[i]) && !is.na(x[i-1]) && x[i]>x[i-1]) 
{ 
use[i:min(i+5,length(x))]<-0
}
}

#lines(lx,)
outl<-lx
outl[use==0]<-NA
if (doplot)
{
lines(lx,)
lines(outl,col=2)
}
return(median (diff(outl),na.rm=T))
}

#----------------------------------------------------------------------------------
do.plot.forecast<-function(filedir,flen=288,fint=96,fnum=3)
{


filelist<-list.files(filedir, pattern = glob2rx("*dat_*"),recursive=TRUE)
forecastlist<-list.files(filedir, pattern = glob2rx("*csv_*"),recursive=TRUE)
f_out<-get.summary.results.forc(filedir,filelist,forecastlist,cdata,flen,fint,fnum)
#o_out<-get.summary.results(filedir,filelist[1],NULL,cdata,0,0)
write.csv(f_out,file = join(filedir,"f_stats.csv"),  quote=FALSE, row.names=FALSE) 
f_out<-f_out[,c(4,1:3,5:(dim(f_out)[2]))]
write.summary.table(f_out,join(filedir,"f_summary.out"))
write.summary.table(f_out,join(filedir,"f_summary.out"),no_r2)
return(f_out)
}

