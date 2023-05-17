# install.packages("Rblpapi")
# install.packages("quantmod")
# install.packages("candlesticks", repos="http://R-Forge.R-project.org")

library(Rblpapi)
library(quantmod)
library(lmtest)
library(tseries)
library(ggplot2)
library(reshape2)
library(foreach)
library(doParallel)
library(plyr)
library(lubridate)



blpConnect(host = getOption("blpHost", "192.168.91.172"),port = getOption("blpPort", 10194L), default = TRUE)

############download

name=c('CKC')

month=toupper(c('f',
                'g',
                'h',
                'j',
                'k',
                'm',
                'n',
                'q',
                'u',
                'v',
                'x',
                'z'))

year=7:8

symbol=sapply(name, function(x)
{
  sapply(year,function(y)
  {
    sapply(month, function(z)
    {
      paste(x,z,y," Comdty", sep="")
    })
  })
})

symbol=symbol[-(1:4),]



###########CKC###########
ckc_vo=lapply(symbol,
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
              })

names(ckc_vo)=symbol

no_data=(1:length(ckc_vo))[sapply(ckc_vo,nrow)==0]

ckc_vo_matrix=do.call(cbind,lapply(ckc_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


ckc_vo_max=apply(ckc_vo_matrix,1,which.max)
ckc_changepoint=rle(ckc_vo_max)

start.date=c(head(time(ckc_vo_matrix),1),as.Date(names(ckc_changepoint[[1]])[-length(ckc_changepoint[[1]])]))
end.date=as.Date(names(ckc_changepoint[[2]]))
bbg_symbol=symbol[-no_data][ckc_changepoint$values]

#####api

ckc=mapply(function(x,y,z)
  {
  getMultipleTicks(x, eventType = c("TRADE"), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
},
bbg_symbol,
start.date,
end.date,
SIMPLIFY = F)

####clean

#the old version of nx_time() in function, just for this script
nxd=function(x,n=1,time_object=time(security))
{
  x=as.POSIXct(x)
  time_object=as.POSIXct(time_object)
  i= match(x,time_object)
  as.POSIXct(ifelse(i+n<1,
                    time_object[1],
                    ifelse((i+1)>length(time_object), 
                           tail(time_object,1), 
                           time_object[i+n])),origin=as.POSIXct( '1970-01-01 08:00:00'))
}


#extract time, size, value from different active contract, and then concatenate
ckc_time=sapply(ckc,function(x){x$time})
ckc_size=sapply(ckc,function(x){x$size})
ckc_value=sapply(ckc,function(x){x$value})

#calculate the gap between one active contract to the next active contract
ckc_diff=cumsum(sapply(2:length(ckc_value), function(x)
{
  gap=head(ckc_value[[x]],1)-tail(ckc_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

#add the gap to or subtract the gap from the first active contract
ckc_value_adj=append(ckc_value[1],sapply(2:length(ckc_value), function(x)
{
  ckc_value[[x]]-ckc_diff[x-1]
}))

#format the data to xts
ckc_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},ckc_value_adj,ckc_size,ckc_time))
names(ckc_px_vo)=c('px','Volume')

#push the market open 1 tick later ,push the market close 1 tick earlier
#so that open and close would not be represented by 1 bar
#representing open and close (1 tick) by 1 bar (1 second, 1 minute, or even 1 hour!) would give them too much importance
ckc_time_ex_opcl=as.POSIXct(sapply(time(ckc_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(ckc_px_vo))
  else if(hour==15) nxd(x,-1,time(ckc_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

#format the data to xts
ckc_px_vo_ex_opcl=xts(ckc_px_vo,ckc_time_ex_opcl)










#### START!!!!!
chartSeries(to.period(kee_px_vo_ex_opcl,period = 'minute'))



subset=unlist(unique(mapply(FUN = function(x,y) paste(x,y,sep='-'),as.POSIXlt(df$`entry time`)$year+1900,as.POSIXlt(df$`entry time`)$mon+1,SIMPLIFY = F)))
#subset=c('2017','2018')

pdf('price movement.pdf')
graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(security,
                  TA = 'addTA(overlay_n,on=1,col=rgb(0,0,1,0.5));addTA(overlay_x,on=1,col=rgb(1,0,0,0.5));addMACD(8,88,30);addSMA(100)',
                  subset = x)
    },error = function (e)
    {
      print(x)
    }
  )
})
dev.off()


ckc_1s=to.period(ckc_px_vo_ex_opcl,'seconds')
ckc_1s_roc=ROC(Cl(ckc_1s))

ckc_1s_roc_vector=`names<-`(as.numeric(ckc_1s_roc),time(ckc_1s_roc))

head(sort(ckc_1s_roc_vector,decreasing = T))

chartSeries(ckc_1s,subset='2018-02-14 14:59:00/2018-02-22 09:01:00')

kee_1s=to.period(kee_px_vo_ex_opcl,'seconds')
kee_1s_roc=ROC(Cl(kee_1s))

kee_1s_roc_vector=`names<-`(as.numeric(kee_1s_roc),time(kee_1s_roc))

chartSeries(kee_1s,subset='2018-02-22 09:00:00/2018-02-22 09:05:00')


####cross correlation
ckc_1s=to.period(ckc_px_vo,'seconds')
ckc_1s_roc=ROC(Cl(ckc_1s))

kee_1s=to.period(kee_px_vo,'seconds')
kee_1s_roc=ROC(Cl(kee_1s))

dt=cbind(kee_1s_roc,ckc_1s_roc)
dt_cpl=dt[complete.cases(dt),]
names(dt_cpl)=c('kee','ckc')

ccf_result=ccf(as.ts(dt_cpl$kee),as.ts(dt_cpl$ckc))

#kee caused by ckc
granger_result1=grangertest(kee ~ ckc, order=5,data=dt_cpl)

#ckc caused by kee
granger_result2=grangertest(ckc ~ kee, order=5,data=dt_cpl)



#### now i want to discount the gap of open and close
#### idea is to insert some na between trading sessions
#### since the correlation greatly diminish after lag of 10, i will insert 10 NA
unique(as.POSIXlt(time(ckc_1s))$hour)
unique(as.POSIXlt(time(kee_1s))$hour)

#sample1 lead
sample1=ROC(c(2:6,NA,6:2))
sample2=ROC(c(1:6,NA,6:3))

test1=ccf(sample1,sample2,na.action = na.contiguous)
test2=ccf(sample1,sample2,na.action = na.omit)


sample1<- c(1,2,3,4) 
sample2<- c(1,2,NA,4) 
test1=ccf(sample1, sample2, na.action = na.pass, plot = F) 
test2=ccf(sample1, sample2, na.action = na.pass, plot = F,type = 'covariance') 
