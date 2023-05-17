csv=read.csv("IPO list 20160713-20180712.csv",
             header = T,
             colClasses = 'character',
             na.strings = 'N/A')


symbol=paste(csv$Code,' HK Equity', sep='')


csv=`rownames<-`(csv,symbol)

date=as.POSIXct(paste(csv[,1],' 00:00:01',sep=''),format='%m/%d/%Y %H:%M:%S')

install.packages('Rblpapi')
library(Rblpapi)
blpConnect()


tick_data=mapply(
  function(x,y){
    getMultipleTicks(x, 
                     eventType = c('BID','ASK','TRADE'), 
                     startTime = y,
                     endTime = y+60*60*24*30,
                     tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  symbol,
  date,
  SIMPLIFY = F
)


cpt=mapply(function(x,y)
{
  if(nrow(x)==0) F
  else
  {
    as.Date(head(x,1)$times)==as.Date(y)+1
  }
},
tick_data,
date)

library(quantmod)

tick_data_xts=lapply(tick_data[cpt],
                     function(x)
                       {
                       x$type=as.character(x$type)
                       temp=xts(x[,-1],x[,1])
                     })

# 
# temp=getMultipleTicks('784 HK Equity', 
#                  eventType = c('BID','ASK','TRADE'), 
#                  startTime = as.POSIXct('2017-12-28'),
#                  endTime = as.POSIXct('2017-12-28')+60*60*24*30,
#                  tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
# tick_data[['0784 HK Equity']]=temp