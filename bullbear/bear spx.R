library(Rblpapi)
library(quantmod)

prevT=function(x,n,i)
{
  if(i==0) return(n)
  if(x[i]==F | is.na(x[i])) return(n)
  return(prevT(x,n+1,i-1))
}

blpConnect()

SPX=bdh("SPX Index",c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST","VOLUME","CUR_MKT_CAP","PUT_CALL_VOLUME_RATIO_CUR_DAY","PCT_MEMB_PX_GT_100D_MOV_AVG","PCT_MEMB_PX_GT_150D_MOV_AVG","PCT_MEMB_PX_GT_250D_MOV_AVG"),start.date = as.Date("1977-01-01"))
SPX=xts(SPX[,-1],as.Date(SPX[,1]))
SPX=`names<-`(SPX,c('Open','High','Low','Close','Volume','Market Cap','Put Call Ratio','100D Breadth','150D Breadth','250D Breadth'))

uerate_raw=read.csv('UNRATE.csv')
uerate_raw=xts(uerate_raw[,-1],as.Date(uerate_raw[,1]))

#might be useful for bull phase 1
uerate_incr=seriesIncr(uerate_raw)
uerate_consecincr=xts(sapply(1:length(uerate_incr), function(i) prevT(uerate_incr,0,i)),time(uerate_incr))
uerate_consecincr=cbind(uerate_consecincr,SPX)[,1]
uerate_consecincr=na.locf(uerate_consecincr)

uerate_incr_6month=xts(apply(lag(uerate_incr,0:5),1,sum,na.rm=T),time(uerate_incr))
uerate_incr_6month=cbind(uerate_incr_6month,SPX)[,1]
uerate_incr_6month=na.locf(uerate_incr_6month)

uerate_incr_4month=xts(apply(lag(uerate_incr,0:3),1,sum,na.rm=T),time(uerate_incr))
uerate_incr_4month=cbind(uerate_incr_4month,SPX)[,1]
uerate_incr_4month=na.locf(uerate_incr_4month)

uerate=cbind(uerate_raw,SPX)[,1]
uerate=na.locf(uerate_raw)

uerate_sma3=SMA(uerate_raw,3)
uerate_sma3=cbind(uerate_sma3,SPX)[,1]
uerate_sma3=na.locf(uerate_sma3)

#6 and 12 useful
uerate_sma6=SMA(uerate_raw,6)
uerate_sma6_mmt=ROC(uerate_sma6)
uerate_sma6=cbind(uerate_sma6,SPX)[,1]
uerate_sma6=na.locf(uerate_sma6)
uerate_sma6_mmt=cbind(uerate_sma6_mmt,SPX)[,1]
uerate_sma6_mmt=na.locf(uerate_sma6_mmt)

uerate_sma12=SMA(uerate_raw,12)
uerate_sma12_mmt=ROC(uerate_sma12)
uerate_sma12=cbind(uerate_sma12,SPX)[,1]
uerate_sma12=na.locf(uerate_sma12)
uerate_sma12_mmt=cbind(uerate_sma12_mmt,SPX)[,1]
uerate_sma12_mmt=na.locf(uerate_sma12_mmt)

uerate_sma24=SMA(uerate_raw,24)
uerate_sma24=cbind(uerate_sma24,SPX)[,1]
uerate_sma24=na.locf(uerate_sma24)

SPK=bdh("SPX Index",c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST","VOLUME","CUR_MKT_CAP","PUT_CALL_VOLUME_RATIO_CUR_DAY","PCT_MEMB_PX_GT_100D_MOV_AVG","PCT_MEMB_PX_GT_150D_MOV_AVG","PCT_MEMB_PX_GT_250D_MOV_AVG"),start.date = as.Date("1977-01-01"))
SPK=xts(SPK[,-1],as.Date(SPK[,1]))
SPK=`names<-`(SPK,c('Open','High','Low','Close','Volume','Market Cap','Put Call Ratio','100D Breadth','150D Breadth','250D Breadth'))

VIX=bdh("VIX Index",c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST","VOLUME","CUR_MKT_CAP","PUT_CALL_VOLUME_RATIO_CUR_DAY"),start.date = as.Date("1977-01-01"))
VIX=xts(VIX[,-1],as.Date(VIX[,1]))
VIX=`names<-`(VIX,c('Open','High','Low','Close','Volume','Market Cap','Put Call Ratio'))

yieldspread=bdh('USYC2Y10 Index',c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST","VOLUME"),start.date = as.Date('1977-01-01'))
yieldspread=xts(yieldspread[,-1],as.Date(yieldspread[,1]))
yieldspread=`names<-`(yieldspread,c('Open','High','Low','Close','Volume'))

votocap_SPX=na.omit(Vo(SPX)/SPX$`Market Cap`)
votocap_SPK=na.omit(Vo(SPK)/SPK$`Market Cap`)

rsi14=RSI(na.omit(Cl(SPX),14))

atr20=ATR(na.omit(HLC(SPX)),n = 20)$atr

atr20pct=atr/Cl(SPX)

#Unemployment rate
head=1977
tail=2018
by=3
subset=paste(seq(head,tail,by = by),ifelse(is.na(Next(seq(head,tail,by = by)-1)),"",Next(seq(head,tail,by = by)-1)),sep = "/")

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate,col="red");addTA(uerate_sma3,on=2,col="orange");addTA(uerate_sma12,on=2,col="yellow")',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate,col="red");addTA(uerate_sma6,on=2,col="orange");addTA(uerate_sma24,on=2,col="yellow")',log.scale = T,subset = x)})

#6 and 12 useful and is enough, 12 better
chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma6);addTA(uerate_sma6_mmt)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma12);addTA(uerate_sma12_mmt)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma12);addTA(uerate_sma12_mmt);addTA(uerate_consecincr)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma12);addTA(uerate_sma12_mmt);addTA(uerate_consecincr);addTA(uerate_incr_6month)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma6);addTA(uerate_sma6_mmt);addTA(uerate_consecincr);addTA(uerate_incr_4month)',log.scale = T,subset = x)})

#Unemployment rate + sma
chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addSMA(100);addTA(uerate_sma12,col="orange");addTA(uerate_sma12_mmt,col="green");addTA(xts(rep(0,length(uerate_sma12_mmt)), order.by=time(uerate_sma12_mmt)),on=3,col="red")',log.scale = T,subset = x)})

#Unemployment rate + ROC
chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addSMA(100);addTA(uerate_sma12,col="orange");addTA(uerate_sma12_mmt,col="green");addTA(xts(rep(0,length(uerate_sma12_mmt)), order.by=time(uerate_sma12_mmt)),on=3,col="red");addROC(n=20)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma12,col="orange");addTA(uerate_sma12_mmt,col="green");addTA(xts(rep(0,length(uerate_sma12_mmt)), order.by=time(uerate_sma12_mmt)),on=3,col="red");addROC(n=20);addBBands(n=20,draw="width")',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma12,col="orange");addTA(uerate_sma12_mmt,col="green");addTA(xts(rep(0,length(uerate_sma12_mmt)), order.by=time(uerate_sma12_mmt)),on=3,col="red");addROC(n=20);addTA(atr20pct)',log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(uerate_sma6,col="orange");addTA(uerate_sma6_mmt,col="green");addTA(xts(rep(0,length(uerate_sma6_mmt)), order.by=time(uerate_sma6_mmt)),on=3,col="red");addROC(n=20)',log.scale = T,subset = x)})
#20d ROC <-.05 seems useful, atr and bbw not very useful, Unemployment rate 6 month is quicker but more false signal

bear=(uerate_sma6_mmt>0 & (ROC(x = Cl(SPX),n=20)<.05*-1))+0
chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA='addTA(bear)',log.scale = T,subset = x)})

#put call ratio

head=1995
tail=2018
by=2
subset=paste(seq(head,tail,by = by),ifelse(is.na(Next(seq(head,tail,by = by)-1)),"",Next(seq(head,tail,by = by)-1)),sep = "/")
chart=sapply(subset, function(x){chartSeries(OHLC(SPX),TA = 'addTA(na.omit(SPX$`Put Call Ratio`,5),col="yellow");addTA(SMA(na.omit(SPX$`Put Call Ratio`),10),col="orange")',log.scale = T,subset = x)})



pdf("votocap SMA20.pdf")
chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(votocap,legend=F,col=\"green\");addTA(SMA(votocap,20),legend=F,col=\"blue\")",log.scale = T,subset = x)})
dev.off()

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(vo20tovo200)",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(SMA(HSCI_breadth$`ARMS_weekly`,50))",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(votocap20tovotocap200)",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(votocap20tovotocap200)",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(HSI$`P/EBITDA`)",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(SMA(na.omit(HSI$`PUT/CALL`),50))",log.scale = T,subset = x)})

chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(HSI_breadth$`10D>SMA`)",log.scale = T,subset = x)})

pdf("HSCI breadth EMA20.pdf")
chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(EMA(na.omit(HSCI_breadth$`150D>SMA`),20),legend=NULL)",log.scale = T,subset = x)})
dev.off()

pdf("HSCI breadth Channel 50.pdf")
chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(DonchianChannel(na.omit(HSCI_breadth$`150D>SMA`),50),legend=NULL);addTA(HSCI_breadth$`150D>SMA`,legend=NULL,col=\"green\",on=2)",log.scale = T,subset = x)})
dev.off()

pdf("VHSI.pdf")
chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(Cl(VHSI),legend=F)",log.scale = T,subset = x)})
dev.off()

pdf("VHSI Channel 60.pdf")
chart=sapply(subset, function(x){chartSeries(OHLC(HSI),TA="addTA(DonchianChannel(na.omit(Cl(VHSI)),60),legend=NULL);addTA(Cl(VHSI),legend=F,col=\"green\",on=2)",log.scale = T,subset = x)})
dev.off()

