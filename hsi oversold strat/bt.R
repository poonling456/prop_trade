library(quantmod)

nxd=function(x,n=1)
{
  if(x+n>=tail(date,1)) tail(date,1)
  else if(x+n<=head(date,1)) head(date,1)
  else date[which(date==x)+n]
}

hwm=function(x,date,n)
{
  period=as.Date(nxd(date,1):nxd(date,n))
  max(hi[period,x])/op[nxd(date,1),x]
}

nsl=function(...){xts(-Inf,as.Date("9999-12-31"))}

nsw=function(...){xts(Inf,as.Date("9999-12-31"))}

tsl_atr0=function(symbol, sig_n ,k)
{
  hwm=Cl(price[[symbol]])[sig_n]
  lim=hwm-k*atr[sig_n,symbol]
  tsl(symbol, nxd(sig_n,1), hwm, lim, k)
}

tsl_atr=function(symbol, date_cur, hwm, lim, k)
{
  if(date_cur>tail(date,1)) xts(Inf, order.by = as.Date("9999-12-31"))
  else
  {
    lo=as.numeric(Lo(price[[symbol]])[date_cur])
    hi=as.numeric(Hi(price[[symbol]])[date_cur])
    op=as.numeric(Op(price[[symbol]])[date_cur])
    cl=as.numeric(Cl(price[[symbol]])[date_cur])
    atr=as.numeric(atr[date_cur,symbol])
    if(any(is.na(c(lo,hi,op,cl,atr)))) xts(Inf, order.by = as.Date("9999-12-31"))
    else if(op<lim) xts(op, order.by = date_cur)
    else if(lo<lim) xts(lo, order.by = date_cur)
    else 
    {
      if(hi>hwm)
      {
        hwm=hi
        lim=hwm-k*atr
      }
      tsl_atr(symbol, nxd(date_cur,1), hwm, lim, k)
    }
  }
}

trade=function(symbol, 
               sig_n_series, 
               sig_x_series=xts(rep(F,length(sig_n_series)),order.by = time(sig_n_series)), 
               sl=nsl, 
               sw=nsw,
               ...)
{
  if( (!any(sig_n_series,na.rm = T)) ) return()
  else 
  {
    sig_n=head(sig_n_series[sig_n_series],1)
    entry=Op(price[[symbol]])[nxd(time(sig_n))]
    
    sig_x=head(sig_x_series[time(sig_x_series)>time(entry) & sig_x_series],1)
    sig_sl=sl(symbol,time(sig_n),...)
    sig_sw=sw(symbol,time(sig_n),...)
    
    method=which.min(c(time(sig_x),time(sig_sl),time(sig_sw)))
    exit=c(time(sig_x),time(sig_sl),time(sig_sw))[method]
    
    if(time(exit)>tail(date,1)) return()
    else
    {
      temp=cbind.data.frame(symbol=symbol,
                            `entry date`=time(entry),
                            `entry price`=as.numeric(entry),
                            `exit date`=time(exit),
                            `exit price`=time(exit),
                            method=c('signal','stop loss','stop win')[method])
      print(time(entry))
      print(time(exit))
      print('')
      return(rbind(temp,
                   trade(symbol,
                         sig_n_series[paste(as.character(time(entry)),'/')],
                         sig_x_series[paste(as.character(time(entry)),'/')],
                         sl,
                         sw,
                         ...)))
    }
  }
}
      
cbind.n=function(x)
{
  x=do.call(cbind,x)
  `names<-`(x,name)
}

name=names(price)
date=time(pctB)

# debug zone


pctB=cbind.n(sapply(price,function(x){BBands(na.omit(Cl(x)))$pctB}))



signalx=pctB<.5
#signalx=`colnames<-`(signalx,name)
signaln=pctB>1.2

trade(symbol = name[10], pctB[,name[10]]<.5,pctB[,name[10]]>1.5,nsl,nsw)

traderesult=do.call(rbind,sapply(name,function(x)
{
  trade(x,
        signaln = signaln[,x],
        signalx = signalx[,x] ,
        sl = nsl)
}))
return=log(traderesult[,5]/traderesult[,3])
day=sapply(traderesult[,4], function(x){which(as.Date(x)==date)}) - sapply(traderesult[,2], function(x){which(as.Date(x)==date)})+1
dayreturn=return/day

occ=length(return)
winrate=length(which(return>0))/occ
meanreturn=mean(return)
meanwin=mean(return[return>0])
meanlose=mean(return[return<0])
dur=mean(day)
durwin=mean(day[return>0])
durlose=mean(day[return<0])
var5=quantile(dayreturn,0.05)
var1=quantile(dayreturn,0.01)

perf=cbind.data.frame(occurence=occ, `winning rate`=winrate, `mean return`=meanreturn, `mean winning return`=meanwin, `mean losing return`=meanlose,duration=dur,`winning duration`=durwin,`losing duration`=durlose,VaR5=var5,VaR1=var1)
perflog=rbind(perflog,perf)
rownames(perflog)[nrow(perflog)]="lag(pctB)<.6 & pctB>1.1 & vo>2* smavo10 & atrprice<0.04 & cl>1, 6 days no 3 days high, rsi <45, 5 day drop, 5 day close downward"

write.csv(x = perflog,file = "perflog 7-1-2018.csv")
#cl>4
#atr<0.04
#vo>2smavo10


#analysis


ranked_trade=traderesult[order(return),]
worst_trade=head(ranked_trade,20)
best_trade=tail(ranked_trade,20)

atrtoprice=apply(ranked_trade, 1,function(x){atr[nxd(as.Date(x["sig_n"]),-1),x["name"]]/as.numeric(Cl(price[[x["name"]]])[nxd(as.Date(x["sig_n"]),-1)])})
ranked_return=sort(return)
plot(ranked_return,atrtoprice)  
#less atr to price, better
price=apply(ranked_trade, 1,function(x){as.numeric(Cl(price[[x["name"]]])[nxd(as.Date(x["sig_n"]),-1)])})
plot(ranked_return,log(price,10)  )

graph=apply(best_trade, 1,function(x){chartSeries(price[[x["name"]]],subset = paste(nxd(as.Date(x[["sig_n"]]),-5),nxd(as.Date(x[["datex"]]),5),sep="/"),log.scale = T,TA="addVo();addBBands();addRSI(5)")})
graph=apply(worst_trade, 1,function(x){chartSeries(price[[x["name"]]],subset = paste(nxd(as.Date(x[["sig_n"]]),-5),nxd(as.Date(x[["datex"]]),5),sep="/"),log.scale = T,TA="addVo();addBBands();addRSI(5)")})

#worst trade due to 
#entrance in downtrend
#十字星入場
#cut too slow, cut when rsi5 <60
#3 consec yin candle/3 consec drop
#5 no new high


mdd_lo_pct=cbind.data.frame(apply(traderesult, 1, function(x){mdd_lo(x["name"],x["sig_n"],x["datex"])})/traderesult[,"Open"],return>0)

ggplot(data = mdd_lo_pct,mapping = aes(mdd_lo_pct[,1]))+geom_density(mapping = aes(group=mdd_lo_pct[,2],col=mdd_lo_pct[,2]))

mdd_lo_atr=cbind.data.frame((apply(traderesult, 1, function(x){mdd_lo(x["name"],x["sig_n"],x["datex"])})-apply(traderesult,1, function(x){cl[nxd(x["sig_n"],-1),x["name"]]}))/apply(traderesult,1, function(x){atr[nxd(x["sig_n"],-1),x["name"]]}),return>0)

ggplot(data = mdd_lo_atr,mapping = aes(mdd_lo_atr[,1]))+geom_density(mapping = aes(group=mdd_lo_atr[,2],col=mdd_lo_atr[,2]))

#ledger
chrono=traderesult[order(traderesult$sig_n),]
ledger=xts(matrix(NA,nrow = length(date),ncol = 100),order.by = date)
for(i in 1:nrow(chrono))
{
  for(j in 1:100)
  {
    if(is.na(ledger[chrono$sig_n[i],j]))
    {
      ledger[chrono$sig_n[i],j]=log(OpCl(price[[as.character(chrono$name[i])]])[chrono$sig_n[i]]+1)
      ledger[as.Date(nxd(chrono$sig_n[i]):nxd(chrono$datex[i],-1)),j]=roc[as.Date(nxd(chrono$sig_n[i]):nxd(chrono$datex[i],-1)),as.character(chrono$name[i])]
      ledger[chrono$datex[i],j]=log(chrono$pricex[i]/Cl(price[[as.character(chrono$name[i])]])[nxd(chrono$datex[i],-1)])
      break
    }
  }
}
dailyreturn=xts(rowMeans(ledger,na.rm = T),order.by = time(ledger))
totalreturn=exp(sum(dailyreturn[!is.nan(dailyreturn)]))-1
