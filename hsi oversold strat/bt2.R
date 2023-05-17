cbind.n=function(x)
{
  x=do.call(cbind,x)
  `names<-`(x,name)
}

nxd=function(x,n=1)
{
  cond_tail=x+n>=tail(date,1)
  cond_head=x+n<=head(date,1)
  
  as.Date(mapply(function(x,cond_tail,cond_head)
  {
    if(cond_tail) tail(date,1)
    else if(cond_head) head(date,1)
    else date[which(date==x)+n]
  },x,cond_tail,cond_head))
}

no_exit=function(...){xts(rep(F,length(date)),date)}

no_stop=function(...){xts(-Inf,as.Date("9999-12-31"))}

trailing_stop=function(symbol, k, dates, datex)
{
  td=dates
  hwm=as.numeric(Cl(price[[symbol]][td]))
  if(is.na(atr[td,symbol])) return(xts(-Inf,as.Date("9999-12-31")))
  limit=hwm-as.numeric(atr[td,symbol])*k
  
  repeat
  {
    td=nxd(td)
    if(td>=tail(time(price[[symbol]]),1) || td>=datex) return(xts(-Inf,as.Date("9999-12-31")))
    op=Op(price[[symbol]][td])  
    lo=Lo(price[[symbol]][td])  
    cl=Cl(price[[symbol]][td])
    if(any(is.na(c(op,lo,cl))) || length(op)==0 || length(lo)==0 || length(cl)==0) return(xts(-Inf,as.Date("9999-12-31")))
    if(as.numeric(op)<limit) return(op)
    if(as.numeric(lo)<limit) return(xts(limit,td))
    if(as.numeric(cl)>hwm) 
    {
      hwm=as.numeric(cl)
      limit=hwm-as.numeric(atr[td,symbol])*k
    }
  }
}
# symbol='11 HK Equity'
# dates=as.Date('2000-04-13')
# k=3
# trailing_stop(symbol = symbol,dates = dates,k = k)

trade=function(input, sig_en, sig_ex, stoploss, ...)
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.Date('1900-01-01'))
  exit=xts(numeric(),as.Date('1900-01-01'))
  
  repeat
  {
    sig_en=sig_en[paste(time(exit),'/',sep = '')] #extract sig_en after last exit
    if(length(sig_en[sig_en])<=0 || head(time(sig_en[sig_en]),1)==tail(date,1) || !(nxd(head(time(sig_en[sig_en]),1)) %in% time(input))) break  #no more sig-> break
    
    entry=head(time(sig_en[sig_en]),1)
    entry=xts(Op(input)[nxd(entry)],nxd(entry)) #entry with the first True
    
    sig_ex=sig_ex[paste(time(entry),'/',sep = '')] #extract sig_ex after last entry
    
    if(length(sig_ex[sig_ex])<=0 || head(time(sig_ex[sig_ex]),1)==tail(date,1) || !(nxd(head(time(sig_ex[sig_ex]),1)) %in% time(input))) exit_sig=xts(-Inf,as.Date("9999-12-31"))
    else
    {
      exit_sig=head(time(sig_ex[sig_ex]),1)
      exit_sig=xts(Op(input)[nxd(exit_sig)],nxd(exit_sig))
    }
    
    exit_sl=stoploss(dates=nxd(time(entry),-1),datex=time(exit_sig),...)#impletment other stoploss later
    
    if(all(c(time(exit_sig),time(exit_sl))>tail(time(input),1))) break    #if all length 0 break
    if(time(exit_sig)<time(exit_sl)) exit=exit_sig
    else if(time(exit_sl)<time(exit_sig)) exit=exit_sl
    else exit=xts(max(exit_sl,exit_sig),time(exit_sl))        #use the closest one, if sl collide at the same day, use the highest one
    
    temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
    df=rbind(df,temp)
  }
  df
}

ledger=lapply(name, function(x)
{
  df=trade(input = price[[x]],
           sig_en = (clcl>0 &lag(clcl<.03))[,x]& (benchmark_cl>benchmark_sma40),
           sig_ex = benchmark_cl<benchmark_sma40,
           stoploss = trailing_stop,
           symbol=x,k= 4.25)
  if(nrow(df)>0) cbind(symbol=x,df)
})

ledger_df=do.call(rbind,ledger)
ledger_df$symbol=as.character(ledger_df$symbol)

ret=lapply(split(ledger_df,1:nrow(ledger_df)),
           function(x)
             {
               if(x$`exit date`==x$`entry date`)
               {
                 xts(log(x$`exit price`/x$`entry price`),order.by = x$`entry date`)
               }else
               {
                 c(log(Cl(price[[x$symbol]])[as.character(x$`entry date`)]/x$`entry price`)-comm,
                   ROC(Cl(price[[x$symbol]]))[paste(nxd(x$`entry date`,1),'/',nxd(x$`exit date`,-1),sep = '')],
                   xts(log(x$`exit price`/Cl(price[[x$symbol]])[nxd(x$`exit date`,-1)]),order.by = x$`exit date`)-comm)
               }
             })

ret_df=do.call(cbind,ret)

p=length(which(ledger_df$`exit price`/ledger_df$`entry price`>1))/nrow(ledger_df)
dur=mean(sapply(ledger_df$`exit date`,function(x) date[x==date])-sapply(ledger_df$`entry date`,function(x) date[x==date]))

op_position=xts(apply(ret_df,1,function(x) length(x[!is.na(x)])),as.Date(names(apply(ret_df,1,function(x) length(x[!is.na(x)])))))

daily_ret_vt=apply(ret_df, 1, mean, na.rm=T)
daily_ret=xts(daily_ret_vt,order.by = as.Date(names(daily_ret_vt)))
daily_ret_cu=cumsum(daily_ret)

sr=mean(daily_ret)/sqrt(var(daily_ret))*sqrt(250)
mdd=maxdrawdown(daily_ret_cu)
mdd_pct=mdd$maxdrawdown
mdd_dur=mdd$to-mdd$from


chartSeries(daily_ret_cu,TA='addTA(benchmark_ret_cu,on=1);addTA(op_position)')
print("brake!")
print("stop!")
#perflog=data.frame(`APR`=numeric(),`SR`=numeric(),`MDD`=numeric(),`MDD duration`=integer(),`Win rate`=numeric(),`Holding period`=numeric(),`Description`=character())
perflog=rbind(perflog,data.frame(`APR`=tail(daily_ret_cu,1)/18,`SR`=sr,`MDD`=mdd_pct,`MDD duration`=mdd_dur,`Win rate`=p,`Holding period`=dur,
                                 `Description`='rebound after 3% drop when >sma40, exit when <sma40 tsl 4.25'))

# hi=trade(input = price[['11 HK Equity']],
#          sig_en = Cl(price[['11 HK Equity']])>SMA(Cl(price[['11 HK Equity']]),50),
#          sig_ex = Cl(price[['11 HK Equity']])<SMA(Cl(price[['11 HK Equity']]),50),
#          stoploss = trailing_stop,symbol='11 HK Equity',k= 3)
# 
# bye=lapply(name[1:5], function(x){print(x)
#   cbind(symbol=x, 
#   trade(input = price[[x]],
#   sig_en = Cl(price[[x]])>SMA(Cl(price[[x]]),500),
#   sig_ex = Cl(price[[x]])<SMA(Cl(price[[x]]),500),
#   stoploss = trailing_stop,symbol=x,k= 100))})
# foo=function(x,y,z){sum(x,y*2,z*3)}
# boo=function(y,...){foo(y,...)}
# boo(y=10,x=7,z=8)

# ledger=lapply(name, function(x)
# {
#   print(x)
#   df=trade( input = price[[x]],
#             sig_en = Cl(price[[x]])>SMA(Cl(price[[x]]),20),
#             sig_ex = Cl(price[[x]])<SMA(Cl(price[[x]]),20),
#             stoploss = trailing_stop,symbol=x,k= 3)
# if(nrow(df)>0) cbind(symbol=x,df)
# }
# )
# ledger1=do.call(rbind,ledger)
# ledger2=ledger1[5,]
# 
# hi=lapply(split(ledger1[1:10,],1:10),
#       function(x)
# {
#   if(x$`exit date`==x$`entry date`)
#   {
#     xts(x$`exit price`/x$`entry price`,order.by = x$`entry date`)
#   }else 
#   {
#     c(OpCl(price[[x$symbol]])[x$`entry date`],
#       ROC(Cl(price[[x$symbol]]))[paste(nxd(x$`entry date`,1),'/',nxd(x$`exit date`,-1),sep = '')],
#       xts(log(x$`exit price`/Cl(price[[x$symbol]])[nxd(x$`exit date`,-1)]),order.by = x$`exit date`))
#   }
# })
# hihi=do.call(cbind,hi)
# 
# hi=lapply(split(ledger1[1:10,],1:10),
#           function(x)
#           {
#             print(x['symbol'])
#             print(class(x['symbol']))
#           })
# 
# 
# hi=apply(ledger1[1:10,],1,
#          function(x)
#          {
#            class(as.Date(x['entry date']))
#          })
# 
# 
# date[head(paste(ledger2$`entry date`,'/',nxd(ledger2$`exit date`,-1),sep=''),1)]
# r1=xts(c(1,2,3),order.by = as.Date(c('2018-01-01','2018-01-02','2018-01-03')))
# r2=xts(c(1,2,3),order.by = as.Date(c('2018-01-02','2018-01-03','2018-01-04')))
# do.call(cbind,list(r1,r2))
