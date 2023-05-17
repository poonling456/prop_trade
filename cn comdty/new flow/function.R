# if the x is not in time_object, will repace x with 
# the closest element in time_object after x
nx_time=function(x,n=1,time_object=time(security))
{
  x=as.POSIXct(x)
  time_object=as.POSIXct(time_object)
  i= match(x,time_object)
  i=mapply(function(x,i)
  {
    if(is.na(i)) head(which(x<time_object),1)
    else i
  },x,i)
    

  as.POSIXct(ifelse(i+n<1,
                    time_object[1],
                   ifelse((i+1)>length(time_object),
                          tail(time_object,1),
                          time_object[i+n])),origin=as.POSIXct( '1970-01-01 08:00:00'))
}

n_laply=function(x,FUN,dimension)
{
  arg=paste('x',dimension:1,sep='')
  body=paste(sapply(arg,function(arg) paste('function(',arg,') laply(',arg,',',sep = ''))[-1*dimension],collapse = '')
  eval(parse(text=paste('laply(x,',
                        body,
                        'function(',
                        arg[dimension],
                        ')',
                        FUN,
                        paste(rep(')',dimension),collapse = ''))))
}

ndarray_which=function(arr,FUN)
{
  which(arr==FUN(arr),arr.ind = T)
}

golden_cross=function(cross,get_crossed)
{
  lag(cross<get_crossed) & cross>get_crossed
}

no_exit_signal=function(entry_signal)
{
  xts(rep(F,nrow(entry_signal)),time(entry_signal))
}
#trailing_stop_pct(time_object_signal = time(security),time_sig_n = as.POSIXct('2017-11-20 13:49:57'),time_sig_x = as.POSIXct('9999-12-31'),second_price = to.period(dce_px_vo,period = 'second'))


trailing_stop_pct=function(second_price, time_object_signal, time_sig_n, time_sig_x, threshold=0.995)
{
  #assume OHLC input
  price_period=second_price[paste(nxd(time_sig_n,1,time_object_signal),time_sig_x,sep = '/')]
  second_time=time(price_period)
  
  lo=Lo(price_period)
  hi=Hi(price_period)
  
  hwm=cummax(hi)
  limit=lag(hwm)*threshold
  
  touch=head(lo[lo<limit],1)
  
  if(length(touch)==0) return(logical())
  
  px=head(Op(price_period)[nxd(time(touch),time_object = second_time)],1)
  
  list(`time`=time(px),`price`=as.numeric(px),`method`='trailing stop')
  
}


no_stop=function(...)
{
  logical()
}

#buggy if both sig_x and sig_n is True 
#refrain this
bt_stoploss=function(security, sig_n, sig_x=!sig_n,stoploss, ...)
{
  # initialize
  remain_n=sig_n
  remain_x=sig_x
  
  df=data.frame(`entry time`=as.POSIXct(character()),
                `entry price`=numeric(),
                `exit time`=as.POSIXct(character()), 
                `exit price`=numeric(),
                `exit method`=character())
  
  time_sig_x=as.POSIXct('1900-01-01')
  time_end=tail(time(security),1)
  
  repeat
  {
    #truncate sig_n before last signal of exit
    after_x=paste(time_sig_x,'/',sep = '')
    remain_n=remain_n[after_x]
    first_sig_n=head(remain_n[remain_n],1)
    
    #no more sig -> break
    if(length(first_sig_n)==0) break
    
    #entry next open
    time_sig_n=time(first_sig_n)
    px_n=Op(security)[nxd(time_sig_n,time_object = time(security))]
    
    #truncate sig_x before signal of entry
    after_n=paste(time_sig_n,'/',sep = '')
    remain_x=remain_x[after_n]
    first_sig_x=head(remain_x[remain_x],1)
    
    #if no more exit signal-> check stop loss
    if(length(first_sig_x)==0 || time(first_sig_x)==time_end) 
    {
      #stop loss should return list of time, price and method
      #if no stop loss, return NA
      exit_sl=stoploss(time_object_signal=time(security),
                       time_sig_n = time_sig_n,
                       time_sig_x = time_end,
                       ...)
      
      if(length(exit_sl)==0) break
      
      #exit by stop loss if there is stop loss
      time_x=exit_sl$time
      px_x=exit_sl$price
      method=exit_sl$method
    }
    # if there is exit signal
    else
    {
      time_sig_x=time(first_sig_x)
      exit_sl=stoploss(time_object_signal=time(security),
                       time_sig_n = time_sig_n,
                       time_sig_x = time_sig_x,
                       ...)
      
      #case1: no stop loss
      if(length(exit_sl)==0) 
      {
        time_x=nx_time(time_sig_x,time_object=time(security))
        px_x=Op(security)[time_x]
        method='signal'
      }
      #case2: signal comes first
      else if(exit_sl$time>time_sig_x)
      {
        time_x=nx_time(time_sig_x,time_object=time(security))
        px_x=Op(security)[time_x]
        method='signal'
      }
      #case3: stop loss comes first
      else
      {
        time_x=exit_sl$time
        px_x=exit_sl$price
        method=exit_sl$method
      }
    }
    
    time_sig_x=time_x
    
    temp=cbind.data.frame(`entry time`=time(px_n),
                          `entry price`=as.numeric(px_n),
                          `exit time`=time_x, 
                          `exit price`=as.numeric(px_x),
                          `exit method`=method)
    df=rbind(df,temp)
    
  }
  df
}



##### price is a price series with bar interval that you want(must be smaller than or equal to that used to generate signal)
performance=function(ledger, price, bid_ask_spread=0,commission=0,commission_type='*')
{
  if(nrow(ledger)>0)
  {
    ledger$`entry price`=ledger$`entry price`+bid_ask_spread
    ledger$`exit price`=ledger$`exit price`-bid_ask_spread
    
    if(commission_type=='*')
    {
      ledger$`entry price`=ledger$`entry price`*(1+commission)
      ledger$`exit price`=ledger$`exit price`*(1-commission)
    }
    else if(commission_type=='+')
    {
      ledger$`entry price`=ledger$`entry price`+commission
      ledger$`exit price`=ledger$`exit price`-commission
    }
    
    ret_bar=xts(rep(0,nrow(price)),time(price))
    
    ret_bar[ledger$`entry time`]=log(Cl(price[ledger$`entry time`])/ledger$`entry price`)
    
    hold_period=paste(nx_time(ledger$`entry time`,1,time(price)),'/',nx_time(ledger$`exit time`,-1,time(price)),sep = '')
    ret_bar[hold_period]=ROC(Cl(price))[hold_period]
    
    ret_bar[as.character(nx_time(ledger$`exit time`,0,time(price)))]=log(ledger$`exit price`/Cl(price[as.character(nx_time(ledger$`exit time`,-1,time(price)))]))
    
    mdd=maxdrawdown(cumsum(ret_bar))
    
    list(`ledger`=ledger,
         `bar return`=ret_bar,
         `mean bar return`=mean(ret_bar),
         `trade`=nrow(ledger),
         `win rate`=length(which(ledger$`exit price`>ledger$`entry price`))/nrow(ledger),
         `up day`=length(which(ret_bar>0))/length(ret_bar),
         `down day`=length(which(ret_bar<0))/length(ret_bar),
         `holding period`=length(ret_bar[ret_bar!=0])/nrow(ledger),
         `sd`=sqrt(var(ret_bar)),
         `sr`=mean(ret_bar)/sqrt(var(ret_bar)),
         `mdd`=mdd$maxdrawdown,
         `mdd period`=head(mdd$to,1)-head(mdd$from,1))
  }
  else
  {
    list(`ledger`=NA,
         `bar return`=NA,
         `mean bar return`=NA,
         `trade`=NA,
         `win rate`=NA,
         `up day`=NA,
         `down day`=NA,
         `holding period`=NA,
         `sd`=NA,
         `sr`=NA,
         `mdd`=NA,
         `mdd period`=head(mdd$to,1)-head(mdd$from,1))
  }
}

########working until here, seems correct

# 
# ####testing the function####
# 
# macd_hist=macd$macd - macd$signal
# 
# signal_n=seriesIncr(macd_hist) & macd_hist<0
# signal_x=seriesDecr(macd_hist)
# 
# trade3=bt_stoploss(security = security ,
#                    sig_n = signal_n ,
#                    sig_x = signal_x,
#                    stoploss = trailing_stop_pct,  
#                    second_price=to.period(dce_px_vo,period = 'seconds'))
# 
# metrics3=performance(trade3,price = dce_10m,bid_ask_spread = 1,commission = 0.000165,commission_type = '*')
# metrics4=performance(trade3,price = dce_10m)
# 
# ####testing the function####