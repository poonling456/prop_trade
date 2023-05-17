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

trailing_stop_pct=function(tick_price,dates, datex, k=0.995)
{
  price_period=tick_price[paste(nxd(dates,1,time(tick_price)),nxd(datex,-1,time(tick_price)),sep = '/')]
  hwm=cummax(price_period)
  limit=hwm*k
  
  touch=head(price_period[price_period<limit],1)
  
  if(length(touch)==0) NA
  return(head(tick_price[time(touch)+1],1))
  
}


bt_stoploss=function(sig_n, sig_x=!sig_n,stoploss, ...)
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.POSIXct('1900-01-01'))
  exit=xts(numeric(),as.POSIXct('1900-01-01'))
  
  repeat
  {
    remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
    if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
    
    #extract sig_x after sig_n
    #extract bsl after sig_n
    #extract tsl after sig_n
    #extract psl after sig_n(nsl return nothing)
    #if all length 0 break
    #use the closest one, if sl collide at the same day, use the highest one
    
    # construct entry and exit
    
    entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
    if(length(entry)==0) break
    entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
    
    remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
    
    if(length(remain_x[remain_x])<=0) 
    {
      exit_sl=stoploss(dates=nxd(time(entry),-1),datex='',...)
      if(is.na(exit_sl)) break
      else exit=exit_sl
    }
    else
    {
      exit_sig=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
      exit_sig=xts(Op(security)[as.character(nxd(exit_sig))],nxd(exit_sig))
      exit_sl=stoploss(dates=nxd(time(entry),-1),datex=time(exit_sig),...)
    
      if(is.na(exit_sl)) exit=exit_sig
      else if(time(exit_sig)<time(exit_sl)) exit=exit_sig
      else exit=exit_sl
    }
    
    if(is.na(exit) || length(exit)==0) break
    
    temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
    df=rbind(df,temp)
     
  }
  df
}


bt_basic=function(sig_n, sig_x=!sig_n)
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.POSIXct('1900-01-01'))
  exit=xts(numeric(),as.POSIXct('1900-01-01'))
  
  sig_n=sig_n
  sig_x=sig_x
  
  
  repeat
  {
    remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
    if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
    
    #extract sig_x after sig_n
    #extract bsl after sig_n
    #extract tsl after sig_n
    #extract psl after sig_n(nsl return nothing)
    #if all length 0 break
    #use the closest one, if sl collide at the same day, use the highest one
    
    # construct entry and exit
    
    entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
    if(length(entry)==0) break
    entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
    
    remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
    if(length(remain_x[remain_x])<=0) break
    else
    {
      exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
      exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
      
      temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
      df=rbind(df,temp)
      
    }
  }
  df
}

performance=function(df)
{
  ret_day=xts(rep(0,length(time(security))),time(security))
  
  ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
  
  hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
  ret_day[hold_period]=ROC(Cl(security))[hold_period]
  
  ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
  
  mdd=maxdrawdown(cumsum(ret_day))
  
  setNames(c(mean(ret_day),
             nrow(df),
             length(which(df$`exit price`>df$`entry price`))/nrow(df),
             length(which(ret_day>0))/length(ret_day),
             length(which(ret_day<0))/length(ret_day),
             length(ret_day[ret_day!=0])/nrow(df),
             sqrt(var(ret_day)),
             mean(ret_day)/sqrt(var(ret_day)),
             mdd$maxdrawdown,
             head(mdd$to,1)-head(mdd$from,1)),
           c('minute return',
             'trade',
             'win rate',
             'up day',
             'down day',
             'holding period',
             'sd',
             'sr',
             'mdd',
             'mdd period'
           ))
}