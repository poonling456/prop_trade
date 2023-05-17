dce_time=sapply(dce,function(x){x$time})
dce_size=sapply(dce,function(x){x$size})
dce_value=sapply(dce,function(x){x$value})

dce_diff=cumsum(sapply(2:length(dce_value), function(x)
{
  gap=head(dce_value[[x]],1)-tail(dce_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

dce_value_adj=append(dce_value[1],sapply(2:length(dce_value), function(x)
{
  dce_value[[x]]-dce_diff[x-1]
}))

dce_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},dce_value_adj,dce_size,dce_time))
names(dce_px_vo)=c('px','Volume')

dce_time_ex_opcl=as.POSIXct(sapply(time(dce_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(dce_px_vo))
  else if(hour==15) nxd(x,-1,time(dce_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

dce_px_vo_ex_opcl=xts(dce_px_vo,dce_time_ex_opcl)

dce_1m=to.period(dce_px_vo_ex_opcl,
                 period = 'minutes',
                 OHLC = TRUE)
names(dce_1m)=c('Open','High','Low','Close','Volume')


dce_10m=to.period(dce_px_vo_ex_opcl,
                 period = 'minutes',
                 k=10,
                 OHLC = TRUE)
names(dce_10m)=c('Open','High','Low','Close','Volume')



  
rbt_time=sapply(rbt,function(x){x$time})
rbt_size=sapply(rbt,function(x){x$size})
rbt_value=sapply(rbt,function(x){x$value})

rbt_diff=cumsum(sapply(2:length(rbt_value), function(x)
{
  gap=head(rbt_value[[x]],1)-tail(rbt_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

rbt_value_adj=append(rbt_value[1],sapply(2:length(rbt_value), function(x)
{
  rbt_value[[x]]-rbt_diff[x-1]
}))

rbt_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},rbt_value_adj,rbt_size,rbt_time))
names(rbt_px_vo)=c('px','Volume')

rbt_time_ex_opcl=as.POSIXct(sapply(time(rbt_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(rbt_px_vo))
  else if(hour==15) nxd(x,-1,time(rbt_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

rbt_px_vo_ex_opcl=xts(rbt_px_vo,rbt_time_ex_opcl)

rbt_1m=to.period(rbt_px_vo_ex_opcl,
                 period = 'minutes',
                 OHLC = TRUE)
names(rbt_1m)=c('Open','High','Low','Close','Volume')





cu_time=sapply(cu,function(x){x$time})
cu_size=sapply(cu,function(x){x$size})
cu_value=sapply(cu,function(x){x$value})

cu_diff=cumsum(sapply(2:length(cu_value), function(x)
{
  gap=head(cu_value[[x]],1)-tail(cu_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

cu_value_adj=append(cu_value[1],sapply(2:length(cu_value), function(x)
{
  cu_value[[x]]-cu_diff[x-1]
}))

cu_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},cu_value_adj,cu_size,cu_time))
names(cu_px_vo)=c('px','Volume')

cu_time_ex_opcl=as.POSIXct(sapply(time(cu_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(cu_px_vo))
  else if(hour==15) nxd(x,-1,time(cu_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

cu_px_vo_ex_opcl=xts(cu_px_vo,cu_time_ex_opcl)

cu_1m=to.period(cu_px_vo_ex_opcl,
                 period = 'minutes',
                 OHLC = TRUE)
names(cu_1m)=c('Open','High','Low','Close','Volume')







ioe_time=sapply(ioe,function(x){x$time})
ioe_size=sapply(ioe,function(x){x$size})
ioe_value=sapply(ioe,function(x){x$value})

ioe_diff=cumsum(sapply(2:length(ioe_value), function(x)
{
  gap=head(ioe_value[[x]],1)-tail(ioe_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

ioe_value_adj=append(ioe_value[1],sapply(2:length(ioe_value), function(x)
{
  ioe_value[[x]]-ioe_diff[x-1]
}))

ioe_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},ioe_value_adj,ioe_size,ioe_time))
names(ioe_px_vo)=c('px','Volume')

ioe_time_ex_opcl=as.POSIXct(sapply(time(ioe_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(ioe_px_vo))
  else if(hour==15) nxd(x,-1,time(ioe_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

ioe_px_vo_ex_opcl=xts(ioe_px_vo,ioe_time_ex_opcl)

ioe_1m=to.period(ioe_px_vo_ex_opcl,
                 period = 'minutes',
                 OHLC = TRUE)
names(ioe_1m)=c('Open','High','Low','Close','Volume')








kee_time=sapply(kee,function(x){x$time})
kee_size=sapply(kee,function(x){x$size})
kee_value=sapply(kee,function(x){x$value})

kee_diff=cumsum(sapply(2:length(kee_value), function(x)
{
  gap=head(kee_value[[x]],1)-tail(kee_value[[x-1]],1)
  if(length(gap)==0) 0
  else gap
}))

kee_value_adj=append(kee_value[1],sapply(2:length(kee_value), function(x)
{
  kee_value[[x]]-kee_diff[x-1]
}))

kee_px_vo=do.call(rbind,mapply(function(x,y,z){xts(cbind(x,y),z)},kee_value_adj,kee_size,kee_time))
names(kee_px_vo)=c('px','Volume')

kee_time_ex_opcl=as.POSIXct(sapply(time(kee_px_vo),function(x)
{
  hour=as.POSIXlt(x)$hour
  if(hour==8) nxd(x,1,time(kee_px_vo))
  else if(hour==15) nxd(x,-1,time(kee_px_vo))
  else x
}),origin=as.POSIXct( '1970-01-01 08:00:00'))

kee_px_vo_ex_opcl=xts(kee_px_vo,kee_time_ex_opcl)

kee_1m=to.period(kee_px_vo_ex_opcl,
                 period = 'minutes',
                 OHLC = TRUE)
names(kee_1m)=c('Open','High','Low','Close','Volume')





