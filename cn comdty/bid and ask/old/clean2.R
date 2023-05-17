rearrange=function(df)
{
  q=df[,c("type","times","value","size")]
  initial=list('time'= NA,
               'TRADE'=data.frame('value'=NA, 'size'=NA), 
               'BID'=data.frame('value'=NA, 'size'=NA), 
               'ASK'=data.frame('value'=NA, 'size'=NA))
  ls=NULL
  temp=initial
  time=head(q,1)$times
  i=0
  
  repeat(
    {
      if(nrow(q)==0 || c('TRADE'=1,'BID'=2,'ASK'=3)[head(q,1)$type]<=i || head(q,1)$times != time)
      {
        ls=append(ls,list(temp))
        temp=initial
        if(nrow(q)==0) return(ls)
      }
      
      i=c('TRADE'=1,'BID'=2,'ASK'=3)[head(q,1)$type]
      time=head(q,1)$times
      temp$time=time
      temp[[head(q,1)$type]]=head(q,1)[,c('value','size')]
      q=q[-1,]
    }
  )
}


rearrange=function(df)
{
  #df=df[,c('times','type','value','size')]
  id=Lag(as.numeric(df$type),0:1)
  flag=apply(id,1, function(x) ifelse(x[1]<x[2],0,1))
  flag[1]=0
  df_temp=`colnames<-`(cbind(df,cumsum(flag)),c('times','type','value','size','flag'))
  dcast(setDT(df_temp),
        flag + times ~ type, 
        value.var = c('value','size'))
}


rearrange(head(dce_df,50)[c(1,2,6,8,12,17,18)*-1,])

clean=function(x,xts=F)
{
  #extract trade to calculate rolling gap
  x_trade=lapply(x,function(x){subset(x,type=='TRADE')})
  
  #calculate the gap between one active contract to the next active contract
  x_diff=cumsum(sapply(2:length(x_trade), function(x)
  {
    gap=head(x_trade[[x]]$value,1)-tail(x_trade[[x-1]]$value,1)
    if(length(gap)==0) 0
    else gap
  }))
  
  #extract trade time, size, type, value from different active contract, and then concatenate
  x_times=sapply(x,function(x){x$times})
  x_type=sapply(x,function(x){as.character(x$type)})
  x_size=sapply(x,function(x){x$size})
  x_value=sapply(x,function(x){x$value})
  x_condcode=sapply(x,function(x){as.character(x$condcode)})
  
  #add the gap to or subtract the gap from the first active contract
  x_value_adj=append(x_value[1],lapply(2:length(x_value), function(x)
  {
    x_value[[x]]-x_diff[x-1]
  }))
  
  #format the data to xts
   do.call(rbind,
           mapply( function(type,value,size,times,condcode)
           {
             if(xts) xts(cbind(type,value,size,condcode),times)
             else cbind.data.frame(times,type,value,size,condcode)
           },
           x_type,
           x_value_adj,
           x_size,
           x_times,
           x_condcode,
           SIMPLIFY = F))
}

dce_xts=clean(dce,T)
dce_df=clean(dce)

stopwatch=proc.time()
Lag(head(as.numeric(dce_df$type),1000),0:1)
proc.time()-stopwatch


View(temp)

temp=dce_df[1:50,c('times','type','value','size')]
temp=temp[c(1,2,6,8,12,17,18)*-1,]
id=Lag(as.numeric(temp$type),0:1)
flag=apply(id,1, function(x) ifelse(x[1]<x[2],0,1))
flag[1]=0
flag_cum=cumsum(flag)
test=cbind(temp,flag_cum)
dcast(setDT(test),times+flag_cum~ type, value.var = c('value','size'))

rearrange(head(dce_df,50)[c(1,2,6,8,12,17,18)*-1,])

rbt_xts=clean(rbt,T)
rbt_df=clean(rbt)
rbt_rearrange=rearrange(rbt_df)



cu_xts=clean(cu,T)
cu_df=clean(cu)
cu_rearrange=rearrange(cu_df)



ioe_xts=clean(ioe,T)
ioe_df=clean(ioe)
ioe_rearrange=rearrange(ioe_df)



kee_xts=clean(kee,T)
kee_df=clean(kee)
kee_rearrange=rearrange(kee_df)



xii_xts=clean(xii,T)
xii_df=clean(xii)
xii_rearrange=rearrange(xii_df)



rt_xts=clean(rt,T)
rt_df=clean(rt)
rt_rearrange=rearrange(rt_df)



zna_xts=clean(zna,T)
zna_df=clean(zna)
zna_rearrange=rearrange(zna_df)



pt_xts=clean(pt,T)
pt_df=clean(pt)
pt_rearrange=rearrange(pt_df)



ckc_xts=clean(ckc,T)
ckc_df=clean(ckc)
ckc_rearrange=rearrange(ckc_df)