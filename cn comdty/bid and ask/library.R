# install.packages("Rblpapi")
# install.packages("quantmod")
# install.packages("candlesticks", repos="http://R-Forge.R-project.org")

library(Rblpapi)
library(quantmod)
library(reshape2)
library(data.table)
library(purrr)
library(ggplot2)
rearrange=function(df)
{
  df=df[,c('times','type','value','size')]
  type_id=Lag(as.numeric(df$type),0:1)
  index=apply(type_id,1, function(x) ifelse(x[1]<x[2],0,1))
  index[1]=0
  df_temp=`colnames<-`(cbind(df,cumsum(index)),c('times','type','value','size','index'))
  dcast(setDT(df_temp),
        index + times ~ type, 
        value.var = c('value','size'))
}


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

get_object=function(x,dataset)
{
  e1=new.env()
  load(dataset,e1)
  output=get(x,envir = e1)
  rm(e1)
  gc()
  output
}



is_period=function(now_time,start_time,end_time,format_time='%H:%M:%S')
{
  format(now_time,format_time)<=end_time & format(now_time,format_time)>=start_time
}

# 
# 
# breakeven2=function(df,width,upside_limit,downside_limit,long=T,intraday=T)
# {
#   rollapply(df,
#             width,
#             function(x)
#             {
#               cat(as.character(Sys.time()), ' ', x[1,'index'], '\n',file = 'progress.txt',append = T)
#               if(long)
#               {
#                 initial=as.numeric(x[1,'value_ASK'],1)
#                 i=detect_index(as.numeric(x[-1,'value_BID']), ~ .x>=initial+upside_limit | .x<=initial-downside_limit)
#                 sign=ifelse(as.numeric(x[i+1,'value_BID'])>initial, 1,-1)
#                 
#                 if(intraday) i=ifelse( as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
#                                          (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
#                                        i,
#                                        0)
#                 
#                 if(i==0) time=0 # 0 time means does not touch limit within width
#                 else time=as.POSIXct(x[i+1,'times'])-as.POSIXct(x[1,'times'])+1 #1 means same second, 2 mean the next second
#                 
#                 c('tick'=i*sign,'time'=time*sign)
#               }
#               else
#               {
#                 initial=as.numeric(x[1,'value_BID'],1)
#                 i=detect_index(as.numeric(x[-1,'value_ASK']), ~ .x<=initial-upside_limit | .x>=initial+downside_limit)
#                 sign=ifelse(as.numeric(x[i+1,'value_ASK'])<initial, 1,-1)
#                 
#                 if(intraday) i=ifelse( as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
#                                          (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
#                                        i,
#                                        0)
#                 
#                 if(i==0) time=0 # 0 time means does not touch limit within width
#                 else time=as.POSIXct(x[i+1,'times'])-as.POSIXct(x[1,'times'])+1 #1 means same second, 2 mean the next second
#                 
#                 c('tick'=i*sign,'time'=time*sign)
#               }
#             },
#             by.column=F,
#             fill=NA,
#             align='left')
# }
# 
# breakeven=function(df,time_limit,upside_limit,downside_limit,width=time_limit*3,long=T,intraday=T) #default width is because for ioe at most 3 tick within 1 second
# {
#   rollapply(df,
#             width,
#             function(x)
#             {
#               cat(as.character(Sys.time()), ' ', x[1,'index'], '\n',file = 'progress.txt',append = T)
#               if(long)
#               {
#                 initial=as.numeric(x[1,'value_ASK'],1)
#                 start=as.POSIXct(x[1,'times'])
#                 i=detect_index(as.numeric(subset(x,subset = as.POSIXct(x[,'times'])<=start+time_limit)[-1,'value_BID']),
#                                ~ .x>=initial+upside_limit | .x<=initial-downside_limit)
#                 sign=ifelse(as.numeric(x[i+1,'value_BID'])>initial, 1,-1)
#                 end=as.POSIXct(x[i+1,'times'])
#                 if(intraday) i=ifelse( as.Date(start)==as.Date(end) && 
#                                          (all(is_period(c(start,end),'08:59:00','15:01:00')) || all(is_period(c(start,end),'19:59:00','23:30:00'))),
#                                        i,
#                                        0)
#                 
#                 if(i==0) time=0 # 0 time means does not touch limit within width
#                 else time=end-start+1 #1 means same second, 2 mean the next second
#                 
#                 c('tick'=i*sign,'time'=time*sign)
#               }
#               else
#               {
#                 initial=as.numeric(x[1,'value_BID'],1)
#                 start=as.POSIXct(x[1,'times'])
#                 i=detect_index(as.numeric(subset(x,subset = as.POSIXct(x[,'times'])<=start+time_limit)[-1,'value_ASK']),
#                                ~ .x<=initial-upside_limit | .x>=initial+downside_limit)
#                 sign=ifelse(as.numeric(x[i+1,'value_ASK'])<initial, 1,-1)
#                 end=as.POSIXct(x[i+1,'times'])
#                 
#                 if(intraday) i=ifelse( as.Date(start)==as.Date(end) && 
#                                          (all(is_period(c(start,end),'08:59:00','15:01:00')) || all(is_period(c(start,end),'19:59:00','23:30:00'))),
#                                        i,
#                                        0)
#                 
#                 if(i==0) time=0 # 0 time means does not touch limit within width
#                 else time=end-start+1 #1 means same second, 2 mean the next second
#                 
#                 c('tick'=i*sign,'time'=time*sign)
#               }
#             },
#             by.column=F,
#             fill=NA,
#             align='left')
# }
# 
# 



breakeven=function(df,time_limit,upside_limit,downside_limit,width=time_limit*3,long=T,intraday=T)
{
  rollapply(df,
            width,
            function(x)
            {
              tryCatch(
                {
                  cat(as.character(Sys.time()), ' ', x[1,'index'], '\n',file = 'progress.txt',append = T)
                },
                error= function(e)
                {
                  #do nothing
                })
              
              if(long)
              {
                initial=as.numeric(x[1,'value_ASK'],1)
                i=detect_index(as.numeric(x[-1,'value_BID']), ~ .x>=initial+upside_limit | .x<=initial-downside_limit)
                sign=ifelse(as.numeric(x[i+1,'value_BID'])>initial, 1,-1)
                
                if(intraday) i=ifelse( as.POSIXct(x[i+1,'times'])<=as.POSIXct(x[1,'times'])+time_limit && 
                                         as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
                                         (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
                                       i,
                                       0)
                
                if(i==0) time=0 # 0 time means does not touch limit within width
                else time=as.POSIXct(x[i+1,'times'])-as.POSIXct(x[1,'times'])+1 #1 means same second, 2 mean the next second
                
                c('tick'=i*sign,'time'=time*sign)
              }
              else
              {
                initial=as.numeric(x[1,'value_BID'],1)
                i=detect_index(as.numeric(x[-1,'value_ASK']), ~ .x<=initial-upside_limit | .x>=initial+downside_limit)
                sign=ifelse(as.numeric(x[i+1,'value_ASK'])<initial, 1,-1)
                
                if(intraday) i=ifelse( as.POSIXct(x[i+1,'times'])<=as.POSIXct(x[1,'times'])+time_limit && 
                                         as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
                                         (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
                                       i,
                                       0)
                
                if(i==0) time=0 #0 time means does not touch limit within width
                else time=as.POSIXct(x[i+1,'times'])-as.POSIXct(x[1,'times'])+1 #1 means same second, 2 mean the next second
                
                c('tick'=i*sign,'time'=time*sign)
              }
            },
            by.column=F,
            fill=NA,
            align='left')
}




breakeven=function(df,time_limit,upside_limit,downside_limit,width=time_limit*3,long=T,intraday=T)
{
  rollapply(df,
            width,
            function(x)
            {
              tryCatch(
                {
                  cat(as.character(Sys.time()), ' ', x[1,'index'], '\n',file = 'progress.txt',append = T)
                },
                error= function(e)
                {
                  #do nothing
                })
              
              if(long)
              {
                initial=as.numeric(x[1,'value_ASK'],1)
                i=detect_index(as.numeric(x[-1,'value_BID']), ~ .x>=initial+upside_limit | .x<=initial-downside_limit)
                sign=ifelse(as.numeric(x[i+1,'value_BID'])>=initial, 1,-1)
                
                if(intraday) i=ifelse( as.POSIXct(x[i+1,'times'])<=as.POSIXct(x[1,'times'])+time_limit && 
                                         as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
                                         (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
                                       i,
                                       0)
                
                if(i==0)
                {
                  time=NA # 0 time means does not touch limit within width
                  i=NA
                } 
                else time=difftime(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times']),units = 'secs')+1 #1 means same second, 2 mean the next second
                
                c('tick'=i*sign,'time'=time*sign)
              }
              else
              {
                initial=as.numeric(x[1,'value_BID'],1)
                i=detect_index(as.numeric(x[-1,'value_ASK']), ~ .x<=initial-upside_limit | .x>=initial+downside_limit)
                sign=ifelse(as.numeric(x[i+1,'value_ASK'])<=initial, 1,-1)
                
                if(intraday) i=ifelse( as.POSIXct(x[i+1,'times'])<=as.POSIXct(x[1,'times'])+time_limit && 
                                         as.Date(as.POSIXct(x[i+1,'times']))==as.Date(as.POSIXct(x[1,'times'])) && 
                                         (all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'08:59:00','15:01:00')) || all(is_period(c(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times'])),'19:59:00','23:30:00'))),
                                       i,
                                       0)
                
                if(i==0)
                {
                  time=NA # 0 time means does not touch limit within width
                  i=NA
                }
                else time=difftime(as.POSIXct(x[i+1,'times']),as.POSIXct(x[1,'times']),units = 'secs')+1 #1 means same second, 2 mean the next second
                
                c('tick'=i*sign,'time'=time*sign)
              }
            },
            by.column=F,
            fill=NA,
            align='left')
}
