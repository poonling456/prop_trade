setwd("C:/Users/Anthony/Desktop/ongoing/cn comdty/bid and ask")

ioe_rearrange=readRDS('ioe_rearrange.rds')

ioe_rearrange$index=1:nrow(ioe_rearrange)
ioe_rearrange$value_ASK=na.locf(ioe_rearrange$value_ASK)
ioe_rearrange$value_BID=na.locf(ioe_rearrange$value_BID)
ioe_rearrange$size_ASK=na.locf(ioe_rearrange$size_ASK)
ioe_rearrange$size_BID=na.locf(ioe_rearrange$size_BID)

#na_row=ioe_rearrange[!complete.cases(ioe_rearrange),]

#ioe_rearrange_xts=xts(ioe_rearrange,as.POSIXct(as.character(ioe_rearrange[,times])))

#unique(as.Date(na_row$times))


#cal mid price and ratio
bid_to_ask=log(ioe_rearrange$size_BID/ioe_rearrange$size_ASK)
mid=(ioe_rearrange$value_ASK+ioe_rearrange$value_BID)/2

stopwatch=Sys.time()
winTime_long=breakeven(ioe_rearrange,time_limit = 60*10,0,1)
Sys.time()-stopwatch
saveRDS(winTime_long,'winTime_long.rds')

stopwatch=Sys.time()
winTime_short=breakeven(ioe_rearrange,time_limit = 60*10,0,1,long = F)
Sys.time()-stopwatch
saveRDS(winTime_short,'winTime_short.rds')

#last_tick_before_breakeven_long=abs(winTime_long[,'tick'])==1


#cumsum(rle(abs(diff(winTime_long[,'tick'])))$lengths)+1
df_graph_long=data.frame(ioe_rearrange,
                         'BtoA'=bid_to_ask,
                         'WinTick'=na.fill(winTime_long[,1],0),
                         'WinTime'=na.fill(winTime_long[,2],0),
                         'OnePeriodMMT'=c(0,seriesIncr(mid)-seriesDecr(mid)))

#graph
setwd("C:/Users/Anthony/Desktop/ongoing/cn comdty/bid and ask/all data point")

ggplot(df_graph_long, aes(x=BtoA, y= WinTime, color=factor(OnePeriodMMT)))+
  geom_point(alpha=.1,shape=16,size=1)+
  geom_density_2d()+
  labs(title='Aggregate')
ggsave('BtoA vs WinTime time limit 10 min aggregate.jpg',device='jpeg')


ggplot(subset(df_graph_long,subset = OnePeriodMMT==1), aes(x=BtoA, y= WinTime))+
  geom_point(alpha=.1,shape=16)+
  geom_density_2d()+
  labs(title='Last Tick Up')
ggsave('BtoA vs WinTime time limit 10 min last tick up.jpg',device='jpeg')

ggplot(subset(df_graph_long,subset = OnePeriodMMT==-1), aes(x=BtoA, y= WinTime))+
  geom_point(alpha=.1,shape=16)+
  geom_density_2d()+
  labs(title='Last Tick Down')
ggsave('BtoA vs WinTime time limit 10 min last tick down.jpg',device='jpeg')


ggplot(subset(df_graph_long,subset = OnePeriodMMT==0), aes(x=BtoA, y= WinTime))+
  geom_point(alpha=.1,shape=16)+
  geom_density_2d()+
  labs(title='Flat')
ggsave('BtoA vs WinTime time limit 10 min last tick flat.jpg',device='jpeg')


temp=add_count(subset(df_graph_long,subset = OnePeriodMMT==-1 & WinTick%in%-1:1),WinTick)
temp$n=df$n/nrow(temp)

setwd("C:/Users/Anthony/Desktop/ongoing/cn comdty/bid and ask/one tick before")

pdf('density plot.pdf')

temp=add_count(subset(df_graph_long,subset = OnePeriodMMT==1 & WinTick%in%-1:1),WinTick)
temp$n=temp$n/nrow(temp)
ggplot(temp, aes(x=BtoA,y=..density..,weight=n/sum(n),fill= factor(WinTick)))+
  geom_density(alpha=0.25)+
  scale_x_continuous(limits = c(-10,10), minor_breaks = seq(-10, 10, 0.1))+
  labs(title='Last Tick Up')

temp=add_count(subset(df_graph_long,subset = OnePeriodMMT==-1 & WinTick%in%-1:1),WinTick)
temp$n=temp$n/nrow(temp)
ggplot(temp, aes(x=BtoA,y=..density..,weight=n/sum(n),fill= factor(WinTick)))+
  geom_density(alpha=0.25)+
  scale_x_continuous(limits = c(-10,10), minor_breaks = seq(-10, 10, 0.1))+
  labs(title='Last Tick Down')

temp=add_count(subset(df_graph_long,subset = OnePeriodMMT==0 & WinTick%in%-1:1),WinTick)
temp$n=temp$n/nrow(temp)
ggplot(temp, aes(x=BtoA,y=..density..,weight=n/sum(n),fill= factor(WinTick)))+
  geom_density(alpha=0.25)+
  scale_x_continuous(limits = c(-10,10), minor_breaks = seq(-10, 10, 0.1))+
  labs(title='Last Tick Flat')

temp=add_count(subset(df_graph_long,subset = WinTick%in%-1:1),WinTick)
temp$n=temp$n/nrow(temp)
ggplot(temp, aes(x=BtoA,y=..density..,weight=n/sum(n),fill= factor(WinTick)))+
  geom_density(alpha=0.25)+
  scale_x_continuous(limits = c(-10,10), minor_breaks = seq(-10, 10, 0.1))+
  labs(title='Aggregate')
dev.off()

pdf('scatter plot.pdf')
ggplot(subset(df_graph_long,subset = OnePeriodMMT==1 & (WinTick==1 | WinTick==-1)), aes(x=BtoA, y=WinTime))+
  geom_point(size=1,alpha=.2,shape=16, color= 'light blue')+
  scale_x_continuous(limits = c(-10,10))+
  labs(title='Last Tick Up')

ggplot(subset(df_graph_long,subset = OnePeriodMMT==-1 & (WinTick==1 | WinTick==-1)), aes(x=BtoA, y=WinTime))+
  geom_point(size=1,alpha=.2,shape=16, color= 'pink')+
  scale_x_continuous(limits = c(-10,10))+
  labs(title='Last Tick Down')


ggplot(subset(df_graph_long,subset = OnePeriodMMT==0 & (WinTick==1 | WinTick==-1)), aes(x=BtoA, y=WinTime))+
  geom_point(size=1,alpha=.2,shape=16, color= 'light green')+
  scale_x_continuous(limits = c(-10,10))+
  labs(title='Last Tick Flat')

ggplot(subset(df_graph_long,subset = WinTick==1 | WinTick==-1), aes(x=BtoA, y=WinTime, color= factor(OnePeriodMMT)), adjust=2)+
  geom_point(size=1,alpha=.2,shape=16)+
  scale_x_continuous(limits = c(-10,10))+
  labs(title='Aggregate')
dev.off()