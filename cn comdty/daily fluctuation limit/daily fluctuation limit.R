# install.packages("Rblpapi")
# install.packages("quantmod")
# install.packages("candlesticks", repos="http://R-Forge.R-project.org")

library(Rblpapi)
library(quantmod)
library(candlesticks)
library(tseries)
library(ggplot2)
library(reshape2)
library(foreach)
library(doParallel)
library(plyr)
library(lubridate)

#dalian ex, 4% trading limit
dce_daily_morning=to.period(dce_px_vo['T08/T15'],'day')
#no evening
dce_daily_evening=to.period(dce_px_vo['T16/T07'],'day')

dce_daily=rbind(dce_daily_morning,dce_daily_evening)

dce_HiCl=log(Hi(dce_daily)/lag(Cl(dce_daily)))

#dalian ex, 4% trading limit
ioe_daily_morning=to.period(ioe_px_vo['T08/T15'],'day')
#no evening
ioe_daily_evening=to.period(ioe_px_vo['T16/T07'],'day')

ioe_daily=rbind(ioe_daily_morning,ioe_daily_evening)

ioe_HiCl=log(Hi(ioe_daily)/lag(Cl(ioe_daily)))




#dalian ex, 4% trading limit
kee_daily_morning=to.period(kee_px_vo['T08/T15'],'day')
#no evening
kee_daily_evening=to.period(kee_px_vo['T16/T07'],'day')

kee_daily=rbind(kee_daily_morning,kee_daily_evening)

kee_HiCl=log(Hi(kee_daily)/lag(Cl(kee_daily)))




#dalian ex, 4% trading limit
cu_daily_morning=to.period(cu_px_vo['T08/T15'],'day')
#no evening
cu_daily_evening=to.period(cu_px_vo['T16/T07'],'day')

cu_daily=rbind(cu_daily_morning,cu_daily_evening)

cu_HiCl=log(Hi(cu_daily)/lag(Cl(cu_daily)))

