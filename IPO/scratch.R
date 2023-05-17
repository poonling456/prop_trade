setwd('C:/Users/Anthony/Desktop/ongoing/IPO')

cl=do.call(rbind,lapply(tick_data[cpt],function(x)
{
 head(x[x$condcode=='OC',],1)
}))


op=do.call(rbind,lapply(tick_data[cpt],function(x)
{
  head(x[x$condcode=='U',],1)
}))

ret_1stday=cl$value/op$value



temp=tick_data_xts[[1]]
temp=subset(tick_data_xts[[1]],condcode=='U')

op=lapply(temp,function(x)
{
 head(x[x$condcode=='OC',],1)
})

px_daily=lapply(tick_data_xts,
                function(x)
                {
                  
                  `colnames<-`(to.daily(x[x$type=='TRADE',2:3]),c('px','Volume'))
                })

op=do.call(rbind,lapply(tick_data[valid], function(x)
  {
  head(x,1)
}))

complete=as.Date(csv[valid,'Date'],format = '%m/%d/%Y')==as.Date(op$times)

df_opcl=csv[valid,][complete,]