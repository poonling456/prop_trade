# blpConnect() # automatic if option("blpAutoConnect") is TRUE

blpConnect(host = getOption("blpHost", "192.168.91.172"),port = getOption("blpPort", 10194L), default = TRUE)

############download

name=c('DCE',
       'RBT',
       'CU',
       'IOE',
       'KEE')

month=toupper(c('f',
                'g',
                'h',
                'j',
                'k',
                'm',
                'n',
                'q',
                'u',
                'v',
                'x',
                'z'))

year=7:8

symbol=sapply(name, function(x)
{
  sapply(year,function(y)
  {
    sapply(month, function(z)
    {
      paste(x,z,y," Comdty", sep="")
    })
  })
})





###########DCE###########
dce_vo=lapply(symbol[,'DCE'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
              })

dce_vo=do.call(cbind,lapply(dce_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))

dce_vo_max=apply(dce_vo,1,which.max)
dce_changepoint=rle(dce_vo_max)
dce_active=cbind.data.frame(symbol[dce_changepoint[[2]][-1],1],
                            as.Date(names(dce_changepoint[[1]])[-length(dce_changepoint[[1]])]),
                            as.Date(names(dce_changepoint[[2]])[-1]))




###########RBT###########

rbt_vo=lapply(symbol[,'RBT'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
              })

rbt_vo=do.call(cbind,lapply(rbt_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))

rbt_vo_max=apply(rbt_vo,1,which.max)
rbt_changepoint=rle(rbt_vo_max)
rbt_active=cbind.data.frame(symbol[rbt_changepoint[[2]][-1],'RBT'],
                            as.Date(names(rbt_changepoint[[1]])[-length(rbt_changepoint[[1]])]),
                            as.Date(names(rbt_changepoint[[2]])[-1]))



###########CU###########

cu_vo=lapply(symbol[,'CU'],
             function(x)
             {
               bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
             })

cu_vo=do.call(cbind,lapply(cu_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))

cu_vo_max=apply(cu_vo,1,which.max)
cu_changepoint=rle(cu_vo_max)
cu_active=cbind.data.frame(symbol[cu_changepoint[[2]][-1],'CU'],
                           as.Date(names(cu_changepoint[[1]])[-length(cu_changepoint[[1]])]),
                           as.Date(names(cu_changepoint[[2]])[-1]))





###########IOE###########

ioe_vo=lapply(symbol[,'IOE'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
              })

ioe_vo=do.call(cbind,lapply(ioe_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))

ioe_vo_max=apply(ioe_vo,1,which.max)
ioe_changepoint=rle(ioe_vo_max)
ioe_active=cbind.data.frame(symbol[ioe_changepoint[[2]][-1],'IOE'],
                            as.Date(names(ioe_changepoint[[1]])[-length(ioe_changepoint[[1]])]),
                            as.Date(names(ioe_changepoint[[2]])[-1]))





###########KEE###########

kee_vo=lapply(symbol[,'KEE'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2017-01-01", format="%Y-%m-%d") )
              })

kee_vo=do.call(cbind,lapply(kee_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))

kee_vo_max=apply(kee_vo,1,which.max)
kee_changepoint=rle(kee_vo_max)
kee_active=cbind.data.frame(symbol[kee_changepoint[[2]][-1],'KEE'],
                            as.Date(names(kee_changepoint[[1]])[-length(kee_changepoint[[1]])]),
                            as.Date(names(kee_changepoint[[2]])[-1]))

