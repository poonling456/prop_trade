name=c('DCE',
       'RBT',
       'CU',
       'IOE',
       'KEE',
       'XII',
       'RT',
       'ZNA',
       'PT',
       'CKC')

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

year=7:9

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

names(dce_vo)=symbol[,'DCE']

no_data=(1:length(dce_vo))[sapply(dce_vo,nrow)==0]

dce_vo_matrix=do.call(cbind,lapply(dce_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


dce_vo_max=apply(dce_vo_matrix,1,which.max)
dce_changepoint=rle(dce_vo_max)
dce_active=data.frame(
  start.date=c(head(time(dce_vo_matrix),1),as.Date(names(dce_changepoint[[1]])[-length(dce_changepoint[[1]])]))+1,
  end.date=as.Date(names(dce_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][dce_changepoint$values],
  stringsAsFactors = F)


###########rbt###########
rbt_vo=lapply(symbol[,'RBT'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(rbt_vo)=symbol[,'RBT']

no_data=(1:length(rbt_vo))[sapply(rbt_vo,nrow)==0]

rbt_vo_matrix=do.call(cbind,lapply(rbt_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


rbt_vo_max=apply(rbt_vo_matrix,1,which.max)
rbt_changepoint=rle(rbt_vo_max)
rbt_active=data.frame(
  start.date=c(head(time(rbt_vo_matrix),1),as.Date(names(rbt_changepoint[[1]])[-length(rbt_changepoint[[1]])]))+1,
  end.date=as.Date(names(rbt_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][rbt_changepoint$values],
  stringsAsFactors = F)

###########CU###########
cu_vo=lapply(symbol[,'CU'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(cu_vo)=symbol[,'CU']

no_data=(1:length(cu_vo))[sapply(cu_vo,nrow)==0]

cu_vo_matrix=do.call(cbind,lapply(cu_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


cu_vo_max=apply(cu_vo_matrix,1,which.max)
cu_changepoint=rle(cu_vo_max)
cu_active=data.frame(
  start.date=c(head(time(cu_vo_matrix),1),as.Date(names(cu_changepoint[[1]])[-length(cu_changepoint[[1]])]))+1,
  end.date=as.Date(names(cu_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][cu_changepoint$values],
  stringsAsFactors = F)

###########IOE###########
ioe_vo=lapply(symbol[,'IOE'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(ioe_vo)=symbol[,'IOE']

no_data=(1:length(ioe_vo))[sapply(ioe_vo,nrow)==0]

ioe_vo_matrix=do.call(cbind,lapply(ioe_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


ioe_vo_max=apply(ioe_vo_matrix,1,which.max)
ioe_changepoint=rle(ioe_vo_max)
ioe_active=data.frame(
  start.date=c(head(time(ioe_vo_matrix),1),as.Date(names(ioe_changepoint[[1]])[-length(ioe_changepoint[[1]])]))+1,
  end.date=as.Date(names(ioe_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][ioe_changepoint$values],
  stringsAsFactors = F)



###########KEE###########
kee_vo=lapply(symbol[,'KEE'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(kee_vo)=symbol[,'KEE']

no_data=(1:length(kee_vo))[sapply(kee_vo,nrow)==0]

kee_vo_matrix=do.call(cbind,lapply(kee_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


kee_vo_max=apply(kee_vo_matrix,1,which.max)
kee_changepoint=rle(kee_vo_max)
kee_active=data.frame(
  start.date=c(head(time(kee_vo_matrix),1),as.Date(names(kee_changepoint[[1]])[-length(kee_changepoint[[1]])]))+1,
  end.date=as.Date(names(kee_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][kee_changepoint$values],
  stringsAsFactors = F)


###########XII###########
xii_vo=lapply(symbol[,'XII'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(xii_vo)=symbol[,'XII']

no_data=(1:length(xii_vo))[sapply(xii_vo,nrow)==0]

xii_vo_matrix=do.call(cbind,lapply(xii_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


xii_vo_max=apply(xii_vo_matrix,1,which.max)
xii_changepoint=rle(xii_vo_max)
xii_active=data.frame(
  start.date=c(head(time(xii_vo_matrix),1),as.Date(names(xii_changepoint[[1]])[-length(xii_changepoint[[1]])]))+1,
  end.date=as.Date(names(xii_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][xii_changepoint$values],
  stringsAsFactors = F)



###########RT###########
rt_vo=lapply(symbol[,'RT'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(rt_vo)=symbol[,'RT']

no_data=(1:length(rt_vo))[sapply(rt_vo,nrow)==0]

rt_vo_matrix=do.call(cbind,lapply(rt_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


rt_vo_max=apply(rt_vo_matrix,1,which.max)
rt_changepoint=rle(rt_vo_max)
rt_active=data.frame(
  start.date=c(head(time(rt_vo_matrix),1),as.Date(names(rt_changepoint[[1]])[-length(rt_changepoint[[1]])]))+1,
  end.date=as.Date(names(rt_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][rt_changepoint$values],
  stringsAsFactors = F)



###########ZNA###########
zna_vo=lapply(symbol[,'ZNA'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(zna_vo)=symbol[,'ZNA']

no_data=(1:length(zna_vo))[sapply(zna_vo,nrow)==0]

zna_vo_matrix=do.call(cbind,lapply(zna_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


zna_vo_max=apply(zna_vo_matrix,1,which.max)
zna_changepoint=rle(zna_vo_max)
zna_active=data.frame(
  start.date=c(head(time(zna_vo_matrix),1),as.Date(names(zna_changepoint[[1]])[-length(zna_changepoint[[1]])]))+1,
  end.date=as.Date(names(zna_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][zna_changepoint$values],
  stringsAsFactors = F)


###########PT###########
pt_vo=lapply(symbol[,'PT'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(pt_vo)=symbol[,'PT']

no_data=(1:length(pt_vo))[sapply(pt_vo,nrow)==0]

pt_vo_matrix=do.call(cbind,lapply(pt_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


pt_vo_max=apply(pt_vo_matrix,1,which.max)
pt_changepoint=rle(pt_vo_max)
pt_active=data.frame(
  start.date=c(head(time(pt_vo_matrix),1),as.Date(names(pt_changepoint[[1]])[-length(pt_changepoint[[1]])]))+1,
  end.date=as.Date(names(pt_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][pt_changepoint$values],
  stringsAsFactors = F)





###########CKC###########
ckc_vo=lapply(symbol[,'CKC'],
              function(x)
              {
                bdh(x,c("VOLUME"),start.date = as.Date("2018-01-01", format="%Y-%m-%d") )
              })

names(ckc_vo)=symbol[,'CKC']

no_data=(1:length(ckc_vo))[sapply(ckc_vo,nrow)==0]

ckc_vo_matrix=do.call(cbind,lapply(ckc_vo, function(x)
{
  date=x$date
  vo=x$VOLUME
  xts(vo,date)
}))


ckc_vo_max=apply(ckc_vo_matrix,1,which.max)
ckc_changepoint=rle(ckc_vo_max)
ckc_active=data.frame(
  start.date=c(head(time(ckc_vo_matrix),1),as.Date(names(ckc_changepoint[[1]])[-length(ckc_changepoint[[1]])]))+1,
  end.date=as.Date(names(ckc_changepoint[[2]]))+1,
  bbg_symbol=symbol[-no_data][ckc_changepoint$values],
  stringsAsFactors = F)

