foo=function(x)
{
  return()
}

a=foo(10)

boo=function(x)
{
  
}
b=boo(10)

koo=function(x)
{
  x+10
}

loo=function(x)
{
  x^2
}

date=as.Date(c("2018-01-01","2018-01-02","2018-01-03","2018-01-04","2018-01-05"))

a=xts(c(F,F,F,F,F),order.by = date)
b=head(a[a])
min(time(b),as.Date('2017-01-01'))

a=xts(100,as.Date("2018-01-11"))
b=xts(90,as.Date("2018-02-01"))
c=xts(120,as.Date("2018-01-02"))

method=which.min(time(c(a,b,c)))
exit=c(a,b,c)[method]

a=cbind.data.frame(symbol="ihi",daten=as.Date('2018-01-01'),pricen=100,datex=as.Date('2018-01-05'),pricex=102,method='yo')

b=cbind.data.frame(symbol="ihi",daten=as.Date('2018-01-01'),pricen=100,datex=as.Date('2018-01-05'),pricex=102,method='yo')

nsl=function(...){xts(-Inf,as.Date("9999-12-31"))}
tsl0=function(name, daten, k){xts(10,as.Date("2017-12-31"))}
