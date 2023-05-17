library(quantmod)

setwd('C:/Users/Anthony/OneDrive - CASH ALGO FINANCE GROUP LIMITED/tick data')

export=function(x)
{
  mapply(function(x,y)
  {
    print(head(x))
    print(tail(x))
    filename=paste(y,'.csv',sep='')
    print(filename)
    write.csv(x,filename)
  },
  x,
  names(x))
}

export(ckc)
export(cu)
export(dce)
export(ioe)
export(kee)
export(pt)
export(rbt)
export(rt)
export(xii)
export(zna)
