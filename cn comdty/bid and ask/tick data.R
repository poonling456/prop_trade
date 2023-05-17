dce=mapply(
  function(x,y,z)
    {
      getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
    },
  dce_active$bbg_symbol,
  dce_active$start.date,
  dce_active$end.date,
  SIMPLIFY = F)






rbt=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  rbt_active$bbg_symbol,
  rbt_active$start.date,
  rbt_active$end.date,
  SIMPLIFY = F)





cu=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  cu_active$bbg_symbol,
  cu_active$start.date,
  cu_active$end.date,
  SIMPLIFY = F)








ioe=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  ioe_active$bbg_symbol,
  ioe_active$start.date,
  ioe_active$end.date,
  SIMPLIFY = F)








kee=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  kee_active$bbg_symbol,
  kee_active$start.date,
  kee_active$end.date,
  SIMPLIFY = F)









xii=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  xii_active$bbg_symbol,
  xii_active$start.date,
  xii_active$end.date,
  SIMPLIFY = F)








rt=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  rt_active$bbg_symbol,
  rt_active$start.date,
  rt_active$end.date,
  SIMPLIFY = F)









zna=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  zna_active$bbg_symbol,
  zna_active$start.date,
  zna_active$end.date,
  SIMPLIFY = F)








pt=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  pt_active$bbg_symbol,
  pt_active$start.date,
  pt_active$end.date,
  SIMPLIFY = F)







ckc=mapply(
  function(x,y,z)
  {
    getMultipleTicks(x, eventType = c('BID','ASK','TRADE'), startTime = y,endTime = z,tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  },
  ckc_active$bbg_symbol,
  ckc_active$start.date,
  ckc_active$end.date,
  SIMPLIFY = F)
