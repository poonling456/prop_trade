dce=apply(dce_active,1,function(x)
  {
    getMultipleTicks(as.character(x[1]), eventType = c("TRADE"), startTime = as.Date(x[2]),endTime = as.Date(x[3]),tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
  })


rbt=apply(rbt_active,1,function(x)
{
  getMultipleTicks(as.character(x[1]), eventType = c("TRADE"), startTime = as.Date(x[2]),endTime = as.Date(x[3]),tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
})


cu=apply(cu_active,1,function(x)
{
  getMultipleTicks(as.character(x[1]), eventType = c("TRADE"), startTime = as.Date(x[2]),endTime = as.Date(x[3]),tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
})



ioe=apply(ioe_active,1,function(x)
{
  getMultipleTicks(as.character(x[1]), eventType = c("TRADE"), startTime = as.Date(x[2]),endTime = as.Date(x[3]),tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
})



kee=apply(kee_active,1,function(x)
{
  getMultipleTicks(as.character(x[1]), eventType = c("TRADE"), startTime = as.Date(x[2]),endTime = as.Date(x[3]),tz = Sys.getenv("TZ", unset = "Asia/Taipei"))
})
