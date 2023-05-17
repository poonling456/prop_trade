rearrange=function(df)
{
  q=df[,c("type","times","value","size")]
  initial=list('time'= NA,
               'TRADE'=data.frame('value'=NA, 'size'=NA), 
               'BID'=data.frame('value'=NA, 'size'=NA), 
               'ASK'=data.frame('value'=NA, 'size'=NA))
  ls=NULL
  temp=initial
  time=head(q,1)$times
  i=0
  
  repeat(
    {
      if(nrow(q)==0 || c('TRADE'=1,'BID'=2,'ASK'=3)[head(q,1)$type]<=i || head(q,1)$times != time)
      {
        ls=append(ls,list(temp))
        temp=initial
        if(nrow(q)==0) return(ls)
      }
      
      i=c('TRADE'=1,'BID'=2,'ASK'=3)[head(q,1)$type]
      time=head(q,1)$times
      temp$time=time
      temp[[head(q,1)$type]]=head(q,1)[,c('value','size')]
      q=q[-1,]
    }
  )
}
