complete_timeline = function() {
  
  # make timeline
  tl = seq.Date(as.Date(ISOdate(1954,01,01)),as.Date(ISOdate(2012,12,30)),by='d') 
  
  fusion = data.frame(year=as.POSIXlt(tl)$year+1900,month=as.POSIXlt(tl)$mon+1,day=as.POSIXlt(tl)$mday+0)
  
  # make correct dates
  rownames(fusion) = sprintf('%04d%02d%02d',fusion$year,fusion$month,fusion$day); 
  
  fusion[sprintf('%04d%02d%02d',par.6[,1],par.6[,2],par.6[,3]),'P6'] <- as.numeric(par.6[,4])
  head(fusion)
  fusion[sprintf('%04d%02d%02d',par.12[,1],par.12[,2],par.12[,3]),'P12'] <- as.numeric(par.12[,4])
  fusion[sprintf('%04d%02d%02d',par.18[,1],par.18[,2],par.18[,3]),'P18'] <- as.numeric(par.18[,4])
  head(fusion)

  
#   -----------------
  fusion[sprintf('%04d%02d%02d',par.6[,1],par.6[,2],par.6[,3]),"P6"] <- cbind(par.6[,4],par.12[,4])
  
  > head(fusion)
  year month day P6.1 P6.2
  19540101 1954     1   1   -3   3
  19540102 1954     1   2   NA   NA
  19540103 1954     1   3   -37   NA
  19540104 1954     1   4   NA   NA
  19540105 1954     1   5   NA   7.7
  19540106 1954     1   6   NA   NA

}