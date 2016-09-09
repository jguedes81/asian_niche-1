sdModulator <- function(data.df, temp.change.sd, t.base){
  tmin <- ((data.df[['tmin']][,"SD"]/10)*temp.change.sd) + (data.df[['tmin']][,"MEAN"]/10)
  tmax <- ((data.df[['tmax']][,"SD"]/10)*temp.change.sd) + (data.df[['tmax']][,"MEAN"]/10)
  
  return(sum(calcGDD(tmin, tmax, t.base=t.base)))
}