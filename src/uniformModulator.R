uniformModulator <- function(data.df, temp.change, t.base){
  tmin <- (data.df[['tmin']][,"MEAN"]/10) + temp.change
  tmax <- (data.df[['tmax']][,"MEAN"]/10) + temp.change
  
  return(sum(calcGDD(tmin, tmax, t.base=t.base)))
}