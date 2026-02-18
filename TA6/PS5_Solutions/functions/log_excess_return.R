log_excess_return <- function(equity, riskless){
  
  # --- compute log returns
  r = diff(log(equity))
  
  # --- lagged riskless rate
  f = riskless[1:(nrow(riskless))-1,]
  
  # --- compute excess log returns 
  y = r - f
  
  # --- add a missing value row in the beggining to maintain alignment
  y = rbind(rep(NA, ncol(y)),y)
  
  return(y)
}