fit_arma <- function(ts) {
  
  final.aic <- Inf
  final.order <- c(0,0,0)
  
  for (i in 0:4) for (j in 0:4) {
    
    current.aic <- AIC(arima(ts, order=c(i, 0, j)))
    
    if (current.aic < final.aic) {
      
      final.aic <- current.aic
      final.order <- c(i, 0, j)
      final.arma <- arima(ts, order= final.order)
      
    }
    
  }
  
  list(aic = final.aic, order = final.order, model = final.arma)
  
}