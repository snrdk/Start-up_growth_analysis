# Performance measurement

fitness <- function(y, yhat, k)
{
  n <- length(y)
  e.est <- y - yhat
  sse.est <- sum(e.est^2)  
  mse.est <- sse.est / n
  rmse.est <- sqrt(mse.est)
  mae.est <- sum(abs(e.est))/n
  mape.est <- sum(abs(e.est)/y)/n
  r2 <- 1 - sse.est/sum((y-mean(y))^2)
  adjr2.est <- 1-(1-r2)*(n-1)/(n-k)
  aic.est <- n*log(sse.est/n) + 2*k
  
  return(rbind(c("SSE", "MSE", "RMSE", "MAE", "MAPE", "Adj.R2", "AIC"),
               c(sse.est, mse.est, rmse.est, mae.est, mape.est, adjr2.est, aic.est)))
}
