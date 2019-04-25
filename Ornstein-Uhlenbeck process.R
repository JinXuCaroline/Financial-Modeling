# define Ornstein-Uhlenbeck process
ou_sim <- function(alpha, b, sigma) {
  
  # initial value
  r_initial <- 1*exp(-alpha*1/500)
  
  # drift and volatility
  T_interval <- 500
  drift <- b*(1-exp(-alpha*1/500))
  vol <- sigma/sqrt(2*alpha)*sqrt(1-exp(-2*alpha*1/500))
  
  r_sim <- rep(r_initial, T_interval)
  for (i in 2:T_interval){
    r_sim[i] <- r_sim[i-1] + drift + vol * rnorm(1) 
  }
  return(r_sim)
}

