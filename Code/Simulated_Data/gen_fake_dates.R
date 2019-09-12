gen_fake_dates <- function(dates, pDay, pMonth, pYear){
  n <- length(dates)
  fake_dates <- as.Date(dates)
  
  # add random errors in day
  fake_dates <- fake_dates %m+% days(rbinom(n,1, prob = pDay)*round(sqrt(sqrt(3.5))*rnorm(n)))
  
  # add random errors in month
  fake_dates <- fake_dates %m+% months(rbinom(n,1, prob = pMonth) * round(sqrt(6)*rnorm(n)))
  
  # add random errors in year 
  fake_dates <- fake_dates %m+% years(rbinom(n,1, prob = pYear)*round(sqrt(2.5)*rnorm(n)))
  return(fake_dates)
}