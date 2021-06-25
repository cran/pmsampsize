
cstat2rsq <- function(cstatistic, prevalence, seed = 123456){

  # define ss for simulation dataset
  n <- 1000000

  # define mu as a function of the C-statistic
  mu   <- sqrt(2) * qnorm(cstatistic)

  # simulate large sample linear prediction based on two normals
  # for non-eventsN(0, 1), events and N(mu, 1)
  set.seed(seed)
  LP <- c(rnorm(prevalence*n,  mean=0, sd=1), rnorm((1-prevalence)*n, mean=mu, sd=1))
  y <- c(rep(0, prevalence*n), rep(1, (1-prevalence)*n))

  # Fit a logistic regression with LP as covariate;
  # this is essentially a calibration model, and the intercept and
  # slope estimate will ensure the outcome proportion is accounted
  # for, without changing C-statistic
  model <- glm( y ~LP, family = binomial)

  return(list(R2.coxsnell = as.numeric(1-exp(-1*(model$null.deviance-model$deviance)/n))
              ))
}


#q <- cstat2rsq(cstatistic = 0.81, prevalence = 0.77)
