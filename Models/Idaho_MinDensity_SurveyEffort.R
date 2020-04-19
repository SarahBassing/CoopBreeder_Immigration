#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Linear Regression: Idaho Wolf Density ~ Survey Effort
  #  Code by Bassing et al. (2020)
#-------------------------------------------------------------------------------
  #  This model estimates the relationship between survey effort and annual 
  #  minimum density of wolves in both study areas. Given there was some variation
  #  in annual survey effort, does that variation influence the estiamted minimum  
  #  density of wolves in central Idaho?

  #  SVY: continuous variable representing the number of predicted rendezvous
  #  sites surveyed each year in each study area.
  #  Survey effort has been centered & scaled.
#-------------------------------------------------------------------------------
  #  Load packages and data

  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  load("./Input/ID_Gdensity.RData")
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  sink("Idaho_MinDensity_SurveyEffort.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.001)

    #  Prior for survey effort
    B1.SVY ~ dnorm(0, 0.001)

    #  Priors for error on linear model
    tau ~ dgamma(0.001, 0.001)
    sigma <- 1/sqrt(tau)

  #  Likelihood

    #  Testing for effect of survey effort on the minimum density of wolves
    #  Data are normally distributed based on density and random variance parameter
  
    for(i in 1:nrow){
        dens[i] <- B0 + B1.SVY * SVY[i]
        y[i] ~ dnorm(dens[i], tau)
    }#i    

  #  Derived parameters

    #  Mean minimum density of wolves in Central Idaho study areas per year

    for(i in 1:nrow){
        mean.den[i] <- B0 + B1.SVY * i
    } #SVY[i]

    #  Mean minimum density of wolves in Ventral Idaho study areas across years

    Den <- mean(mean.den[])
    
  }
  ", fill=TRUE)
  sink()
#-------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  require(dplyr)
  IDd <- ID_Gdensity %>%
    group_by(Year, Harvest) %>%
    summarise(
      n = as.numeric(sum(n)),
      den = as.numeric(mean(Density)),
      ss = as.numeric(sum(nSites))
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  nrow <- nrow(IDd)
  
   win.data <- list(
    "y" = IDd[,4],
    "YR" = as.numeric(as.factor(IDd[,1])), 
    "SVY" = IDd[,5], 
    "nrow" = nrow
  )
  
  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.SVY = runif(1,-1,1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.SVY", "mean.den", "Den", "sigma", "tau")
  
  #### MCMC Settings #### 
  ni <- 30000
  nt <- 1
  nb <- 20000
  nc <- 3
  
  # Call JAGS 
  out <- jags(
    win.data, 
    inits, 
    params, 
    "Idaho_MinDensity_SurveyEffort.txt", 
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Save and take a look
  print(out, dig=2)
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_MinDensity_SurveyEffort.txt", sep = "\t")
  # Idaho_MinDensity_SurveyEffort_output <- out
  # save(Idaho_MinDensity_SurveyEffort_output, file = "./Output/Idaho_MinDensity_SurveyEffort.RData")
  
  