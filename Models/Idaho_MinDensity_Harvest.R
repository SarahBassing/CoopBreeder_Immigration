#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Linear Regression: Idaho Wolf Density ~ Harvest
  #  Code by Bassing et al.
#-------------------------------------------------------------------------------
  #  This model estimates the relationship between harvest of wolves and the 
  #  annual minimum density of wolves in central Idaho.
  
  #  HARV: categorical variable for when harvest did not occur (0) and when
  #  harvest did occur (1)
  
  #  Dimensions are: i = unique observation per group per year
  #                  h = harvest did or did not occur
#-------------------------------------------------------------------------------
  #  Load packages and data
  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  #load("./Input/ID_Gdensity.RData") 
  load("G:/My Drive/1_Repositories/CoopBreeder_Immigration/Input/ID_Gdensity.RData")
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  #sink("Idaho_MinDensity_Harvest.txt")
  sink("G:/My Drive/1_Repositories/CoopBreeder_Immigration/Idaho_MinDensity_Harvest.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.001)

    #  Prior for harvest
    B1.HARV ~ dnorm(0, 0.001)

    #  Priors for error on linear model
    tau ~ dgamma(0.001, 0.001)
    sigma <- 1/sqrt(tau)


  #  Likelihood

    #  Testing for effect of harvest on minimum density of wolves in Central Idaho
    #  Data are normally distributed based on density and variance parameter
  
    for(i in 1:nrow){
        dens[i] <- B0 + B1.HARV * HARV[i]
        y[i] ~ dnorm(dens[i], tau)
    }#i    

  #  Derived parameters

    #  Mean density across all groups when harvest does and does not occur in 
    #  Central Idaho
  
    for(h in 1:2){
        harv.den[h] <- B0 + B1.HARV * (h - 1)
    }

    
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
      den = as.numeric(mean(Density))
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  nrow <- nrow(IDd)
  
   win.data <- list(
    "y" = IDd[,4],
    "YR" = as.numeric(as.factor(IDd[,1])), 
    "HARV" = IDd[,2]-1, 
    "nyear" = length(IDd$Year),
    "nrow" = nrow
  )

  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.HARV = runif(1,-1,1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "tau", "sigma",  "B1.HARV", "harv.den")
  
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
    #"Idaho_MinDensity_Harvest.txt", 
    "G:/My Drive/1_Repositories/CoopBreeder_Immigration/Idaho_MinDensity_Harvest.txt",
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )

  #  Take a look and save
  
  print(out, dig=2)
  
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_MinDensity_Harvest.txt", sep = "\t")
  # Idaho_MinDensity_Harvest_output <- out
  # save(Idaho_MinDensity_Harvest_output, file = "./Output/Idaho_MinDensity_Harvest.RData")
  