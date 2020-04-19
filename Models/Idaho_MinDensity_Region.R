#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Linear Regression: Idaho Wolf Density ~ Survey Effort
  #  Code by Bassing et al. (2020)
#-------------------------------------------------------------------------------
  #  This model estimates the relationship between study area and annual minimum 
  #  density of wolves in central Idaho.

  #  sarea: categorical variable for East study area (0) and West study area (1)
  #  in Central Idaho
  
  #  Dimensions are: i = unique observation per group per year
  #                  sa = one of two study areas
#-------------------------------------------------------------------------------
  #  Load packages and data

  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  load("./Input/ID_GDensity.RData")
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  sink("Idaho_MinDensity_Region.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.001)

    #  Prior for study area (region)
    B1.Region ~ dnorm(0, 0.001)

    #  Priors for error on linear model
    tau ~ dgamma(0.001, 0.001)
    sigma <- 1/sqrt(tau)

  #  Likelihood

    #  Testing for effect of study area on the minimum density of wolves
    #  Data are normally distributed based on density and variance parameter

    for(i in 1:nrow){
        dens[i] <- B0 + B1.Region * sarea[i]
        y[i] ~ dnorm(dens[i], tau)
    }#i    

  #  Derived parameters

    #  Mean minimum density of wolves in each study area in central Idaho

    for(sa in 1:2){
        sa.den[sa] <- B0 + B1.Region * (sa - 1)
    }
    
  }
  ", fill=TRUE)
  sink()
#-------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  nrows <- nrow(ID_Gdensity)

   win.data <- list(
    "sarea" = ID_Gdensity[,1]-1,
    "y" = ID_Gdensity[,2],
    "YR" = as.numeric(as.factor(ID_Gdensity[,4])), 
    "HARV" = ID_Gdensity[,5], 
    "nyear" = length(unique(ID_Gdensity$Year)),
    "nrow" = nrows
  )
  
  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.Region = runif(1,-1,1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.Region", "sa.den", "sigma", "tau")
  
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
    "Idaho_MinDensity_Region.txt",
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Save and take a look
  print(out, dig=2)
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_MinDensity_Region.txt", sep = "\t")
  # Idaho_MinDensity_Region_output <- out
  # save(Idaho_MinDensity_Region_output, file = "./Output/Idaho_MinDensity_Region.RData")
  