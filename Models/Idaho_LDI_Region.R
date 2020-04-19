#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Logistic Regression: Pr(Long-Distance Immigrant) ~ Region
  #  Code by Bassing et al. (2020)
#-------------------------------------------------------------------------------
  #  This model tests for the effect of study area on the probability a wolf group 
  #  in central Idaho had a long-distance immigrant in it and estiamtes the mean 
  #  probabilty of a group containing a long-distance immigrant in the east and 
  #  west study areas in central Idaho.
  
  #  sarea: categorical variable for east study area (0) and west study area (1)
  
  #  Dimensions are: i = unique observation per group per year
  #                  sa = one of two study areas
  #                  p = number of groups observed
#-------------------------------------------------------------------------------
  #  Load packages and data

  require(R2jags)
  require(mcmcplots)            
  require(dplyr)
  
  load.module("glm")
  
  load("./Input/ID_imms.RData")
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  sink("./Idaho_LDI_Region.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.35)T(-10, 10)

    #  Prior for harvest
    B1.Region ~ dnorm(0, 0.35)T(-10, 10)

  #  Likelihood

    #  Testing for effect of study area on Pr(LDI joins a group)
    #  Data are binomially distributed based on immigrant probability and number
    #  of wolves sampled per group per year
  
    for(i in 1:nrow){
        logit(ldi[i]) <- B0 + B1.Region * sarea[i]
        y[i] ~ dbin(ldi[i], packd[i])  # packd group & year specific
    } #i    

  #  Derived parameters

    #  Probability per group in the east vs west study area that each group will 
    #  contain LD immigrants 
  
    for(sa in 1:2) {
      for(p in 1:npack) {
        mean.imm[sa, p] <- 1/(1 + exp(-(B0 + B1.Region * (sa - 1))))
      } #p
    } #sa
  
    #  Mean probabilty across all groups that a group will contain LD immigrants 
    #  in the east vs west study area
  
    for(sa in 1:2) {
      sa.imm[sa] <- mean(mean.imm[sa,])
    }
    
  }
  ", fill=TRUE)
  sink()
#-------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  #  Sample size for dbin based on number of sampled individuals per group per year
  packd <- ID_imms %>%
      group_by(Year, Putative, Region) %>%
      summarise(
        Nsampled = length(unique(wolf_id)),
        Nimm = sum(LDI == 1)
      ) %>%
    ungroup()
    
  nrows <- nrow(packd)
  
  win.data <- list(
    "y" = packd$Nimm, 
    "sarea" = packd$Region - 1, 
    "packd" = as.numeric(packd$Nsampled),
    "npack" = length(unique(packd$Putative)), 
    "nrow" = nrows
  )
  
  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.Region = runif(1,-1,1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.Region", "sa.imm")
  
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
    "./Idaho_LDI_Region.txt", 
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Look and Save
  print(out, dig=2)
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_LDI_Region.txt", sep = "\t")
  # Idaho_LDI_Region_output <- out
  # save(Idaho_LDI_Region_output, file = "./Output/Idaho_LDI_Region.RData")
  
  