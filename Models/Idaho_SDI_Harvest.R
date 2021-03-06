#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Logistic Regression: Pr(Short-Distance Immigrant) ~ Harvest
  #  Code by Bassing et al. (2020)
#-------------------------------------------------------------------------------
  #  This model tests for the effect of harvest on the probability a wolf group 
  #  in central Idaho had a short-distance immigrant in it and estiamtes the mean 
  #  probabilty of a group containing a short-distance immigrant when harvest did 
  #  and did not occur.
  
  #  HARV: categorical variable for years when harvest did not occur (0) and 
  #  did occur (1)
  
  #  Dimensions are: i = unique observation per group per year
  #                  h = harvest did or did not occur
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
  
  #sink("./Idaho_SDI_Harvest.txt")
  sink("G:/My Drive/1_Repositories/CoopBreeder_Immigration/Idaho_SDI_Harvest.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.35)T(-10, 10)

    #  Prior for harvest
    B1.HARV ~ dnorm(0, 0.35)T(-10, 10)

  #  Likelihood

    #  Testing for effect of harvest on Pr(SDI joins a group).
    #  Data are binomially distributed based on immigrant probability and number of
    #  wolves sampled per group per year
  
    for(i in 1:nrow){
        logit(ldi[i]) <- B0 + B1.HARV * HARV[i]
        y[i] ~ dbin(ldi[i], packd[i])  # packd group & year specific
    }#i    

  #  Derived parameters

    #  Probability per group that it will contain SD immigrants when harvest
    #  does & does not occur
  
    for(h in 1:2){
      for(p in 1:npack){
        mean.imm[h, p] <- 1/(1 + exp(-(B0 + B1.HARV*(h-1))))
      }
    }
    
    #  Mean probabilty across all groups that a group will contain SD immigrants 
    #  when harvest & does not occur
  
    for(h in 1:2){
      harv.imm[h] <- mean(mean.imm[h,])
    }
    
  }
  ", fill=TRUE)
  sink()
#----------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  #  Sample size for dbin based on number of sampled individuals per group per year
  packd <- ID_imms %>%
      group_by(Year, Putative, Harvest) %>%
      summarise(
        Nsampled = length(unique(wolf_id)),
        Nimm = sum(SDI == 1)
      ) %>%
    ungroup()
    
  nrows <- nrow(packd)
  
  win.data <- list(
    "y" = packd$Nimm, 
    "HARV" = packd$Harvest, 
    "packd" = as.numeric(packd$Nsampled),
    "npack" = length(unique(packd$Putative)), 
    "nrow" = nrows
  )
  
  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.HARV = runif(1,-1,1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.HARV", "harv.imm")
  
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
    "./Idaho_SDI_harvest.txt", 
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Look and Save
  print(out, dig=2)
  mcmcplot(out)  
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_SDI_Harvest.txt", sep = "\t")
  # Idaho_SDI_Harvest_output <- out
  # save(Idaho_SDI_Harvest_output, file = "./Output/Idaho_SDI_Harvest.RData")
  

  