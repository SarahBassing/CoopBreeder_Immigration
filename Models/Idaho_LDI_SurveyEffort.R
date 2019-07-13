#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Logistic Regression: Pr(Long-Distance Immigrant) ~ Survey Effort
  #  Code by Bassing et al.
#-------------------------------------------------------------------------------
  #  This model tests for the effect of survey effort on the probability a wolf  
  #  group in central Idaho has a long-distance immigrant in it. Given there was  
  #  some variation in annual survey effort, does that variation (which could 
  #  affect probability of detecting groups) influence the estimated probability
  #  that a group contains immigrants in central Idaho?
  
  #  SVY: continuous variable representing the number of predicted rendezvous
  #  sites surveyed each year in each study area.
  #  Survey effort has been centered & scaled.
#-------------------------------------------------------------------------------
  #  Load packages and data

  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  #load("./Input/ID_imms.RData")
  load("G:/My Drive/1_Repositories/CoopBreeder_Immigration/Input/ID_imms.RData")
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  #sink("./Idaho_LDI_SurveyEffort.txt")
  sink("G:/My Drive/1_Repositories/CoopBreeder_Immigration/Idaho_LDI_SurveyEffort.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.35)T(-10, 10)

    #  Prior for harvest
    B1.SVY ~ dnorm(0, 0.35)T(-10, 10)

  #  Likelihood

    #  Testing for effect of survey effort on Pr(LDI joins a group)
    #  Data are binomially distributed based on immigrant probability and number of
    #  wolves sampled per group per year
  
    for(i in 1:nrow){
        logit(ldi[i]) <- B0 + B1.SVY * SVY[i]
        y[i] ~ dbin(ldi[i], packd[i])  # packd group & year specific
    } #i    

  #  Derived parameters

    #  Mean probabilty across all groups that a group will contain LD immigrants 
    #  given survey effort
  
    for(i in 1:nrow){
        mean.imm[i] <- 1/(1 + exp(-(B0 + B1.SVY*i)))
    } #i
    mu.imm <- mean(mean.imm[])
  
    
  }
  ", fill=TRUE)
  sink()
#-------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  #  Sample size for dbin based on number of sampled individuals per group per year
  packd <- ID_imms %>%
      group_by(Year, Putative, nSites) %>%
      summarise(
        Nsampled = length(unique(wolf_id)),
        Nimm = sum(LDI == 1)
      ) %>%
    ungroup()
    
  nrows <- nrow(packd)
  
  win.data <- list(
    "y" = packd$Nimm, 
    "SVY" = packd$nSites, 
    "packd" = as.numeric(packd$Nsampled),
    "npack" = length(unique(packd$Putative)), 
    "nrow" = nrows
  )
  
  #  The initial values for B1.SVY are important for the suvey effort model to run
  inits <- function(){
    list(
      B0 = runif(1,-1,1),
      B1.SVY = runif(1,-0.1,0.1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.SVY", "mu.imm")
  
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
    #"./Idaho_LDI_survey.txt",
    "G:/My Drive/1_Repositories/CoopBreeder_Immigration/Idaho_LDI_SurveyEffort.txt",
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Look and Save

  print(out, dig=2)
  
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/Idaho_LDI_SurveyEffort.txt", sep = "\t")
  # Idaho_LDI_SurveyEffort_output <- out
  # save(Idaho_LDI_SurveyEffort_output, file = "./Output/Idaho_LDI_SurveyEffort.RData")

  