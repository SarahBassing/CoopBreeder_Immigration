#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Logistic Regression: Pr(Long-Distance Immigrant) ~ Study Region
  #  Code by Bassing et al.
#-------------------------------------------------------------------------------
  #  This model tests for regional differences in the probability a wolf group  
  #  has a long-distance immigrant in it based on study area (Central Idaho vs. 
  #  Southwest Alberta). 
  
  #  sarea: categorical variable for Central Idaho study area (0) and Southwest
  #  Alberta study area (1)
  
  #  Dimensions are: i = unique observation per group per year
  #                  sa = one of two study areas
  #                  p = number of groups observed
#-------------------------------------------------------------------------------
#  Load packages and data

  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  setwd("C:/Sarah B/Genetic Analyses/Hypothesis Tests/Regressions/JAGS/Final_Models_for_Publication")
  load("./Input/Harv_imms.RData")
################################################################################
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  #sink("./IDvsAB_LDI_Region.txt")
  sink("G:/My Drive/1_Repositories/CoopBreeder_Immigration/IDvsAB_LDI_Region.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
    B0 ~ dnorm(0, 0.35)T(-10, 10)

    #  Prior for harvest
    B1.Region ~ dnorm(0, 0.35)T(-10, 10)

  #  Likelihood

    #  Testing for effect of region on Pr(LDI joins a pack)
    #  Data are binomially distributed based on immigrant probability and number of
    #  wolves sampled per group per year
  
    for(i in 1:nrow){
        logit(ldi[i]) <- B0 + B1.Region * sarea[i]
        y[i] ~ dbin(ldi[i], packd[i])  # packd group & year specific
    }#i    

  #  Derived parameters

    #  Probability per group in Central Idaho vs Southwest Alberta that each group  
    #  will contain LD immigrants
  
    for(sa in 1:2) {
      for(p in 1:npack) {
        mean.imm[sa, p] <- 1/(1 + exp(-(B0 + B1.Region * (sa - 1))))
      }
    }
  
    #  Mean probabilty across all groups that a group will contain LD immigrants 
    #  in Central Idaho vs Southwest Alberta
    
    for(sa in 1:2) {
      sa.imm[sa] <- mean(mean.imm[sa,])
    }
    
  }
  ", fill=TRUE)
  sink()
################################################################################
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  #  Combine data from East and West study areas in Central Idaho into single  
  #  annual mean for the Idaho region
  packd <- Harv_imms %>%
      group_by(Year, Putative, Region) %>%
      summarise(
        Nsampled = length(unique(wolf_id)),
        Nimm = sum(LDI == 1)
      ) %>%
      mutate(
        StudyArea = as.numeric(Region),
        StudyArea = ifelse(StudyArea == 2, 1, StudyArea),
        StudyArea = ifelse(StudyArea == 3, 2, StudyArea)
      ) %>%
    ungroup() %>%
    dplyr::select(-Region)

  nrows <- nrow(packd)
  
  win.data <- list(
    "y" = packd$Nimm, 
    "sarea" = packd$StudyArea - 1, 
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
    #"./IDvsAB_LDI_Region.txt", 
    "G:/My Drive/1_Repositories/CoopBreeder_Immigration/IDvsAB_LDI_Region.txt",
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Look and Save

  print(out, dig=2)
  
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/IDvsAB_LDI_Region.txt", sep = "\t")
  # IDvsAB_LDI_Region_output <- out
  # save(IDvsAB_LDI_Region_output, file = "./Output/IDvsAB_LDI_Region.RData")
  
  