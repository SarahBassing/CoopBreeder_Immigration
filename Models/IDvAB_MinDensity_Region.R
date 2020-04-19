#-------------------------------------------------------------------------------
  #  Cooperative Breeders, Harvest, & Immigration
  #  Linear Regression: Minimum Wolf Density ~ Study Region
  #  Code by Bassing et al. (2020)
#-------------------------------------------------------------------------------
  #  This model tests for regional differences in annual minimum density of
  #  wolves in Central Idaho after harvest was initiated compared to Southwest 
  #  Alberta where longterm harvest occurred.
  
  #  sarea: categorical variable for Central Idaho study area (0) and Southwest 
  #  Alberta study area (1)
  #  Remove 2013 Alberta data: catastrophic flooding limited sampling that year

  
  #  Dimensions: i = unique observation per group per year
  #              sa = one of two study areas
#-------------------------------------------------------------------------------
#  Load packages and data

  require(R2jags)
  require(mcmcplots)
  require(dplyr)
  
  load.module("glm")
  
  load("./Input/Harv_Gdensity.RData")
  Harv_MinDensity <- Harv_GDensity[-7,]
#-------------------------------------------------------------------------------
  ####  Model  ####
  
  #  Specify model in BUGS language
  
  sink("IDvAB_MinDensity_Region.txt")
  cat("
  model {

  #  Specify priors
  
    #  Prior for intercept
      B0 ~ dnorm(0, 0.001)

    #  Prior for study areas (Idaho & Alberta Regions)
      B1.Region ~ dnorm(0, 0.001)


    #  Priors for error on linear model
    tau ~ dgamma(0.001, 0.001)
    sigma <- 1/sqrt(tau)

  #  Likelihood

    #  Testing for effect of region on minimum density of wolves (Idaho & Alberta)
    #  Data are normally distributed based on density and variance parameter
  
    for(i in 1:nrow){
        dens[i] <- B0 + B1.Region * sarea[i]
        y[i] ~ dnorm(dens[i], tau)
    }#i    

  #  Derived parameters

    #  Mean minimum density of wolves in each region (Idaho & Alberta)
  
    for(sa in 1:2){
        mean.den[sa] <- B0 + B1.Region * (sa - 1)
    }
  
  }
  ", fill=TRUE)
  sink()
#-------------------------------------------------------------------------------
  #### JAGS input ####  
  
  #  Bundle data, specify parameters & MCMC settings, and run JAGS
  
  #  Define & Bundle Data
  #  Combine data from East and West study areas in Central Idaho into single  
  #  annual mean for the Idaho region
  MinDensity <- Harv_MinDensity %>%
    mutate(
        StudyArea = as.numeric(Region),
        StudyArea = ifelse(StudyArea == 2, 1, StudyArea),
        StudyArea = ifelse(StudyArea == 3, 2, StudyArea)
      ) %>%
      group_by(Year, StudyArea) %>%
      summarise(
        MeanDensity = mean(Density)
      ) %>%
    ungroup()
  Harv_MinDensity <- as.data.frame(MinDensity)
  
  # Define & Bundle Data
  nrows <- nrow(Harv_MinDensity)
  
  win.data <- list(
    "sarea" = Harv_GDensity[,2] -1,
    "y" = Harv_GDensity[,3],
    "nrow" = nrows
  )
  
  inits <- function(){
    list(
      B0 = runif(1, -1, 1),
      B1.Region = runif(1, -1, 1)
      )
  }
  
  # Parameters to keep track of and report
  params <- c("B0", "B1.Region", "tau", "sigma", "mean.den")
  
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
    "IDvAB_MinDensity_Region.txt", 
    n.chains=nc, 
    n.thin=nt, 
    n.iter=ni, 
    n.burnin=nb
  )
  
  #  Look and Save
  print(out, dig=2)
  mcmcplot(out)
  
  # write.table(out$BUGS$summary, file = "./Output/IDvAB_MinDensity_Region.txt", sep = "\t")
  # IDvAB_MinDensity_Region_output <- out
  # save(IDvAB_MinDensity_Region_output, file = "./Output/IDvAB_MinDensity_Region.RData")
  