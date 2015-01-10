setwd("C:/Users/rushingc/Desktop/Backup/WOTH_retro")
#setwd("~/Desktop/Clark Saved/Projects/3_Working on/WOTH_retro")
library(lattice)
library(coda)
library(R2jags)
library(crushingr)

# Model


#Data----
load("woth_retro_data.RData")

br_forest <- br_forest*10
# MLE tests for univariate significance
for(i in 1:17){
  fit1 <- glm(pop_abun[,i] ~ year + nb1_evi.t, family="quasipoisson")
  fit2 <- glm(pop_abun[,i] ~ year + nb2_evi.t, family="quasipoisson")
  fit3 <- glm(pop_abun[,i] ~ year + nb3_evi.t, family="quasipoisson")
  fit4 <- glm(pop_abun[,i] ~ year + nb4_evi.t, family="quasipoisson")
  fit5 <- glm(pop_abun[,i] ~ year + nb5_evi.t, family="quasipoisson")
  fit6 <- glm(pop_abun[,i] ~ year + nb1_evi.tm1, family="quasipoisson")
  fit7 <- glm(pop_abun[,i] ~ year + nb2_evi.tm1, family="quasipoisson")
  fit8 <- glm(pop_abun[,i] ~ year + nb3_evi.tm1, family="quasipoisson")
  fit9 <- glm(pop_abun[,i] ~ year + nb4_evi.tm1, family="quasipoisson")
  fit10 <- glm(pop_abun[,i] ~ year + nb5_evi.tm1, family="quasipoisson")
  fit11 <- glm(pop_abun[,i] ~ year + nb1_forest.tm1, family="quasipoisson")
  fit12 <- glm(pop_abun[,i] ~ year + nb2_forest.tm1, family="quasipoisson")
  fit13 <- glm(pop_abun[,i] ~ year + nb3_forest.tm1, family="quasipoisson")
  fit14 <- glm(pop_abun[,i] ~ year + nb4_forest.tm1, family="quasipoisson")
  fit15 <- glm(pop_abun[,i] ~ year + nb5_forest.tm1, family="quasipoisson")
  fit16 <- glm(pop_abun[,i] ~ year + br_forest[,i], family="quasipoisson")
  fit17 <- glm(pop_abun[,i] ~ year + br_clim[,i], family="quasipoisson")

betas <- c(coef(fit1)[3],coef(fit2)[3],coef(fit3)[3],coef(fit4)[3],coef(fit5)[3],coef(fit6)[3],coef(fit7)[3],
           coef(fit8)[3],coef(fit9)[3],coef(fit10)[3],coef(fit11)[3],coef(fit12)[3],coef(fit13)[3],
           coef(fit14)[3],coef(fit15)[3],coef(fit16)[3],coef(fit17)[3])
pop.lcl <- c(confint(fit1,level = .8)[3,1],confint(fit2,level = .8)[3,1],confint(fit3,level = .8)[3,1],
         confint(fit4,level = .8)[3,1],confint(fit5,level = .8)[3,1],confint(fit6,level = .8)[3,1],
         confint(fit7,level = .8)[3,1],confint(fit8,level = .8)[3,1],confint(fit9,level = .8)[3,1],
         confint(fit10,level = .8)[3,1],confint(fit11,level = .8)[3,1],confint(fit12,level = .8)[3,1],
         confint(fit13,level = .8)[3,1],confint(fit14,level = .8)[3,1],confint(fit15,level = .8)[3,1],
         confint(fit16,level = .8)[3,1],confint(fit17,level = .8)[3,1])
pop.ucl <- c(confint(fit1,level = .8)[3,2],confint(fit2,level = .8)[3,2],confint(fit3,level = .8)[3,2],
         confint(fit4,level = .8)[3,2],confint(fit5,level = .8)[3,2],confint(fit6,level = .8)[3,2],
         confint(fit7,level = .8)[3,2],confint(fit8,level = .8)[3,2],confint(fit9,level = .8)[3,2],
         confint(fit10,level = .8)[3,2],confint(fit11,level = .8)[3,2],confint(fit12,level = .8)[3,2],
         confint(fit13,level = .8)[3,2],confint(fit14,level = .8)[3,2],confint(fit15,level = .8)[3,2],
         confint(fit16,level = .8)[3,2],confint(fit17,level = .8)[3,2])
  temp.df <- data.frame(pop=rep(i,17), predictor = seq(1:17), est = betas, lcl=pop.lcl, ucl=pop.ucl)
  ifelse(i==1, pop.df <- temp.df, pop.df <- rbind(pop.df,temp.df))
}

# Set indicator to zero for predictors w/ 80% CI that overlap 0
  pop.df$impt <- ifelse(pop.df$est > 0 & pop.df$lcl > 0, 1,
                        ifelse(pop.df$est < 0 & pop.df$ucl < 0, 1, 0))

# Set indicator to zero based on MC
  pop.df$impt[pop.df$pop%in%c(1,2,4,12,13)&pop.df$predictor%in%c(5,10,15)] <- 0 # NE
  pop.df$impt[pop.df$pop%in%c(3,7,14,16,17)&pop.df$predictor%in%c(1,5,6,10,11,15)] <- 0 # SE
  pop.df$impt[pop.df$pop%in%c(8,9,10)&pop.df$predictor%in%c(1,6,11)] <- 0 # SW
  pop.df$impt[pop.df$pop%in%c(5,6,11,15)&pop.df$predictor%in%c(1,2,6,7,11,12)] <- 0 # NW

# Set indicator to zero for correlated predictors
  pop.df$est[pop.df$pop==5][c(13,15,16)]
  pop.df$impt[pop.df$pop==1][11] <- 0
  pop.df$impt[pop.df$pop==3][12] <- 0
  pop.df$impt[pop.df$pop==7][12] <- 0
  pop.df$impt[pop.df$pop==9][12] <- 0
  pop.df$impt[pop.df$pop==13][c(11,12)] <- 0
  pop.df$impt[pop.df$pop==14][12] <- 0
  pop.df$impt[pop.df$pop==16][12] <- 0
  pop.df$impt[pop.df$pop==11][15] <- 0 # Breeding forest correlated with NB5 forest
  pop.df$impt[pop.df$pop==5][15] <- 0  # Breeding forest correlated with NB5 forest

  #write.csv(pop.df, "pop_df.csv")
  pop.df <- read.csv("pop_df.csv")
  pop.ind <- matrix(pop.df$impt, nrow=17,byrow=FALSE)
apply(matrix(pop.df$impt, nrow=17,byrow=FALSE),1,sum)


# Single Parameter Models ----
sink("single_param.jags")
cat("
    model {
    
    ######################## LIKELIHOOD #########################
    
    for (k in 1:nroutes){
      for (t in 1:nyears){
        noise[t,k] ~ dnorm(0, tau.noise)
        count[t,k] ~ dpois(lambda[t,k])
        log(lambda[t,k]) <- trend[pop[k]] * year[t]   # Trend
          + beta[pop[k]]      * param[t]              # Parameter, year t
          + beta.nov          * first[t,k]            # Novice observer effect
          + alpha[pop[k]]                             # Population intercept             
          + obs[newobs[t,k]]                          # Observer effect
          + route[k]                                  # Route effect
          + noise[t,k]                                # Noise
    
    ### GOF statistics ###
        pres[t,k] <- (count[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
        count.new[t,k] ~ dpois(lambda[t,k])
        pres.new[t,k] <- (count.new[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
      }  # End t loop
    } # End k loop
    
    ####################### PRIORS ###############################
    
    ### Priors on population intercepts, trends, & covariate effects
    for(i in 1:npops){
      alpha[i]   ~ dnorm(0, 0.001)
      trend[i]   ~ dnorm(0, 0.001)
      theta[i]   ~ dnorm(0, 0.001)
    } # End j loop
    
    ### Priors on novice observer effect, noise sd, & pop intercept sd
    beta.nov ~ dnorm(0,0.001)
    tau.noise  ~ dgamma(0.001, 0.001)
    sd.noise  <-  1/pow(tau.noise,0.5)
    tau.alpha <- 1/(sd.alpha*sd.alpha)
    sd.alpha  ~ dunif(0, 10)
    
    
    ### Beta coefficients
    for(j in 1:npops) {
      g[j] ~ dbern(0.5)
      beta[j] <- theta[j]*g[j]
    }
    
    
    ### Priors on observer effects
    for (o in 1:nobs){
      obs[o] ~ dnorm(0, tau.obs)T(-5,5)
    }
    tau.obs  ~ dgamma(0.001,0.001)
    sd.obs  <-  1/pow(tau.obs, 0.5)
    
    ### Priors on route effects
    for (r in 1:nroutes){
      route[r] ~ dnorm(0, tau.rte)T(-5,5)
    }
    tau.rte  ~ dgamma(0.001,0.001)
    sd.rte  <-  1/pow(tau.rte,0.5)

  } # End model
    ",fill = TRUE)
sink()

### Data ----

# Initial values
inits <- function()list(alpha=runif(1,-1,1), beta.nov=runif(1,-1,1), trend=runif(1,-1,1), 
                        obs = runif(nobs.pop,-1,1), route = runif(ncol(count.pop), -1,1), tau.noise=10,
                        sd.alpha=1, 
                        tau.obs=1, tau.rte=10) 


# Parameters monitored
parameters.gvs <- c("beta", "g")

# MCMC settings
ni <- 30000
nt <- 3
nb <- 15000
nc <- 2

for(i in 1:17){
  cat("Covariate", i)

  parameters <- list(nb1_evi.tm1, nb2_evi.tm1, nb3_evi.tm1, nb4_evi.tm1, nb5_evi.tm1,
                     nb1_evi.t, nb2_evi.t, nb3_evi.t, nb4_evi.t, nb5_evi.t,
                     nb1_forest.tm1, nb2_forest.tm1, nb3_forest.tm1, nb4_forest.tm1, nb5_forest.tm1,
                     br_forest[,i], br_clim[,i])
  
  cov.data <- list(count = count, npops = npops, nroutes=nroutes, nyears=nyears, pop=pop,
                   first = first, year = year, nobs= nobs, newobs = newobs, 
                   param = parameters[[i]])
  
  
  ### Call Jags
  mod.name <- paste("fit_cov", i, sep="")
  mod.file <- paste("fit_cov", i, ".csv", sep="")
  mod.name <- jags(cov.data, inits=NULL,parameters.gvs, "single_param.jags", 
                   n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                   working.directory = getwd())
  
  cov_mcmc <- as.mcmc(mod.name$BUGSoutput$sims.matrix)
  write.csv(cov_mcmc,file=mod.file)
  
  print(mod.name, digits = 3)
}



# Single Population Models ----
sink("pop_model.jags")
cat("
    model {
    
    ######################## LIKELIHOOD #########################
    
    for (k in 1:nroutes){
      for (t in 1:nyears){
        noise[t,k] ~ dnorm(0, tau.noise)
        count[t,k] ~ dpois(lambda[t,k])
        log(lambda[t,k]) <- trend * year[t]   # Trend
            + beta[1]   * nb1_evi.t[t]        # NB reg 1 climate, year t
            + beta[2]   * nb2_evi.t[t]        # NB reg 2 climate, year t
            + beta[3]   * nb3_evi.t[t]        # NB reg 3 climate, year t
            + beta[4]   * nb4_evi.t[t]        # NB reg 4 climate, year t
            + beta[5]   * nb5_evi.t[t]        # NB reg 5 climate, year t
            + beta[6]   * nb1_evi.tm1[t]      # NB reg 1 climate, year t-1
            + beta[7]   * nb2_evi.tm1[t]      # NB reg 2 climate, year t-1
            + beta[8]   * nb3_evi.tm1[t]      # NB reg 3 climate, year t-1
            + beta[9]   * nb4_evi.tm1[t]      # NB reg 4 climate, year t-1
            + beta[10]  * nb5_evi.tm1[t]      # NB reg 5 climate, year t-1
            + beta[11]  * nb1_forest.tm1[t]   # NB1 forest, year t - 1
            + beta[12]  * nb2_forest.tm1[t]   # NB2 forest, year t - 1
            + beta[13]  * nb3_forest.tm1[t]   # NB3 forest, year t - 1
            + beta[14]  * nb4_forest.tm1[t]   # NB4 forest, year t - 1
            + beta[15]  * nb5_forest.tm1[t]   # NB5 forest, year t - 1
            + beta[16]  * br_forest[t]        # Breeding forest, year t - 1
            + beta[17]  * br_clim[t]          # Breeding climate, year t - 1
            + beta.nov  * first[t,k]          # Novice observer effect
            + alpha                           # Population intercept             
            + obs[newobs[t,k]]                              # Observer effect
            + route[k]                                      # Route effect
            + noise[t,k]                                    # Noise
            
    ### GOF statistics ###
        pres[t,k] <- (count[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
        count.new[t,k] ~ dpois(lambda[t,k])
        pres.new[t,k] <- (count.new[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
      }  # End t loop
    } # End k loop
    
    ####################### PRIORS ###############################
    
    ### Priors on population intercepts, trends, & covariate effects
    alpha   ~ dnorm(0, 0.001)
    trend   ~ dnorm(0, 0.001)

    for (j in 1:nvars){
      theta[j] ~ dnorm(0, 0.001)
    } # End j loop
    
    ### Priors on novice observer effect, noise sd, & pop intercept sd
    beta.nov ~ dnorm(0,0.001)
    tau.noise  ~ dgamma(0.001, 0.001)
    sd.noise  <-  1/pow(tau.noise,0.5)
    tau.alpha <- 1/(sd.alpha*sd.alpha)
    sd.alpha  ~ dunif(0, 10)
    
    
    ### Beta coefficients
    for(j in 1:nvars) {
      g[j] ~ dbern(0.5*pop.ind[j])
      beta[j] <- theta[j]*g[j]
    }

    
    ### Priors on observer effects
    for (o in 1:nobs){
      obs[o] ~ dnorm(0, tau.obs)T(-5,5)
    }
    tau.obs  ~ dgamma(0.001,0.001)
    sd.obs  <-  1/pow(tau.obs, 0.5)
    
    ### Priors on route effects
    for (r in 1:nroutes){
      route[r] ~ dnorm(0, tau.rte)T(-5,5)
    }
    tau.rte  ~ dgamma(0.001,0.001)
    sd.rte  <-  1/pow(tau.rte,0.5)
    
    ################ DERIVED PARAMETERS ######################
    
    fit <- sum(pres[,])
    fit.new <- sum(pres.new[,])
    resid.tot <- sum(resid[,])
    
    for(t in 1:nyears){
      meanN[t] <- mean(lambda[t,])
    }
    
    B.tot <- (meanN[nyears]/meanN[1])^(1/nyears)
    trend.tot <- 100*(B.tot-1)

    } # End model
    ",fill = TRUE)
sink()

### Data ----

# Initial values
inits <- function()list(alpha=runif(1,-1,1), beta.nov=runif(1,-1,1), trend=runif(1,-1,1), 
                        obs = runif(nobs.pop,-1,1), route = runif(ncol(count.pop), -1,1), tau.noise=10,
                        sd.alpha=1, 
                        tau.obs=1, tau.rte=10) 


# Parameters monitored
parameters.gvs <- c("sd.alpha", "beta.nov", "beta", "g", "sd.obs", "sd.noise", "fit", 
                    "fit.new", "sd.rte", "trend.tot", "meanN", "resid.tot")

# MCMC settings
ni <- 40000
nt <- 5
nb <- 10000
nc <- 2

for(i in 1:17){
  cat("Population", i)
  num.id <- data.frame(obs = unique(c(newobs[,which(pop==i)])), 
                       num = seq(1:length(unique(c(newobs[,which(pop==i)])))))
  obs.id <- data.frame(obs = c(newobs[,which(pop==i)]))
  for(j in 1:nrow(num.id)){
    obs.id$id[obs.id$obs %in% num.id$obs[j]] <- num.id$num[j]
  }
  count.pop <- count[,which(pop==i)]
  first.pop <- first[,which(pop==i)]
  obs.pop <- matrix(obs.id$id, nrow=12, byrow=FALSE)
  br_forest.pop <- br_forest[,i]*10
  br_clim.pop <- br_clim[,i]
  nobs.pop <- length(unique(c(newobs[,which(pop==i)])))

  pop.data <- list(count=count.pop, nvars = 17, nroutes=ncol(count.pop), nyears=nyears, 
                    first=first.pop, year = year,
                    nobs= nobs.pop, newobs = obs.pop, 
                    nb1_evi.tm1=nb1_evi.tm1, nb2_evi.tm1=nb2_evi.tm1, 
                    nb3_evi.tm1=nb3_evi.tm1, nb4_evi.tm1=nb4_evi.tm1,nb5_evi.tm1=nb5_evi.tm1,
                    nb1_evi.t=nb1_evi.t, nb2_evi.t=nb2_evi.t, nb3_evi.t=nb3_evi.t, 
                    nb4_evi.t=nb4_evi.t,nb5_evi.t=nb5_evi.t,
                    nb1_forest.tm1=nb1_forest.tm1, nb2_forest.tm1=nb2_forest.tm1,
                    nb3_forest.tm1=nb3_forest.tm1, nb4_forest.tm1=nb4_forest.tm1,nb5_forest.tm1=nb5_forest.tm1,
                    br_forest=br_forest.pop, br_clim=br_clim.pop,
                    pop.ind=pop.ind[,i])


### Call Jags
mod.name <- paste("fit_pop", i, sep="")
mod.file <- paste("fit2_pop", i, ".csv", sep="")
mod.name <- jags(pop.data, inits=NULL,parameters.gvs, "pop_model.jags", 
                         n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                         working.directory = getwd())

pop_mcmc <- as.mcmc(mod.name$BUGSoutput$sims.matrix)
write.csv(pop_mcmc,file=mod.file)

print(mod.name, digits = 3)
}


index <- rep(NA, 17)
for (j in 1:17) { index[j]  <-  2^(j-1) }

pop.fit <- rep(NA, 17)
resid <- rep(NA,17)
abun <- rep(NA,17)
trend <- rep(NA,17)
pop.abun <- data.frame(pop = rep(1:17, each = 12),
                       year = rep(1:12, 17),
                       abun = rep(NA, 17*12))
for(j in 1:17){
  cat("Population", j)
  mod.file <- paste("fit2_pop", j, ".csv", sep="")
  pop_mcmc <- read.csv(mod.file)
  abun.temp <- colMeans(pop_mcmc[,40:51])
  ifelse(j==1, abun1 <- abun.temp, abun1 <- c(abun1,abun.temp))
  #resid[j] <- mean(pop_mcmc$fit)
#   trend[j] <- mean(pop_mcmc$trend.tot)
  abun[j] <- mean(colMeans(pop_mcmc[,40:51]))
#   mdl  <-  apply(pop_mcmc, 1, function(x){sum(x[23:39]* index)})
#   mdl.freq <- summary(as.factor(mdl))
#   mdl.temp <- data.frame(pop = rep(j, length(mdl.freq)),
#                          model = names(mdl.freq),
#                          freq = mdl.freq/nrow(pop_mcmc))
#   mdl.temp <- mdl.temp[with(mdl.temp,order(-freq)),]
#   ifelse(j==1, mdl.df <- mdl.temp, mdl.df <- rbind(mdl.df, mdl.temp))
#   fit <- pop_mcmc$fit
#   fit.new <- pop_mcmc$fit.new
#   bp <- round(mean(fit.new > fit),digits=2)
#   plot(fit, fit.new, main = paste("Population", j, sep = " "), xlab = 
#          "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE,
#        xlim=c(-175,175), ylim=c(-150,150))
#   abline(0, 1, lwd = 2, col = "black")
#   text(x=140,y=-115,substitute(P[B]==bp, list(bp=bp)),cex=0.8)
#   pop.fit[j] <- bp
#   beta.temp <- rep(NA,17)
#   lci.temp <- rep(NA,17)
#   uci.temp <- rep(NA,17)
#   for(i in 2:18){
#     beta.temp[i-1] <- round(mean(pop_mcmc[pop_mcmc[,i+21]==1,i]), digits=3)
#     lci.temp[i-1] <- round(quantile(pop_mcmc[pop_mcmc[,i+21]==1,i],probs = 0.025), digits=3)
#     uci.temp[i-1] <- round(quantile(pop_mcmc[pop_mcmc[,i+21]==1,i],probs = 0.975), digits=3)
#   }
#   g.temp <- round(colMeans(pop_mcmc[,23:39]), digits=3)
#   ifelse(j==1, beta <- beta.temp, beta <- c(beta,beta.temp))
#   ifelse(j==1, lci <- lci.temp, lci <- c(lci,lci.temp))
#   ifelse(j==1, uci <- uci.temp, uci <- c(uci,uci.temp))
#   ifelse(j==1, g <- g.temp, g <- c(g, g.temp))
}

pop.abun$abun <- abun1
pop.fit
pop.df$g <- g
pop.df$beta <- beta
pop.df$lci <- lci
pop.df$uci <- uci
pop.df$trend <- rep(trend, each=17)
pop.df$abun <- rep(abun, each=17)


mdl.df2 <- mdl.df[mdl.df$freq>0.05,]
write.csv(mdl.df2, "mdl_df.csv")

write.csv(pop.df, "pop_df.csv")
ggplot(pop.df, aes(x=predictor, y=pop, fill=g))+geom_raster()+
  scale_fill_continuous(low="white", high="black")+
  scale_x_continuous(breaks=c(1:17))+
  scale_y_continuous(breaks=c(1:17))+
  geom_vline(xintercept=c(5.5,10.5,15.5))

# Single Population Models w/o covariates ----
sink("pop_model_base.jags")
cat("
    model {
    
    ######################## LIKELIHOOD #########################
    
    for (k in 1:nroutes){
    for (t in 1:nyears){
    noise[t,k] ~ dnorm(0, tau.noise)
    count[t,k] ~ dpois(lambda[t,k])
    log(lambda[t,k]) <- trend * year[t]   # Trend
    + beta.nov  * first[t,k]          # Novice observer effect
    + alpha                           # Population intercept             
    + obs[newobs[t,k]]                              # Observer effect
    + route[k]                                      # Route effect
    + noise[t,k]                                    # Noise
    
    ### GOF statistics ###
    SS.resid[t,k] <- pow((count[t,k] - lambda[t,k]),2)
    pres[t,k] <- (count[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
    count.new[t,k] ~ dpois(lambda[t,k])
    pres.new[t,k] <- (count.new[t,k] - lambda[t,k]) / sqrt(lambda[t,k])
    }  # End t loop
    } # End k loop
    
    ####################### PRIORS ###############################
    
    ### Priors on population intercepts, trends, & covariate effects
    alpha   ~ dnorm(0, 0.001)
    trend   ~ dnorm(0, 0.001)
    beta.nov ~ dnorm(0,0.001)
    tau.noise  ~ dgamma(0.001, 0.001)
    sd.noise  <-  1/pow(tau.noise,0.5)
    tau.alpha <- 1/(sd.alpha*sd.alpha)
    sd.alpha  ~ dunif(0, 10)
    
    ### Priors on observer effects
    for (o in 1:nobs){
    obs[o] ~ dnorm(0, tau.obs)T(-5,5)
    }
    tau.obs  ~ dgamma(0.001,0.001)
    sd.obs  <-  1/pow(tau.obs, 0.5)
    
    ### Priors on route effects
    for (r in 1:nroutes){
    route[r] ~ dnorm(0, tau.rte)T(-5,5)
    }
    tau.rte  ~ dgamma(0.001,0.001)
    sd.rte  <-  1/pow(tau.rte,0.5)
    
    ################ DERIVED PARAMETERS ######################
    
    fit <- sum(pres[,])
    fit.new <- sum(pres.new[,])
    
    for(t in 1:nyears){
    meanN[t] <- mean(lambda[t,])
    }
    
    B.tot <- (meanN[nyears]/meanN[1])^(1/nyears)
    trend.tot <- 100*(B.tot-1)
    
    } # End model
    ",fill = TRUE)
sink()

### Data ----

# Initial values
inits <- function()list(alpha=runif(1,-1,1), beta.nov=runif(1,-1,1), trend=runif(1,-1,1), 
                        obs = runif(nobs.pop,-1,1), route = runif(ncol(count.pop), -1,1), tau.noise=10,
                        sd.alpha=1, 
                        tau.obs=1, tau.rte=10) 


# Parameters monitored
parameters.gvs <- c("sd.alpha", "beta.nov", "sd.obs", "sd.noise", "fit", 
                    "fit.new", "sd.rte", "trend.tot", "meanN", "resid.tot")

# MCMC settings
ni <- 30000
nt <- 5
nb <- 10000
nc <- 2

for(i in 1:17){
  cat("Population", i)
  num.id <- data.frame(obs = unique(c(newobs[,which(pop==i)])), 
                       num = seq(1:length(unique(c(newobs[,which(pop==i)])))))
  obs.id <- data.frame(obs = c(newobs[,which(pop==i)]))
  for(j in 1:nrow(num.id)){
    obs.id$id[obs.id$obs %in% num.id$obs[j]] <- num.id$num[j]
  }
  count.pop <- count[,which(pop==i)]
  first.pop <- first[,which(pop==i)]
  obs.pop <- matrix(obs.id$id, nrow=12, byrow=FALSE)
  br_forest.pop <- br_forest[,i]*10
  br_clim.pop <- br_clim[,i]
  nobs.pop <- length(unique(c(newobs[,which(pop==i)])))
  
  pop.data <- list(count=count.pop, nroutes=ncol(count.pop), nyears=nyears, 
                   first=first.pop, year = year,
                   nobs= nobs.pop, newobs = obs.pop)
  
  
  ### Call Jags
  mod.name <- paste("fit_pop", i, sep="")
  mod.file <- paste("base_mod", i, ".csv", sep="")
  mod.name <- jags(pop.data, inits=NULL,parameters.gvs, "pop_model_base.jags", 
                   n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                   working.directory = getwd())
  
  pop_mcmc <- as.mcmc(mod.name$BUGSoutput$sims.matrix)
  write.csv(pop_mcmc,file=mod.file)
  
  print(mod.name, digits = 3)
}

pop.fit2 <- rep(NA, 17)
resid2 <- rep(NA,17)
for(j in 1:17){
  mod.file <- paste("base_mod", j, ".csv", sep="")
  pop_mcmc <- read.csv(mod.file)
  resid2[j] <- mean(pop_mcmc$fit)
#   fit <- pop_mcmc$fit
#   fit.new <- pop_mcmc$fit.new
#   bp <- round(mean(fit.new > fit),digits=2)
#   plot(fit, fit.new, main = paste("Population", j, "(base)", sep = " "), xlab = 
#          "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE,
#        xlim=c(-175,175), ylim=c(-150,150))
#   abline(0, 1, lwd = 2, col = "black")
#   text(x=140,y=-115,substitute(P[B]==bp, list(bp=bp)),cex=0.8)
#   pop.fit2[j] <- bp
}

fit.df <- data.frame(Population = seq(1:17),
                     Fit.annual = pop.fit,
                     Fit.base = pop.fit2)
fit.df$diff <- with(fit.df, Fit.annual/Fit.base)

resid.df <- data.frame(Population = seq(1:17),
                     Resid.annual = resid,
                     Resid.base = resid2)
resid.df$diff <- with(resid.df, (Resid.base-Resid.annual)/Resid.base)
