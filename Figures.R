
rm(list=ls())
setwd("C:/Users/rushingc/Desktop/Backup/WOTH_retro")
setwd("~/Desktop/Clark Saved/Projects/3_Working on/WOTH_retro")
require(plyr)
require(crushingr)

### Forest loss figures ----
# Breeding populations
  pop_loss <- read.csv("pop_loss.csv")
  head(pop_loss)
  
  
  df <- pop_loss %>%
          group_by(groups.hopt) %>%
            mutate(cum_loss_perc=cumsum(perc_loss))

  df <- df %>%
          group_by(groups.hopt) %>%
            mutate(cum_loss=cumsum(reg_loss))
  df$year <- pop_loss$year_loss
  
  ggplot(df,aes(x=year,y=cum_loss/100000,color=as.factor(groups.hopt),label=groups.hopt)) + geom_line() +
    scale_y_continuous("Cumulative Loss (# Cells)")+
    scale_x_continuous("Year", breaks = c(2001:2012), limits=c(2001,2012))+
    geom_text(data = df[df$year ==2012&df$groups.hopt%in%c(1,6,7,8,9,14,17), ], aes(label = groups.hopt), hjust = -1,
            vjust = 0, color="black")+theme(legend.position="none")


  ggplot(df,aes(x=year,y=cum_loss_perc*100,color=as.factor(groups.hopt),label=groups.hopt)) + geom_line() +
    scale_y_continuous("Cumulative Loss (% of 2000 cover)")+
    scale_x_continuous("Year", breaks = c(2001:2012), limits=c(2001,2012))+
    geom_text(data = df[df$year ==2012&df$groups.hopt%in%c(1,8,9,10,14,17), ], aes(label = groups.hopt), hjust = -1,
              vjust = 0, color="black")+theme(legend.position="none")+
    geom_text(data = df[df$year ==2012&df$groups.hopt%in%c(7), ], aes(label = groups.hopt), vjust = 1.2,hjust=-1,
            vjust = 0, color="black")+theme(legend.position="none")


# Non-breeding populations
  nonbr_loss <- read.csv("nb_loss.csv")
  nonbr_loss <- subset(nonbr_loss,select = c(year,loss, yr_forest, 
                                                 stand_forest, region))
  names(nonbr_loss)<- c("year", "loss", "yr_forest", "perc_forest", "region")
  
  nb.df <- nonbr_loss %>%
            group_by(region) %>%
              mutate(cum_loss=cumsum(loss))

  nb.df$cum_loss_perc <- 1-nb.df$perc_forest

    
  ggplot(nb.df,aes(x=year,y=cum_loss/100000,color=as.factor(region))) + geom_line() + #geom_point()+
    scale_y_continuous("Cumulative Loss (# Cells)")+
    scale_x_continuous("Year", breaks = c(2001:2012))+    
    geom_text(data = nb.df[nb.df$year ==2012, ], aes(label = region), hjust = -.75,
              vjust = 0, color="black")+theme(legend.position="none")

  ggplot(nb.df,aes(x=year,y=cum_loss_perc*100,color=as.factor(region))) + geom_line() + #geom_point()+
    scale_y_continuous("Cumulative Loss (% 2000 forest cover)")+
    scale_x_continuous("Year", breaks = c(2001:2012))+    
    geom_text(data = nb.df[nb.df$year ==2012, ], aes(label = region), hjust = -.75,
              vjust = 0, color="black")+theme(legend.position="none")


### Climate figures ----
# Breeding
  br_evi <- read.csv("br_evi.csv")
  head(br_evi)
  br_evi <- br_evi[,-1]
  names(br_evi)<- c("pop", "year", "evi", "evi_stan")
  
  ggplot(br_evi,aes(x=year,y=evi,color=as.factor(pop))) +#geom_point(alpha=.5)+
    scale_y_continuous("EVI")+
    scale_x_continuous("Year", breaks = c(2001:2013))+    
    stat_smooth(method="lm",se = FALSE,size=1)+
    geom_text(data = br_evi[br_evi$year ==2013, ], aes(label = pop), hjust = -.75,
              vjust = 0, color="black")+theme(legend.position="none")+
    geom_line(alpha=.25)
    
# Non-breeding
  nonbr_evi <- read.csv("nonbr_evi.csv")
  head(nonbr_evi)
  names(nonbr_evi)<- c("X","year", "1", "2", "3", "4")
  nonbr_evi<-nonbr_evi[,-1]
  
  nb_evi <- gather(nonbr_evi, region, evi, -year)
  head(nb_evi)
  
  ggplot(nonbr_evi,aes(x=year,y=evi,color=as.factor(region)))+geom_line(alpha=.25)+
    scale_y_continuous("EVI")+
    scale_x_continuous("Year", breaks = c(2001:2013))+    
    #geom_text(data = nb_evi[nb_evi$year ==2013, ], aes(label = region), hjust = -.75,vjust=-1,
    #          vjust = 0, color="black")+#theme(legend.position="none")+
    stat_smooth(method="lm",se = FALSE,size=1)

### Model summaries ----
# Create summary data.frames----
# Read data
  load("woth_retro_data.RData")

  full.fit <- readRDS("full_fit.rds")
  
  full.mcmc <- matrix(ncol=ncol(full.fit[[1]]), nrow=50000)
  for(i in 1:ncol(full.fit[[1]])){
    temp.mcmc <- c(full.fit[[1]][,i], full.fit[[2]][,i])#,full.fit[[3]][,i],full.fit[[4]][,i])
    temp.mcmc2 <- temp.mcmc[c(rep(FALSE,9),TRUE)]
    full.mcmc[,i] <- temp.mcmc2
  }
  colnames(full.mcmc) <- colnames(full.fit[[1]])

# Annual population counts
  full.mean <- apply(full.mcmc, 2, base::mean)

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
  abun[j] <- base::mean(colMeans(pop_mcmc[,40:51]))
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

  popN <- data.frame(N = pop.abun$abun,
                     Year = rep(seq(from=2001, to=2012, by = 1), 17),
                     Population = pop.abun$pop,
                     NB1 = rep(nb1_forest.tm1, 17),
                     NB2 = rep(nb2_forest.tm1, 17),
                     NB3 = rep(nb3_forest.tm1, 17),
                     NB4 = rep(nb4_forest.tm1, 17),
                     NB5 = rep(nb5_forest.tm1, 17))#,
                     #Reg.t = rep(reg_loss.t, 18),
                     #Reg.tm1 = rep(reg_loss.tm1, 18),
                     #Br_evi = rep(br_evi,18),
                     #NB3_evi = rep(nb3_evi.t, 18))

# Coefficients
  beta.est <- numeric()
  beta.lower <- numeric()
  beta.upper <- numeric()
  mbeta.est <- numeric()
  mbeta.lower <- numeric() 
  mbeta.upper <- numeric()
  
  for(i in 1:136){
    beta.est[i] <- mean(full.mcmc[full.mcmc[,i+139]!=0,i]) 
    beta.lower[i] <- quantile(full.mcmc[full.mcmc[,i+139]!=0,i],probs=0.025) 
    beta.upper[i] <- quantile(full.mcmc[full.mcmc[,i+139]!=0,i],probs=0.975)
    mbeta.est[i] <- mean(full.mcmc[,i]) 
    mbeta.lower[i] <- quantile(full.mcmc[,i],probs=0.025) 
    mbeta.upper[i] <- quantile(full.mcmc[,i],probs=0.975)
  }

# Forest change stats
br_loss <- read.csv('Br_loss.txt', header=TRUE)
br_gain <- read.csv('Br_gain.txt', header=TRUE)
pop_forest <- read.csv("pop_forest.csv")
pop_forest <- pop_forest[with(pop_forest, order(pop)),]

br_change <- plyr::join(br_loss, br_gain, by=c("OBJECTID", "Pop", "COUNT", "AREA"))
names(br_change) <- c("ID", "Pop", "Total_cells", "Total_area", "Total_loss", "Total_gain")
br_change <- br_change[,-1]
br_change$forest <- pop_forest$X2000_forest
br_change$Prop_loss <-with(br_change, (Total_loss/forest)*100)
br_change$Prop_gain <-with(br_change, (Total_gain/forest)*100)
br_change$Net_change <- with(br_change, Total_gain-Total_loss)
br_change$Prop_change <- with(br_change, Prop_gain-Prop_loss)

setwd("~/Desktop/Clark Saved/Projects/3_Working on/Population_grouping/data/WOTH")

woth_mcmc <- read.csv("woth_trend_mcmc.csv")

woth_poptrend <- as.data.frame(woth_mcmc[,(ncol(woth_mcmc)-16):ncol(woth_mcmc)])
woth_popabun <- as.data.frame(woth_mcmc[,2:(18)])


  reg <- c("NE", "NE", "CE", "NE", "NW", "NW", "SE", "SW", "SW", "SW", "NW", "NE", "NE",
           "SE","NW","CE","SE")
  pop.eff <- data.frame(Population = rep(seq(1:17),8),
                        Rank = rep(c(2,13,17,14,16,8,3,15,6,9,10,5,4,7,11,12,1),8),
                        Trend = rep(full.mean[478:494],8),
                        Indicator = rep(seq(1:8),each=17),
                        Beta = rep(seq(1:8), each = 17),
                        Ind.est = full.mean[140:275],
                        Beta.est = beta.est,
                        Beta.lower = beta.lower,
                        Beta.upper = beta.upper,
                        Region = rep(reg,8),
                        Trend2=rep(colMeans(woth_poptrend),8),
                        trend.lower = rep(apply(woth_poptrend,2,quantile,probs=0.025),8),
                        trend.upper = rep(apply(woth_poptrend,2,quantile,probs=0.975),8),
                        Abundance=rep(colMeans(woth_popabun),8),
                        abun.lower = rep(apply(woth_popabun,2,quantile,probs=0.025),8),
                        abun.upper = rep(apply(woth_popabun,2,quantile,probs=0.975),8),
                        Prop_change = rep(br_change$Prop_change, 8),
                        Net_change = rep(br_change$Net_change, 8),
                        Prop_loss = rep(br_change$Prop_loss, 8),
                        Prop_gain = rep(br_change$Prop_gain, 8),
                        Total_loss = rep(br_change$Prop_loss, 8),
                        Total_gain = rep(br_change$Prop_gain, 8),
                        Forest = rep(br_change$forest, 8))

# Model fit
bp <- round(mean(full.mcmc[,139]>full.mcmc[,138]),digits=4)
plot(full.mcmc[,139]~full.mcmc[,138])
abline(0,1,lwd=2)
text(x=475,y=-625,substitute(P[B]==bp, list(bp=bp)),cex=0.8)


# Indicator variables
pop.df <- read.csv("pop_df.csv")
ggplot(pop.df, aes(x=predictor, y=pop, fill=g))+geom_raster()+
  scale_y_continuous(breaks=c(1:17))+
  scale_x_continuous(breaks=c(1:17))+
  scale_fill_gradient2(low="white",high="black")+
  geom_vline(xintercept=c(5.5,10.5,15.5))

# Population abundance index vs. NB3 forest loss
pop.df$beta[pop.df$pop==9&pop.df$predictor==13] <- NA
pop.df$g[pop.df$pop==9&pop.df$predictor==13] <- 0
ggplot(pop.df[pop.df$predictor%in%c(13,14),], aes(x=abun, y=g, label=pop))+
  stat_smooth(method="glm",family="binomial")+
  geom_point(alpha=.5,size=3, aes(color=as.factor(predictor)))+
  #geom_line(alpha=.5)+
  theme_classic()+
  scale_y_continuous("Gamma")+#, limits=c(0,1))+
  scale_x_continuous("Abundance")+
  theme(legend.position="none")+
  geom_text(vjust=.001)
with(pop.df[pop.df$predictor%in%c(13,14)&pop.df,], summary(glmer(g~(1|pop)+abun, family="binomial")))
with(pop.df[pop.df$predictor%in%c(13),], summary(glm(g~abun, family="binomial")))
with(pop.df[pop.df$predictor%in%c(14),], summary(lm(g~abun, family="binomial")))

ggplot(pop.df[pop.df$predictor%in%c(13,14),], aes(x=abun, y=beta, label=pop))+
  stat_smooth(method="lm")+
  geom_point(alpha=.5,size=3, aes(color=as.factor(predictor)))+
  #geom_line(alpha=.5)+
  theme_classic()+
  scale_y_continuous("Gamma")+#, limits=c(0,1))+
  scale_x_continuous("Abundance")+
  theme(legend.position="none")+
  geom_text(vjust=.001)
with(pop.df[pop.df$predictor%in%c(13,14),], summary(lmer(beta2~(1|pop)+abun)))
with(pop.df[pop.df$predictor%in%c(13),], summary(lm(beta2~abun)))
with(pop.df[pop.df$predictor%in%c(14),], summary(lm(beta2~abun)))
###Annual abundance plots
ggplot(popN, aes(x=Year, y=N, color=as.factor(Population)))+geom_line()

for(i in 1:17){
  pop16 <- ggplot(popN[popN$Population==i,], aes(x=year, y=N))+
    geom_point(size=5)+
    stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1, color="grey50")+
    stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
    scale_x_continuous("Year",
                       limits = diff(range(year)) * c(-0.2, 0.2) + range(year)) +
    coord_cartesian(xlim = range(pretty(year)))+
    scale_y_continuous("Breeding Abundance")+
    theme(legend.position="none",
          panel.grid.major =  element_blank())
  print(pop16)
}
# Population abundance index vs. NB3 forest loss
require(gridExtra)
summary(glm(N~NB2,data=popN[popN$Population==17&popN$Year!=2011,],family="poisson"))

popN <- plyr::ddply(popN, "Population", mutate, resid_N = resid(lm(N~Year)))

regions <- list(c(1,2,4,12), # Northeast
                c(3,13,14,15,16,17),
                c(7,8,9,10),
                c(5,6,11))
for(i in 1:length(regions)){
pop16 <- ggplot(popN[popN$Population%in%regions[[i]],], aes(x=NB3, y=N, color=as.factor(Population)))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1)+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE) +
  scale_x_continuous("Residual Forest Cover",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
print(pop16)
}

pop3 <- ggplot(popN[popN$Population==3,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop3

pop14 <- ggplot(popN[popN$Population==14,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop14

pop17 <- ggplot(popN[popN$Population==17,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop17

pop13 <- ggplot(popN[popN$Population==13,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop13

pop7 <- ggplot(popN[popN$Population==7,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop7

pop12 <- ggplot(popN[popN$Population==12,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop12

pop1 <- ggplot(popN[popN$Population==1,], aes(x=NB3, y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(popN$NB3)) * c(-0.2, 0.2) + range(popN$NB3)) +
  coord_cartesian(xlim = range(pretty(popN$NB3)))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
pop1

gridExtra::grid.arrange(pop1,pop12,pop13,pop17, ncol=2)


### Abundance vs. Breeding forest loss

br7 <- ggplot(popN[popN$Population==7,], aes(x=br_forest[,7], y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(br_forest[,7])) * c(-0.2, 0.2) + range(br_forest[,7])) +
  coord_cartesian(xlim = range(pretty(br_forest[,7])))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
br7

br14 <- ggplot(popN[popN$Population==14,], aes(x=br_forest[,14], y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(br_forest[,14])) * c(-0.2, 0.2) + range(br_forest[,14])) +
  coord_cartesian(xlim = range(pretty(br_forest[,14])))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
br14

br17 <- ggplot(popN[popN$Population==17,], aes(x=br_forest[,17], y=N))+
  geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, size=1,color="grey50")+
  stat_smooth(method = "lm",geom = "ribbon", fill = NA, fullrange = TRUE,color="grey50") +
  scale_x_continuous("Annual Forest Loss (%)",
                     limits = diff(range(br_forest[,17])) * c(-0.2, 0.2) + range(br_forest[,17])) +
  coord_cartesian(xlim = range(pretty(br_forest[,17])))+
  scale_y_continuous("Breeding Abundance")+#,limits=c(2,5))+
  theme(legend.position="none",
        panel.grid.major =  element_blank())
br17
# Population abundance index vs. NB3 forest loss
ggplot(popN[popN$Population==4,], aes(x=Year, y=N))+geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, color="grey50", size=1)+
  stat_smooth(method = "lm", color="grey70",geom = "ribbon", fill = NA, fullrange = TRUE) +
  scale_x_continuous("EVI",
                     limits = diff(range(popN$Year)) * c(-0.2, 0.2) + range(popN$Year)) +
  coord_cartesian(xlim = range(pretty(popN$Year)))+
  scale_y_continuous("Breeding Abundance")+
  labs(title="Non-breeding climate (Guat/Bel/Yuc)")+
  theme(legend.position="none",
        panel.grid.major =  element_blank())


# NB3 beta estimate vs. Trend 
ggplot(pop.df[pop.df$predictor==16,], aes(x=g, y=trend))+#facet_wrap(~predictor, nrow=4)+
  stat_smooth(method="lm")+geom_point(aes(color=as.factor(pop)))

summary(with(pop.df[pop.df$predictor==16&pop.df$g!=0,], lm(beta~trend)))
summary(with(pop.df[pop.df$predictor==16,], glm(g~trend, family="binomial")))
summary(with(pop.df[pop.df$predictor==17&pop.df$g!=0,], lm(beta~trend)))
summary(with(pop.df[pop.df$predictor==17,], glm(g~trend, family="binomial")))

cor.test(beta.est[86:102],beta.est[120:136]) #NB3 loss beta vs. NB3 EVI beta
plot(beta.est[86:102],beta.est[120:136])
summary(with(pop.eff[pop.eff$Indicator==6&pop.eff$Ind.est!=0,], lm(Beta.est~Trend2)))
with(pop.eff[pop.eff$Indicator==6&pop.eff$Ind.est!=0,], plot(Trend~Beta.est))
ddply(.data = pop.eff, .(Region), .fun = mean(Ind.est))
with(pop.eff[pop.eff$Indicator==8,], cor.test(Beta.est,Abunance))

### Population coefficient plots
ggplot(data=pop.eff, aes(x=as.factor(Beta), y=Beta.est))+geom_point(size=4)+
  geom_errorbar(aes(ymin=Beta.lower,ymax=Beta.upper),width=0)+theme_classic()+
  scale_size_area()+
  geom_hline(aes(yintercept=0),linetype="dashed", alpha=.5) + 
  facet_grid(Population~.)+
  scale_y_continuous(limits=c(-.1,.1))
  labs(title="Pop 1")+
  scale_x_discrete("", labels=c("Trend", "Local Loss", "Regional Loss", "NB1 Loss", "NB2 Loss", "NB3 Loss",
                                "NB4 Loss", "Breeding EVI", "NB1 EVI", "NB2 EVI", "NB3 EVI", "NB4 EVI"))+
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(data=pop.eff[pop.eff$Beta==6,], aes(x=as.factor(Population), y=Beta.est))+
  geom_point(aes(size=Ind.est, color=Region))+
  geom_errorbar(aes(ymin=Beta.lower,ymax=Beta.upper),width=0)+theme_classic()+
  scale_size_area()+
  geom_hline(aes(yintercept=0),linetype="dashed", alpha=.5)



# Post-hoc correlations


ggplot(pop.eff[1:17,], aes(x=Prop_change, y = Trend,size=Abundance,label=Population))+geom_point(size=5)+
  stat_smooth(method="lm", se=FALSE, fullrange = TRUE, color="grey50", size=1)+
  stat_smooth(method = "lm", color="grey70",geom = "ribbon", fill = NA, fullrange = TRUE) +
  scale_x_continuous("Net Forest Change (%), 2000-2013",
                     limits = diff(range(pop.eff$Prop_change)) * c(-0.2, 0.2) + range(pop.eff$Prop_change)) +
  coord_cartesian(xlim = range(pretty(pop.eff$Prop_change)))+
  scale_y_continuous("Mean annual population change (%), 2000-2013")+
  labs(title="Breeding habitat loss")+
  theme(legend.position="none",
        panel.grid.major =  element_blank())


ggplot(dd, aes(x = x, y = y)) + geom_point() + stat_smooth(method = "lm", se = FALSE) + 
  stat_smooth(method = "lm", colour = "red", geom = "ribbon", fill = NA, fullrange = TRUE) + 
  scale_x_continuous(limits = diff(range(dd$x)) * c(-0.2, 0.2) + range(dd$x)) + 
  coord_cartesian(xlim = range(pretty(dd$x)))
with(pop.eff[c(1:11,14:16),], summary(lm(Trend~Prop_change)))

### Geography tests ---
require(foreign)

pop.dismat <- read.dbf(file = "pop_distmat.dbf")
pop.latlong <- read.table("pop_latlong.txt", sep = ",", header=TRUE)
pop.latlong <- subset(x = pop.latlong,select = c("Shape_Area", "Pop", "cen_x", "cen_y"))
pop.latlong <- pop.latlong[with(pop.latlong, order(Pop)),]

fit1 <- lm(pop.df$beta[pop.df$predictor==13]~1)
fit2 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_x)
fit3 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_y)
fit4 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_x+pop.latlong$cen_y)
fit5 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_x+pop.latlong$cen_y+I(pop.latlong$cen_y^2))
fit6 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_x+pop.latlong$cen_y+I(pop.latlong$cen_x^2))
fit7 <- lm(pop.df$beta[pop.df$predictor==13]~pop.latlong$cen_x+pop.latlong$cen_y+I(pop.latlong$cen_x^2)+
             I(pop.latlong$cen_y^2))
anova(fit1,fit2)
anova(fit1,fit3)
anova(fit1,fit4)

### Cluster tests ---
require(ade4)

pop.distmat <- dist(cbind(pop.latlong$cen_x, pop.latlong$cen_y))
ind <- t(matrix(pop.df$g, nrow=17,ncol=17))
ind <- ifelse(ind<0.25, 0, 1)
g.distmat <- dist(ind[,c(13,14)], method="euc")
#g.clust <- hclust(dist.mat, method="ward.D")
#plot(g.clust)

mantel.rtest(g.distmat, pop.distmat, nrepet = 9999)
