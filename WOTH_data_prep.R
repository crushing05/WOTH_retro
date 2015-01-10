### Preperation of data for WOTH retrospective analysis
# Based on recommendations from Grobois et al. (2008) Biol Rev

rm(list=ls())
setwd("C:/Users/rushingc/Desktop/Backup/WOTH_retro")
require(raster)
require(rgdal)
require(sp)
require(foreign)
require(maptools)
require(plyr)

### Indexing objects for tm1 ----

setwd("C:/Users/rushingc/Desktop/Backup/WOTH_retro")
setwd("~/Desktop/Clark Saved/Projects/3_Working on/WOTH_retro/data_sets/BBS")
woth_data <- read.csv("woth_countall2.csv")

woth_data <- woth_data[with(woth_data, order(groups.hopt, RID)),]

nyears <- length(unique(woth_data$year))
nroutes  <- length(unique(woth_data$RID))
nobs  <- length(unique(woth_data$Obs))
npops <- length(unique(woth_data$groups.hopt))


### data for tm1 ----
# Routes
  route  <- woth_data$RID

# Populations
  pop  <- woth_data[duplicated(woth_data$RID)==FALSE,15]

# Year
  year  <- (unique(woth_data$year)-2002)/11

# BBS counts
  count <- matrix(woth_data$SpeciesTotal, ncol=nroutes, byrow=FALSE)
  #count <- count[2:12,]

# observers
  obs  <- matrix(woth_data$Obs, ncol=nroutes, byrow=FALSE)
  a  <- as.numeric(levels(factor(obs)))
  newobs <- obs
  for(j in 1:length(a)){newobs[which(woth_data$Obs==a[j])]<-j}
  #newobs <- newobs[2:12,]

# Novice dummy variable
  first  <- matrix(woth_data$first, ncol=nroutes, byrow=FALSE)
  #first <- first[2:12,]


# Residual Breeding forest, year t - 1

  pop_loss <- read.csv("pop_yrforest.csv")
  pop_loss <- pop_loss[,-c(1:2)]
  pop_loss <- ddply(pop_loss, "pop", mutate, resid_loss = resid(lm(perc_forest~year)))
  
  
  pop.df <- data.frame(pop=woth_data$groups.hopt, year=woth_data$year_lossb)
  pop.df$id <- 1:nrow(pop.df)
  rte_regloss <- merge(pop.df, pop_loss, by = c("pop", "year"), type="right") 
  rte_regloss <- rte_regloss[with(rte_regloss, order(id)),]
  
  br_forest <- matrix(rte_regloss$resid_loss, ncol=nroutes, byrow=FALSE)
  ggplot(rte_regloss, aes(x=year, y=perc_forest, color=as.factor(pop)))+geom_line()

# Residual Breeding climate, year t - 1

  br_evi <- read.csv("br_evi.csv")
  head(br_evi)

  br_evi <- ddply(br_evi, "pop", mutate, resid_evi = resid(lm(evi~year)))
  ggplot(br_evi, aes(x=year, y=evi))+geom_line()+facet_grid(pop~.)

  rte_regevi <- merge(pop.df, br_evi, by = c("pop", "year"), type="right") 
  rte_regevi <- rte_regevi[with(rte_regevi, order(id)),]

  br_clim <- matrix(rte_regevi$resid_evi, ncol=nroutes, byrow=FALSE)
  
  ggplot(rte_regevi, aes(x=year, y=resid_evi))+geom_line()+
      stat_smooth(method="lm")+facet_grid(pop~.)


# Non-breeding forest cover, linear trend removed, PCA scores instead of raw scores
# Note t == t - 1, tm1 == t - 2 to be consistent with climate data
  nonbr_loss <- read.csv("nb_loss.csv")
  head(nonbr_loss)

  nonbr_loss <- ddply(nonbr_loss, "region", mutate, resid_loss = resid(lm((yr_forest/1000000)~year)))

  
  ggplot(nonbr_loss, aes(x=year, y=stand_forest, color=as.factor(region)))+geom_line()+
  scale_y_continuous("Forest Cover (% of 2000)")

  ggplot(nonbr_loss, aes(x=year, y=resid_loss, color=as.factor(region)))+geom_line()+
  scale_y_continuous("Residual Forest Cover")

  nb1_forest.tm1 <- nonbr_loss$resid_loss[nonbr_loss$region==1][2:13]
  nb2_forest.tm1 <- nonbr_loss$resid_loss[nonbr_loss$region==2][2:13]
  nb3_forest.tm1 <- nonbr_loss$resid_loss[nonbr_loss$region==3][2:13]
  nb4_forest.tm1 <- nonbr_loss$resid_loss[nonbr_loss$region==4][2:13]
  nb5_forest.tm1 <- nonbr_loss$resid_loss[nonbr_loss$region==5][2:13]

  nb1_forest.tm2 <- nonbr_loss$resid_loss[nonbr_loss$region==1][1:12]
  nb2_forest.tm2 <- nonbr_loss$resid_loss[nonbr_loss$region==2][1:12]
  nb3_forest.tm2 <- nonbr_loss$resid_loss[nonbr_loss$region==3][1:12]
  nb4_forest.tm2 <- nonbr_loss$resid_loss[nonbr_loss$region==4][1:12]
  nb5_forest.tm2 <- nonbr_loss$resid_loss[nonbr_loss$region==5][1:12]

# Residual Nonbreeding climate year t - 1

  nonbr_evi <- read.csv("nonbr_evi.csv")
  head(nonbr_evi)
  
  ggplot(nonbr_evi, aes(x=year, y=evi, color=as.factor(region)))+geom_line()+stat_smooth(method='lm')
  
  
  nonbr_evi <- ddply(nonbr_evi, "region", mutate, resid_evi = resid(lm(evi~year)))
  ggplot(nonbr_evi, aes(x=year, y=resid_evi))+geom_line()+facet_grid(region~.)

  nb1_evi.tm1 <- nonbr_evi$resid_evi[nonbr_evi$region==1][2:13]
  nb2_evi.tm1 <- nonbr_evi$resid_evi[nonbr_evi$region==2][2:13]
  nb3_evi.tm1 <- nonbr_evi$resid_evi[nonbr_evi$region==3][2:13]
  nb4_evi.tm1 <- nonbr_evi$resid_evi[nonbr_evi$region==4][2:13]
  nb5_evi.tm1 <- nonbr_evi$resid_evi[nonbr_evi$region==5][2:13]


  plot(nb1_evi.tm1, type="l")
  lines(nb2_evi.tm1, col="red")
  lines(nb3_evi.tm1,col="blue")
  lines(nb4_evi.tm1,col="green")


# Residual Non-breeding climate, year t
  nb1_evi.t <- nonbr_evi$resid_evi[nonbr_evi$region==1][3:14]
  nb2_evi.t <- nonbr_evi$resid_evi[nonbr_evi$region==2][3:14]
  nb3_evi.t <- nonbr_evi$resid_evi[nonbr_evi$region==3][3:14]
  nb4_evi.t <- nonbr_evi$resid_evi[nonbr_evi$region==4][3:14]
  nb5_evi.t <- nonbr_evi$resid_evi[nonbr_evi$region==5][3:14]



### Save data
first.occ <- function(x){
  index <- !duplicated(x)
  return(seq_along(x)[index])
}

pop.index <- first.occ(pop)
br_forest <- br_forest[,pop.index] 
br_clim <- br_clim[,pop.index] 
count <- count + 1

pop_mean <- read.csv("pop_mean_abun.csv")
pop_abun <- read.csv("pop_abun.csv")

setwd("~/Desktop/Clark Saved/Projects/3_Working on/WOTH_retro")
save(count, nroutes,nyears, first, year, nobs, newobs, npops, pop, # count data & parameters
     br_forest, br_clim, # Breeding data
     nb1_forest.tm1, nb2_forest.tm1, nb3_forest.tm1,nb4_forest.tm1, nb5_forest.tm1,  # Non-breeding forest, t - 1 
     nb1_forest.tm2, nb2_forest.tm2, nb3_forest.tm2,nb4_forest.tm2, nb5_forest.tm2, # Non-breeding forest, t - 2 
     nb1_evi.t, nb2_evi.t, nb3_evi.t, nb4_evi.t, nb5_evi.t,  # Non-breeding evi, t 
     nb1_evi.tm1, nb2_evi.tm1, nb3_evi.tm1, nb4_evi.tm1, nb5_evi.tm1,  # Non-breeding evi, t 
     pop_mean, pop_abun,
     file="woth_retro_data.RData")


#### Test for linear effects ----
### Breeding climate
# Pop 17
fit.trend <- with(nonbr_evi[nonbr_evi$region==5,], lm(evi~scale(year)))
fit.fix <- with(nonbr_evi[nonbr_evi$region==5,], lm(evi~1))
anova(fit.fix, fit.trend)


#### Test for pairwise correlations
# Non-breeding forest loss
    cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[14:26]) # 1 vs 2
    cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[27:39]) # 1 vs 3
    cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[40:52]) # 1 vs 4
    cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[53:65]) # 1 vs 5
    cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[27:39]) # 2 vs 3
    cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[40:52]) # 2 vs 4
    cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[53:65]) # 2 vs 5
    cor.test(nb_yrloss$resid_loss[27:39], nb_yrloss$resid_loss[40:52]) # 3 vs 4
    cor.test(nb_yrloss$resid_loss[27:39], nb_yrloss$resid_loss[53:65]) # 3 vs 4
    cor.test(nb_yrloss$resid_loss[40:52], nb_yrloss$resid_loss[53:65]) # 4 vs 5

# Non-breeding forest loss
cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[14:26]) # CosPan vs Nic
cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[27:39]) # CosPan vs HonEl
cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[40:52]) # CosPan vs Guat
cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[53:65]) # CosPan vs BelYuc
cor.test(nb_yrloss$resid_loss[1:13], nb_yrloss$resid_loss[66:78]) # CosPan vs Mex
cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[27:39]) # Nic vs HonEl
cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[40:52]) # Nic vs Guat
cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[53:65]) # Nic vs BelYuc
cor.test(nb_yrloss$resid_loss[14:26], nb_yrloss$resid_loss[66:78]) # Nic vs Mex
cor.test(nb_yrloss$resid_loss[27:39], nb_yrloss$resid_loss[40:52]) # HonEl vs Guat
cor.test(nb_yrloss$resid_loss[27:39], nb_yrloss$resid_loss[53:65]) # HonEl vs BelYuc
cor.test(nb_yrloss$resid_loss[27:39], nb_yrloss$resid_loss[66:78]) # Honel vs Mex
cor.test(nb_yrloss$resid_loss[40:52], nb_yrloss$resid_loss[53:65]) # Guat vs BelYuc
cor.test(nb_yrloss$resid_loss[40:52], nb_yrloss$resid_loss[66:78]) # Guat vs Mex
cor.test(nb_yrloss$resid_loss[53:65], nb_yrloss$resid_loss[66:78]) # BelYuc vs Mex


# Non-breeding climate
cor.test(nonbr_evi$resid_evi[1:13], nonbr_evi$resid_evi[14:26]) # CosPan vs Nic
cor.test(nonbr_evi$resid_evi[1:13], nonbr_evi$resid_evi[27:39]) # CosPan vs HonEl
cor.test(nonbr_evi$resid_evi[1:13], nonbr_evi$resid_evi[40:52]) # CosPan vs Guat
cor.test(nonbr_evi$resid_evi[1:13], nonbr_evi$resid_evi[53:65]) # CosPan vs BelYuc
cor.test(nonbr_evi$resid_evi[1:13], nonbr_evi$resid_evi[66:78]) # CosPan vs Mex
cor.test(nonbr_evi$resid_evi[14:26], nonbr_evi$resid_evi[27:39]) # Nic vs HonEl
cor.test(nonbr_evi$resid_evi[14:26], nonbr_evi$resid_evi[40:52]) # Nic vs Guat
cor.test(nonbr_evi$resid_evi[14:26], nonbr_evi$resid_evi[53:65]) # Nic vs BelYuc
cor.test(nonbr_evi$resid_evi[14:26], nonbr_evi$resid_evi[66:78]) # Nic vs Mex
cor.test(nonbr_evi$resid_evi[27:39], nonbr_evi$resid_evi[40:52]) # HonEl vs Guat
cor.test(nonbr_evi$resid_evi[27:39], nonbr_evi$resid_evi[53:65]) # HonEl vs BelYuc
cor.test(nonbr_evi$resid_evi[27:39], nonbr_evi$resid_evi[66:78]) # Honel vs Mex
cor.test(nonbr_evi$resid_evi[40:52], nonbr_evi$resid_evi[53:65]) # Guat vs BelYuc
cor.test(nonbr_evi$resid_evi[40:52], nonbr_evi$resid_evi[66:78]) # Guat vs Mex
cor.test(nonbr_evi$resid_evi[53:65], nonbr_evi$resid_evi[66:78]) # BelYuc vs Mex

## Breeding loss & non-breeding loss (only for connected regions)
# Pop1 - connected to 1,2,3
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[1:13]) #0.34, p=0.25
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[14:26]) # -0.06, p = 0.84
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[27:39]) # 0.52, 0.09
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[40:52]) # 0.52, 0.09
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[53:65]) # 0.52, 0.09
   cor.test(pop_loss[pop_loss$pop==1,7], nb_reg_loss$resid_loss[66:78]) # 0.40, 0.16

# Pop2 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==2,7], nonbr_loss$resid_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==2,7], nonbr_loss$pca_loss[14:26]) # -0.31, p=0.31
  cor.test(pop_loss[pop_loss$pop==2,7], nonbr_loss$pca_loss[27:39]) # 0.51, p=0.077

# Pop3 - connected to 2,3 
  cor.test(pop_loss[pop_loss$pop==3,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==3,7], nonbr_loss$pca_loss[14:26]) #0.54, 0.055
  cor.test(pop_loss[pop_loss$pop==3,7], nonbr_loss$pca_loss[27:39]) #0.56, 0.048
  cor.test(pop_loss[pop_loss$pop==3,7], nonbr_loss$pca_loss[40:52])

# Pop4 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==4,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==4,7], nonbr_loss$pca_loss[14:26]) # -0.60, 0.029
  cor.test(pop_loss[pop_loss$pop==4,7], nonbr_loss$pca_loss[27:39]) # 0.22, 0.46
  cor.test(pop_loss[pop_loss$pop==4,7], nonbr_loss$pca_loss[40:52])

# Pop5 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==5,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==5,7], nonbr_loss$pca_loss[14:26])
  cor.test(pop_loss[pop_loss$pop==5,7], nonbr_loss$pca_loss[27:39]) # 0.433, p = 0.13
  cor.test(pop_loss[pop_loss$pop==5,7], nonbr_loss$pca_loss[40:52]) # 0.84, p = 0.00025

# Pop6 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==6,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==6,7], nonbr_loss$pca_loss[14:26])
  cor.test(pop_loss[pop_loss$pop==6,7], nonbr_loss$pca_loss[27:39]) #0.21, p=0.49
  cor.test(pop_loss[pop_loss$pop==6,7], nonbr_loss$pca_loss[40:52]) #0.76, p=0.0026

# Pop7 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==7,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==7,7], nonbr_loss$pca_loss[14:26]) #-0.18, p=0.55
  cor.test(pop_loss[pop_loss$pop==7,7], nonbr_loss$pca_loss[27:39]) #0.39, p=0.18
  cor.test(pop_loss[pop_loss$pop==7,7], nonbr_loss$pca_loss[40:52])

# Pop8 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==8,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==8,7], nonbr_loss$pca_loss[14:26]) 
  cor.test(pop_loss[pop_loss$pop==8,7], nonbr_loss$pca_loss[27:39]) #0.39, p=0.18
  cor.test(pop_loss[pop_loss$pop==8,7], nonbr_loss$pca_loss[40:52]) #0.10, p = 0.73

# Pop9 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==9,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==9,7], nonbr_loss$pca_loss[14:26]) 
  cor.test(pop_loss[pop_loss$pop==9,7], nonbr_loss$pca_loss[27:39]) #0.39, p=0.19
  cor.test(pop_loss[pop_loss$pop==9,7], nonbr_loss$pca_loss[40:52]) #0.25, p = 0.40

# Pop10 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==10,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==10,7], nonbr_loss$pca_loss[14:26]) 
  cor.test(pop_loss[pop_loss$pop==10,7], nonbr_loss$pca_loss[27:39]) #0.64, p=0.018
  cor.test(pop_loss[pop_loss$pop==10,7], nonbr_loss$pca_loss[40:52]) #0.77, p = 0.0018

# Pop11 - connected to 3,4
  cor.test(pop_loss[pop_loss$pop==11,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==11,7], nonbr_loss$pca_loss[14:26]) 
  cor.test(pop_loss[pop_loss$pop==11,7], nonbr_loss$pca_loss[27:39]) #0.79, p=0.0012
  cor.test(pop_loss[pop_loss$pop==11,7], nonbr_loss$pca_loss[40:52]) #0.82, p = 0.0005

# Pop12 - connected to 1,2,3
  cor.test(pop_loss[pop_loss$pop==12,7], nonbr_loss$pca_loss[1:13]) # 0.21, p = 0.49
  cor.test(pop_loss[pop_loss$pop==12,7], nonbr_loss$pca_loss[14:26]) #-0.091, p=0.76
  cor.test(pop_loss[pop_loss$pop==12,7], nonbr_loss$pca_loss[27:39]) #0.28, p=0.35
  cor.test(pop_loss[pop_loss$pop==12,7], nonbr_loss$pca_loss[40:52]) 

# Pop13 - connected to 1,2,3
  cor.test(pop_loss[pop_loss$pop==13,7], nonbr_loss$pca_loss[1:13]) # -0.32, p = 0.27
  cor.test(pop_loss[pop_loss$pop==13,7], nonbr_loss$pca_loss[14:26]) #-0.63, p=0.021
  cor.test(pop_loss[pop_loss$pop==13,7], nonbr_loss$pca_loss[27:39]) #0.14, p=0.63
  cor.test(pop_loss[pop_loss$pop==13,7], nonbr_loss$pca_loss[40:52]) 

# Pop14 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==14,7], nonbr_loss$pca_loss[1:13])
  cor.test(pop_loss[pop_loss$pop==14,7], nonbr_loss$pca_loss[14:26]) #-0.5, p=0.079
  cor.test(pop_loss[pop_loss$pop==14,7], nonbr_loss$pca_loss[27:39]) #0.24, p=0.42
  cor.test(pop_loss[pop_loss$pop==14,7], nonbr_loss$pca_loss[40:52])

# Pop15 - connected to 2,3,4
  cor.test(pop_loss[pop_loss$pop==15,7], nonbr_loss$pca_loss[1:13]) 
  cor.test(pop_loss[pop_loss$pop==15,7], nonbr_loss$pca_loss[14:26]) #0.29, p=0.32
  cor.test(pop_loss[pop_loss$pop==15,7], nonbr_loss$pca_loss[27:39]) #0.66, p=0.012
  cor.test(pop_loss[pop_loss$pop==15,7], nonbr_loss$pca_loss[40:52]) #0.80, p = 0.0009

# Pop16 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==16,7], nonbr_loss$pca_loss[1:13]) 
  cor.test(pop_loss[pop_loss$pop==16,7], nonbr_loss$pca_loss[14:26]) #0.62, p=0.024
  cor.test(pop_loss[pop_loss$pop==16,7], nonbr_loss$pca_loss[27:39]) #0.54, p=0.053
  cor.test(pop_loss[pop_loss$pop==16,7], nonbr_loss$pca_loss[40:52])

# Pop17 - connected to 2,3
  cor.test(pop_loss[pop_loss$pop==17,7], nonbr_loss$pca_loss[1:13]) 
  cor.test(pop_loss[pop_loss$pop==17,7], nonbr_loss$pca_loss[14:26]) #0.004, p=0.98
  cor.test(pop_loss[pop_loss$pop==17,7], nonbr_loss$pca_loss[27:39]) #0.54, p=0.057
  cor.test(pop_loss[pop_loss$pop==17,7], nonbr_loss$pca_loss[40:52])


#### PCA to reduce multi-collinearity
nonbr_loss_pca <- prcomp(matrix(nb_reg_loss$resid_loss, ncol=6, nrow=13),
                         center = TRUE,
                         scale. = TRUE)
print(nonbr_loss_pca)
summary(nonbr_loss_pca)

biplot(nonbr_loss_pca)
nonbr_loss_pca$rotation
nb_reg_loss$pca_loss <- c(nonbr_loss_pca$x)
nb_reg_loss$pca_region <- rep(c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"), each = 13)

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(nonbr_loss_pca, obs.scale = 1, var.scale = 1, 
              groups = unique(nonbr_loss$region), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

