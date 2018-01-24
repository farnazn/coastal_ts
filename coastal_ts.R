################################################################################
# 1. Setup
################################################################################
pkgs <- c("devtools", "randomForest", "dplyr", "rjags", "arm", "xtable","caret",
          "ggplot2","knitr")
cran_no <- pkgs[!pkgs %in% installed.packages()[,1]]
for(i in cran_no){
  install.packages(i)
}
gh_pkgs <- c("usepa/LakeTrophicModelling")
gh_no <- gh_pkgs[!basename(gh_pkgs) %in% installed.packages()[,1]]
for(i in gh_no){
  devtools::install_github(i)
}
all_pkgs <- c(basename(gh_pkgs),pkgs)
lapply(all_pkgs, library, character.only = T)
################################################################################
# 2. Data
################################################################################

coastal <- read.csv("CoastalWQ_20161129.csv", stringsAsFactors = FALSE)
# VISNUM = 1
coastal <- coastal[coastal[,"VISNUM"]==1,]
# Col_loc = "SURFACE" 
coastal <- coastal[coastal[,"Col_loc"]=="SURFACE",]
# SAMPYEAR=2010
coastal <- coastal[coastal[,"SAMPYEAR"]=="2010",]
# SAMPYEAR=2015
coastal <- coastal[coastal[,"SAMPYEAR"]=="2015",]

coastal[,"REGION"] <- as.factor(coastal[,"REGION"])
coastal[,"SUBREGIONS"] <- as.factor(coastal[,"SUBREGIONS"])
#
predictors_coastal <- colnames(coastal)

predictors_coastal <- predictors_coastal[predictors_coastal!="Col_Date" &
                                           predictors_coastal!="VISNUM" &
                                           predictors_coastal!="Col_loc"&
                                           predictors_coastal!="Site_Visnum_Layer"&
                                           predictors_coastal!="UID"&
                                           predictors_coastal!="SITE_ID"&
                                           predictors_coastal!="STATE"&
                                           predictors_coastal!="SAMPYEAR"&
                                           predictors_coastal!="TSS..mg.L."&
                                           predictors_coastal!="CHLA..ug.L."]

# Removing the missing values:
coastal<- coastal[!is.na(coastal[,"SECCHI_MEAN..m."]) 
                  & !is.na(coastal[,"DIP..mgP.L."]) 
                  & !is.na(coastal[,"DIN..mgN.L."])
                  & !is.na(coastal[,"TN..mgN.L."])
                  & !is.na(coastal[,"TP..mgP.L."])
                  & !is.na(coastal[,"SUBREGIONS"])
                  & !is.na(coastal[,"CHLA..ug.L."]),]

# Replacing the non-detects with 2010 NCCA MDL values
coastal[coastal[,"TP..mgP.L."]==0,"TP..mgP.L."] <- 0.0012
coastal[coastal[,"DIN..mgN.L."]==0,"DIN..mgN.L."] <- 0.001
coastal[coastal[,"DIP..mgP.L."]==0,"DIP..mgP.L."] <- 0.0027

# Three categories based on Bricker et al, 2003
# Chlorophyll a
coastal <- coastal %>% mutate(TS_Chla=cut(CHLA..ug.L., breaks=c(-Inf, 5, 20, Inf), labels=c("Oligo", "Meso", "Eu")))
# Nitrogen
coastal <- coastal %>% mutate(TS_N=cut(TN..mgN.L., breaks=c(-Inf, 0.1, 1, Inf), labels=c("Oligo", "Meso", "Eu")))
# Phosphorus
coastal <- coastal %>% mutate(TS_P=cut(TP..mgP.L., breaks=c(-Inf, 0.01, 0.1, Inf), labels=c("Oligo", "Meso", "Eu")))
# SD
coastal <- coastal %>% mutate(TS_SD=cut(SECCHI_MEAN..m., breaks=c(-Inf, 1, 3, Inf), labels=c("Oligo", "Meso", "Eu")))
# Equal Quantile
Breaks_Chla_Q <- c(quantile(coastal[,"CHLA..ug.L."], probs = seq(0, 1, by = 1/3)))
coastal <- coastal %>% mutate(TS_Chla_Q=cut(CHLA..ug.L., breaks=Breaks_Chla_Q, labels=c("Oligo", "Meso", "Eu")))



# consistent_ts <- ifelse((coastal$TS_Chla==coastal$TS_P 
#                       & coastal$TS_Chla==coastal$TS_N
#                       ), 1, 0)
# 
# coastal <- cbind(coastal, consistent_ts)


# Four categories
# coastal <- coastal %>% mutate(TS_Chla=cut(CHLA..ug.L., breaks=c(-Inf, 5, 20, 60 , Inf), labels=c("Oligo", "Meso", "Eu","Hyper")))
# Equal Quantile
Breaks_Chla_Q <- c(quantile(coastal[,"CHLA..ug.L."], probs = seq(0, 1, by = 1/4)))
coastal <- coastal %>% mutate(TS_Chla_Q=cut(CHLA..ug.L., breaks=Breaks_Chla_Q, labels=c("Oligo", "Meso", "Eu", "Hyper")))


##################################################
#All Variables
#Clean Up Data - Complete Cases
all_coastal <- data.frame(coastal[predictors_coastal],LogCHLA=log10(coastal$CHLA..ug.L.))
row.names(all_coastal)<-coastal$SITE_ID
all_coastal <- all_coastal[complete.cases(all_coastal),]

################################################################################
# 3. Random Forest for Variable Selection
################################################################################

##################################################
#Model: All Variables
all_rf<-randomForest(y=all_coastal$LogCHLA,x=all_coastal[,predictors_coastal]
                     , ntree=5000, importance=TRUE, proximity=TRUE
                     , keep.forest=TRUE,keep.inbag=TRUE)
################################################################################
# 4. Random Forest for Variable Selection - Evaluation
################################################################################
# 
# data_def <- read.csv("data_def.csv", stringsAsFactors = FALSE)
# all_imp <- importance(all_rf)
# 
# var_importance <- varImportance(all_imp,"All variables")
# 
# dplyr::arrange(var_importance,desc(mean_decrease_acc))
# 
# importancePlot(all_rf, data_def=data_def,type='acc',size=3)
# 
# dplyr::arrange(var_importance,desc(mean_decrease_gini))
# 
# importancePlot(all_rf, data_def=data_def,type='gini',size=3)
################################################################################
# 7. JAGS Model
################################################################################
# Removing the missing values:
coastal<- coastal[!is.na(coastal[,"SECCHI_MEAN..m."]) 
                  & !is.na(coastal[,"DIP..mgP.L."]) 
                  & !is.na(coastal[,"DIN..mgN.L."])
                  & !is.na(coastal[,"TN..mgN.L."])
                  & !is.na(coastal[,"TP..mgP.L."])
                  & !is.na(coastal[,"SUBREGIONS"])
                  & !is.na(coastal[,"CHLA..ug.L."]),]

# Replacing the non-detects with 2010 NCCA MDL values
coastal[coastal[,"TP..mgP.L."]==0,"TP..mgP.L."] <- 0.0012
coastal[coastal[,"DIN..mgN.L."]==0,"DIN..mgN.L."] <- 0.001
coastal[coastal[,"DIP..mgP.L."]==0,"DIP..mgP.L."] <- 0.0027

# Data splitting for cross validation 90%/10%
set.seed(100)

# Sample <- sample(nrow(coastal_consistent),size= round(0.1*dim(coastal)[1]),replace=FALSE)
# 
Sample <- sample(nrow(coastal),size= round(0.1*dim(coastal)[1]),replace=FALSE)
# coastal_consistent <- subset(coastal, coastal$consistent_ts==1)

# Evaluation <- coastal_consistent[Sample,]
# Model <- rbind(coastal_consistent[-Sample,], subset(coastal, coastal$consistent_ts==0))


Evaluation <- coastal[Sample,]
Model <- coastal[-Sample,]
#set up the initializations 
# Three cut-off points
cutpt.inits <- array(dim= c(3))

for (k in 1:3){
  cutpt.inits[k] <- rnorm(1)
}

# Two cut-off points
# cutpt.inits <- array(dim= c(2))
# 
# for (k in 1:2){
#   cutpt.inits[k] <- rnorm(1)
# }


inits <- function () {list("cutpt_raw" = cutpt.inits)}
# Center, scale, and log transform the predictors
SDD.C <- as.numeric(scale(log(Model$SECCHI_MEAN..m.), center = TRUE, scale = TRUE))
TN.C <- as.numeric(scale(log(Model$TN..mgN.L.), center = TRUE, scale = TRUE))
TP.C <- as.numeric(scale(log(Model$TP..mgP.L.), center = TRUE, scale = TRUE))
DIN.C <- as.numeric(scale(log(Model$DIN..mgN.L.), center = TRUE, scale = TRUE))
DIP.C <- as.numeric(scale(log(Model$DIP..mgP.L.), center = TRUE, scale = TRUE))

DataList = list('TS' = factor(Model[,"TS_Chla_Q"])
                ,'SD' = SDD.C
                ,'Nitrogen' = TN.C
                ,'Phosphorus' = TP.C
                ,'DIN' = DIN.C
                ,'DIP' = DIP.C
                ,'Subregion' = factor(Model[,"SUBREGIONS"]))


#The parameter(s) to be monitored
parameters = c('alpha_SD', 'alpha_N', 'alpha_P', 'alpha_DIN', 'alpha_DIP', 'alpha_SubR'
               , 's'
               , 'C')

# Number of steps to "tune" the samplers.
adaptSteps = 3000          

# Number of steps to "burn-in" the samplers.
#changes from 1000 the initail setting
burnInSteps = 5000    

# Number of chains to run.       
nChains = 1 # Change to 3 chains for final run

# Total number of steps in chains to save.     
numSavedSteps=10000    # Change to 50000 for the final run

# Number of steps to "thin" (1=keep every step).
thinSteps= 5

# Steps per chain.
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Start the clock!
ptm <- proc.time()
coastal_jags <- jags.model('coastal_jags.R',data = DataList 
                           , inits, n.chains = nChains, n.adapt = adaptSteps)
# Stop the clock
proc.time() - ptm
################################################################################
################################################################################
#8. JAGS Model Diagnostics
################################################################################
# Start the clock!
ptm <- proc.time()
coastal_coda <- coda.samples(coastal_jags, parameters, n.iter=10000)  # Change to 50000 for the final run
# Stop the clock
proc.time() - ptm
#################################################
# Plot
plot(coastal_coda[,1:3])
plot(coastal_coda[,4:6])
plot(coastal_coda[,7:9])
plot(coastal_coda[,10:12])
# Table
print(xtable(cbind(summary(coastal_coda)$quantiles, summary(coastal_coda)$statistics[,2])), floating=FALSE)
################################################################################
################################################################################
#9. JAGS Model Evaluation
################################################################################
# 3 Chains Combined
simCodaOne.Coda.Coastal <- NULL
for (i in 1:nChains) simCodaOne.Coda.Coastal <- rbind(simCodaOne.Coda.Coastal, coastal_coda[[i]])

MCMC.Coastal <- as.mcmc(simCodaOne.Coda.Coastal)

Coeff.Coastal.Summary <- matrix(NA, 42, 4)
# Coeff.Coastal.Summary <- matrix(NA, 41, 4)

for (i in 1:42){ Coeff.Coastal.Summary[i,] <- cbind(mean(simCodaOne.Coda.Coastal[,i])
                                                    , sd(simCodaOne.Coda.Coastal[,i])
                                                    , quantile(simCodaOne.Coda.Coastal[,i], c(0.025), type = 1)
                                                    , quantile(simCodaOne.Coda.Coastal[,i], c(0.975), type = 1))}
colnames(Coeff.Coastal.Summary) <- cbind("mean", "sd", "2.5%", "97.5%")

rownames(Coeff.Coastal.Summary ) <-colnames(MCMC.Coastal)
print(xtable(Coeff.Coastal.Summary, floating=FALSE))

# Coefficient Matrix
Alpha <- rbind(Coeff.Coastal.Summary["alpha_SD",]
               , Coeff.Coastal.Summary["alpha_N",]
               , Coeff.Coastal.Summary["alpha_P",]
               , Coeff.Coastal.Summary["alpha_DIN",]
               , Coeff.Coastal.Summary["alpha_DIP",]
               , Coeff.Coastal.Summary[9:41,])
# Center, scale, and log transform the evaluation data
Eval.SDD.C <- as.numeric(scale(log(Evaluation$SECCHI_MEAN..m.), center = TRUE, scale = TRUE))
Eval.TN.C <- as.numeric(scale(log(Evaluation$TN..mgN.L.), center = TRUE, scale = TRUE))
Eval.TP.C <- as.numeric(scale(log(Evaluation$TP..mgP.L.), center = TRUE, scale = TRUE))
Eval.DIN.C <- as.numeric(scale(log(Evaluation$DIN..mgN.L.), center = TRUE, scale = TRUE))
Eval.DIP.C <- as.numeric(scale(log(Evaluation$DIP..mgP.L.), center = TRUE, scale = TRUE))

# Subregion Matrix
SubRegion <- matrix(0, dim(Evaluation)[1], length(levels(Evaluation[,"SUBREGIONS"])))
for(j in 1:dim(Evaluation)[1]){
    for(i in 1:length(levels(Evaluation[,"SUBREGIONS"]))){
      if (factor(Evaluation[j, "SUBREGIONS"])==levels(Evaluation[,"SUBREGIONS"])[i]) 
          SubRegion[j,i] <-1
    }
  }

# Evaluation predictors
Eval.Predictors <- cbind(Eval.SDD.C, Eval.TN.C, Eval.TP.C, Eval.DIN.C, Eval.DIP.C, SubRegion)

predict.EvaluationAll <- Eval.Predictors %*% Alpha[,"mean"]
# Remove NA's
predict.EvaluationAll <- predict.EvaluationAll[!is.na(predict.EvaluationAll)]

Predict.CatAll <-  vector(length = length(predict.EvaluationAll))
C <- rbind(Coeff.Coastal.Summary["C[1]",],  Coeff.Coastal.Summary["C[2]",],  Coeff.Coastal.Summary["C[3]",])

for (i in 1:length(predict.EvaluationAll)){
  if (predict.EvaluationAll[i]< C[1]) Predict.CatAll[i] <- "Oligo"
  if (predict.EvaluationAll[i]< C[2] && predict.EvaluationAll[i]> C[1]) Predict.CatAll[i] <- "Meso"
  if (predict.EvaluationAll[i]< C[3] && predict.EvaluationAll[i]> C[2]) Predict.CatAll[i] <- "Eu"
  if (predict.EvaluationAll[i]> C[3]) Predict.CatAll[i] <- "Hyper"
}

Pred.CatAll <- factor(Predict.CatAll, levels=c("Oligo", "Meso", "Eu", "Hyper"), ordered=TRUE)

True.CatAll <- Evaluation[, "TS_Chla_Q"]

# True.CatAll <- True.CatAll[!is.na(log(Evaluation$SECMEAN))]

CM.TS.Multilevel <- confusionMatrix(Pred.CatAll, True.CatAll)
CM.TS.Multilevel
xtable(CM.TS.Multilevel$table)
CM.TS.Multilevel$overall["Accuracy"]
CM.TS.Multilevel$byClass[,"Balanced Accuracy"]
################################################################################
# Functions
################################################################################
expected <- function(x, c1.5, c2.5, c.3.5, sigma){
  p1.5 <- invlogit((x-c1.5)/sigma)
  p2.5 <- invlogit((x-c2.5)/sigma)
  p3.5 <- invlogit((x-c3.5)/sigma)
  return((1*(1-p1.5)+2*(p1.5-p2.5)+3*(p2.5-p3.5)+4*p3.5))
  # return((1*(1-p1.5)+2*(p1.5-p2.5)+3*p2.5))
}

# for plotting logistic regression model
jitter.binary <- function(a, jitt=.05, up=1){
  up*(a + (1-2*a)*runif(length(a),0,jitt))
}

logit <- function(x) return(log(x/(1-x)))

invlogit <- function(x) return(1/(1+exp(-x)))
Model_SubRegion <- matrix(0, dim(Model)[1], length(levels(Model[,"SUBREGIONS"])))
for(j in 1:dim(Model)[1]){
  for(i in 1:length(levels(Model[,"SUBREGIONS"]))){
    if (factor(Model[j, "SUBREGIONS"])==levels(Model[,"SUBREGIONS"])[i]) Model_SubRegion[j,i] <-1
    
  }
}
################################################################################
# Plots
################################################################################

beta <- Alpha[,"mean"]
c1.5 <- C[1]
c2.5 <- C[2]
c3.5 <- C[3]
sigma <- Coeff.Coastal.Summary["s","mean"]

pdf("Figures/POLR.pdf", width=8, height=10)
par(mar=c(3,3,0.25,0.25), mgp=c(1.5,0.25,0), tck=-0.005)
plot(0, 0, xlim=c(-800,700), ylim=c(1,4), xlab="TSI", ylab="TS",
     type="n", axes=F)
axis(1)
axis(2, at=1:4, labels=c("Oligo","Meso","Eutro", "Hyper"), las=1)
lines(rep(c1.5, 2), c(1,2))
lines(rep(c2.5, 2), c(2,3))
lines(rep(c3.5, 2), c(3,4))
curve(expected(x, c1.5, c2.5, c3.5, sigma), add=TRUE)
# curve(expected(x, c1.5, c2.5, sigma), add=TRUE)

with(Model, points(cbind(SDD.C, TN.C, TP.C, DIN.C, DIP.C, Model_SubRegion)%*%beta,
                   jitter.binary(as.numeric(ordered(Model[,"TS_Chla_Q"]))),  col="azure4"))
invisible(dev.off())
#################################################

#################################################
beta_AllVar <-  Alpha[,"mean"]
kappa_AllVar <- C

c1.5_AllVar <- kappa_AllVar[1]
c2.5_AllVar <- kappa_AllVar[2]
c3.5_AllVar <- kappa_AllVar[3]
sigma_AllVar <- Coeff.Coastal.Summary["s","mean"]

X <- cbind(SDD.C, TN.C, TP.C, DIN.C, DIP.C, Model_SubRegion)
TSI <- X%*% beta_AllVar
TSI <- TSI[!is.na(TSI)]
# se of kappas
se.c <-  cbind(Coeff.Coastal.Summary["C[1]","sd"], Coeff.Coastal.Summary["C[2]","sd"], Coeff.Coastal.Summary["C[3]","sd"])
Ibcg <- seq(range(TSI)[1],range(TSI)[2], length.out = 100)
pA <- invlogit((kappa_AllVar[1] - Ibcg)/sigma_AllVar)
pB <- invlogit((kappa_AllVar[2] - Ibcg)/sigma_AllVar) -  invlogit((kappa_AllVar[1] - Ibcg)/sigma_AllVar)
pC <- invlogit((kappa_AllVar[3] - Ibcg)/sigma_AllVar) -  invlogit((kappa_AllVar[2] - Ibcg)/sigma_AllVar)
pNA <- 1.0 - invlogit((kappa_AllVar[3] - Ibcg)/sigma_AllVar)

# Figure
# Graphical presentation of the POLR model. 
# The x-axis is the trophic state index, the y-axis is the probability of being classified into one of the 4 trophic state classes, and the vertical lines and blue bars are the cutpoints $\pm$ one standard error.
pdf("Figures/POLR_Prob.pdf", width=8, height=10)

par(mar=c(3,3,2,0.25), mgp=c(1.5,0.5,0), tck=-0.01)
plot(range(Ibcg), c(0,1), type="n",     xlab="Tropic State Index", ylab="Prob")
polygon(x=c(c1.5_AllVar-se.c[1], c1.5_AllVar+se.c[1], c1.5_AllVar+se.c[1],c1.5_AllVar-se.c[1]),
        y=c(0,0,1,1), col="azure3", density=-1, border=NA)
polygon(x=c(c2.5_AllVar-se.c[2], c2.5_AllVar+se.c[2], c2.5_AllVar+se.c[2],c2.5_AllVar-se.c[2]),
        y=c(0,0,1,1), col="azure3", density=-1, border=NA)
polygon(x=c(c3.5_AllVar-se.c[3], c3.5_AllVar+se.c[3], c3.5_AllVar+se.c[3],c3.5_AllVar-se.c[3]),
        y=c(0,0,1,1), col="azure3", density=-1, border=NA)
segments(x0=c(c1.5_AllVar,c2.5_AllVar,c3.5_AllVar), y0=rep(0,3),
         x1=c(c1.5_AllVar,c2.5_AllVar,c3.5_AllVar), y1=rep(1,3),col=grey(0.3))
axis(3, at=c(c1.5_AllVar,c2.5_AllVar, c3.5_AllVar), labels=c("Oligo|Meso","Meso|Eu" ,"Eu|Hyper"))
lines(Ibcg, pA, lwd=2)
lines(Ibcg, pB, lty=2, lwd=2)
lines(Ibcg, pC, lty=3, lwd=2)
lines(Ibcg, pNA, lty=4, lwd=2)
legend(400, 0.5, legend=c("Oligo", "Meso","Eu", "Hyper"),
       lty=1:4, cex=0.75, bty="n")
invisible(dev.off())

################################################
# All Data

# Center, scale, and log transform the coastal data
Coastal.SDD.C <- as.numeric(scale(log(coastal$SECCHI_MEAN..m.), center = TRUE, scale = TRUE))
Coastal.TN.C <- as.numeric(scale(log(coastal$TN..mgN.L.), center = TRUE, scale = TRUE))
Coastal.TP.C <- as.numeric(scale(log(coastal$TP..mgP.L.), center = TRUE, scale = TRUE))
Coastal.DIN.C <- as.numeric(scale(log(coastal$DIN..mgN.L.), center = TRUE, scale = TRUE))
Coastal.DIP.C <- as.numeric(scale(log(coastal$DIP..mgP.L.), center = TRUE, scale = TRUE))

# Subregion Matrix
SubRegion <- matrix(0, dim(coastal)[1], length(levels(coastal[,"SUBREGIONS"])))
for(j in 1:dim(coastal)[1]){
  for(i in 1:length(levels(coastal[,"SUBREGIONS"]))){
    if (factor(coastal[j, "SUBREGIONS"])==levels(coastal[,"SUBREGIONS"])[i]) 
      SubRegion[j,i] <-1
  }
}

# Evaluation predictors
Coastal.Predictors <- cbind(Coastal.SDD.C, Coastal.TN.C, Coastal.TP.C, Coastal.DIN.C, Coastal.DIP.C, SubRegion)

predict.CoastalAll <- Coastal.Predictors %*% Alpha[,"mean"]
# Remove NA's
predict.CoastalAll <- predict.CoastalAll[!is.na(predict.CoastalAll)]

Predict.CatAll <-  vector(length = length(predict.CoastalAll))
C <- rbind(Coeff.Coastal.Summary["C[1]",],  Coeff.Coastal.Summary["C[2]",],  Coeff.Coastal.Summary["C[3]",])

for (i in 1:length(predict.CoastalAll)){
  if (predict.CoastalAll[i]< C[1]) Predict.CatAll[i] <- "Oligo"
  if (predict.CoastalAll[i]< C[2] && predict.CoastalAll[i]> C[1]) Predict.CatAll[i] <- "Meso"
  if (predict.CoastalAll[i]< C[3] && predict.CoastalAll[i]> C[2]) Predict.CatAll[i] <- "Eu"
  if (predict.CoastalAll[i]> C[3]) Predict.CatAll[i] <- "Hyper"
}

Pred.CatAll <- factor(Predict.CatAll, levels=c("Oligo", "Meso", "Eu", "Hyper"), ordered=TRUE)
coastal2010 <- c(coastal, predict.CoastalAll, Predict.CatAll)


coastal$predict_TSI <- predict.CoastalAll
coastal$predict_Cat  <- Predict.CatAll
View(coastal)
write.csv(coastal, file = "coastal2010.csv")
