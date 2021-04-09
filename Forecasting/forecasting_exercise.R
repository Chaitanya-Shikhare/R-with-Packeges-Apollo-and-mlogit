
library(apollo)
library(tidyverse)
library(readr)
library(GGally)
library(pROC)
library(fastDummies)

rm(list = ls())

database <- read.delim("telephone.dat.txt")
database$ID <- as.integer(seq(1,length(database$choice),1))
database_copy <- database

# Challenge Question 1 ----------------------------------------------------

apollo_initialise()
apollo_control=list(modelName="Base",
                    modelDescr="Basline model: challenge question 1",
                    indivID="ID"
                    ,panelData = FALSE
)

apollo_beta=c(asc_BM     = 0,
              asc_SM     = 0,
              asc_LF     = 0,
              asc_EF     = 0,
              asc_MF     = 0,
              b_cost     = 0
)

apollo_fixed=c("asc_SM")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  
  P = list()	
  V = list()
  
  V[["BM"]] = asc_BM + b_cost * log(cost1) 
  V[["SM"]] = asc_SM + b_cost * log(cost2) 
  V[["LF"]] = asc_LF + b_cost * log(cost3) 
  V[["EF"]] = asc_EF + b_cost * log(cost4) 
  V[["MF"]] = asc_MF + b_cost * log(cost5) 
  
  mnl_settings   = list(						        
    alternatives = c(BM=1,SM=2,LF=3,EF=4,MF=5),
    avail        = list(BM=avail1,
                        SM=avail2,
                        LF=avail3,
                        EF=avail4,
                        MF=avail5), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  return(P)
}

Model.0 = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)
apollo_modelOutput(Model.0)


## Market Segmentation Prediction Test
sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(BM=1,SM=2,LF=3,EF=4,MF=5)
sharesTest_settings[["choiceVar"]]    = database$choice
sharesTest_settings[["subsamples"]]   = list(
  inc10k = (database$inc == 1),
  inc20k = (database$inc == 2),
  inc30k = (database$inc == 3),
  inc40k = (database$inc == 4),
  inc50k = (database$inc == 5)
)
apollo_inputs = apollo_validateInputs()
apollo_sharesTest(Model.0,
                  apollo_probabilities,
                  apollo_inputs,
                  sharesTest_settings)

## cross Validation / Out of Sample Test
apollo_outOfSample(apollo_beta, apollo_fixed,
                   apollo_probabilities, apollo_inputs)

mean(CV_base_ll$outOfSample_model)

# ROC Base Model
apollo_inputs = apollo_validateInputs()
M.0.predict = data.frame(
  apollo_prediction(Model.0, apollo_probabilities, apollo_inputs)
)

roc.M.0 <- cbind(M.0.predict[3:7],dummy_cols(database$choice))

roc.BM.M0 <- roc(roc.M.0$.data_1, roc.M.0$BM)
roc.SM.M0 <- roc(roc.M.0$.data_2, roc.M.0$SM)
roc.LF.M0 <- roc(roc.M.0$.data_3, roc.M.0$LF)
roc.EF.M0 <- roc(roc.M.0$.data_4, roc.M.0$EF)
roc.MF.M0 <- roc(roc.M.0$.data_5, roc.M.0$MF)

ggroc(list(roc.BM.M0,roc.SM.M0,roc.LF.M0,roc.EF.M0,roc.MF.M0), legacy.axes=TRUE) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1),color="darkgrey",linetype="dashed") +
  scale_color_discrete(name = "Altern.", 
                       labels = c("BM","SM","LF","EF","MF")
  ) +
  ggtitle("Base Model") +
  theme_bw()

# Policy Prediction --------------------------------------------------------------

# ini
bm.price.mod <- 1
bm.cost.scen <- data.frame(Market.Share= numeric(0), 
                           Avg.Revenues=numeric(0), 
                           BM.Price.Factor = numeric(0),
                           Alternative =character(0))

## begin for loop
for (bm.price.mod in seq(0.5,1.5,0.05) ) {
  
  database[database$area == 2,]$cost1 <- 
    database[database$area == 2,]$cost1 * bm.price.mod
  apollo_inputs = apollo_validateInputs()
  Model.0.pred = data.frame(apollo_prediction(Model.0, 
                                              apollo_probabilities, 
                                              apollo_inputs )
  )
  # sample market shares
  m.shares <- colMeans(Model.0.pred[,3:7])
  # average sample revenues
  revenues <- c(
    mean(Model.0.pred$BM * database$cost1),
    mean(Model.0.pred$SM * database$cost2),
    mean(Model.0.pred$LF * database$cost3),
    mean(Model.0.pred$EF * database$cost4),
    mean(Model.0.pred$MF * database$cost5)
  )
  bm.cost.scen <- rbind(bm.cost.scen,
                        cbind(m.shares, revenues, bm.price.mod, Alternative=c("BM","SM","LF","EF","MF"))
  )
  database <- database_copy
  ## end for loop
}

ggplot(bm.cost.scen, aes(x=bm.price.mod, 
                         y=as.numeric(m.shares),
                         fill = factor(Alternative) 
) ) + 
  scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity")

ggplot(bm.cost.scen, aes(x=as.numeric(bm.price.mod), 
                         y=as.numeric(revenues), 
                         fill=factor(Alternative))) + 
  scale_fill_brewer(palette = "Set1") +
  geom_area()



# Challenge Question 2 ----------------------------------------------------

apollo_initialise()
apollo_control=list(modelName  ="Model 1",
                    modelDescr ="Enhanced model according to Q2",
                    indivID    ="ID",
                    panelData  = FALSE
)

apollo_beta=c(asc_BM     = 0,
              asc_SM     = 0,
              asc_LF     = 0,
              asc_EF     = 0,
              asc_MF     = 0,
              
              b_usr_flat = 0,
              
              b_co_in_1  = 0,
              b_co_in_2  = 0,
              b_co_in_3  = 0,
              b_co_in_4  = 0,
              b_co_in_5  = 0
              
)

apollo_fixed=c("asc_SM")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  P = list()								
  V = list()								 
  
  beta_cost =  b_co_in_1 * (inc == 1) + 
    b_co_in_2 * (inc == 2) +
    b_co_in_3 * (inc == 3) +                   
    b_co_in_4 * (inc == 4) +
    b_co_in_5 * (inc == 5) 
  
  #emus = users/(1+employ)
  
  V[["BM"]] = asc_BM +                    + beta_cost * log(cost1) 
  V[["SM"]] = asc_SM +                    + beta_cost * log(cost2) 
  V[["LF"]] = asc_LF + b_usr_flat * users + beta_cost * log(cost3) 
  V[["EF"]] = asc_EF + b_usr_flat * users + beta_cost * log(cost4) 
  V[["MF"]] = asc_MF + b_usr_flat * users + beta_cost * log(cost5) 
  
  mnl_settings   = list(						        
    alternatives = c(BM=1,SM=2,LF=3,EF=4,MF=5),
    avail        = list(BM=avail1,
                        SM=avail2,
                        LF=avail3,
                        EF=avail4,
                        MF=avail5), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  return(P)
}

Model.1 = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)
apollo_modelOutput(Model.1)

## Market Segmentation Prediction Test

sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(BM=1,SM=2,LF=3,EF=4,MF=5)
sharesTest_settings[["choiceVar"]]    = database$choice
sharesTest_settings[["subsamples"]]   = list(
  inc10k = (database$inc == 1),
  inc20k = (database$inc == 2),
  inc30k = (database$inc == 3),
  inc40k = (database$inc == 4),
  inc50k = (database$inc == 5)
)
apollo_inputs = apollo_validateInputs()
apollo_sharesTest(Model.1,apollo_probabilities,apollo_inputs,sharesTest_settings)

## cross Validation / Out of Sample Test
apollo_outOfSample(apollo_beta, apollo_fixed,
                   apollo_probabilities, apollo_inputs)

mean(CV_ll_Model1$outOfSample_model)


# ROC Model 1
apollo_inputs = apollo_validateInputs()
M.1.predict = data.frame(
  apollo_prediction(Model.1, apollo_probabilities, apollo_inputs)
)

roc.M.1 <- cbind(M.1.predict[3:7],dummy_cols(database$choice))

roc.BM.M1 <- roc(roc.M.1$.data_1, roc.M.1$BM)
roc.SM.M1 <- roc(roc.M.1$.data_2, roc.M.1$SM)
roc.LF.M1 <- roc(roc.M.1$.data_3, roc.M.1$LF)
roc.EF.M1 <- roc(roc.M.1$.data_4, roc.M.1$EF)
roc.MF.M1 <- roc(roc.M.1$.data_5, roc.M.1$MF)

ggroc(list(roc.BM.M1,roc.SM.M1,roc.LF.M1,roc.EF.M1,roc.MF.M1), legacy.axes=TRUE) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1),color="darkgrey",linetype="dashed") +
  scale_color_discrete(name = "Altern.", 
                       labels = c("BM","SM","LF","EF","MF")
  ) +
  ggtitle("Model 1") +
  theme_bw()

auc.M.0 <- c(roc.BM.M0$auc,roc.SM.M0$auc,roc.LF.M0$auc,roc.EF.M0$auc,roc.MF.M0$auc)
auc.M.1 <- c(roc.BM.M1$auc,roc.SM.M1$auc,roc.LF.M1$auc,roc.EF.M1$auc,roc.MF.M1$auc)
auc.M.0
auc.M.1



# Challenge Question 3 ----------------------------------------------------


# alle Kosten variieren (ausser EF)
rev.scen <- data.frame(
  Avg.Revenues    = numeric(0), 
  Alternative     = character(0),
  BM.Price.Factor = numeric(0),
  SM.Price.Factor = numeric(0),
  LF.Price.Factor = numeric(0),
  MF.Price.Factor = numeric(0),
  Scenario        = numeric(0)
)
scen <- 1
## begin for loop
for (bm.price.mod in seq(0.9,1.1,0.1) ) {
  for(sm.price.mod in seq(0.9,1.1,0.1) ) {
    for(lf.price.mod in seq(0.9,1.1,0.1) ) {
      for(mf.price.mod in seq(0.9,1.1,0.1) ) {
        database$cost1 <- database$cost1 * bm.price.mod
        database$cost2 <- database$cost2 * sm.price.mod
        database$cost3 <- database$cost3 * lf.price.mod
        database$cost5 <- database$cost5 * mf.price.mod
        apollo_inputs = apollo_validateInputs()
        Model.1.cost.scen = data.frame(apollo_prediction(Model.1, apollo_probabilities, apollo_inputs ))
        revenues <- c(
          mean(Model.1.cost.scen$BM * database$cost1),
          mean(Model.1.cost.scen$SM * database$cost2),
          mean(Model.1.cost.scen$LF * database$cost3),
          mean(Model.1.cost.scen$EF * database$cost4),
          mean(Model.1.cost.scen$MF * database$cost5)
        )
        rev.scen <- rbind(rev.scen,
                          cbind(revenues,  
                                Alternative=c("BM","SM","LF","EF","MF"),
                                bm.price.mod,
                                sm.price.mod,
                                lf.price.mod,
                                mf.price.mod,scen
                          )
        )
        database <- database_copy 
        scen <- scen + 1
      } ## end mf for loop
    } ## end lf for loop
  }## end sm for loop
}## end bm for loop


ggplot(rev.scen, aes(x=as.numeric(scen), 
                     y=as.numeric(revenues),
                     fill = factor(Alternative) 
) ) + 
  scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity")


# Challenge Question 4 ----------------------------------------------------

## true market shares
true_BM = 0.20
true_SM = 0.25
true_LF = 0.35
true_EF = 0.05
true_MF = 0.15

Model.1.New <- Model.1
crit = 100

while(crit > 0.005){
  
  apollo_inputs = apollo_validateInputs()
  pred.M1.ASC = data.frame(apollo_prediction(Model.1.New, apollo_probabilities, apollo_inputs ))
  
  Model.1.New$estimate["asc_BM"] = Model.1.New$estimate["asc_BM"] + log(true_BM / mean(pred.M1.ASC$BM))
  Model.1.New$estimate["asc_SM"] = Model.1.New$estimate["asc_SM"] + log(true_SM / mean(pred.M1.ASC$SM))
  Model.1.New$estimate["asc_LF"] = Model.1.New$estimate["asc_LF"] + log(true_LF / mean(pred.M1.ASC$LF))
  Model.1.New$estimate["asc_EF"] = Model.1.New$estimate["asc_EF"] + log(true_EF / mean(pred.M1.ASC$EF))
  Model.1.New$estimate["asc_MF"] = Model.1.New$estimate["asc_MF"] + log(true_MF / mean(pred.M1.ASC$MF))
  
  crit = mean(
    abs(true_BM - mean(pred.M1.ASC$BM)),
    abs(true_SM - mean(pred.M1.ASC$SM)),
    abs(true_LF - mean(pred.M1.ASC$LF)),
    abs(true_EF - mean(pred.M1.ASC$EF)),
    abs(true_MF - mean(pred.M1.ASC$MF))
  )
  colMeans(pred.M1.ASC)
  crit
}

colMeans(pred.M1.ASC)
crit


