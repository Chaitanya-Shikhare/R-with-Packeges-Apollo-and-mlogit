library(apollo)
library(tidyverse)
library(readr)
library(GGally)

database <- read_delim("quebec.dat.txt", 
                       "\t", escape_double = FALSE, 
                       trim_ws = TRUE)
database_copy <- database

# Challenge Question 1 ----------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="Base",
                    modelDescr="Basline model: challenge question 1",
                    indivID="obs",
                    panelData = FALSE
                    )

apollo_beta=c(asc_gg     = 0,
              asc_ge     = 0,
              asc_deo    = 0,
              asc_dee    = 0,
              asc_oo     = 0,
              asc_oe     = 0,
              asc_ee     = 0,
              asc_we     = 0,
              asc_wee    = 0,
              b_op_cost  = 0,
              b_fi_cost  = 0
            )

#all coefficients are estimated, none is fixed
apollo_fixed=c("asc_gg")

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  
  P = list()								 ### Create list of probabilities P
  V = list()								 ### List of utilities

  V[["gg"]]  = asc_gg  + b_op_cost * op_cost.1 + b_fi_cost * fix_cost.1
  V[["ge"]]  = asc_ge  + b_op_cost * op_cost.2 + b_fi_cost * fix_cost.2
  V[["deo"]] = asc_deo + b_op_cost * op_cost.3 + b_fi_cost * fix_cost.3
  V[["dee"]] = asc_dee + b_op_cost * op_cost.4 + b_fi_cost * fix_cost.4
  V[["oo"]]  = asc_oo  + b_op_cost * op_cost.5 + b_fi_cost * fix_cost.5
  V[["oe"]]  = asc_oe  + b_op_cost * op_cost.6 + b_fi_cost * fix_cost.6
  V[["ee"]]  = asc_ee  + b_op_cost * op_cost.7 + b_fi_cost * fix_cost.7
  V[["we"]]  = asc_we  + b_op_cost * op_cost.8 + b_fi_cost * fix_cost.8
  V[["wee"]] = asc_wee + b_op_cost * op_cost.9 + b_fi_cost * fix_cost.9
  
  mnl_settings = list(						        
    alternatives = c(gg=1,ge=2,deo=3,dee=4,oo=5,oe=6,ee=7,we=8,wee=9),
    avail        = list(gg=avail.1,
                        ge=avail.2,
                        deo=avail.3,
                        dee=avail.4,
                        oo=avail.5,
                        oe=avail.6,
                        ee=avail.7,
                        we=avail.8,
                        wee=avail.9), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
#  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
}

Model.0 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Model.0)


# Challenge Question 2 ----------------------------------------------------

Model.0.pred = apollo_prediction(Model.0, apollo_probabilities, apollo_inputs )
head(Model.0.pred, 1)


# Challenge Question 3 ----------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="Model 1",
                    modelDescr="Income-FixedCost Interaction",
                    indivID="obs",
                    panelData = FALSE
)

apollo_beta=c(asc_gg     = 0,
              asc_ge     = 0,
              asc_deo    = 0,
              asc_dee    = 0,
              asc_oo     = 0,
              asc_oe     = 0,
              asc_ee     = 0,
              asc_we     = 0,
              asc_wee    = 0,
              b_op_cost  = 0,
              b_fi_cost  = 0,
              b_in_fi_co = 0,
              inc_gg     = 0,
              inc_ge     = 0,
              inc_deo    = 0,
              inc_dee    = 0,
              inc_oo     = 0,
              inc_oe     = 0,
              inc_ee     = 0,
              inc_we     = 0,
              inc_wee    = 0
)

#all coefficients are estimated, none is fixed
apollo_fixed=c("asc_gg", "inc_gg")

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  
  P = list()								 ### Create list of probabilities P
  V = list()								 ### List of utilities
  
  V[["gg"]]  = asc_gg  + b_op_cost * op_cost.1 + (b_fi_cost + b_in_fi_co * income )* fix_cost.1 + inc_gg  * income 
  V[["ge"]]  = asc_ge  + b_op_cost * op_cost.2 + (b_fi_cost + b_in_fi_co * income )* fix_cost.2 + inc_ge  * income 
  V[["deo"]] = asc_deo + b_op_cost * op_cost.3 + (b_fi_cost + b_in_fi_co * income )* fix_cost.3 + inc_deo * income
  V[["dee"]] = asc_dee + b_op_cost * op_cost.4 + (b_fi_cost + b_in_fi_co * income )* fix_cost.4 + inc_dee * income
  V[["oo"]]  = asc_oo  + b_op_cost * op_cost.5 + (b_fi_cost + b_in_fi_co * income )* fix_cost.5 + inc_oo  * income 
  V[["oe"]]  = asc_oe  + b_op_cost * op_cost.6 + (b_fi_cost + b_in_fi_co * income )* fix_cost.6 + inc_oe  * income 
  V[["ee"]]  = asc_ee  + b_op_cost * op_cost.7 + (b_fi_cost + b_in_fi_co * income )* fix_cost.7 + inc_ee  * income 
  V[["we"]]  = asc_we  + b_op_cost * op_cost.8 + (b_fi_cost + b_in_fi_co * income )* fix_cost.8 + inc_we  * income 
  V[["wee"]] = asc_wee + b_op_cost * op_cost.9 + (b_fi_cost + b_in_fi_co * income )* fix_cost.9 + inc_wee * income
  
  mnl_settings = list(						        
    alternatives = c(gg=1,ge=2,deo=3,dee=4,oo=5,oe=6,ee=7,we=8,wee=9),
    avail        = list(gg=avail.1,
                        ge=avail.2,
                        deo=avail.3,
                        dee=avail.4,
                        oo=avail.5,
                        oe=avail.6,
                        ee=avail.7,
                        we=avail.8,
                        wee=avail.9), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  #  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
}

Model.1 = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(Model.1)

## compare models ##
apollo_lrTest(Model.0, Model.1)

### plot fixed cost coefficient ###
Income <- seq(min(database$income), max(database$income),0.01)
beta_fix_cost <- Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * Income
plot.income.fix.cost <-  data.frame(cbind(Income, beta_fix_cost))

ggplot(plot.income.fix.cost, aes(Income, beta_fix_cost)) + 
  geom_line() ## we may add the empirical coefficents as points

ggplot(database, aes(fix_cost.8, op_cost.8)) + 
  geom_point() +
  stat_smooth(method = "loess", col = "red")


# Challenge Question 4 ----------------------------------------------------
# probabilities
Model.1.pred = apollo_prediction(Model.1, apollo_probabilities, apollo_inputs )


# fixed cost direct elasticities
Model.1.pred$fc.elast.gg <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
                            database$fix_cost.1 * (1-Model.1.pred$gg)

Model.1.pred$fc.elast.ge <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.2 * (1-Model.1.pred$ge)

Model.1.pred$fc.elast.deo <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.3 * (1-Model.1.pred$deo)

Model.1.pred$fc.elast.dee <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.4 * (1-Model.1.pred$dee)

Model.1.pred$fc.elast.oo <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.5 * (1-Model.1.pred$oo)

Model.1.pred$fc.elast.oe <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.6 * (1-Model.1.pred$oe)

Model.1.pred$fc.elast.ee <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.7 * (1-Model.1.pred$ee)

Model.1.pred$fc.elast.we <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.8 * (1-Model.1.pred$we)

Model.1.pred$fc.elast.wee <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) * #fixed cost coefficient
  database$fix_cost.9 * (1-Model.1.pred$wee)

# operational cost direct elasticities
Model.1.pred$oc.elast.gg  <- Model.1$estimate["b_op_cost"] * database$op_cost.1 * (1-Model.1.pred$gg)
Model.1.pred$oc.elast.ge  <- Model.1$estimate["b_op_cost"] * database$op_cost.2 * (1-Model.1.pred$ge)
Model.1.pred$oc.elast.deo <- Model.1$estimate["b_op_cost"] * database$op_cost.3 * (1-Model.1.pred$deo)
Model.1.pred$oc.elast.dee <- Model.1$estimate["b_op_cost"] * database$op_cost.4 * (1-Model.1.pred$dee)
Model.1.pred$oc.elast.oo  <- Model.1$estimate["b_op_cost"] * database$op_cost.5 * (1-Model.1.pred$oo)
Model.1.pred$oc.elast.oe  <- Model.1$estimate["b_op_cost"] * database$op_cost.6 * (1-Model.1.pred$oe)
Model.1.pred$oc.elast.ee  <- Model.1$estimate["b_op_cost"] * database$op_cost.7 * (1-Model.1.pred$ee)
Model.1.pred$oc.elast.we  <- Model.1$estimate["b_op_cost"] * database$op_cost.8 * (1-Model.1.pred$we)
Model.1.pred$oc.elast.wee <- Model.1$estimate["b_op_cost"] * database$op_cost.9 * (1-Model.1.pred$wee)

# histograms
ggplot(Model.1.pred, aes(Model.1.pred$fc.elast.ee) ) + 
  geom_histogram()
# mostly positive because fixed cost coefficient is mostly positive



Model.1.pred$fix_cost_ee <- database$fix_cost.7 # add fixed cost of ee alternative (nr. 7) to dataframe for plotting purposes
Model.1.pred$choice <- database$choice
ggplot(Model.1.pred, aes(x=Model.1.pred$fix_cost_ee, y=Model.1.pred$fc.elast.ee)) +
  geom_point(aes(color=factor(Model.1.pred$choice))) +
  stat_smooth(method = "loess",
              col = "red",
              size = 1)


# Challenge Question 5 ----------------------------------------------------

# cross price elasticities for alternative oil-oil (oo, alt. 5) - fix cost
Model.1.pred$cross.elast.fc.oo <- - (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income) *
  database$fix_cost.5 * Model.1.pred$oo
# op cost
Model.1.pred$cross.elast.oc.oo  <- - Model.1$estimate["b_op_cost"] * database$op_cost.5 * (1-Model.1.pred$oo)

summary(Model.1.pred$cross.elast.fc.oo) # mostly negative: if fix cost oo increase then probability of other alternatives DECREASE (positive coefficient)
summary(Model.1.pred$cross.elast.oc.oo) # >> 1 : if op cost oo increase, then probabilities of other alternatives increase (elastic!!!!)

#  #############################################################################################
## Compare versus Apollo Manual: example of direct elasticities of oo-alternative (alt. 5) ##
change = 1.01
database$op_cost.5 = change*database$op_cost.5 
### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
Model.1.pred.new = apollo_prediction(Model.1, apollo_probabilities, apollo_inputs)
### Return to original data
database$op_cost.5 = (1/change)*database$op_cost.5

log(sum(Model.1.pred.new$oo)/sum(Model.1.pred$oo))/log(change)
summary(Model.1.pred$oc.elast.oo)
### take closer look on differences ###
#  #############################################################################################


# Challenge Question 6 ----------------------------------------------------

# Test for IIA

database_copy <- database

database <- subset(database, database$choice != 1)

apollo_initialise()

apollo_control=list(modelName="Model 1 IIA",
                    modelDescr="Reduced Choice Set for IIA Test",
                    indivID="obs",
                    panelData = FALSE
)

apollo_beta=c(asc_gg =0,
              asc_ge     = 0,
              asc_deo    = 0,
              asc_dee    = 0,
              asc_oo     = 0,
              asc_oe     = 0,
              asc_ee     = 0,
              asc_we     = 0,
              asc_wee    = 0,
              b_op_cost  = 0,
              b_fi_cost  = 0,
              b_in_fi_co = 0,
              inc_gg     = 0,
              inc_ge     = 0,
              inc_deo    = 0,
              inc_dee    = 0,
              inc_oo     = 0,
              inc_oe     = 0,
              inc_ee     = 0,
              inc_we     = 0,
              inc_wee    = 0
              
)

#all coefficients are estimated, none is fixed
apollo_fixed=c("asc_ge", "inc_ge", "asc_gg", "inc_gg")

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  
  P = list()								 ### Create list of probabilities P
  V = list()								 ### List of utilities
  
  V[["gg"]]  = asc_gg  + b_op_cost * op_cost.1 + (b_fi_cost + b_in_fi_co * income )* fix_cost.1 + inc_gg  * income 
  V[["ge"]]  = asc_ge  + b_op_cost * op_cost.2 + (b_fi_cost + b_in_fi_co * income )* fix_cost.2 + inc_ge  * income 
  V[["deo"]] = asc_deo + b_op_cost * op_cost.3 + (b_fi_cost + b_in_fi_co * income )* fix_cost.3 + inc_deo * income
  V[["dee"]] = asc_dee + b_op_cost * op_cost.4 + (b_fi_cost + b_in_fi_co * income )* fix_cost.4 + inc_dee * income
  V[["oo"]]  = asc_oo  + b_op_cost * op_cost.5 + (b_fi_cost + b_in_fi_co * income )* fix_cost.5 + inc_oo  * income 
  V[["oe"]]  = asc_oe  + b_op_cost * op_cost.6 + (b_fi_cost + b_in_fi_co * income )* fix_cost.6 + inc_oe  * income 
  V[["ee"]]  = asc_ee  + b_op_cost * op_cost.7 + (b_fi_cost + b_in_fi_co * income )* fix_cost.7 + inc_ee  * income 
  V[["we"]]  = asc_we  + b_op_cost * op_cost.8 + (b_fi_cost + b_in_fi_co * income )* fix_cost.8 + inc_we  * income 
  V[["wee"]] = asc_wee + b_op_cost * op_cost.9 + (b_fi_cost + b_in_fi_co * income )* fix_cost.9 + inc_wee * income
  
  mnl_settings = list(						        
    alternatives = c(gg=1,
                     ge=2,deo=3,dee=4,oo=5,oe=6,ee=7,we=8,wee=9),
    avail        = list(gg=0, ###   make alternative gas-gas (gg, alt 1) not available for IIA test
                        ge=avail.2,
                        deo=avail.3,
                        dee=avail.4,
                        oo=avail.5,
                        oe=avail.6,
                        ee=avail.7,
                        we=avail.8,
                        wee=avail.9), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
}

Model.1.IIA = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(Model.1.IIA)


full.m.coef <- Model.1$estimate[c(3:12, 15:21)]
sub.m.coef <- Model.1.IIA$estimate[c(3:12, 15:21)]
length(full.m.coef)
length(sub.m.coef)
full.m.vc <- Model.1$robvarcov[c(2:11, 13:19),c(2:11, 13:19)]
sub.m.vc <- Model.1.IIA$robvarcov

hmft <- ( (sub.m.coef- full.m.coef) %*% 
          solve(sub.m.vc - full.m.vc) 
          ) %*%  (sub.m.coef- full.m.coef)
abs(hmft)
qchisq(.05, length(Model.1.IIA$gradient), lower.tail = TRUE)


database <- database_copy

# Challenge Question 7 & 8 ----------------------------------------------------

# wtp fc for a one unit reduction in op

database$wtp.fc <- Model.1$estimate["b_op_cost"] / 
  (Model.1$estimate["b_fi_cost"] + 
     Model.1$estimate["b_in_fi_co"] * database$income 
   )
summary(database$wtp.fc)
# histograms
ggplot(database, aes(database$wtp.fc) ) + 
  geom_histogram(binwidth = 1.5)

# Question 8:
ggplot(database, aes(database$income, database$wtp.fc)) + geom_point()
zero.break <-  - Model.1$estimate["b_fi_cost"] / Model.1$estimate["b_in_fi_co"]
database$wtp.fc.positive <- "negative cost coefficient"
database$wtp.fc.positive[database$income > zero.break] <- "positive cost coefficient"
ggplot(database, aes(database$income, database$wtp.fc)) + geom_point() +
  geom_line(aes(color=database$wtp.fc.positive))

# Challenge Question 9 & 10 ----------------------------------------------------

alpha <- Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * database$income
r <- alpha / Model.1$estimate["b_op_cost"]
alpha_r <- alpha/r


summary(alpha)
summary(r)
summary(alpha_r)

r_32k <- (Model.1$estimate["b_fi_cost"] + Model.1$estimate["b_in_fi_co"] * 3.25) / Model.1$estimate["b_op_cost"]
r_32k


# Challenge Question 11 ---------------------------------------------------

# gas alternatives: gg(1), ge(2)

database <- database_copy
database$op_cost.1 = 1.15 * database$op_cost.1 
database$op_cost.2 = 1.15 * database$op_cost.2 

apollo_inputs = apollo_validateInputs()
Model.1.pred15pct = apollo_prediction(Model.1, apollo_probabilities, apollo_inputs )

# plot shares
marketshares <- data.frame(colSums(Model.1.pred15pct)[3:11])
marketshares <- add_rownames(marketshares, var = "alternatives")
ggplot(marketshares, aes(x="", y=colSums.Model.1.pred15pct..3.11., fill=alternatives)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# Challenge Question 12 ---------------------------------------------------

apollo_initialise()
apollo_control=list(modelName="Model 2",modelDescr="Improved MNL",indivID="obs",panelData = FALSE)

apollo_beta=c(asc_gg     = 0,
              asc_ge     = 0,
              asc_deo    = 0,
              asc_dee    = 0,
              asc_oo     = 0,
              asc_oe     = 0,
              asc_ee     = 0,
              asc_we     = 0,
              asc_wee    = 0,
              b_op_cost  = 0,
              b_fi_cost  = 0,
              #b_in_fi_co = 0,
              
              b_fci_ru   = 0,
              b_fci_su   = 0,
              b_fci_ur   = 0,
              b_fci_hd   = 0,
              
              inc_gg     = 0,
              inc_ge     = 0,
              inc_deo    = 0,
              inc_dee    = 0,
              inc_oo     = 0,
              inc_oe     = 0,
              inc_ee     = 0,
              inc_we     = 0,
              inc_wee    = 0,
              
              b_d4h_we     = 0,
              b_d4h_wee    = 0,
              
              b_own_we     = 0,
              b_own_wee    = 0,
              
              b_nop_gg     = 0,
              b_nop_ge     = 0,
              b_nop_deo    = 0,
              b_nop_dee    = 0,
              b_nop_oo     = 0,
              b_nop_oe     = 0,
              b_nop_ee     = 0,
              b_nop_we     = 0,
              b_nop_wee    = 0
)

apollo_fixed=c("asc_gg", "inc_gg", "b_nop_gg"
               #, "b_fci_su", "b_fci_ur"
               )

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)			 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 		
  P = list()								 ### Create list of probabilities P
  V = list()								 ### List of utilities
  
  ## interaction
  #b_ficost_val = b_fi_cost + b_in_fi_co * income
  b_ficost_val = b_fi_cost + (  b_fci_ru * (sector == 1) +
                                b_fci_su * (sector == 2) +
                                b_fci_ur * (sector == 3) +  
                                b_fci_hd * (sector == 4) 
                              ) * income
  var_d4h = (surface/nb_rooms) * hdd
    
  V[["gg"]]  = asc_gg  + b_nop_gg  * nb_pers +                                                     b_op_cost * log(1+op_cost.1) + b_ficost_val * log(1+fix_cost.1) + inc_gg  * income 
  V[["ge"]]  = asc_ge  + b_nop_ge  * nb_pers +                                                     b_op_cost * log(1+op_cost.2) + b_ficost_val * log(1+fix_cost.2) + inc_ge  * income 
  V[["deo"]] = asc_deo + b_nop_deo * nb_pers +                                                     b_op_cost * log(1+op_cost.3) + b_ficost_val * log(1+fix_cost.3) + inc_deo * income
  V[["dee"]] = asc_dee + b_nop_dee * nb_pers +                                                     b_op_cost * log(1+op_cost.4) + b_ficost_val * log(1+fix_cost.4) + inc_dee * income
  V[["oo"]]  = asc_oo  + b_nop_oo  * nb_pers +                                                     b_op_cost * log(1+op_cost.5) + b_ficost_val * log(1+fix_cost.5) + inc_oo  * income 
  V[["oe"]]  = asc_oe  + b_nop_oe  * nb_pers +                                                     b_op_cost * log(1+op_cost.6) + b_ficost_val * log(1+fix_cost.6) + inc_oe  * income 
  V[["ee"]]  = asc_ee  + b_nop_ee  * nb_pers +                                                     b_op_cost * log(1+op_cost.7) + b_ficost_val * log(1+fix_cost.7) + inc_ee  * income 
  V[["we"]]  = asc_we  + b_nop_we  * nb_pers + b_own_we  * (conv_year > 3) + b_d4h_we  * var_d4h + b_op_cost * log(1+op_cost.8) + b_ficost_val * log(1+fix_cost.8) + inc_we  * income 
  V[["wee"]] = asc_wee + b_nop_wee * nb_pers + b_own_wee * (conv_year > 3) + b_d4h_wee * var_d4h + b_op_cost * log(1+op_cost.9) + b_ficost_val * log(1+fix_cost.9) + inc_wee * income
  
  mnl_settings   = list(						        
    alternatives = c(gg=1,ge=2,deo=3,dee=4,oo=5,oe=6,ee=7,we=8,wee=9),
    avail        = list(gg=avail.1,ge=avail.2,deo=avail.3,dee=avail.4,oo=avail.5,oe=avail.6,ee=avail.7,we=avail.8,wee=avail.9), 
    choiceVar    = choice,          
    V            = V)
  
  P[["model"]]   = apollo_mnl(mnl_settings, functionality)	 
  P              = apollo_prepareProb(P, apollo_inputs, functionality)	 
  return(P)
}

Model.2 = apollo_estimate(apollo_beta,apollo_fixed,apollo_probabilities,apollo_inputs)
apollo_modelOutput(Model.2)

beta_fix_cost_Model2 = Model.2$estimate["b_fi_cost"] + (
    Model.2$estimate["b_fci_ru"] * (database$sector == 1) +
    Model.2$estimate["b_fci_su"] * (database$sector == 2) +
    Model.2$estimate["b_fci_ur"] * (database$sector == 3) +
    Model.2$estimate["b_fci_hd"] * (database$sector == 4) 
  ) * database$income

summary(beta_fix_cost_Model2)

plot.income.fix.cost$b.fc.M2.ru <- Model.2$estimate["b_fi_cost"] + (Model.2$estimate["b_fci_ru"]  ) * Income 
plot.income.fix.cost$b.fc.M2.su <- Model.2$estimate["b_fi_cost"] + (Model.2$estimate["b_fci_su"]  ) * Income 
plot.income.fix.cost$b.fc.M2.ur <- Model.2$estimate["b_fi_cost"] + (Model.2$estimate["b_fci_ur"]  ) * Income 
plot.income.fix.cost$b.fc.M2.hd <- Model.2$estimate["b_fi_cost"] + (Model.2$estimate["b_fci_hd"]  ) * Income 

fixed.cost.beta <- gather(plot.income.fix.cost, sector, beta, 2:6)

ggplot(fixed.cost.beta, aes(Income, beta)) + 
  geom_line(aes(color=sector)) ## we may add the empirical coefficents as points


names(database)

database$chosenProbs <- Model.2$avgCP
ggplot(database, aes(chosenProbs) ) + 
  geom_histogram(binwidth = 0.01) + 
  facet_wrap(~ choice, scales = "free")

outliers <- subset(database, database$choice > 7 & database$chosenProbs < 0.1)
summary(outliers)
ggpairs(outliers, columns = c(2,3,5:12,50), ggplot2::aes(colour=factor(choice))) 


# shares test für sector, dann fix price coefficient für sector

sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(gg=1,ge=2,deo=3,dee=4,oo=5,oe=6,ee=7,we=8,wee=9)
sharesTest_settings[["choiceVar"]] = database$choice
sharesTest_settings[["subsamples"]] = list(rural     =(database$sector==1),
                                           smallUrban=(database$sector==2),
                                           urban     =(database$sector==3),
                                           dense     =(database$sector ==4)
                                           )
apollo_inputs = apollo_validateInputs()
apollo_sharesTest(Model.2,apollo_probabilities,apollo_inputs,sharesTest_settings)





