
#Read in Data
database <- read.delim("marketing.dat.txt")
database<-database[order(database$ID),]

#or
database=read.csv(file="ChoiceLab.csv",header=TRUE)
database<-database[order(database$ID),]

library(apollo)								# run apollo package
apollo_initialise()

#  Question 1 -------------------------------------------------------------

apollo_beta=c(beta_stay              = 0,
              beta_age               = 0,
              beta_nbempl		         = 0,
              beta_negprofit		     = 0		
)

apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']]   =  beta_stay + beta_age*Age+beta_nbempl*NbEmpl+beta_negprofit*NegProfit
  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion1 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion1)


# Question 2 --------------------------------------------------------------

apollo_control=list(modelName="Fashion 2",
                    modelDescr="Some Description",indivID="ID")

apollo_beta=c(beta_stay              = 0,
              beta_age               = 0,
              beta_nbempl		 = 0,
              beta_negprofit		 = 0		
)

apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  = 0
  V[['stay']] = beta_stay + beta_age*LnAge+beta_nbempl*LnNbEmpl+beta_negprofit*NegProfit
  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion2 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion2)


# Question 3 --------------------------------------------------------------

apollo_control=list(modelName="Fashion 3",
                    modelDescr="Some Description",indivID="ID")

apollo_beta=c(beta_stay                = 0,
              beta_age1                = 0,
              beta_age2                = 0,
              beta_age3                = 0,
              beta_nbempl		           = 0,
              beta_negprofit		       = 0		
)

apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  Age1<-pmin(Age,20)
  Age2<-pmax(0,pmin(Age-20,30))	
  Age3<-pmax(0,Age-50)
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']] =   beta_stay + beta_age1*Age1 + beta_age2*Age2+ beta_age3*Age3 + beta_nbempl*LnNbEmpl+beta_negprofit*NegProfit
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion3 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion3)


# Question 4 --------------------------------------------------------------

apollo_control=list(modelName="Fashion 4",
                    modelDescr="Some Description",indivID="ID")

apollo_beta=c(beta_stay               = 0,
              beta_age                = 0,
              beta_nbempl		          = 0,
              beta_negprofit	        = 0,
              beta_year			          = 0,
              beta_rating	            = 0,
              beta_y_r	              = 0			
)

apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =   0
  V[['stay']]   = beta_stay + beta_age*LnAge + beta_nbempl*LnNbEmpl+beta_negprofit*NegProfit+
    beta_year*(Year==2002)+beta_rating*Rating+beta_y_r*(Year==2002)*Rating  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion4 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion4)

# Question 5 --------------------------------------------------------------

apollo_control=list(modelName="Fashion 4",
                    modelDescr="Some Description",indivID="ID")

apollo_beta=c(beta_stay              = 0,
              beta_age               = 0,
              beta_nbempl		         = 0,
              beta_negprofit	    	 = 0,
              beta_year			         = 0,
              beta_rating		         = 0,
              beta_y_r			         = 0,	
              beta_web		        	 = 0,
              beta_crm			         = 0,
              beta_internet	      	 = 0			
)

apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  = 0
  V[['stay']] =  beta_stay + beta_age*LnAge + beta_nbempl*LnNbEmpl + beta_negprofit*NegProfit +
    beta_year*(Year==2002)+ beta_rating*Rating + beta_y_r*(Year==2002)*Rating +
    beta_web*Web + beta_crm*CRM + beta_internet*Internet  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion5 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion5)

apollo_lrTest(Fashion4,Fashion5)


# Model Comparisons -----------------------------------------

adjust_rho<-function(LLmax,LL0,k){
  
  ret<-1-((LLmax-k)/LL0)
  
  return(ret)
  
}


HoTest<-function(rho_hi,rho_low,L_0,k_high,k_low){
  
  H<- -sqrt(-2*(rho_hi-rho_low)*L_0+k_high-k_low)
  
  return(H)
  
}


#Test Model 1 and 2

k1<-length(Fashion1$estimate)
k2<-length(Fashion2$estimate)

LL0<-Fashion1$LL0
LL1<-Fashion1$LLout
LL2<-Fashion2$LLout

#rho1<-adjust_rho(LL1,LL0,k1)
#rho2<-adjust_rho(LL2,LL0,k2)

rho1<-0.3101
rho2<-0.3173

pnorm(HoTest(rho2,rho1,LL0,k2,k1))


#Test Model 2 and 3

k2<-length(Fashion2$estimate)
k3<-length(Fashion3$estimate)

LL0<-Fashion1$LL0
LL2<-Fashion2$LLout
LL3<-Fashion3$LLout

rho2<-adjust_rho(LL2,LL0,k2)
rho3<-adjust_rho(LL3,LL0,k3)


pnorm(HoTest(rho2,rho3,LL0,k2,k3))




#Likelihood-ratio-tests


apollo_lrTest(Fashion2,Fashion4)
apollo_lrTest(Fashion4,Fashion5)


# Determine the predicted choice probabilities for the chosen alte --------



apollo_beta=c(beta_stay              = 0,
              beta_age               = 0,
              beta_nbempl		 = 0,
              beta_negprofit		 = 0		
)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']]   =  beta_stay + beta_age*Age+beta_nbempl*NbEmpl+beta_negprofit*NegProfit
  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Fashion1 = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(Fashion1)


predictions_new = apollo_prediction(Fashion1, apollo_probabilities, apollo_inputs)

predictions_new<-data.frame(predictions_new)

hist(predictions_new$chosen)







