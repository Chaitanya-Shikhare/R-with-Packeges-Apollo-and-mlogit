install.packages("apollo")
library(apollo)
apollo_initialise()

database=read.csv(file="ModeChoiceData.csv")
database = subset(database, choice == 1 | choice == 4)

apollo_control=list(modeName="Spec5",
                    modelDescr="Specification 5 answering challenge questions", indivID="ID")

apollo_beta=c(asc_rail              = 0,
              b_cost                = 0,
              b_tt_car              = 0,
              b_tt_rail             = 0,
              b_business            = 0,
              b_female              = 0,
              b_wifi                = 0)

apollo_fixed=NULL
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['car']]  = 	 0        * 1 +
    b_cost   * cost_car +
    b_tt_car * time_car
  
  V[['rail']] =    asc_rail * 1 + 
    b_wifi     * (service_rail==2) + 
    b_female   * female + 
    b_business * business + 
    b_cost     * cost_rail  + 
    b_tt_rail  * time_rail
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(car=1, rail=4),					 ### assign names to values in choice column
    avail        = list(car=av_car, rail=av_rail), ## choice set
    choiceVar    = choice,                     ## column name that stores the choices (here "choice")
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID; needed for panel structure
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}

Spec5 = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(Spec5)
