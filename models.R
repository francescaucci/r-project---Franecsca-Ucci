# Basic logit model creation - MODEL 1

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model 1",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)


apollo_beta=c(b_alt1 =0,
              b_alt2=0,
              b_wald_ =0,
              b_groe_ =0,
              b_FoUnt_ =0,
              b_ConSh_  =0,
              b_HaAge_ =0,
              b_prei_ =0)


# no fix parameters
apollo_fixed = c()

# to validate: 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P=list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_wald_*wald_1 + b_groe_*groe_1 + b_FoUnt_*FoUnt_1 + b_ConSh_*ConSh_1 + b_HaAge_*HaAge_1 + b_prei_*prei_1
  V[['alt2']] = b_alt2 + b_wald_*wald_2 + b_groe_*groe_2 + b_FoUnt_*FoUnt_2 + b_ConSh_*ConSh_2 + b_HaAge_*HaAge_2 + b_prei_*prei_2
  V[['alt3']] = 0  # utility of opt out, normalized to zero
  
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1,
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model1 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs,
                         estimate_settings=list(hessianRoutine="maxLik"))

#library(kableExtra)
#kable(apollo_modelOutput(model1, modelOutput_settings = list(printPVal=T)), digits = 3) %>% kable_styling()


# Modification 1 - MODEL 2
## Include squared functional forms in order to display diminishing marginal utility - for wald, groe

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model2",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)

apollo_beta=c(b_alt1 =0,  # Alternative specific constant (intercept parameters)
              b_alt2 =0,
              b_wald_ =0,
              b_wald2 =0,
              b_groe_ =0,
              b_prei_ =0,
              b_ConSh_ =0,
              b_HaAge_ =0,
              b_FoUnt_ =0)

# no fix parameter
apollo_fixed = c()

# validate
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_wald_*SQwald_1 + b_wald2*SQwald2_1 + b_groe_*groe_1 + b_FoUnt_*FoUnt_1 + b_ConSh_*ConSh_1 + b_HaAge_*HaAge_1 + b_prei_*prei_1
  V[['alt2']] = b_alt2 + b_wald_*SQwald_2 + b_wald2*SQwald2_2 + b_groe_*groe_2 + b_FoUnt_*FoUnt_2 + b_ConSh_*ConSh_2 + b_HaAge_*HaAge_2 + b_prei_*prei_2
  V[['alt3']] = 0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model2 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(hessianRoutine="maxLik"))



# Modification 2 - MODEL 3
##Include Interaction terms:
#Relevant in order to account for possible omitted variable bias etc. + also possibility to make a difference to the original paper


apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model3",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)

apollo_beta=c(b_alt1 =0,  # Alternative specific constant (intercept parameters)
              b_alt2 =0,
              b_wald_ =0,
              b_wald2 =0,
              b_groeHalf =0,
              b_prei_ =0,
              b_FoUnt_  =0,
              b_ConSh_ =0,
              b_HaAge_ =0,
              b_IncXFoUnt_ =0,
              b_NatDayXFoUnt_ =0)


### no fix parameter 
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_wald_*SQwald_1 + b_wald2*SQwald2_1 +  b_groeHalf* groe_1 + b_prei_ * prei_1 + b_FoUnt_ * FoUnt_1 + b_ConSh_ * ConSh_1 + b_HaAge_ * HaAge_1 + b_IncXFoUnt_ * (FoUnt_1*IncomeMC) + b_NatDayXFoUnt_ * (FoUnt_1*NatDayMC)
  V[['alt2']] = b_alt2 + b_wald_*SQwald_2 + b_wald2*SQwald2_2 + b_groeHalf* groe_2 + b_prei_ * prei_2 + b_FoUnt_ * FoUnt_2 + b_ConSh_ * ConSh_2 + b_HaAge_ * HaAge_2 + b_IncXFoUnt_ * (FoUnt_2*IncomeMC) + b_NatDayXFoUnt_ * (FoUnt_2*NatDayMC)
  V[['alt3']] = 0   # utility of opt out, normalized to zero
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model3 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(hessianRoutine="maxLik"))
