# ################################################################# 
############FINAL RESULTS: Substitution among tobacco-related products ####
################## December 06 2024 ##########################

# This code processes final results using a Multinomial logit model and joint estimation. The objective is to obtain informative priors for the attributes and recalculate a Bayesian D-efficient design for the final experimental setup  

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #





### Clear memory
rm(database,databasem,apollo_inputs,apollo_control,apollo_draws)
print(ID)

### Load libraries
library(apollo)
library(readr)
library(purrr)
library(writexl)
library(tidyverse)
library(gridExtra)


### Define the case to run. Just edit the index of pais and grupo in line 26
grupo = c("SOLOCONVENMUJER", "SOLOCONVENHOMBRE", "VAPEAMUJERexc", "VAPEAMUJERdual","VAPEAHOMBREexc", "VAPEAHOMBREdual", "NOFUMAMUJER", "NOFUMAHOMBRE")
#              1                      2                3                 4                5                   6               7                8
CASO = paste(grupo[ID],sep="_")
print(CASO)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = paste("MMNL_WTP_cont_sinInt_pool_gender_",CASO,sep=""),
  modelDescr      = "Mixed Logit (MMNL) model, based on final data",
  indivID         = "unique_id",  # Name of column in the database with each individual's ID
  mixing          = TRUE,
  outputDirectory = "output"
)


# if ( Sys.getenv("COMPUTERNAME")=="CNP98288") {
#   setwd("C:/Users/paul.rodriguez/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
# } else if ( Sys.getenv("USERNAME")=="paul.rodriguez")  {
#   setwd("C:/Users/paul.rodriguez/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
# } else if ( Sys.getenv("COMPUTERNAME")=="LAPTOP-2N0HQP2I") {
#   setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
# } else if ( Sys.getenv("USERNAME")=="rosme")  {
#   setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
# } else {
#   setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
# }

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

databasem <- read.csv('price_continous.csv')
database <- databasem

# Define the precise subset of the dataset based on the desired outcome

conda="1==1"

if (grepl("SOLOCONVENMUJER"    , CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVENMUJERxx ==1")
if (grepl("SOLOCONVENHOMBRE"    , CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVENHOMBRExx ==1")
if (grepl("VAPEAMUJERexc"         , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAMUJERexcxx ==1")
if (grepl("VAPEAMUJERdual"      , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAMUJERdualxx ==1")
if (grepl("VAPEAHOMBREexc"     , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAHOMBREexcxx ==1")
if (grepl("VAPEAHOMBREdual"     , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAHOMBREdualxx ==1")
if (grepl("NOFUMAMUJER"     , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMAMUJERxx ==1")
if (grepl("NOFUMAHOMBRE"     , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMAHOMBRExx ==1")



print(conda)
database <- subset(databasem, eval(parse(text =conda)) )

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(wtp_asc_disp_mu  = 0, wtp_asc_disp_sig = 0, 
              wtp_asc_rech_mu  = 0, wtp_asc_rech_sig = 0,
              wtp_asc_cig_mu   = 0, wtp_asc_cig_sig  = 0,
              lambda_mu        =-1, lambda_sig       = 0,
              wtp_ext1_mu      = 0, wtp_ext1_sig     = 0,
              wtp_ext2_mu      = 0, wtp_ext2_sig     = 0,
              wtp_ext3_mu      = 0, wtp_ext3_sig     = 0,
              wtp_harm_mu      = 0, wtp_harm_sig     = 0,
              wtp_hide_mu      = 0, wtp_hide_sig     = 0,
              wtp_flavour1_mu  = 0, wtp_flavour1_sig = 0,
              wtp_flavour2_mu  = 0, wtp_flavour2_sig = 0,
              mu_col  =  1, 
              mu_arg  =  1,
              mu_chi  =  1
)

if (grepl("VAPEAHOMBREexc"    , CASO, ignore.case = TRUE)) {
  
  apollo_beta = c(
    wtp_asc_disp_mu = -0.45222,
    wtp_asc_disp_sig = 2.56662,
    wtp_asc_rech_mu = -2.10165,
    wtp_asc_rech_sig = 3.02964,
    wtp_asc_cig_mu = 0.95968,
    wtp_asc_cig_sig = 3.03213,
    lambda_mu = 0.49814,
    lambda_sig = 2.72472,
    wtp_ext1_mu = 0.67984,
    wtp_ext1_sig = -0.45715,
    wtp_ext2_mu = 0.95589,
    wtp_ext2_sig = 0.09023,
    wtp_ext3_mu = 0.98405,
    wtp_ext3_sig = -0.49598,
    wtp_harm_mu = 0.45056,
    wtp_harm_sig = -0.61209,
    wtp_hide_mu = 0.05796,
    wtp_hide_sig = -0.10957,
    wtp_flavour1_mu = -0.65478,
    wtp_flavour1_sig = -0.35178,
    wtp_flavour2_mu = -0.58303,
    wtp_flavour2_sig = 0.80199,
    mu_col = 1.00000,
    mu_arg = 29.15008,
    mu_chi = 3.94161
  )
}




##If we want to keep parameters fixed to their starting values during the estimation (eg. asc), we include their names in the character vector apollo_fixed. 
## this vector is kept empty (apollo_fixed = c()) if all parameters are to be estimated. Parameters included in apollo_fixed are kept at the value used in apollo_beta, which may not be zero

apollo_fixed = c("mu_col")


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws. Now we have inter draws to account for heterogeneity intra-individual (across choices for the same individual). 
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 1000,
  interNormDraws = c("draws_wtp_asc_disp","draws_wtp_asc_rech","draws_wtp_asc_cig",
                     "draws_lambda",
                     "draws_wtp_ext1","draws_wtp_ext2","draws_wtp_ext3",
                     "draws_wtp_harm","draws_wtp_hide",
                     "draws_wtp_flavour1","draws_wtp_flavour2")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["wtp_asc_disp"]]  = wtp_asc_disp_mu   + wtp_asc_disp_sig  * draws_wtp_asc_disp
  randcoeff[["wtp_asc_rech"]]  = wtp_asc_rech_mu   + wtp_asc_rech_sig  * draws_wtp_asc_rech
  randcoeff[["wtp_asc_cig"]]   = wtp_asc_cig_mu    + wtp_asc_cig_sig   * draws_wtp_asc_cig  
  
  randcoeff[["lambda"]]    = -exp(lambda_mu     + lambda_sig    * draws_lambda)
  randcoeff[["wtp_ext1"]]     = wtp_ext1_mu      + wtp_ext1_sig     * draws_wtp_ext1   
  randcoeff[["wtp_ext2"]]     = wtp_ext2_mu      + wtp_ext2_sig     * draws_wtp_ext2
  randcoeff[["wtp_ext3"]]     = wtp_ext3_mu      + wtp_ext3_sig     * draws_wtp_ext3
  randcoeff[["wtp_harm"]]     = wtp_harm_mu      + wtp_harm_sig     * draws_wtp_harm
  randcoeff[["wtp_hide"]]     = wtp_hide_mu      + wtp_hide_sig     * draws_wtp_hide
  randcoeff[["wtp_flavour1"]] = wtp_flavour1_mu  + wtp_flavour1_sig * draws_wtp_flavour1
  randcoeff[["wtp_flavour2"]] = wtp_flavour2_mu  + wtp_flavour2_sig * draws_wtp_flavour2
  
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands 
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["ecigdisp_col"]]  =  lambda * (price1 + wtp_asc_disp + 
                                      wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                      wtp_harm*harm1 + wtp_hide*hide1 + 
                                      wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  V[["ecigrech_col"]]  =  lambda * (price2 + wtp_asc_rech + 
                                      wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                      wtp_harm*harm2 + wtp_hide*hide2 + 
                                      wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  V[["cig_col"]]       =  lambda * (price3 + wtp_asc_cig + 
                                      wtp_ext2*ext3 + wtp_harm*harm3 + wtp_hide*hide3 +
                                      wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  V[["optout_col"]]    =  0
  
  ### Compute probabilities for 'Colombia' of the data using MNL model
  mnl_settings_col   = list(
    alternatives = c(ecigdisp_col=1, ecigrech_col=2, cig_col=3, optout_col=4), 
    avail        = list(ecigdisp_col=1, ecigrech_col=1, cig_col=1, optout_col=1),
    choiceVar    = chosen_option,
    utilities    = list(ecigdisp_col= mu_col*V[["ecigdisp_col"]],  
                        ecigrech_col= mu_col*V[["ecigrech_col"]],
                        cig_col     = mu_col*V[["cig_col"]],
                        optout_col  = mu_col*V[["optout_col"]]),
    rows         = (col==1),
    componentName = "col"
  )
  
  ### Compute probabilities using MNL model
  P[["col"]] = apollo_mnl(mnl_settings_col, functionality)
  
  
  ### Compute probabilities for Argentina sub sample
  V = list()
  V[["ecigdisp_arg"]]  =  lambda * (price1 + wtp_asc_disp + 
                                      wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                      wtp_harm*harm1 + wtp_hide*hide1 + 
                                      wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  V[["ecigrech_arg"]]  =  lambda * (price2 + wtp_asc_rech + 
                                      wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                      wtp_harm*harm2 + wtp_hide*hide2 + 
                                      wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  V[["cig_arg"]]       =  lambda * (price3 + wtp_asc_cig + 
                                      wtp_ext2*ext3 + wtp_harm*harm3 + wtp_hide*hide3 +
                                      wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  V[["optout_arg"]]    =  0
  
  ### Compute probabilities for 'Argentina' of the data using MNL model
  mnl_settings_arg   = list(
    alternatives     = c(ecigdisp_arg=1, ecigrech_arg=2, cig_arg=3, optout_arg=4), 
    avail            = list(ecigdisp_arg=1, ecigrech_arg=1, cig_arg=1, optout_arg=1),
    choiceVar        = chosen_option,
    utilities        = list(ecigdisp_arg= mu_arg*V[["ecigdisp_arg"]],  
                            ecigrech_arg= mu_arg*V[["ecigrech_arg"]],
                            cig_arg     = mu_arg*V[["cig_arg"]],
                            optout_arg  = mu_arg*V[["optout_arg"]]),
    rows         = (arg==1),
    componentName = "arg"
  )
  
  ### Compute probabilities using MNL model
  P[["arg"]] = apollo_mnl(mnl_settings_arg, functionality)
  
  ### Compute probabilities for Chile sub sample
  V = list()
  V[["ecigdisp_chi"]]  =  lambda * (price1 + wtp_asc_disp + 
                                      wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                      wtp_harm*harm1 + wtp_hide*hide1 + 
                                      wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  V[["ecigrech_chi"]]  =  lambda * (price2 + wtp_asc_rech + 
                                      wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                      wtp_harm*harm2 + wtp_hide*hide2 + 
                                      wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  V[["cig_chi"]]       =  lambda * (price3 + wtp_asc_cig + 
                                      wtp_ext2*ext3 + wtp_harm*harm3 + wtp_hide*hide3 +
                                      wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  V[["optout_chi"]]    =  0
  
  ### Compute probabilities for 'Chile' of the data using MNL model
  mnl_settings_chi   = list(
    alternatives     = c(ecigdisp_chi=1, ecigrech_chi=2, cig_chi=3, optout_chi=4), 
    avail            = list(ecigdisp_chi=1, ecigrech_chi=1, cig_chi=1, optout_chi=1),
    choiceVar        = chosen_option,
    utilities        = list(ecigdisp_chi= mu_chi*V[["ecigdisp_chi"]],  
                            ecigrech_chi= mu_chi*V[["ecigrech_chi"]],
                            cig_chi     = mu_chi*V[["cig_chi"]],
                            optout_chi  = mu_chi*V[["optout_chi"]]),
    rows         = (chi==1),
    componentName = "chi"
  )
  
  ### Compute probabilities using MNL model
  P[["chi"]] = apollo_mnl(mnl_settings_chi, functionality)
  
  
  ## Combined model - Joint estimation 
  P = apollo_combineModels(P,apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

sub1=apollo_deltaMethod(model, list(operation="diff",parName1="wtp_asc_cig_mu",parName2="wtp_asc_disp_mu"))
sub2=apollo_deltaMethod(model, list(operation="diff",parName1="wtp_asc_cig_mu",parName2="wtp_asc_rech_mu"))
sub3=apollo_deltaMethod(model, list(operation="diff",parName1="wtp_asc_disp_mu",parName2="wtp_asc_rech_mu"))

extra_tab <- data.frame(
  Parameter = c("Diff Conventional vs Disposable","Diff Conventional vs Rechargable","Diff Disposable vs Rechargable"),
  Estimate      = c(sub1[1],sub2[1],sub3[1]),
  Std_Error_rob = c(sub1[2],sub2[2],sub3[2]),
  t_stat_rob    = c(sub1[3],sub2[3],sub3[3])
)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)


coef_table <- data.frame(
  Parameter = names( model$estimate[names(model$estimate) != "mu_col"]   ),
  Estimate      = model$estimate[names(model$estimate) != "mu_col"],
  Std_Error_rob =  model$seBGW,
  t_stat_rob    =  model$tstatBGW
)

coef_table=rbind(coef_table,extra_tab)
coef_table


write_xlsx(coef_table, paste("output/",apollo_control$modelName,".xlsx",sep=""))

# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()
