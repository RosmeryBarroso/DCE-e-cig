# ################################################################# 
############FINAL RESULTS: Substitution among tobacco-related products ####
################## December 06 2024 ##########################

# This code processes final results using a Multinomial logit model and joint estimation. The objective is to obtain informative priors for the attributes and recalculate a Bayesian D-efficient design for the final experimental setup  

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

# # ### Clear memory
# rm(database,databasem,apollo_inputs,apollo_control,apollo_draws)
# print(ID)


### Load libraries
library(apollo)
library(readr)
library(purrr)
library(writexl)
library(tidyverse)
library(gridExtra)


### Define the case to run. Just edit the index of pais and grupo in line 26
pais=c("arg","chi","col","todos")
grupo = c("all","SOLOCONVEN1825","SOLOCONVEN26","VAPEA1825","VAPEA26","NOFUMA1825","SOLOCONVEN","VAPEA", "VAPEA1825exc","VAPEA26exc","VAPEA1825dual","VAPEA26dual","VAPEAexc","VAPEAdual", "SOLOCONVENMUJER", "SOLOCONVENHOMBRE", "VAPEAMUJERexc", "VAPEAMUJERdual","VAPEAHOMBREexc", "VAPEAHOMBREdual", "NOFUMAMUJER", "NOFUMAHOMBRE", "TIMEBAJO")
#           1          2              3             4           5         6          7           8            9             10             11           12            13         14                15                 16                     17             18                19                20               21             22         23


CASO = paste(pais[pays],grupo[ID],sep="_")
print(CASO)


### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = paste("MMNL_WTP_price_continous_sinInt_",CASO,sep=""), 
  modelDescr      = "Mixed Logit (MMNL) model, based on final data",
  indivID         = "unique_id",  # Name of column in the database with each individual's ID
  mixing          = TRUE,
  nCores          = 4,   
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

### the code would be: database = read.csv("data.csv")

databasem <- read.csv('price_continous.csv')
database <- databasem

# database$TODOS <- with(database, ifelse(SOLOCONVEN1825 == 1 | SOLOCONVEN26p == 1 |
#                               VAPEA1825 == 1 | VAPEA26p == 1, 1, 0))

# Define the precise subset of the dataset based on the desired outcome

conda="1==1"
if (grepl("arg", CASO, ignore.case = TRUE)) conda=paste(conda," & arg== 1")
if (grepl("chi", CASO, ignore.case = TRUE)) conda=paste(conda," & chi== 1")
if (grepl("col", CASO, ignore.case = TRUE)) conda=paste(conda," & col== 1")


if (grepl("all"           , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMA1825xx != 1")
if (grepl("SOLOCONVEN1825", CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVEN1825xx == 1")
if (grepl("SOLOCONVEN26"  , CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVEN26pxx==1")
if (grepl("VAPEA1825"     , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA1825xx==1")
if (grepl("VAPEA26"       , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA26pxx==1")
if (grepl("VAPEA1825exc"  , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA1825exc==1")
if (grepl("VAPEA26exc"    , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA26pexc==1")
if (grepl("VAPEA1825dual" , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA1825dual==1")
if (grepl("VAPEA26dual"   , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEA26pdual==1")

if (grepl("NOFUMA1825"    , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMA1825xx==1")
if (grepl("SOLOCONVEN"    , CASO, ignore.case = TRUE)) conda=paste(conda," & (SOLOCONVEN1825xx == 1 | SOLOCONVEN26pxx==1)")
if (grepl("VAPEA"         , CASO, ignore.case = TRUE)) conda=paste(conda," & (VAPEA1825xx==1 |  VAPEA26pxx==1)")
if (grepl("VAPEAexc"      , CASO, ignore.case = TRUE)) conda=paste(conda," & (VAPEA1825exc==1 |  VAPEA26pexc==1)")
if (grepl("VAPEAdual"     , CASO, ignore.case = TRUE)) conda=paste(conda," & (VAPEA1825dual==1 |  VAPEA26pdual==1)")

if (grepl("SOLOCONVENMUJER"    , CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVENMUJERxx ==1")
if (grepl("SOLOCONVENHOMBRE"    , CASO, ignore.case = TRUE)) conda=paste(conda," & SOLOCONVENHOMBRExx ==1")
if (grepl("VAPEAMUJERexc"         , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAMUJERexcxx ==1")
if (grepl("VAPEAMUJERdual"      , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAMUJERdualxx ==1")
if (grepl("VAPEAHOMBREexc"     , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAHOMBREexcxx ==1")
if (grepl("VAPEAHOMBREdual"     , CASO, ignore.case = TRUE)) conda=paste(conda," & VAPEAHOMBREdualxx ==1")
if (grepl("NOFUMAMUJER"     , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMAMUJERxx ==1")
if (grepl("NOFUMAHOMBRE"     , CASO, ignore.case = TRUE)) conda=paste(conda," & NOFUMAHOMBRExx ==1")

if (grepl("TIMEBAJO"     , CASO, ignore.case = TRUE)) conda=paste(conda," & cat_time == 1")



print(conda)
database <- subset(databasem, eval(parse(text =conda)) )

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
# for smokers, we use as starting value the estimates for "all smokers" in the
# three countries. In most of the cases it converges starting from zeros.
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
              wtp_flavour2_mu  = 0, wtp_flavour2_sig = 0
)

# # For non-smokers a different starting value is required, this do not converge
# # stratgin from zero.

if (grepl("SOLOCONVEN1825"    , CASO, ignore.case = TRUE)) {
apollo_beta = apollo_beta = c(
  wtp_asc_disp_mu   = -2.17346,
  wtp_asc_disp_sig  = -5.04261,
  wtp_asc_rech_mu   = -1.06358,
  wtp_asc_rech_sig  = 5.57206,
  wtp_asc_cig_mu    = -7.78187,
  wtp_asc_cig_sig   = 10.41416,
  lambda_mu         = 1.94135,
  lambda_sig        = 5.05019,
  wtp_ext1_mu       = 1.01660,
  wtp_ext1_sig      = -1.15287,
  wtp_ext2_mu       = 1.31163,
  wtp_ext2_sig      = -0.87519,
  wtp_ext3_mu       = 1.14882,
  wtp_ext3_sig      = -0.98951,
  wtp_harm_mu       = 1.38200,
  wtp_harm_sig      = -0.04941,
  wtp_hide_mu       = 0.22930,
  wtp_hide_sig      = 2.09460,
  wtp_flavour1_mu   = 0.78690,
  wtp_flavour1_sig  = -1.43597,
  wtp_flavour2_mu   = -0.60899,
  wtp_flavour2_sig  = 0.68121
)

}


# #Para Chile 
if (grepl("VAPEAexc", CASO, ignore.case = TRUE)) {

  apollo_beta = c(
    wtp_asc_disp_mu   =  0.10288,
    wtp_asc_disp_sig  = -7.13696,
    wtp_asc_rech_mu   = -5.57930,
    wtp_asc_rech_sig  = 10.52473,
    wtp_asc_cig_mu    =  0.53452,
    wtp_asc_cig_sig   = 11.82085,
    lambda_mu         = -0.31889,
    lambda_sig        =  2.83896,
    wtp_ext1_mu       = -1.28938,
    wtp_ext1_sig      =  2.49793,
    wtp_ext2_mu       =  3.73881,
    wtp_ext2_sig      = -0.42136,
    wtp_ext3_mu       =  2.84867,
    wtp_ext3_sig      = -2.79405,
    wtp_harm_mu       =  2.98061,
    wtp_harm_sig      = -1.55012,
    wtp_hide_mu       =  1.18632,
    wtp_hide_sig      =  2.09700,
    wtp_flavour1_mu   = -0.45532,
    wtp_flavour1_sig  =  0.06619,
    wtp_flavour2_mu   = -0.20894,
    wtp_flavour2_sig  = -3.87240
  )
}
# if (grepl("NOFUMAHOMBRE"    , CASO, ignore.case = TRUE)) {
#   
#   apollo_beta=c(wtp_asc_disp_mu = -38.162,
#                 wtp_asc_disp_sig = 489.861,
#                 wtp_asc_rech_mu = -42.717,
#                 wtp_asc_rech_sig = -449.091,
#                 wtp_asc_cig_mu = -54.785,
#                 wtp_asc_cig_sig = -435.260,
#                 lambda_mu = -2.633,
#                 lambda_sig = 3.995,
#                 wtp_ext1_mu = 66.433,
#                 wtp_ext1_sig = 57.294,
#                 wtp_ext2_mu = 121.618,
#                 wtp_ext2_sig = 75.663,
#                 wtp_ext3_mu = 77.301,
#                 wtp_ext3_sig = -35.912,
#                 wtp_harm_mu = 59.718,
#                 wtp_harm_sig = -88.108,
#                 wtp_hide_mu = 12.813,
#                 wtp_hide_sig = 54.668,
#                 wtp_flavour1_mu = -29.102,
#                 wtp_flavour1_sig = -24.139,
#                 wtp_flavour2_mu = -17.179,
#                 wtp_flavour2_sig = -18.337
#                 
#                 
#                 
#   )
# }
##If we want to keep parameters fixed to their starting values during the estimation (eg. asc), we include their names in the character vector apollo_fixed. 
## this vector is kept empty (apollo_fixed = c()) if all parameters are to be estimated. Parameters included in apollo_fixed are kept at the value used in apollo_beta, which may not be zero

apollo_fixed = c()


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
  
  randcoeff[["lambda"]]       = -exp(lambda_mu     + lambda_sig    * draws_lambda)
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
  V[["ecigdisp"]]  = lambda * (price1 + wtp_asc_disp + 
                               wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                               wtp_harm*harm1 + wtp_hide*hide1 + 
                               wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))                                            
  
  V[["ecigrech"]]  = lambda * (price2 + wtp_asc_rech + 
                                wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                wtp_harm*harm2 + wtp_hide*hide2 + 
                                wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  
  V[["cig"]]       = lambda * (price3 + wtp_asc_cig + 
                               wtp_ext2*ext3 + wtp_harm*harm3 + wtp_hide*hide3 +
                               wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  
  V[["optout"]]    = 0

  ### Compute probabilities for 'Colombia', 'Argentina, 'Chile' of the data using MNL model
  mnl_settings   = list(
    alternatives = c(ecigdisp=1, ecigrech=2, cig=3, optout=4), 
    avail        = list(ecigdisp=1, ecigrech=1, cig=1, optout=1),
    choiceVar    = chosen_option,
    utilities    = list(ecigdisp= V[["ecigdisp"]],  
                        ecigrech= V[["ecigrech"]],
                        cig     = V[["cig"]],
                        optout  = V[["optout"]])
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ## Combined model - Joint estimation 
  
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
  Parameter = names(model$estimate),
  Estimate      = model$estimate,
  Std_Error_rob = model$seBGW,
  t_stat_rob    = model$tstatBGW
)

coef_table=rbind(coef_table,extra_tab)
coef_table

write_xlsx(coef_table, paste("output/",apollo_control$modelName,".xlsx",sep=""))


# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()
