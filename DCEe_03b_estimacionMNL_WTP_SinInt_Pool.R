# ################################################################# 
############FINAL RESULTS: Substitution among tobacco-related products ####
################## December 06 2024 ##########################

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

library(apollo)
library(readr)
library(purrr)
library(writexl)
library(tidyverse)
library(gridExtra)

### Define the case to run. Just edit the index of pais and grupo in line 26
grupo = c("all","SOLOCONVEN1825","SOLOCONVEN26","VAPEA1825","VAPEA26","NOFUMA1825","SOLOCONVEN","VAPEA", "VAPEA1825exc","VAPEA26exc","VAPEA1825dual","VAPEA26dual","VAPEAexc","VAPEAdual", "TIMEBAJO", "TIMEBAJOexcluyendo")
#           1          2              3             4           5         6          7           8            9             10             11           12            13         14            15

ID <- 10
CASO = paste(grupo[ID], sep="_")
print(CASO)

apollo_initialise()

apollo_control = list(
  modelName       = paste("MNL_WTP_cont_sinInt_pool_", CASO, sep=""),
  modelDescr      = "MNL model in WTP space, pooled by country",
  indivID         = "unique_id",
  mixing          = FALSE,
  outputDirectory = "output")

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

databasem <- read.csv('price_continous.csv')
database  <- databasem

conda = "1==1"

if (grepl("all"            , CASO, ignore.case = TRUE)) conda = paste(conda, " & NOFUMA1825xx != 1")
if (grepl("SOLOCONVEN1825" , CASO, ignore.case = TRUE)) conda = paste(conda, " & SOLOCONVEN1825xx == 1")
if (grepl("SOLOCONVEN26"   , CASO, ignore.case = TRUE)) conda = paste(conda, " & SOLOCONVEN26pxx==1")
if (grepl("VAPEA1825"      , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA1825xx==1")
if (grepl("VAPEA26"        , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA26pxx==1")
if (grepl("VAPEA1825exc"   , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA1825exc==1")
if (grepl("VAPEA26exc"     , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA26pexc==1")
if (grepl("VAPEA1825dual"  , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA1825dual==1")
if (grepl("VAPEA26dual"    , CASO, ignore.case = TRUE)) conda = paste(conda, " & VAPEA26pdual==1")
if (grepl("NOFUMA1825"     , CASO, ignore.case = TRUE)) conda = paste(conda, " & NOFUMA1825xx==1")
if (grepl("SOLOCONVEN"     , CASO, ignore.case = TRUE)) conda = paste(conda, " & (SOLOCONVEN1825xx == 1 | SOLOCONVEN26pxx==1)")
if (grepl("VAPEA"          , CASO, ignore.case = TRUE)) conda = paste(conda, " & (VAPEA1825xx==1 | VAPEA26pxx==1)")
if (grepl("VAPEAexc"       , CASO, ignore.case = TRUE)) conda = paste(conda, " & (VAPEA1825exc==1 | VAPEA26pexc==1)")
if (grepl("VAPEAdual"      , CASO, ignore.case = TRUE)) conda = paste(conda, " & (VAPEA1825dual==1 | VAPEA26pdual==1)")
if (grepl("TIMEBAJO"       , CASO, ignore.case = TRUE)) conda = paste(conda, " & cat_time == 1")

print(conda)
database <- subset(databasem, eval(parse(text = conda)))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  asc_disp     = 0,
  asc_rech     = 0,
  asc_cig      = 0,
  bprice       = -1,
  
  wtp_ext1     = 0,
  wtp_ext2     = 0,
  wtp_ext3     = 0,
  wtp_harm     = 0,
  wtp_hide     = 0,
  wtp_flavour1 = 0,
  wtp_flavour2 = 0,
  
  mu_col       = 1,
  mu_arg       = 1,
  mu_chi       = 1
)

apollo_fixed = c("mu_col")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  # ---- COLOMBIA (mu_col fijo en 1, no se multiplica) ----
  V = list()
  V[["ecigdisp_col"]] = asc_disp + bprice * (price1 +
                                               wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                               wtp_harm*harm1 + wtp_hide*hide1 +
                                               wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  
  V[["ecigrech_col"]] = asc_rech + bprice * (price2 +
                                               wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                               wtp_harm*harm2 + wtp_hide*hide2 +
                                               wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  
  V[["cig_col"]] = asc_cig + bprice * (price3 +
                                         wtp_ext1*(ext3==1) + wtp_ext2*(ext3==2) + wtp_ext3*(ext3==3) +
                                         wtp_harm*harm3 + wtp_hide*hide3 +
                                         wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  
  V[["optout_col"]] = 0
  
  mnl_settings_col = list(
    alternatives  = c(ecigdisp_col=1, ecigrech_col=2, cig_col=3, optout_col=4),
    avail         = list(ecigdisp_col=1, ecigrech_col=1, cig_col=1, optout_col=1),
    choiceVar     = chosen_option,
    utilities     = list(ecigdisp_col = V[["ecigdisp_col"]],
                         ecigrech_col = V[["ecigrech_col"]],
                         cig_col      = V[["cig_col"]],
                         optout_col   = V[["optout_col"]]),
    rows          = (col==1),
    componentName = "col"
  )
  
  P[["col"]] = apollo_mnl(mnl_settings_col, functionality)
  
  # ---- ARGENTINA ----
  V = list()
  V[["ecigdisp_arg"]] = asc_disp + bprice * (price1 +
                                               wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                               wtp_harm*harm1 + wtp_hide*hide1 +
                                               wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  
  V[["ecigrech_arg"]] = asc_rech + bprice * (price2 +
                                               wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                               wtp_harm*harm2 + wtp_hide*hide2 +
                                               wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  
  V[["cig_arg"]] = asc_cig + bprice * (price3 +
                                         wtp_ext1*(ext3==1) + wtp_ext2*(ext3==2) + wtp_ext3*(ext3==3) +
                                         wtp_harm*harm3 + wtp_hide*hide3 +
                                         wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  
  V[["optout_arg"]] = 0
  
  mnl_settings_arg = list(
    alternatives  = c(ecigdisp_arg=1, ecigrech_arg=2, cig_arg=3, optout_arg=4),
    avail         = list(ecigdisp_arg=1, ecigrech_arg=1, cig_arg=1, optout_arg=1),
    choiceVar     = chosen_option,
    utilities     = list(ecigdisp_arg = mu_arg * V[["ecigdisp_arg"]],
                         ecigrech_arg = mu_arg * V[["ecigrech_arg"]],
                         cig_arg      = mu_arg * V[["cig_arg"]],
                         optout_arg   = mu_arg * V[["optout_arg"]]),
    rows          = (arg==1),
    componentName = "arg"
  )
  
  P[["arg"]] = apollo_mnl(mnl_settings_arg, functionality)
  
  # ---- CHILE ----
  V = list()
  V[["ecigdisp_chi"]] = asc_disp + bprice * (price1 +
                                               wtp_ext1*(ext1==1) + wtp_ext2*(ext1==2) + wtp_ext3*(ext1==3) +
                                               wtp_harm*harm1 + wtp_hide*hide1 +
                                               wtp_flavour1*(flavour1==1) + wtp_flavour2*(flavour1==2))
  
  V[["ecigrech_chi"]] = asc_rech + bprice * (price2 +
                                               wtp_ext1*(ext2==1) + wtp_ext2*(ext2==2) + wtp_ext3*(ext2==3) +
                                               wtp_harm*harm2 + wtp_hide*hide2 +
                                               wtp_flavour1*(flavour2==1) + wtp_flavour2*(flavour2==2))
  
  V[["cig_chi"]] = asc_cig + bprice * (price3 +
                                         wtp_ext1*(ext3==1) + wtp_ext2*(ext3==2) + wtp_ext3*(ext3==3) +
                                         wtp_harm*harm3 + wtp_hide*hide3 +
                                         wtp_flavour1*(flavour3==1) + wtp_flavour2*(flavour3==2))
  
  V[["optout_chi"]] = 0
  
  mnl_settings_chi = list(
    alternatives  = c(ecigdisp_chi=1, ecigrech_chi=2, cig_chi=3, optout_chi=4),
    avail         = list(ecigdisp_chi=1, ecigrech_chi=1, cig_chi=1, optout_chi=1),
    choiceVar     = chosen_option,
    utilities     = list(ecigdisp_chi = mu_chi * V[["ecigdisp_chi"]],
                         ecigrech_chi = mu_chi * V[["ecigrech_chi"]],
                         cig_chi      = mu_chi * V[["cig_chi"]],
                         optout_chi   = mu_chi * V[["optout_chi"]]),
    rows          = (chi==1),
    componentName = "chi"
  )
  
  P[["chi"]] = apollo_mnl(mnl_settings_chi, functionality)
  
  # ---- COMBINE, PANEL, PREPARE ----
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                        estimate_settings = list(nCores = 4))
# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

sub1 = apollo_deltaMethod(model, list(operation="diff", parName1="asc_cig",  parName2="asc_disp"))
sub2 = apollo_deltaMethod(model, list(operation="diff", parName1="asc_cig",  parName2="asc_rech"))
sub3 = apollo_deltaMethod(model, list(operation="diff", parName1="asc_disp", parName2="asc_rech"))

extra_tab <- data.frame(
  Parameter     = c("Diff Conventional vs Disposable",
                    "Diff Conventional vs Rechargeable",
                    "Diff Disposable vs Rechargeable"),
  Estimate      = c(sub1[1], sub2[1], sub3[1]),
  Std_Error_rob = c(sub1[2], sub2[2], sub3[2]),
  t_stat_rob    = c(sub1[3], sub2[3], sub3[3])
)

apollo_modelOutput(model)
apollo_saveOutput(model)

coef_table <- data.frame(
  Parameter     = names(model$estimate[names(model$estimate) != "mu_col"]),
  Estimate      = model$estimate[names(model$estimate) != "mu_col"],
  Std_Error_rob = model$seBGW,
  t_stat_rob    = model$tstatBGW
)

coef_table = rbind(coef_table, extra_tab)

write_xlsx(coef_table,
           paste("output/", apollo_control$modelName, ".xlsx", sep=""))

apollo_sink() 