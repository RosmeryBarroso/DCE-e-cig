# 
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


#Los números cambian dependiendo de los resultados a correr de los scripts principales


 for ( pays in c(3) ) { 
   for ( ID in c(20,21,22) ) {
     print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
     source("DCEe_03b_estimacionMMNL_WTP_sinInt.R")
   }
 }


 for ( ID in c(9,11,12,13,14) ) {
   print(paste("Utility: Vamos en el grupo...", ID))
   source("DCEe_03b_estimacionMMNL_sinInt_pool.r")
 }



for ( ID in c(7,14,6, 13) ) {
  print(paste("WTP: Vamos en el grupo...", ID))
  source("DCEe_03b_estimacionMMNL_WTP_sinInt_pool.R")
}



 for ( pays in c(1) ) {#Argentina vapea exclusivo no corrio tampoco Colombia
   for ( ID in c(6) ) {
     print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
     source("DCEe_03b_estimacionMMNL_WTP_sinInt.R")
   }
 }




# for ( pays in c(1,2,3) ) {
#   for ( ID in c(1,2,3,4,5,6,7,8,9) ) {
#     print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
#     source("DCEe_01_estimacionMMNL.R")
#   }
# }

#

 for ( ID in c(1,2,3,4,5,6,7,8) ) {
   print(paste("WTP gender: Vamos en el grupo...", ID))
   source("DCEe_03b_estimacionMMNL_WTP_sinInt_pool_gender.R")
 }


