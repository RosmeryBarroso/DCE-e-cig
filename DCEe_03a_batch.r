# Este script ejecuta estimaciones de modelos Mixed Multinomial Logit 
# para el DCE-E-cig.

# Se corren múltiples especificaciones del modelo variando por país (pays) y grupo (ID),
################## May 2026 ##########################




if ( Sys.getenv("COMPUTERNAME")=="CNP98288") {
  setwd("C:/Users/paul.rodriguez/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
} else if ( Sys.getenv("USERNAME")=="paul.rodriguez")  {
  setwd("C:/Users/paul.rodriguez/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
} else if ( Sys.getenv("COMPUTERNAME")=="LAPTOP-2N0HQP2I") {
  setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
} else if ( Sys.getenv("USERNAME")=="rosme")  {
  setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
} else {
  setwd("C:/Users/rosme/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales")
}

for ( pays in c(1,2,3) ) {
  for ( ID in c(24, 25, 27, 28) ) {
    print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
    source("DCEe_03b_estimacionMMNL_WTP_sinInt.R")
  }
}

# Argentina-VAPEAexc90sec - 27


for ( ID in c(20,21,24,29,30,31,32) ) {
  print(paste("WTP: Vamos en el grupo...", ID))
  source("DCEe_03b_estimacionMMNL_WTP_sinInt_pool.R")
}



for ( ID in c(15) ) {
  print(paste("Utility: Vamos en el grupo...", ID))
  source("DCEe_03b_estimacionMMNL_sinInt_pool.r")
}


for ( ID in c(9,10,12,11,13, 14,15,16) ) {
  print(paste("WTP gender: Vamos en el grupo...", ID))
  source("DCEe_03b_estimacionMMNL_WTP_sinInt_pool_gender.R")
}
