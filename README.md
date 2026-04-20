# DCE-e-cig
Orden para correr los archivos DCE e-cig 

1. DCEe_01_CleanData: depura, valida y clasifica las encuestas según las cuotas establecidas (edad, género y tipo de consumidor).

2. DCEe_02_OrganizeData: procesa las respuestas brutas del DCE, las limpia y reestructura en formato largo; combina la información con la matriz de atributos generada en Ngene y genera la base consolidada price_continous.csv lista para modelar en R.

3. DCEe_03a_batch: controla la ejecución automática y secuencial de los scripts de estimación, definiendo las rutas de trabajo y los grupos a correr.

4. DCEe_03b_estimacionMMNL_WTP_sinInt: ejecuta las estimaciones del modelo por país y por grupo
5. DCEe_03b_estimacionMMNL_WTP_sinInt_pool: realiza las mismas estimaciones sobre la muestra combinada por grupo.
6. DCEe_03b_estimacionMMNL_WTP_sinInt_pool_gender: ejecuta las estimaciones por género y por grupo.

El proceso para realizar las figuras es: 
1. Tomar las estimaciones resultantes de cada script
DCEe_03b_estimacionMMNL_WTP_sinInt: Tabla 3
DCEe_03b_estimacionMMNL_WTP_sinInt_pool: Tabla 4 
DCEe_03b_estimacionMMNL_WTP_sinInt_pool_gender: tabla 5

2. Una vez se tengan esas estimaciones realizamos la multiplicación por 20 dado que el precio inicial es por unidad.
3. Pasamos ese resultado al Excel "Resultados5_ListaResultados_21022026"
4. Se concatenan los resultados en las tablas al final de cada hoja y este mismo los pasamos al código en el cual se realizan las figuras. "DCE3_05_Figuras_DCE-cig V3".    

En las figuras se toma por separado los distintos atributos y se grafican todas las características, por ejemplo "Figure 4. Willingness to pay for worse self-harm level, and worse harm to others" toma los coeficientes relacionados a Mildly harmful- Very harmful y Unknown harmful de todos los grupos (Países- edades- genero / por traditional smokers- ENDS-users dual y Non-smokers). 
