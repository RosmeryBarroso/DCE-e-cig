 clear

************************************************************************************************ 
**************** RESULTADOS FINALES - COLOMBIA ******************************************************
************************************************************************************************ 

global RunPath "C:/Users/Usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales" 
*global RunPath "C:\Users\paul.rodriguez\Universidad del rosario\Control Tabaco Facultad Economica - Documentos\DCE e-cig\Resultados finales"

* Crear carpeta de salida si no existe
capture mkdir "$RunPath/Output"

* Definir la ruta de salida
global OutPath "$RunPath/Output"

* Establecer el directorio de trabajo
cd "$RunPath"

import excel "colombia.xlsx", firstrow clear //primera fila nombre de la variable 


*Conserve sólo las respuestas completas y organice id 
drop id 
gen id= _n
keep if lastpage==20 
gen chosen_option=.

********************************************
*******    Filter responses       **********
********************************************

// Gen tasks from 1 to 8
foreach q in 1 2 3 4 5 6 7 8 {
gen task`q'=.	
} 

// Organize responses in one column (Preg_i_Nofumadores [other] doesn't have any responses)// Organizar las respuestas en una columna (Preg_i_Nofumadores [otro] no tiene ninguna respuesta)
foreach i in 1 2 3 4 5 6 7 8 {
replace Preg`i'Fumadores = Preg`i'NoFumadores if Preg`i'Fumadores == "" 			
}
//Reemplace el valor de la variable Preg[i]Fumadorescon el valor de Preg[i]NoFumadoresSOLO si Preg[i]Fumadoresestá vacío.

drop if missing(Preg1Fumadores) // By dropping first task it will drop all			

// Replace task variables with the responses
foreach x in 1 2 3 4 5 6 7 8 {
	tostring task`x', replace 
replace task`x'= Preg`x'Fumadores 		
}


keep id chosen_option EncuestaRandnumber task1 task2 task3 task4 task5 task6 task7 task8 SOLOCONVEN* NOFUMA* VAPEA* female cat_time

*******************************************************************
******* Reshape (remodelar) responses by ID ***********************************
******************************************************************* 
rename EncuestaRandnumber block 

* Reshape by id and task. Takes id as the identificator variable for the rows and consolidate all the task"x" into a single variable called task. 
* Variable task has the responses to each choice situation ordered by choice task and block

reshape long task , i(id) j(choiceTask)

* Replace chosen_option
tostring chosen_option, replace 
replace chosen_option=task //Creamos una variable choice optipon igual a task

replace chosen_option = "1" if chosen_option == "Desechable" 
replace chosen_option = "2" if chosen_option == "Recargable" 	
replace chosen_option = "3" if chosen_option == "Convencional" 	
replace chosen_option = "4" if chosen_option == "Ninguna de las anteriores" | chosen_option == "Otro" | missing(chosen_option)
//Lo volvemos categoría

destring chosen_option, replace 
drop task 


********************************************************************
********** 120 encuestados: 10 eliminados -> 110 encuestados **********
********************************************************************


**# 880 respuestas (110 X 8) 

* La tarea de elección se asignó por orden. 

** Por ejemplo, un participante asignado al bloque 1, la primera tarea de elección que responda será la situación de elección = 3 obtenida en ngene

gen choice_situation=.

***** Block=1  
replace choice_situation= 1 if block == 1 & choiceTask == 1  
replace choice_situation= 4 if block == 1 &  choiceTask == 2  
replace choice_situation= 9 if block == 1 &  choiceTask == 3  
replace choice_situation= 11 if block == 1 &  choiceTask == 4  
replace choice_situation= 14 if block == 1 &  choiceTask == 5  
replace choice_situation= 16 if block == 1 &  choiceTask == 6  
replace choice_situation= 23 if block == 1 &  choiceTask == 7  
replace choice_situation= 24 if block == 1 &  choiceTask == 8 


****** Block=2 
replace choice_situation= 2 if block == 2 & choiceTask == 1  
replace choice_situation= 3 if block == 2 &  choiceTask == 2  
replace choice_situation= 5 if block == 2 &  choiceTask == 3  
replace choice_situation= 7 if block == 2 &  choiceTask == 4  
replace choice_situation= 8 if block == 2 &  choiceTask == 5  
replace choice_situation= 15 if block == 2 &  choiceTask == 6  
replace choice_situation= 17 if block == 2 &  choiceTask == 7  
replace choice_situation= 22 if block == 2 &  choiceTask == 8 

****** Block=3 
replace choice_situation= 6 if block == 3 & choiceTask == 1  
replace choice_situation= 10 if block == 3 &  choiceTask == 2  
replace choice_situation= 12 if block == 3 &  choiceTask == 3  
replace choice_situation= 13 if block == 3 &  choiceTask == 4  
replace choice_situation= 18 if block == 3 &  choiceTask == 5  
replace choice_situation= 19 if block == 3 &  choiceTask == 6  
replace choice_situation= 20 if block == 3 &  choiceTask == 7  
replace choice_situation= 21 if block == 3 &  choiceTask == 8 

tempfile data_process_col 
save `data_process_col'

*************************************************************
****** Fusión con datos de atributos y tratamiento por bloques *******
************************************************************* 

// Block_1= 47 respondants // Block_2= 37 respondants // Block_3= 26

** Assign the atributtes obtained in ngene for each alternative 

**# Block 1
keep if block==1 
rename choice_situation choiceset // Key variable for merging databases
tempfile block1 
save `block1'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==1

merge 1:m choiceset using `block1' // Key variable for merging databases
tempfile block1_P 
save `block1_P'

**# Block 2 
use `data_process_col', clear 
keep if block==2 
rename choice_situation choiceset 
tempfile block2 
save `block2'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==2

merge 1:m choiceset using `block2'
tempfile block2_P 
save `block2_P'


**# Block 3 
use `data_process_col', clear
keep if block==3 
rename choice_situation choiceset 
tempfile block3 
save `block3'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==3

merge 1:m choiceset using `block3'
tempfile block3_P 
save `block3_P' 


**# Append all blocks
use `block1_P', clear 
append using `block2_P' 
append using `block3_P' 

// choiceTask se refiere al orden en las preguntas de los participantes, choiceset se refiere a las diferentes tareas de elección obtenidas en ngene
drop choiceTask 
drop _merge 

gen country_id= "COL" 

// fijar de nuevo los precios a niveles continuos (si es necesario)

* E-cig disposable
replace price1 = 0.025 if price1 == 8.75
replace price1 = 0.050 if price1 == 17.5
replace price1 = 0.100 if price1 == 35.5
replace price1 = 0.160 if price1 == 56

* E-cig rechargeable
replace price2 = 0.035 if price2 == 5
replace price2 = 0.045 if price2 == 15
replace price2 = 0.055 if price2 == 25
replace price2 = 0.080 if price2 == 50

* Cigarette
replace price3 = 0.025 if price3 == 0.5
replace price3 = 0.050 if price3 == 1
replace price3 = 0.200 if price3 == 4
replace price3 = 0.350 if price3 == 7


tempfile final_col
save `final_col'

**#  ARGENTINA

************************************************************************************************ 
********************    FINAL- RESULTS ARGENTINA    *******************************************
************************************************************************************************ 

** Testing with final data for obtaining priors and recalculate Bayesian design (informative priors) 

import excel "argentina.xlsx", firstrow clear 

* Keep only complete responses   
keep if lastpage==20 
gen chosen_option=.


********************************************
**********    Filter responses   ***********
********************************************

// Gen tasks from 1 to 8
foreach q in 1 2 3 4 5 6 7 8 {
gen task`q'=.	
} 

/*
In terms of utility "Otro" and "Ninguno" refers to the opt-out option, 
then Preg_i_Nofumadores and Preg_i_Nofumadores[Other] give us the same information, so I keep only one of them and 
organize the responses in one column  
*/

foreach i in 1 2 3 4 5 6 7 8 {
replace Preg`i'Fumadores = Preg`i'NoFumadores if Preg`i'Fumadores == "" 			
}

drop if missing(Preg1Fumadores) // By dropping first task it will drop all			

// Replace task variables with the responses
foreach x in 1 2 3 4 5 6 7 8 {
	tostring task`x', replace 
replace task`x'= Preg`x'Fumadores 		
}


* Organize ID
drop id 
gen id= _n
keep id chosen_option EncuestaRandnumber task1 task2 task3 task4 task5 task6 task7 task8 SOLOCONVEN* NOFUMA* VAPEA* female cat_time

*******************************************************************
******* Reshape responses by ID ***********************************
******************************************************************* 
rename EncuestaRandnumber block 

* Reshape by id and task. Takes id as the identificator variable for the rows and consolidate all the task_"x" into a single variable called task. 
* Variable task has the responses to each choice situation ordered by choice task and block

reshape long task , i(id) j(choiceTask)

* Replace chosen_option
tostring chosen_option, replace 
replace chosen_option=task 

replace chosen_option = "1" if chosen_option == "Desechable" 
replace chosen_option = "2" if chosen_option == "Recargable" 	
replace chosen_option = "3" if chosen_option == "Convencional" 	
replace chosen_option = "4" if chosen_option == "Ninguna de las anteriores" | chosen_option == "Otro" | missing(chosen_option)

destring chosen_option, replace 
drop task 

**# Assign each choice task (order in the questions for each participant) to the choice_situation generated in ngene  
gen choice_situation=.

***** Block=1  
replace choice_situation= 1 if block == 1 & choiceTask == 1  
replace choice_situation= 4 if block == 1 &  choiceTask == 2  
replace choice_situation= 9 if block == 1 &  choiceTask == 3  
replace choice_situation= 11 if block == 1 &  choiceTask == 4  
replace choice_situation= 14 if block == 1 &  choiceTask == 5  
replace choice_situation= 16 if block == 1 &  choiceTask == 6  
replace choice_situation= 23 if block == 1 &  choiceTask == 7  
replace choice_situation= 24 if block == 1 &  choiceTask == 8 


****** Block=2 
replace choice_situation= 2 if block == 2 & choiceTask == 1  
replace choice_situation= 3 if block == 2 &  choiceTask == 2  
replace choice_situation= 5 if block == 2 &  choiceTask == 3  
replace choice_situation= 7 if block == 2 &  choiceTask == 4  
replace choice_situation= 8 if block == 2 &  choiceTask == 5  
replace choice_situation= 15 if block == 2 &  choiceTask == 6  
replace choice_situation= 17 if block == 2 &  choiceTask == 7  
replace choice_situation= 22 if block == 2 &  choiceTask == 8 

****** Block=3 
replace choice_situation= 6 if block == 3 & choiceTask == 1  
replace choice_situation= 10 if block == 3 &  choiceTask == 2  
replace choice_situation= 12 if block == 3 &  choiceTask == 3  
replace choice_situation= 13 if block == 3 &  choiceTask == 4  
replace choice_situation= 18 if block == 3 &  choiceTask == 5  
replace choice_situation= 19 if block == 3 &  choiceTask == 6  
replace choice_situation= 20 if block == 3 &  choiceTask == 7  
replace choice_situation= 21 if block == 3 &  choiceTask == 8 

tempfile data_process_arg 
save `data_process_arg'


*************************************************************
****** Merge with atributte data and process by block *******
************************************************************* 

**# Block 1
keep if block==1 
rename choice_situation choiceset // Key variable for merging databases
tempfile block1A 
save `block1A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==1

merge 1:m choiceset using `block1A'
tempfile block1_PA 
save `block1_PA'

**# Block 2 
use `data_process_arg', clear 
keep if block==2 
rename choice_situation choiceset // Key variable for merging databases
tempfile block2A 
save `block2A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==2

merge 1:m choiceset using `block2A'
tempfile block2_PA 
save `block2_PA'


**# Block 3 
use `data_process_arg', clear 
keep if block==3 
rename choice_situation choiceset // Key variable for merging databases
tempfile block3A 
save `block3A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==3

merge 1:m choiceset using `block3A'
tempfile block3_PA 
save `block3_PA' 


*** Append all blocks ***
use `block1_PA', clear 
append using `block2_PA' 
append using `block3_PA' 


// choiceTask refers to the order in the participant questions, choiceset refers to the different choices obtained in ngene
drop choiceTask 
drop _merge 
gen country_id= "ARG" 
 
 
// fix prices again to continous levels (if necessary) -> Ngene forum Michiel Bliemer response

* E-cig disposable
replace price1 = 0.025 if price1 == 8.75
replace price1 = 0.050 if price1 == 17.5
replace price1 = 0.100 if price1 == 35.5
replace price1 = 0.160 if price1 == 56

* E-cig rechargeable
replace price2 = 0.035 if price2 == 5
replace price2 = 0.045 if price2 == 15
replace price2 = 0.055 if price2 == 25
replace price2 = 0.080 if price2 == 50

* Cigarette
replace price3 = 0.025 if price3 == 0.5
replace price3 = 0.050 if price3 == 1
replace price3 = 0.200 if price3 == 4
replace price3 = 0.350 if price3 == 7

tempfile final_Arg 
save `final_Arg'


**#  RESULTS CHILE

************************************************************************************************ 
********************    FINAL RESULTS - CHILE    *******************************************
************************************************************************************************ 

** Testing with final data for obtaining priors and recalculate Bayesian design (informative priors) 

import excel "chile.xlsx", firstrow clear 

* Keep only complete responses   
keep if lastpage==20 
gen chosen_option=.


********************************************
**********    Filter responses   ***********
********************************************

// Gen tasks from 1 to 8
foreach q in 1 2 3 4 5 6 7 8 {
gen task`q'=.	
} 

/*
In terms of utility "Otro" and "Ninguno" refers to the opt-out option, 
then Preg_i_Nofumadores and Preg_i_Nofumadores[Other] give us the same information, so I keep only one of them and 
organize the responses in one column  
*/

foreach i in 1 2 3 4 5 6 7 8 {
replace Preg`i'Fumadores = Preg`i'NoFumadores if Preg`i'Fumadores == "" 			
}

drop if missing(Preg1Fumadores) // By dropping first task it will drop all			

// Replace task variables with the responses
foreach x in 1 2 3 4 5 6 7 8 {
	tostring task`x', replace 
replace task`x'= Preg`x'Fumadores 		
}


* Organize ID
drop id 
gen id= _n
keep id chosen_option EncuestaRandnumber task1 task2 task3 task4 task5 task6 task7 task8 SOLOCONVEN* NOFUMA* VAPEA* female cat_time

*******************************************************************
******* Reshape responses by ID ***********************************
******************************************************************* 
rename EncuestaRandnumber block 

* Reshape by id and task. Takes id as the identificator variable for the rows and consolidate all the task_"x" into a single variable called task. 
* Variable task has the responses to each choice situation ordered by choice task and block

reshape long task , i(id) j(choiceTask)

* Replace chosen_option
tostring chosen_option, replace 
replace chosen_option=task 

replace chosen_option = "1" if chosen_option == "Desechable" 
replace chosen_option = "2" if chosen_option == "Recargable" 	
replace chosen_option = "3" if chosen_option == "Convencional" 	
replace chosen_option = "4" if chosen_option == "Ninguna de las anteriores" | chosen_option == "Otro" | missing(chosen_option)

destring chosen_option, replace 
drop task 

**# Assign each choice task (order in the questions for each participant) to the choice_situation generated in ngene  
gen choice_situation=.

***** Block=1  
replace choice_situation= 1 if block == 1 & choiceTask == 1  
replace choice_situation= 4 if block == 1 &  choiceTask == 2  
replace choice_situation= 9 if block == 1 &  choiceTask == 3  
replace choice_situation= 11 if block == 1 &  choiceTask == 4  
replace choice_situation= 14 if block == 1 &  choiceTask == 5  
replace choice_situation= 16 if block == 1 &  choiceTask == 6  
replace choice_situation= 23 if block == 1 &  choiceTask == 7  
replace choice_situation= 24 if block == 1 &  choiceTask == 8 


****** Block=2 
replace choice_situation= 2 if block == 2 & choiceTask == 1  
replace choice_situation= 3 if block == 2 &  choiceTask == 2  
replace choice_situation= 5 if block == 2 &  choiceTask == 3  
replace choice_situation= 7 if block == 2 &  choiceTask == 4  
replace choice_situation= 8 if block == 2 &  choiceTask == 5  
replace choice_situation= 15 if block == 2 &  choiceTask == 6  
replace choice_situation= 17 if block == 2 &  choiceTask == 7  
replace choice_situation= 22 if block == 2 &  choiceTask == 8 

****** Block=3 
replace choice_situation= 6 if block == 3 & choiceTask == 1  
replace choice_situation= 10 if block == 3 &  choiceTask == 2  
replace choice_situation= 12 if block == 3 &  choiceTask == 3  
replace choice_situation= 13 if block == 3 &  choiceTask == 4  
replace choice_situation= 18 if block == 3 &  choiceTask == 5  
replace choice_situation= 19 if block == 3 &  choiceTask == 6  
replace choice_situation= 20 if block == 3 &  choiceTask == 7  
replace choice_situation= 21 if block == 3 &  choiceTask == 8 

tempfile data_process_ch 
save `data_process_ch'


*************************************************************
****** Merge with atributte data and process by block *******
************************************************************* 

**# Block 1
keep if block==1 
rename choice_situation choiceset // Key variable for merging databases
tempfile block1A 
save `block1A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==1

merge 1:m choiceset using `block1A'
tempfile block1_PA 
save `block1_PA'

**# Block 2 
use `data_process_ch', clear 
keep if block==2 
rename choice_situation choiceset // Key variable for merging databases
tempfile block2A 
save `block2A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==2

merge 1:m choiceset using `block2A'
tempfile block2_PA 
save `block2_PA'


**# Block 3 
use `data_process_ch', clear 
keep if block==3 
rename choice_situation choiceset // Key variable for merging databases
tempfile block3A 
save `block3A'

import excel "final_design.xlsx", sheet("matrix") firstrow clear 
rename Block block 
keep if block==3

merge 1:m choiceset using `block3A'
tempfile block3_PA 
save `block3_PA' 


*** Append all blocks ***
use `block1_PA', clear 
append using `block2_PA' 
append using `block3_PA' 


// choiceTask refers to the order in the participant questions, choiceset refers to the different choices obtained in ngene
drop choiceTask 
drop _merge 
gen country_id= "CHI" 
 
 
// fix prices again to continous levels (if necessary) -> Ngene forum Michiel Bliemer response

* E-cig disposable
replace price1 = 0.025 if price1 == 8.75
replace price1 = 0.050 if price1 == 17.5
replace price1 = 0.100 if price1 == 35.5
replace price1 = 0.160 if price1 == 56

* E-cig rechargeable
replace price2 = 0.035 if price2 == 5
replace price2 = 0.045 if price2 == 15
replace price2 = 0.055 if price2 == 25
replace price2 = 0.080 if price2 == 50

* Cigarette
replace price3 = 0.025 if price3 == 0.5
replace price3 = 0.050 if price3 == 1
replace price3 = 0.200 if price3 == 4
replace price3 = 0.350 if price3 == 7

 
tempfile final_Ch 
save `final_Ch'

// Append both data (Colombia and Argentina)
use `final_col', clear 
append using `final_Arg'
append using `final_Ch'


sort country id choiceset

* Group by country and ID for gen an unique identificator
egen unique_id= group(country_id id)

* Identificator for joint estimation in R based on sample (Argentina or Colombia)
gen col = 1 if country_id == "COL"
gen arg = 1 if country_id == "ARG"
gen chi = 1 if country_id == "CHI"
replace col = 0 if missing(col) 
replace arg = 0 if missing(arg)
replace chi = 0 if missing(chi)

drop country_id id 


/* Finally, export to csv depending on how you treat prices (dummy or continous)

export delimited "price_continous.csv", replace
or  
export delimited "price_dummy.csv", replace 

*/

order unique_id block choiceset
export delimited "price_continous.csv", replace
save "price_continous.dta", replace 



* ============================================================
* ANÁLISIS DE PATRONES DE RESPUESTA - VERSIÓN CORREGIDA
* ============================================================

* --- 0. Verificar estructura básica ---
codebook unique_id chosen_option
tab chosen_option, missing

* ============================================================
* PASO 1: Crear todas las variables necesarias en la base long
* ============================================================

* Cuántas preguntas respondió cada persona (debe ser 8)
bysort unique_id: gen n_preguntas = _N

* Cuántas veces eligió cada opción cada persona
bysort unique_id chosen_option: gen freq_opcion = _N

* Cuántas opciones DISTINTAS eligió cada persona
bysort unique_id: egen n_opciones_distintas = nvals(chosen_option)

* Opción más frecuente por persona (moda)
bysort unique_id (freq_opcion chosen_option): gen opcion_modal = chosen_option[_N]

* Frecuencia máxima (cuántas veces repitió su opción más común)
bysort unique_id: egen max_freq = max(freq_opcion)

* Índice de concentración
gen concentracion = max_freq / n_preguntas

* Flags de straight-lining
gen straight_line     = (n_opciones_distintas == 1)
gen casi_straightline = (max_freq >= 7)

* ============================================================
* PASO 2: Colapsar a nivel persona para los reportes
* ============================================================

preserve
    bysort unique_id: keep if _n == 1   // una fila por persona

    di "================================================"
    di "PARTE 1: Número de opciones distintas por persona"
    di "================================================"
    tab n_opciones_distintas, missing

    di "================================================"
    di "PARTE 2: Opción modal (más frecuente) por persona"
    di "================================================"
    tab opcion_modal, missing

    di "================================================"
    di "PARTE 3: Índice de concentración (0.5 - 1.0)"
    di "================================================"
    summarize concentracion, detail
    tab concentracion

    di "================================================"
    di "PARTE 4: STRAIGHT-LINING"
    di "8/8 misma opción:"
    tab straight_line
    di "7+/8 misma opción:"
    tab casi_straightline
    di "================================================"

    di "================================================"
    di "PARTE 5: Estadísticas de freq_opcion por opción"
    di "================================================"
    tabstat max_freq, by(opcion_modal) stats(n mean sd min p50 max)

restore




preserve
    bysort unique_id: keep if _n == 1

    gen tareas_iguales = max_freq

    contract tareas_iguales, freq(freq)
    gen pct = freq / 4366 * 100

    graph bar pct, over(tareas_iguales, ///
        relabel(1 "2/8" 2 "3/8" 3 "4/8" 4 "5/8" 5 "6/8" 6 "7/8" 7 "8/8")) ///
        bar(1, color(ltblue)) ///
        ytitle("Respondents (%)") ///
        title("Distribution of Response Concentration Index") ///
        note("Number of tasks in which respondent chose their most frequent option") ///
        blabel(bar, format(%4.1f) size(small)) ///
        graphregion(color(white)) ///
        bgcolor(white)

    graph export "figura_concentracion.png", replace width(2400)

restore