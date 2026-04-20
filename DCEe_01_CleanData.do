clear
* DESC: Este archivo cuenta cuantas encuestas completas se lograron y clasifica
*       en cuotas

global RunPath "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE e-cig/Resultados finales" 
/*
 glo RunPath= "C:\Users\paul.rodriguez\Universidad del rosario\Control Tabaco Facultad Economica - Documentos\DCE e-cig\Resultados finales"
*/



* Establecer el directorio de trabajo
cd "$RunPath






// *Argentina
// glo pais="argentina"
// import excel "results-survey761835.xlsx", firstrow clear
//








// Chile
glo pais="chile"
import excel "results-survey924556.xlsx", firstrow clear

rename genero G01Q05








// * Col
// import excel "results-survey119693.xlsx", firstrow clear
// glo pais="colombia"
// rename genero G01Q05




drop if ticket=="[[PID"
drop if ticket==""


* ANALISIS DE FRASES COMPLETAS POR TIPO DE CONSUMO

* 1. Concatenar respuestas abiertas
egen texto_unido = concat( ///
    Preg1NoFumadoresother ///
    Preg2NoFumadoresother ///
    Preg3NoFumadoresother ///
    Preg4NoFumadoresother ///
    Preg5NoFumadoresother ///
    Preg6NoFumadoresother ///
    Preg7NoFumadoresother ///
    Preg8NoFumadoresother ///
), punct(", ") decode

* 2. Limpiar texto
replace texto_unido = trim(lower(regexr(texto_unido, "[ \t]+", " ")))

************************************************************
* 3. ANALISIS POR GRUPO
************************************************************

* ---------- NO FUMA ----------
preserve
keep if NOFUMA1825 == 1 | NOFUMA26p == 1
keep texto_unido
drop if missing(texto_unido)

contract texto_unido, freq(freq)
gsort -freq

di "==============================================="
di "NO FUMA (18–25 y 26+)"
di "==============================================="

list texto_unido freq, noobs clean
restore


* ---------- SOLO CIGARRILLO CONVENCIONAL ----------
preserve
keep if SOLOCONVEN1825 == 1 | SOLOCONVEN26p == 1
keep texto_unido
drop if missing(texto_unido)

contract texto_unido, freq(freq)
gsort -freq

di "==============================================="
di "SOLO CIGARRILLO CONVENCIONAL (18–25 y 26+)"
di "==============================================="

list texto_unido freq, noobs clean
restore


* ---------- VAPEA / CIGARRILLO ELECTRONICO ----------
preserve
keep if VAPEA1825 == 1 | VAPEA26p == 1
keep texto_unido
drop if missing(texto_unido)

contract texto_unido, freq(freq)
gsort -freq

di "==============================================="
di "VAPEA / CIGARRILLO ELECTRONICO (18–25 y 26+)"
di "==============================================="

list texto_unido freq, noobs clean
restore

************************************************************



/*
* ANALISIS SIMPLE DE TEXTO DE PREGUNTAS ABIERTAS
************************************************************
egen texto_unido = concat(col1 col2 col3), punct(", ")

* 1. Limpiar texto: minúsculas y eliminación de signos
replace texto_unido = lower(texto_unido)
replace texto_unido = regexr(texto_unido, "[[:punct:]]", " ")
replace texto_unido = regexr(texto_unido, "[ \t]+", " ")
replace texto_unido = trim(texto_unido)

* 2. Crear un conjunto temporal solo con texto
preserve
keep texto_unido

* 3. Tokenizar (separar en palabras)
split texto_unido, parse(" ") gen(tok)

* 4. Conservar solo columnas tok*
keep tok*
drop if missing(tok1)

* 5. Apilar palabras en una sola columna (sin reshape, sin guardar)
stack tok*, into(word)
drop if missing(word) | word==""

* 6. Contar frecuencias
contract word, freq(freq)

* 7. Ordenar y mostrar top 40 palabras
gsort -freq
list word freq in 1/40, noobs clean

restore
************************************************************
*/





///////////////////////////////////////////////////////////////////////////////
// En Chile y en Argentina hay muchos duplicados; en ese caso :
// primero, borramos las que no son validas y tienen mas de una entrada; 
// segundo, nos quedamos con la primera entrada 

count // Encuestas con un PID

gen valida = CI=="Sí" & Fin!="" & ///
	( SOLOCONVEN1825==1 | SOLOCONVEN26p ==1 | ///
	  NOFUMA1825 ==1 | VAPEA1825 ==1 | ///
	  VAPEA26p ==1 )
bys ticket: gen ene=_N
gen todrop=1 if ene>1 & valida==0
sort ticket id
count if todrop==1 // Duplicadas no validas
drop if todrop==1

bys ticket: gen enei=_n if todrop!=1
gen todrop2=1 if enei>1 & enei<.


count if todrop2==1 // Duplicadas validas

drop if todrop2==1
drop ene enei todrop todrop2
duplicates list ticket

bysort ticket (id): gen dup=_n  // Ordena por ticket e id y genera numeración
drop if dup > 1  // Conserva solo la primera aparición de cada ticket
drop dup  // Elimina la variable auxiliar
///////////////////////////////////////////////////////////////////////////////

count // Encuestas con un PID unico
count if CI!="Sí" // Encuestas sin CI

keep if CI=="Sí"
count // Encuestas con CI

egen missingDemo = rowmiss( MENOREDAD SOLOCONVEN1825 SOLOCONVEN26p NOFUMA1825 NOFUMA26p VAPEA1825 VAPEA26p)
tab missingDemo // Son 7 variables, así que las observaciones válidas debe 
				// tener exactamente 6 missings
keep if missingDemo==6

count if MENOREDAD==1 // Cuantos menores de edad
keep if MENOREDAD==.

count if NOFUMA26p==1
keep if NOFUMA26p==.

 // Cuantas encuestas sin terminar
count if  Fin==""
keep if Fin!=""

count // Muestra final
count if SOLOCONVEN1825==1
count if SOLOCONVEN26p ==1
count if NOFUMA1825 ==1
count if VAPEA1825 ==1
count if VAPEA26p ==1

* Nuevas cuotas ....................................................
encode G01Q26, gen (ultimovap)
encode FUMO, gen(fumo)

*Convencional no vapeo 12m [18-25]
count if SOLOCONVEN1825==1 | ///
		(fumo>1 & fumo<. & ultimovap==5 ///
		& EDAD<=25)
*Convencional no vapeo 12m [26-64]
count if SOLOCONVEN26p==1 | ///
		(fumo>1 & fumo<. & ultimovap==5 ///
		& EDAD>25)
*Vapeadores ultimos 12m [18-25]
count if ultimovap<5 & EDAD<=25
*Vapeadores ultimos 12m [26-64]
count if ultimovap<5 & EDAD>25
*Vapeadores mas de 12m no conven [18-25]
count if ultimovap==5 & fumo==1 & EDAD<=25
*Vapeadores mas de 12m noven [26-63]
count if ultimovap==5 & fumo==1 & EDAD>25

gen SOLOCONVEN1825xx = SOLOCONVEN1825==1 | ///
		(fumo>1 & fumo<. & ultimovap==5 ///
		& EDAD<=25)
		
gen SOLOCONVEN26pxx = SOLOCONVEN26p==1 | ///
		(fumo>1 & fumo<. & ultimovap==5 ///
		& EDAD>25)

gen VAPEA1825xx = ultimovap<5 & EDAD<=25

gen VAPEA26pxx = ultimovap<5 & EDAD>25

gen VAPEA1825dual = ultimovap<5 & (fumo>1 & fumo<.) & EDAD<=25
gen VAPEA26pdual = ultimovap<5 & (fumo>1 & fumo<.) & EDAD>25

gen VAPEA1825exc = ultimovap<5 & fumo==1 & EDAD<=25 
gen VAPEA26pexc  = ultimovap<5 & fumo==1 & EDAD>25


gen NOFUMA1825xx = NOFUMA1825==1 | (ultimovap==5 & fumo==1 & EDAD<=25)

gen NOFUMA26pxx = NOFUMA26p == 1 | (ultimovap==5 & fumo==1 & EDAD>25)

gen SOLOCONVENHOMBRExx = ((SOLOCONVEN1825==1 | SOLOCONVEN26p==1) & G01Q05 == "Hombre") |(fumo > 1 & fumo < . & ultimovap == 5  & G01Q05 == "Hombre")

gen SOLOCONVENMUJERxx = ((SOLOCONVEN1825==1 | SOLOCONVEN26p==1) & G01Q05 == "Mujer") |(fumo > 1 & fumo < . & ultimovap == 5  & G01Q05 == "Mujer")

gen VAPEAMUJERxx = ultimovap<5 & G01Q05=="Mujer"
gen VAPEAMUJERexcxx = ultimovap<5 & fumo==1 & G01Q05=="Mujer"
gen VAPEAMUJERdualxx = ultimovap<5 & (fumo>1 & fumo<.) & G01Q05=="Mujer"



gen VAPEAHOMBRExx = ultimovap<5 & G01Q05=="Hombre"
gen VAPEAHOMBREexcxx = ultimovap<5 & fumo==1 & G01Q05=="Hombre"
gen VAPEAHOMBREdualxx = ultimovap<5 & (fumo>1 & fumo<.) & G01Q05=="Hombre"

		
gen NOFUMAHOMBRExx = ((NOFUMA1825==1 | NOFUMA26p == 1) & G01Q05=="Hombre") | (ultimovap==5 & fumo==1 & G01Q05=="Hombre")

gen NOFUMAMUJERxx = ((NOFUMA1825==1 | NOFUMA26p == 1) & G01Q05=="Mujer") | (ultimovap==5 & fumo==1 & G01Q05=="Mujer")


gen female = (G01Q05 == "Mujer")	
		
///////////////////////////

*drop groupTime191Temporizacióndel- missingDemo

tab G01Q26 if EDAD<=25




/*
*Colombia 

egen all_time = rowtotal(groupTime194 groupTime195 groupTime196 groupTime273 groupTime274 groupTime275 groupTime276 groupTime277 groupTime278 groupTime279 groupTime280 groupTime197)
gen cat_time = .

replace cat_time = 0 if all_time <= 120
replace cat_time = 1 if all_time > 120
replace cat_time = 2 if all_time <= 60

*/







// *Argentina
// egen all_time = rowtotal(groupTime214 groupTime215 groupTime291 groupTime292 groupTime293 groupTime294 groupTime295 groupTime296 groupTime297 groupTime298 groupTime299 groupTime217)
// gen cat_time = .
//
// replace cat_time = 0 if all_time <= 120
// replace cat_time = 1 if all_time > 120
// replace cat_time = 2 if all_time <= 60
//
//







*Chile
egen all_time = rowtotal(groupTime165 groupTime166 groupTime289 groupTime290 groupTime282 groupTime283 groupTime284 groupTime285 groupTime286 groupTime287 groupTime288 groupTime168)
gen cat_time = .

replace cat_time = 0 if all_time <= 120
replace cat_time = 1 if all_time > 120
replace cat_time = 2 if all_time <= 60



export excel using "$pais.xlsx", firstrow(variables) replace
save "$pais.dta", replace


* ESTADÍSTICAS DESCRIPTIVAS DE TIEMPO - TODOS LOS PAÍSES

use "colombia.dta", clear
keep all_time cat_time ///
     SOLOCONVEN1825xx SOLOCONVEN26pxx ///
     VAPEA1825xx VAPEA26pxx ///
     NOFUMA1825xx NOFUMA26pxx
gen pais = "Colombia"
tempfile colombia_clean
save `colombia_clean'

use "chile.dta", clear
keep all_time cat_time ///
     SOLOCONVEN1825xx SOLOCONVEN26pxx ///
     VAPEA1825xx VAPEA26pxx ///
     NOFUMA1825xx NOFUMA26pxx
gen pais = "Chile"
tempfile chile_clean
save `chile_clean'

use "argentina.dta", clear
keep all_time cat_time ///
     SOLOCONVEN1825xx SOLOCONVEN26pxx ///
     VAPEA1825xx VAPEA26pxx ///
     NOFUMA1825xx NOFUMA26pxx
gen pais = "Argentina"
append using `colombia_clean'
append using `chile_clean'

replace all_time = all_time / 60
label variable all_time "Tiempo (minutos)"

di "--- Distribución cat_time (0=normal, 1=lento, 2=muy rápido) ---"
tab cat_time, missing

di "--- Estadísticas generales de tiempo (minutos) ---"
summarize all_time, detail

di "--- Tiempo promedio por cat_time ---"
tabstat all_time, by(cat_time) stats(n mean sd p25 p50 p75)

di "--- cat_time por grupo de consumo ---"
foreach grupo in SOLOCONVEN1825xx SOLOCONVEN26pxx ///
                 VAPEA1825xx VAPEA26pxx ///
                 NOFUMA1825xx NOFUMA26pxx {
    di "  >> Grupo: `grupo'"
    tabstat all_time if `grupo'==1, ///
        stats(n mean sd p25 p50 p75) col(stats)
    tab cat_time if `grupo'==1, missing
}

preserve
    gen grupo_consumo = ""
    replace grupo_consumo = "Convencional 18-25" if SOLOCONVEN1825xx == 1
    replace grupo_consumo = "Convencional 26+"   if SOLOCONVEN26pxx  == 1
    replace grupo_consumo = "Vapea 18-25"        if VAPEA1825xx      == 1
    replace grupo_consumo = "Vapea 26+"          if VAPEA26pxx       == 1
    replace grupo_consumo = "No fuma 18-25"      if NOFUMA1825xx     == 1
    replace grupo_consumo = "No fuma 26+"        if NOFUMA26pxx      == 1

    tabstat all_time, by(grupo_consumo) ///
        stats(n mean sd p25 p50 p75 max) nototal col(stats)

    collapse (count) n=all_time          ///
             (mean)  mean_time=all_time   ///
             (sd)    sd_time=all_time     ///
             (p25)   p25_time=all_time    ///
             (p50)   median_time=all_time ///
             (p75)   p75_time=all_time    ///
             (max)   max_time=all_time,   ///
             by(grupo_consumo)

    export excel using "descriptivas_tiempo_total.xlsx", ///
        firstrow(variables) replace
    di "  >> Exportado: descriptivas_tiempo_total.xlsx"
restore