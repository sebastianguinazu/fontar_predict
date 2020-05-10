********************************************************************************
***** MODELO PREDICTIVO PARA FONTAR
********************************************************************************

clear all
set more off

***** Rutas *****
global path_raiz "C:\Users\Sebastian\Desktop\BASES Y DO-FILES\FONTAR\Predict"
global path_bdstrab "$path_raiz\BDs"
global path_bds "C:\Users\Sebastian\Desktop\BASES Y DO-FILES\BDs Google Drive"
global path_bdsprog "$path_bds\bds_produccion"


********************************************************************************
***** Armado de base *****
********************************************************************************

use "$path_bdstrab/fontar_0717.dta", clear

rename cuit id
drop Entidad

replace id= id * 1000000000 if id<100
replace id= id * 10 if id<10000000000
format id %11.0f

* Elimino observaciones repetidas
bys id year: gen N=_N
egen aux=mean(ai_) if N==2, by(id)
drop if ai_==. & N==2 & aux!=.
drop N aux
bys id year: gen N=_N
bys id year: drop if _n==1  & N==2 

***** Mergeo con base de beneficiarios *****
merge 1:1 id year using "$path_bdsprog/BDs_fontar"

* Elimino firmas que recibieron antes de 2010 (no tienen 3 obs previas)
egen aux=mean(fontar_cant) if year<2010, by(id)
drop if aux!=.
drop aux N

*** Me quedo con empresas beneficiarias con 3 obs previas
gen d_fontar=_merge==3
drop _merge
gen d_fontar_year=year if d_fontar
egen d_fontar_yearmin=min(d_fontar_year), by(id)

* Elimino las firmas que no son beneficiarias
egen aux=mean(d_fontar), by(id)
gen d_fontar_todo=aux>0 & aux!=.
drop if d_fontar_todo==0
drop aux

* Armo una nuevavariable de fontar_cant con la info que tengo
drop fontar_cant
egen fontar_cant = sum(d_fontar), by(id)

* Genero variable para identificar los anios segun el primer beneficio
gen t=year-d_fontar_yearmin

* Elimino las firmas que no tienen 3 obs previas y ninguna posterior
egen aux=min(t), by(id)
drop if aux>-3 & aux!=. 
egen aux2=max(t), by(id)
drop if aux2==0
drop aux*

* Elimino las obs previas y posteriores de mas de 3 anios (no las vamos a usar)
drop if t<-3
sort id year

***** Genero variables de resultado en t>0
foreach i in i_prod_ i_proc_ i_org_ i_com_ {
	egen aux_`i'= max(`i') if t>0 & t!=., by (id)
	egen target_`i'= max(aux_`i'), by (id)
	replace target_`i'=0 if target_`i'==. 
}
drop if t>=0


* Genero variables con el promedio pret t

* Elimino variables de mas
drop merge1 merge2 Lid_empleo

********************************************************************************
***** Cruzo con variables de bases administrativas que varian en el tiempo
********************************************************************************

* Empleo
merge 1:1 id year using "$path_bds/BDs_empleo.dta", nogen
keep if d_fontar_todo == 1
drop n empleo_total

* Impuestos
merge 1:1 id year using "$path_bds\bds_afip/BDs_afip_impuestos", nogen
keep if d_fontar_todo == 1

* Expo
merge 1:1 id year using "$path_bds/BDs_expo.dta", nogen
keep if d_fontar_todo == 1

* Impo
merge 1:1 id year using "$path_bds/BDs_impo.dta", nogen
keep if d_fontar_todo == 1

* Deuda
merge 1:1 id year using "$path_bds\bds_bcra/BDs_credito", nogen
keep if d_fontar_todo == 1
gen deuda_mora = situacion_ajust > 2 & situacion_ajust != .
gen accede = credito>0 & credito!=.

********************************************************************************
***** Correccion manual de variables *****
********************************************************************************

* Variables de innovación: las pasamos a dummys
replace i_prod_ = 1 if i_prod_ >0 & i_prod_!=.
replace i_proc_ = 1 if i_proc_ >0 & i_proc_!=.
replace i_org_ = 1 if i_org_ >0 & i_org_!=.
replace i_com_ = 1 if i_com_ >0 & i_com_!=.
 
* Variables de patentes: las pasamos a dummys / agrupamos
replace pat_solic= 1 if pat_solic>0 & pat_solic!=.
replace pat_obt= 1 if pat_obt>0 & pat_obt!=.

drop pat_solic_arg_ pat_obt_arg_
gen pat_solict_internacional= (pat_solic_usa_>0 & pat_solic_usa_!=.) | (pat_solic_eur_>0 & pat_solic_eur_!=.) | (pat_solic_rm_>0 | pat_solic_rm_!=.)
drop pat_solic_usa_ pat_solic_eur_ pat_solic_rm_
gen pat_obt_internacional= (pat_obt_usa_>0 & pat_obt_usa_!=.) | (pat_obt_eur_>0 & pat_obt_eur_!=.) | (pat_obt_rm_>0 | pat_obt_rm_!=.)


* Para las variables i_prodmej_ i_int_com habria que ver que son 1 y 2

* Variables de financiamiento: las pasamos a dummys
foreach i of varlist ff_id_-ff_ai_resto {
	replace `i' = 1 if `i'>0 & `i'!=. 
}

********************************************************************************
***** Guardo la base ***** 
********************************************************************************

save "$path_raiz/fontar_predict.dta", replace

********************************************************************************
***** Genero bases wide ***** 
********************************************************************************

use "$path_raiz/fontar_predict.dta", clear

* Genero string de variable tiempo
gen period = "3yearb" if t==-3
replace period = "2yearb" if t==-2
replace period = "1yearb" if t==-1

* Borro las variables que no pude corregir
drop i_prodmej_-i_int_com d_fontar d_fontar_year d_fontar_todo year t

order id period d_fontar_yearmin fontar_cant

reshape wide ai_ - pat_obt_internacional, i(id fontar_cant) j(period) string

********************************************************************************
***** Cruzo con variables de bases administrativas que no varian en el tiempo
********************************************************************************

* Sector
merge 1:1 id using "$path_bds\bds_afip/BDs_afip_actividades", nogen
keep if  fontar_cant != .

* Datos de la empresa
merge 1:1 id using "$path_bds\bds_afip/BDs_afip_creacion", nogen
keep if  fontar_cant != .

* Ubicacion
merge 1:1 id using "$path_bds/BDs_localidad", nogen
keep if  fontar_cant != .

********************************************************************************
***** Guardo la base ***** 
********************************************************************************

use "$path_bdstrab/fontar_0717.dta", clear

export delimited "$path_raiz/w_fontar_predict.csv", replace

save "$path_raiz/w_fontar_predict.dta", replace

