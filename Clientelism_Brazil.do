****************************
* Brasil Only (2010) 
****************************

clear all
set more off
cd "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets"
use "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism_Brazil_2010.dta"

* creating unique identifier.
generate index = _n
label var index "Obs. Num."

*re labeling/naming urban/rural
gen urban=ur
recode urban (2=0), generate(urban1)
drop urban
rename urban1 urban
label define urban 1 "Ur" 0 "Ru", replace
label values urban urban
label variable urban "Urban"

* relabeling q1 "gender" to "male"
gen male=q1
recode male (2=0), generate(male1)
drop male
rename male1 male
label define male 1 "Male" 0 "Female", replace
label values male male
label variable male "Male"

* renaming/relabeling jc15a (is it justificable if the president closes the congress?)
gen demval = jc15a
recode demval (2=1) (1=0), generate(demval1)
drop demval
rename demval1 demval
label define demval 1 "Dem. Values" 0 "Non-Dem. Values", replace
label values demval demval
label variable demval "Dem. Val.: Justifiable President closes Congress?"

*re labeling clien1
gen clientelism = clien1
replace clientelism = . if  clientelism==.c
recode clientelism (1=2) (2=1) (3=0), generate(clientelism1)
drop clientelism
rename clientelism1 clientelism
label variable clientelism "Recieved something during campaigns (fq.)"
label define clientelism 2 "Often" 1 "Sometimes" 0 "Never", replace
label values clientelism clientelism


* gen clien1dummy (clientelism yes/no)
gen clien1dummy=clientelism
label variable clien1dummy "Recieved something during campaigns"
recode clien1dummy (2=1) (1=1) (1=0), generate(clien1dummy1)
drop clien1dummy
rename clien1dummy1 clien1dummy
label define yesno 1 "Yes" 0 "No", replace
label values clien1dummy  yesno


* recoding np2
recode np2 (2=0), generate(np21)
drop np2
rename np21 np2
label define yesno 1 "Yes" 0 "No", replace
label values np2  yesno

* duplicating "income"
gen income = q10

* generating "poor"
gen poor = (income<=3)
label define poor 1 "p" 0 "np", replace
label values poor poor

* generating b31 recoded (perception of non-institutionalization)
gen instit=b31
recast byte instit
label variable instit "Perception of Non-institutionalization"
recode instit (1/3=0) (4/7=1), generate(instit1)
drop instit
rename instit1 instit
label define instit 0 "No Instit." 1 "Instit.", replace
label values instit instit

* Recoding Perception of Corruption
recode exc7 (4=0) (3=1) (2=2) (1=3), gen(exc7new)
drop exc7
rename exc7new exc7




label variable exc7 "Percept. of Corruption"
label variable np2 "Bureaucracy Contacted him/her"
*label variable cp13 "Political Party Contacted him/her"
label variable urban "Urban"
label variable ed "Years of Education"
label variable male "Male"
label variable vb3 "Party Id"



********************************************
* Inspecting and Cleaning
********************************************

* Checking for missings.
tabulate clientelism, missing
replace clientelism = . if clien1 == .a | clien1== .b

tabulate clien1dummy, missing
replace clien1dummy = . if clien1dummy == .a | clien1dummy == .b 

tabulate income, missing
replace income = . if income == .a | q10 == .b | q10 == .c

tab demval, nolabel missing
replace demval = . if demval == .a | demval == .b | demval == .c

tab instit, nolabel missing
replace instit = . if instit == .a | instit == .b 


********************************************
* Saving clean data
********************************************

*save "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism_Brazil_2010_Clean_Relabeled.dta", replace

********************************************
* Appending two new columns (State and Municipality, names corrected)
********************************************


merge 1:1 idnum using "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism_Brazil_2010_states_municipios.dta"

drop _merge
order idnum state_ok municipio_ok
label variable state_ok "State"
label variable municipio_ok "Municipality"

* Labeling Codes/Names of States

tab state_ok, generate(states)
replace states1 = 1 if states1 == 1
replace states2 = 2 if states2 == 1
replace states3 = 3 if states3 == 1
replace states4 = 4 if states4 == 1
replace states5 = 5 if states5 == 1
replace states6 = 6 if states6 == 1
replace states7 = 7 if states7 == 1
replace states8 = 8 if states8 == 1
replace states9 = 9 if states9 == 1
replace states10 = 10 if states10 == 1
replace states11 = 11 if states11 == 1
replace states12 = 12 if states12 == 1
replace states13 = 13 if states13 == 1
replace states14 = 14 if states14 == 1
replace states15 = 15 if states15 == 1
replace states16 = 16 if states16 == 1
replace states17 = 17 if states17 == 1

egen statesok2 = rowtotal(states*)
rename statesok2 temporalstate
drop states*
rename temporalstate state
order state

label define state 1 "Acre" 2 "Alagoas" 3 "Bahia" 4 "Ceará" 5 "Distrito Federal" ///
6 "Goiás" 7 "Mato Grosso" 8 "Minas Gerais" 9 "Paraná" 10 "Pará" 11 "Pernambuco" ///
12 "Rio Grande do Norte" 13 "Rio Grande do Sul" 14 "Rio de Janeiro" 15 "Rondônia" ///
16 "Santa Catarina" 17 "São Paulo", replace 
label values state state

/* checking */
order  state state_ok
sort state state_ok
/* ok */ drop state_ok

* Labeling Codes/Names of Munic.

tab municipio_ok, generate(municipios)

replace municipios1 = 1 if municipios1 == 1
replace municipios2 = 2 if municipios2 == 1
replace municipios3 = 3 if municipios3 == 1
replace municipios4 = 4 if municipios4 == 1
replace municipios5 = 5 if municipios5 == 1
replace municipios6 = 6 if municipios6 == 1
replace municipios7 = 7 if municipios7 == 1
replace municipios8 = 8 if municipios8 == 1
replace municipios9 = 9 if municipios9 == 1
replace municipios10 = 10 if municipios10 == 1
replace municipios11 = 11 if municipios11 == 1
replace municipios12 = 12 if municipios12 == 1
replace municipios13 = 13 if municipios13 == 1
replace municipios14 = 14 if municipios14 == 1
replace municipios15 = 15 if municipios15 == 1
replace municipios16 = 16 if municipios16 == 1
replace municipios17 = 17 if municipios17 == 1
replace municipios18 = 18 if municipios18 == 1
replace municipios19 = 19 if municipios19 == 1
replace municipios20 = 20 if municipios20 == 1
replace municipios21 = 21 if municipios21 == 1
replace municipios22 = 22 if municipios22 == 1
replace municipios23 = 23 if municipios23 == 1
replace municipios24 = 24 if municipios24 == 1
replace municipios25 = 25 if municipios25 == 1
replace municipios26 = 26 if municipios26 == 1
replace municipios27 = 27 if municipios27 == 1
replace municipios28 = 28 if municipios28 == 1
replace municipios29 = 29 if municipios29 == 1
replace municipios30 = 30 if municipios30 == 1
replace municipios31 = 31 if municipios31 == 1
replace municipios32 = 32 if municipios32 == 1
replace municipios33 = 33 if municipios33 == 1
replace municipios34 = 34 if municipios34 == 1
replace municipios35 = 35 if municipios35 == 1
replace municipios36 = 36 if municipios36 == 1
replace municipios37 = 37 if municipios37 == 1
replace municipios38 = 38 if municipios38 == 1
replace municipios39 = 39 if municipios39 == 1
replace municipios40 = 40 if municipios40 == 1
replace municipios41 = 41 if municipios41 == 1
replace municipios42 = 42 if municipios42 == 1
replace municipios43 = 43 if municipios43 == 1
replace municipios44 = 44 if municipios44 == 1
replace municipios45 = 45 if municipios45 == 1
replace municipios46 = 46 if municipios46 == 1
replace municipios47 = 47 if municipios47 == 1
replace municipios48 = 48 if municipios48 == 1
replace municipios49 = 49 if municipios49 == 1
replace municipios50 = 50 if municipios50 == 1
replace municipios51 = 51 if municipios51 == 1
replace municipios52 = 52 if municipios52 == 1
replace municipios53 = 53 if municipios53 == 1
replace municipios54 = 54 if municipios54 == 1

egen municipiosok2 = rowtotal(municipios*)

rename municipiosok2 temporalmunicipios
drop municipios*
rename temporalmunicipios municipality


label define municipality 1 "Acopiara" 2 "Aloandia" 3	"Aparecida de Goiania" ///
4 "Belo Horizonte" 5 "Belem" 6 "Blumenau" 7	"Branquinha" 8 "Brasilia" ///
9 "Capela" 10 "Coronel Ezequiel" 11	"Cuiaba" 12	"Curitibanos" 13 "Duque de Caxias" ///
14 "Embu Guacu" 15 "Fortaleza" 16 "Franca" 17 "Itagiba" 18 "Itaguaje" 19 "Itumbiara" ///
20 "Itupeva" 21 "Jaboatao dos Guararapes" 22 "Jaciara" 23 "Jaragua do Sul" 24 "Ji Parana" ///
25 "Jijoca de Jericoacoara" 26 "Juazeiro" 27 "Lontra" 28 "Marilia" 29 "Minacu" 30 "Mogi das Cruzes" ///
31 "Mossoro" 32 "Narandiba" 33 "Pacaja" 34 "Passos" 35 "Pelotas" 36 "Ponta Grossa" 37 "Porecatu" ///
38 "Porto Esperidiao" 39 "Porto Velho" 40 "Pocoes" 41 "Progresso" 42 "Redencao" ///
43 "Rio Bonito" 44 "Rio Branco" 45 "Rio de Janeiro" 46 "Senador Guiomard" ///
47 "Sao Jose dos Campos" 48	"Sao Joao del Rei" 49 "Sao Lourenco" 50	"Sao Paulo" ///
51 "Timbauba" 52 "Uaua" 53 "Vera Cruz" 54 "Vilhena", replace

label values municipality  municipality

/* checking */
order state municipality municipio_ok
sort municipality municipio_ok
/* ok */ drop municipio_ok
order state municipality


********************************************
* Including data set: proportion of poor people (Census Official Data)
********************************************

merge m:1 municipality using "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/proportionofpoorcensus.dta"
drop _merge fakemunicip
order state municipality wagehalf wagequarter wage70reales wageuntil60median

label variable wagehalf "Density of the Poor"
label variable wagequarter "Proportion of People with a Quarter of the Min. Wage"
label variable wage70reales "Proportion of People with a 15% of the Min. Wage"
label variable wageuntil60median "Proportion of People with a 60% of the Median"

* Recode  wagehalfp in percentage ("wagehalfper")
gen wagehalfper = wagehalf
recode wagehalfper (0/10=10) (10.1/20=20)  (20.1/30=30)  (30.1/40=40)  (40.1/50=50)  (50.1/60=60)  (60.1/70=70)  (70.1/80=80)  (80.1/90=90)  (90.1/100=100), generate(wagehalfper1)
drop wagehalfper
rename wagehalfper1 wagehalfper
label define wagehalfper 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%" 70 "70%" 80 "80%" 90 "90%" 100 "100%", replace
label values wagehalfper wagehalfper
label variable wagehalfper "Proportion of People with Half of the Min. Wage (cum. %)"

* Generating LNwagehalf (normalizing dependent var.)
gen lnwagehalf = ln(wagehalf)
label variable lnwagehalf "Prop. of People with Half of the Min. Wage (ln(wagehalf))"

* Generating LNwagehalfSQ (Sq. term of ln(wagehalf))
gen lnwagehalfsq = lnwagehalf^2
label variable lnwagehalfsq "Prop. of People with Half of the Min. Wage (ln(wagehalf)^2)"

* Generating "wagehalf collapsed" (10-->50 / 60/90)
gen wagehcolper = wagehalfper
recode  wagehcolper (60/90=60)
label define  wagehcolper 10 "10%" 20 "20%" 30 "30%" 40 "40%" 50 "50%" 60 "60%+", replace
label values  wagehcolper wagehcolper
tab  wagehcolper

* Generating "Political Involvement"
recode cp6 (4=0) (3=1) (2=2) (1=3) , generate(polinv1)
recode cp7 (4=0) (3=1) (2=2) (1=3) , generate(polinv2)
recode cp8 (4=0) (3=1) (2=2) (1=3) , generate(polinv3)
recode cp9 (4=0) (3=1) (2=2) (1=3) , generate(polinv4)
recode cp13 (4=0) (3=1) (2=2) (1=3) , generate(polinv5)

egen polinv = rowtotal(polinv1 polinv3 polinv5)
label variable polinv "Political Involvement Index"

* generating large-small size of the poor
recode wagehalf (5.8/27.5 =0) (27.6/80.9=1), gen(large)
drop large /* Im gonna generate this in R, using the actual dataset*/

recast byte clien1dummy urban income np2
drop if clien1dummy==. |  exc7==. | np2==. | cp13==. | urban==. | ed==. | polinv1-polinv5==.
* May 4th 2016: I removed income==. since the idea is to use the wealth index (Cordova 2009), which supposedly corrects bias of self-reporting. If I had this on, I'd still have bias in the index. Alas, I've got no NAs for INCOME.
*drop if clien1dummy==. |  lnwagehalf==. | lnwagehalfpersq==. | income==. | incomesq==. | exc7==. | np2==. | cp13==. | urban==. | ed==. 
*save "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism_Brazil_2010_St_Mun_Corrected_READY.dta", replace


* merge with municipal opposition dataset
*use "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism_Brazil_2010_St_Mun_Corrected_READY.dta", replace
merge m:1 municipality using "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/MunicipalOpposition.dta"
drop _merge


keep municipality wagehalf cp13 vb3 ing4 ed wt urban clientelism clien1dummy np2 income exc7 polinv1 polinv2 polinv3 polinv4 polinv5 polinv /*large*/  munopp male   r1 r3 r4 r4a r5 r6 r7 r12 r14 r15 

rename r1 wealth1
rename r3 wealth2
rename r4 wealth3
rename r4a wealth4
rename r5 wealth5
rename r6 wealth6
rename r7 wealth7
rename r12 wealth8
rename r14 wealth9
rename r15 wealth10

saveold "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism.dta", replace version(12)