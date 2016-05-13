* STATA 12
* Clientelism paper (summer 2013).
* Hector Bahamonde

clear all
set more off
* copy http://datasets.americasbarometer.org/datasets/1038116426AmericasBarometer%20Merged%202004-2012%20Rev1.5_FREE_dta.zip, text
* unzipfile http://datasets.americasbarometer.org/datasets/1038116426AmericasBarometer%20Merged%202004-2012%20Rev1.5_FREE_dta.zip
use "/Users/hectorbahamonde/Datos/LAPOP/merge_total/AmericasBarometer Merged 2004-2012 Rev1_FREE.dta"
save "/Users/hectorbahamonde/RU/research/Clientelism_paper/clientelism.dta", replace


********************************************
* Data cleaning and relabeling
********************************************

* relabeling countries.
gen country=pais
drop if country== 24 | country ==23 | country ==26 | country ==40 | country ==41 | country ==22 | country ==25 | country ==27 | country==27 /* not south america */
label define country 1 Mex 2 Guat 3 ElSalv 4 Hond 5 Nic 6 CRic 7 Pan 8 Col 9 Ecu 10 Bol 11 Per 12 Par 13 Chi 14 Uru 15 Bra 16 Ven 17 Arg 21 RDom 0 Other
label values country country

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


* renaming/relabeling jc15a (is it justificable if the president closes the congress?)
gen demval = jc15a
recode demval (2=1) (1=0), generate(demval1)
drop demval
rename demval1 demval
label define demval 1 "Dem. Values" 0 "Non-Dem. Values", replace
label values demval demval
label variable demval "Dem. Val.: Justifiable President closes Congress?"

* renaming/relabeling np2 (did you present a petition in a public office?)
generate localink=np2
recode localink (2=0), generate(localink1)
drop localink
rename localink1 localink
label define localink 1 "Asked for help" 0 "Didn't Asked for help", replace
label values localink localink
label variable localink "Local Linkages: Presented petition in the Municipality?"

*re labeling year
label variable year "Year"

* generating dummies by country

* MŽxico
gen count_mex = country 
replace count_mex = 0 if count_mex != 1

* Guatemala
gen count_guat = country 
replace count_guat = 0 if count_guat != 2

* El Salvador
gen count_elsalv = country 
replace count_elsalv = 0 if count_elsalv != 3

* Honduras
gen count_hond = country 
replace count_hond = 0 if count_hond != 4

* Nicaragua
gen count_nic = country 
replace count_nic = 0 if count_nic != 5

* Costa Rica
gen count_cric = country 
replace count_cric = 0 if count_cric != 6

* Panam‡
gen count_pan = country 
replace count_pan = 0 if count_pan != 7

* Colombia
gen count_col = country 
replace count_col = 0 if count_col != 8

* Ecuador
gen count_ecu = country 
replace count_ecu = 0 if count_ecu != 9

* Bolivia
gen count_bol = country 
replace count_bol = 0 if count_bol != 10

* Perœ
gen count_per = country 
replace count_per = 0 if count_per != 11

* Paraguay
gen count_par = country 
replace count_par = 0 if count_par != 12

* Chile
gen count_chi = country 
replace count_chi = 0 if count_chi != 13

* Uruguay
gen count_uru = country 
replace count_uru = 0 if count_uru != 14

* Brasil
gen count_bra = country 
replace count_bra = 0 if count_bra != 15

* Venezuela
gen count_ven = country 
replace count_ven = 0 if count_ven != 16

* Argentina
gen count_arg = country 
replace count_arg = 0 if count_arg != 17

* Repœblica Dominicana
gen count_rdom = country 
replace count_rdom = 0 if count_rdom != 21

*labeling country dummies.
label values count_* country

************************************
* Only 13 countries that I will use
************************************
note clien1: we don't have Arg, Ven, Bol, Hon Chi for CLIENTELISM question (clien1).
drop if country==4 | country== 10 | country== 13 | country== 16 | country== 17
drop count_hond count_bol count_chi count_ven count_arg
save "/Users/hectorbahamonde/RU/research/Clientelism_paper/clientelism_ONLY13count.dta", replace

************************************
* ...continuing...
************************************

*re labeling clien1
replace clien1 = . if clien1==.c
label variable clien1 "Recieved something during campaigns (fq.)"
label define clien1 1 Fr 2 R 3 Nv, replace

* gen clien1dummy (clientelism yes/no)
gen clien1dummy=clien1
label variable clien1dummy "Recieved something during campaigns (fq./dummy)"
recode clien1dummy (1/2=1) (3=0), generate(clien1dummy1)
drop clien1dummy
rename clien1dummy1 clien1dummy
label define clien1dummy 1 "Freq/Rare" 0 "Nv", replace
label values clien1dummy clien1dummy

* re labeling clien2
note clien2: we don't have Arg, Ven, Bol, Hon Chi for CLIENTELISM question.
label define clien2 1 MoreIncl 2 LessIncl 3 Neutral, replace
label variable clien2 "Influenced Vote Intention"

* re labeling jc1
label define jc1 1 Jus 2 Not, replace

* re label var q10 (family income)
label variable q10 "Total Familiy Income"
label define q10 0 "NoInc" 1 ">$25" 2 "$26-$50" 3 "$51-$100" 4 "$101-$150" 5 "$151-$200" 6 "$201-$300" 7 "$301-$400" 8 "$401-$500" 9 "$501-$750" 10 "<$750", replace

* generating a dummy when we have "electoral year"
gen electy = 0
replace electy = 1 if country == 5 | country == 6 | country == 8 | country == 14 | country == 15 | country == 21
label define electy 1 ElectYear 0 Other, replace
label variable electy "Electoral Year (dummy)"
label values electy electy
tab electy

* generating var of "remunerated ocupation" (dummy)
gen reumunerated = (ocup4a == 1 | ocup4a == 2) if ocup4a < .
label variable reumunerated "Has a remunerated ocupation (dummy)"
label define reumunerated 1 Remunerated 0 NonRemunerated, replace
label values reumunerated reumunerated


* generating dummies for Q10
gen lincome = (q10 == 0 | q10 == 1 | q10 == 2 | q10 == 3) if q10 < .
label variable lincome "Low Income (NoInc-$51/$100)"

gen mincome = (q10 == 4 | q10 == 5 | q10 == 6) if q10 < .
label variable mincome "Mid. Income ($101-$300)"

gen hincome = (q10 == 7 | q10 == 8 | q10 == 9 | q10 == 10) if q10 < .
label variable mincome "Mid. Income ($301-<$750)"

* generating dummy for each income quintile.
gen income0 = (q10 == 0) if q10 < .
gen income1 = (q10 == 1) if q10 < .
gen income2 = (q10 == 2) if q10 < .
gen income3 = (q10 == 3) if q10 < .
gen income4 = (q10 == 4) if q10 < .
gen income5 = (q10 == 5) if q10 < .
gen income6 = (q10 == 6) if q10 < .
gen income7 = (q10 == 7) if q10 < .
gen income8 = (q10 == 8) if q10 < .
gen income9 = (q10 == 9) if q10 < .
gen income10 = (q10 == 10) if q10 < .

label variable income0 "NoInc"
label variable income1 "$25"
label variable income2 "$26-$50"
label variable income3 "$51-$100"
label variable income4 "$101-$150"
label variable income5 "$151-$200"
label variable income6 "201-$300"
label variable income7 "$301-$400"
label variable income8 "$401-$500"
label variable income9 "$501-$750"
label variable income10 "<$750"

label define income0 1 "NoInc" 0 Other, replace
label define income1 1 "$25" 0 Other, replace
label define income2 1 "$26-$50" 0 Other, replace
label define income3 1 "$51-$100" 0 Other, replace
label define income4 1 "$101-$150" 0 Other, replace
label define income5 1 "$151-$200" 0 Other, replace
label define income6 1 "201-$300" 0 Other, replace
label define income7 1 "$301-$400" 0 Other, replace
label define income8 1 "$401-$500" 0 Other, replace
label define income9 1 "$501-$750" 0 Other, replace
label define income10 1 "<$750" 0 Other, replace

* generating q10d dummy: le alcanza el sueldo?
gen alcanza1 = (q10d == 1 | q10d == 2) if q10d < .
gen alcanza0 = (q10d == 3 | q10d == 4) if q10d < .
label variable alcanza1 "Enough money for a month"
label variable alcanza0 "Not enough money for a month"
recast byte alcanza1
recast byte alcanza0

* generating q10de Q10-ENGLISH Version:
gen q10de = q10d
label variable q10de "Enough money for a month?"
label define q10de 1 "Money Enough, Can Save " 2 "Money Enough, Can't Save" 3 "Money not Enough, difficulties" 4 "Money not Enough, big difficulties", replace
label values q10de q10de
recast byte q10de

* generating var. "poor" (dummy)
gen poor = (q10 <= 4) if q10 < .
label variable poor "Poor (income <= $101-$150)"


* generating dummies for "ing4" (democratic support).
gen ing4a = (q10d == 1) if q10d < .
gen ing4b = (q10d == 2) if q10d < .
gen ing4c = (q10d == 3) if q10d < .
gen ing4d = (q10d == 4) if q10d < .
gen ing4e = (q10d == 5) if q10d < .
gen ing4f = (q10d == 6) if q10d < .
gen ing4g = (q10d == 7) if q10d < .
label variable ing4a "Democracy is preferable"
label variable ing4g "Democracy is not preferable"

* generating clien1 rotated (for ZIP).
gen clien1b = clien1
recast byte clien1b
label variable clien1b "Clien1 rotated"
recode clien1b (2=1) (1=2) (3=0), generate(clien1bc)
drop clien1b
rename clien1bc clien1b
label define clien1b 0 "Nv" 1 "R" 2 "Fr", replace
label values clien1b clien1b

* generating b31 recoded (perception of non-institutionalization)
gen instit=b31
recast byte instit
label variable instit "Perception of Non-institutionalization"
recode instit (1/3=0) (4/7=1), generate(instit1)
drop instit
rename instit1 instit
label define instit 0 "No Instit." 1 "Instit.", replace
label values instit instit

* generating different var's for "tamano".
gen natcapital = (tamano == 1) if tamano < . 
label variable natcapital "National Capital"
label define natcapital 0 "Other" 1 "National Capital", replace
label values natcapital natcapital
*
gen bigcity = (tamano == 2) if tamano < .
label variable bigcity "Big City"
label define bigcity 0 "Other" 1 "Big City", replace
label values bigcity bigcity
*
gen medcity = (tamano == 3) if tamano < .
label variable medcity "Medium City"
label define medcity 0 "Other" 1 "Medium City", replace
label values medcity medcity
*
gen smallcity = (tamano == 4) if tamano < .
label variable smallcity "Small City"
label define smallcity 0 "Other" 1 "Small City", replace
label values smallcity smallcity
*
gen ruralarea = (tamano == 5) if tamano < .
label variable ruralarea "Rural Area"
label define ruralarea 0 "Other" 1 "Rural Area", replace
label values ruralarea ruralarea



************************
************************
************************
************************
************************
************************
************************
************************
************************
************************



* generating "poor proportion"
tab country poor 

* Mex
gen pMex = 0
replace pMex = 811/582 if country == 1 & poor == 1

* Guat
gen pGuat = 0
replace pGuat = 1041/303 if country == 2 & poor == 1

* ElSalv
gen pElSalv = 0
replace pElSalv = 947/517  if country == 3 & poor == 1

* Nic
gen pNic = 0
replace pNic = 1121/330  if country == 5 & poor == 1

* CRic
gen pCRic = 0
replace pCRic = 789/381  if country == 6 & poor == 1

* Pan
gen pPan = 0
replace pPan = 1008/480  if country == 7 & poor == 1

* Col
gen pCol = 0
replace pCol = 875/475  if country == 8 & poor == 1

* Ecu
gen pEcu = 0
replace pEcu = 1635/1183  if country == 9 & poor == 1

* Per
gen pPer = 0
replace pPer = 538/833  if country == 11 & poor == 1

* Par
gen pPar = 0
replace pPar = 417/764  if country == 12 & poor == 1

* Uru
gen pUru = 0
replace pUru = 611/791  if country == 14 & poor == 1

* Bra
gen pBra = 0
replace pBra = 2067/296  if country == 15 & poor == 1

* RDom
gen pRDom = 0
replace pRDom = 869/464  if country == 21 & poor == 1

gen poorprop = pMex + pGuat + pElSalv + pNic + pCRic + pPan + pCol + ///
pEcu + pPer + pPar + pUru + pBra + pRDom


drop pMex pGuat pElSalv pNic pCRic pPan pCol ///
pEcu pPer pPar pUru pBra pRDom


* generating interaction term

gen logpoorporp = ln(poorprop*poor)

********************************************
* Only 2010 (everthing elese dropped)
********************************************

drop if year != 2010
save "/Users/hectorbahamonde/RU/research/Clientelism_paper/clientelism_ONLY2010.dta", replace

tabulate clien1, missing
tabulate clien2, missing
tabulate clien1dummy, missing
tabulate q10, missing

********************************************
* Inspecting and Cleaning
********************************************

* Checking for missings.
tabulate clien1, missing
replace clien1 = . if clien1 == .a | clien1== .b

tabulate clien1b, missing
replace clien1b = . if clien1b == .a | clien1b == .b

tabulate clien2, missing
replace clien2 = . if clien2 == .a | clien2== .b | clien2== .c

tabulate clien1dummy, missing
replace clien1dummy = . if clien1dummy == .a | clien1dummy == .b 

tabulate q10, missing
replace q10 = . if q10 == .a | q10 == .b | q10 == .c

tabulate country, missing /* no missings */
tabulate year, missing /* no missings */

tabulate q10d, missing
replace q10d = . if q10d == .a | q10d == .b

tabulate localink, missing
replace localink = . if localink == .a | localink == .b

tab demval, nolabel missing
replace demval = . if demval == .a | demval == .b | demval == .c

tab instit, nolabel missing
replace instit = . if instit == .a | instit == .b 


********************************************
* saving 4 matching and parametric
********************************************

save "/Users/hectorbahamonde/RU/research/Clientelism_paper/clientelism_2010_4matching.dta", replace








