********************************************************
* The Alcohol Harm Paradox (AHP) in Chile: 
* An assessment using the National Health Survey (NHS)
* ------------------------------------------------------
* Database construction
* Comments to: pabloroblero.v@gmail.com
********************************************************

/* Use this do-file to process the NHS 3 (2016-2017) and generate the final
dataset for the analyses */

*************************
* I. NHS 3: 2016-2017
*************************
{
*---------------------
* General adjustments
*---------------------
clear all
version 15
set more off
cd "G:\Mi unidad\Trabajos e investigaciones\Alcohol JP\5. (2023-24)\Study Chile - ENS"
use NHS2016-17

rename *, lower // all vars lowercase

*-----------------------
* [I] EXPANSION WEIGHTS
*-----------------------
{
/* The NHS 2016-17 used complex, multi-stage sampling (see MINSAL, 2017, p. 7).
The blood tests that allow us to measure liver damage were only applied to the
Random Subsample (Submuestra Aleatoria -SMA- in Spanish). The respective dataset
is EX2.

Our study is restricted to that sample, so we must use the expansion factors
'fexp_ex2p_corr' and 'fexp_f1f2f2ex2p_corr'. According to MINSAL (2017, pp. 8-9)
we should stick with the latter, because we cross-reference data from the EX2, 
F1 and F2 databases (the latter contain individuals' responses to forms 1 and 2:
see pdfs). In the first part of the code, I keep only the SMA, dropping the
individuals with missings in the "fexp_f1f2f2ex2p_corr" (in any case,
I compared the differences in the results using one or the other
expansion factor and it is more or less the same). 
Also, I think "probability" or "sampling" weights are used for inference.
This whole issue of expansion factors is complicated and I'm not entirely
sure I'm doing it right.
*/

*--------------------------------
* (1) ROUNDED FREQUENCY WEIGHTS
*--------------------------------

codebook  fexp_f1f2ex2p_corr // missings: 2,386/6,233; N= 3847
gen fw=round(fexp_f1f2ex2p_corr)
drop if fw == . // Keep sample of interest

/* To count the population:
keep fw
expand fw // population: 14,518,950 */

*--------------------------------------
* [2] PROBABILITY OR SAMPLING WEIGHTS
*--------------------------------------
gen pw = fw/14518950
sum pw, d
}

**--------------------------------
** [II] SOCIODEMOGRAFIC VARIABLES
**--------------------------------
{
**----------
** [3] SEX
**----------
{
codebook sexo
gen sex=.
replace sex=0 if sexo==2
replace sex=1 if sexo==1
label drop Sexo
tab sexo sex

label var sex "Sex"
label define sex 0 "Woman" 1 "Man"
label values sex sex
tab sex, m
tab sex [fw=fw] , m
}

**----------------
** [4] AGE GROUP
**----------------
{
codebook edad
rename edad age
label var age "Age"
tab age, m

sum age [fw=fw], d
histogram age [fw=fw], norm

gen ageg=.
replace ageg=1 if age >=15 & age<30
replace ageg=2 if age >=30 & age<64
replace ageg=3 if age >=65 & age<100
tab ageg [fw=fw], m
label var ageg "Age group"
label define ageg 1 "[15-29]" 2 "[30-64]" 3 "[65+]"
label values ageg ageg

sum age [fw=fw], d

}

**--------------
** [5] RURALITY
**--------------
{
gen rural=0
replace rural=1 if zona==2

label var rural "Rural Area"
label define rural 0 "No" 1 "Yes"
label values rural rural

}
}

**-------------------------------
** [III] SOCIOECONOMIC VARIABLES
**-------------------------------
{
**-----------------------
** [6] EDUCATION GROUP
**-----------------------
{
codebook nedu1_minsal_1
tab nedu1_minsal_1, m

gen education= nedu1_minsal_1
label var education "Education"
label define education 1 "<8 years" 2 "8-12 years" 3 "13+ years"
label values education education
tab education, m
}

**---------------------------------
** [7] PER CAPITA HOUSEHOLD INCOME
**---------------------------------
{//
{// Household income
tab as27, m // 686 (17.83%) missings

// Impute missings with the median income of the corresponding category
tab as27 as28, m

gen household_income = as27
label var household_income "Total household income"

forval i= 1/11 {
sum as27 if as28==`i', d
ret list
replace household_income = r(p50) if household_income == . & as28 == `i'
}

tab household_income, m // 561 (14.58%) missings
histogram household_income, norm

// Impute missings with information about health coverage
codebook as5_1
tab as5_1 if household_income==., m 

forval i=1/9 {
sum household_income if as5_1==`i', d
ret list
replace household_income = r(p50) if household_income == . & as5_1==`i'
}
tab household_income, m // 11 missings (0.29%)

// Doesn't know (code = .a)
forval i=1/3 {
sum household_income if as5_1==.a & education == `i', d
ret list
replace household_income = r(p50) if household_income ==. & as5_1==.a & education == `i'
}
tab household_income, m // No missings
tab household_income [fw=fw], m // No missings
histogram household_income, norm
}

{// Per capita household income
** For methodological details, see [1] (references at the end of the do-file)
gen hincpc = household_income/(n_per^0.7)
label var hincpc "Per capita household income"
tab hincpc
tab hincpc [fw=fw] // rare results: 46.75% < $158,145
histogram hincpc [fw=fw], norm

// Categorize
xtile hincpc_cat = hincpc [fw=fw], nq(3)
label var hincpc_cat "Per capita household income, terciles"
label define hincpc_cat 1 "Lowest" 2 "Middle" 3 "Highest"
label values hincpc_cat hincpc_cat
tab hincpc_cat [fw=fw]
bysort hincpc_cat: sum hincpc [fw=fw]

xtile hincpc_cat5 = hincpc [fw=fw], nq(5)
label var hincpc_cat5 "Per capita household income, quintiles"
label values hincpc_cat5 hincpc_cat5
tab hincpc_cat5 [fw=fw]
bysort hincpc_cat5: sum hincpc [fw=fw]

xtile hincpc_cat10 = hincpc [fw=fw], nq(10)
label var hincpc_cat10 "Per capita household income, quintiles"
label values hincpc_cat10 hincpc_cat10
tab hincpc_cat10 [fw=fw]
bysort hincpc_cat10: sum hincpc [fw=fw]

}
}

**------------------------------
** [8] PRIVATE HEALTH COVERAGE
**------------------------------
{// 
codebook as5_1
tab as5_1, m nolabel

gen private_health=.
replace private_health=0 if as5_1 >= 1 & as5_1 <= 5
replace private_health=1 if as5_1 == 6 | as5_1 == 7

tab hincpc_cat if as5_1==8
replace private_health=1 if as5_1 == 8 & hincpc_cat==3 // Imputed values
replace private_health=0 if as5_1 == 8 & hincpc_cat!=3 // Imputed values

tab as5_esp_1 hincpc_cat if as5_1==9
replace private_health=1 if as5_1 == 9 & hincpc_cat==3 // Imputed values
replace private_health=0 if as5_1 == 9 & hincpc_cat!=3 // Imputed values

tab hincpc_cat if as5_1==.a
replace private_health=1 if as5_1 == .a & hincpc_cat==3
replace private_health=0 if as5_1 == .a & hincpc_cat!=3

label var private_health "Private health coverage"
label define private_health 0 "No" 1 "Yes"
label values private_health private_health

tab private_health [fw=fw], m

tab private_health hincpc_cat  [fw=fw], m col
}

**----------------------------------
** [9] ADDITIONAL HEALTH INSURANCE
**----------------------------------
{// 
codebook as6_1
recode as6_1 (1/2=1) (3=0) (.a=0), gen(health_insurance)
labe var health_insurance "Additional health insurance"
label define health_insurance 0 "No" 1 "Yes"
label values health_insurance health_insurance

tab health_insurance, m

tab health_insurance private_health [fw=fw], m col
tab health_insurance hincpc_cat, m col
}
}

**---------------------
** [IV] ALCOHOL CONSUMPTION
**---------------------
{
**-----------------------
** [10] PREVALENCE-YEAR
** [11] PREVALENCE-MONTH
** [12] PREVALENCE-WEEK
**-----------------------
{
// Prevalence-year: At least once a year
codebook m7p9
tab m7p9, m

recode m7p9 (1=0) (2/5=1), gen(prev_year)
label var prev_year "Year prevalence"
tab prev_year, m
tab prev_year [fw=fw], m // 1: 70% 


// Prevalence-month: More than once a month
recode m7p9 (1/2=0) (3/5=1), gen(prev_month)
label var prev_month "Month prevalence"
tab prev_month, m
tab prev_month [fw=fw], m // 34%

// Prevalence-week: At least 2 times a week
recode m7p9 (1/3=0) (4/5=1), gen(prev_week)
label var prev_week "Week prevalence"
tab prev_week, m
tab prev_week [fw=fw], m // 10%
}

**-----------------------
** [13] AUD
**-----------------------
{ 
**0-7: Low risk
**8-15: Middle risk
**16-19: High risk
**20-40: Probable addiction

// Questions:
describe m7* 

// Possible answers:
label list m7p9  // 1-5
label list m7p17 // 1-3
 
** Recode:
tab m7p9, m // 1 missing
drop if m7p9==.

recode m7p9 (1=0) (2=1) (3=2) (4=3) (5=4), gen(audit1) 
tab audit1 m7p9, m // OK

recode m7p10a (1=0) (2=1) (3=2) (4=3) (5=4) (.=0), gen(audit2) 
tab audit2 m7p10a, m // 1,369 missing values coded as 0 (abstainers)

tab m7p11b, m
recode m7p11b (1=0) (2=1) (3=2) (4=3) (5=4) (.=0), gen(audit3_women)
tab audit3_women m7p11b, m // Vemos el resultado

tab m7p11c, m
recode m7p11c (1=0) (2=1) (3=2) (4=3) (5=4) (.=0), gen(audit3_men)
tab audit3_men m7p11c, m // Vemos el resultado

rename m7p12 p4
rename m7p13 p5
rename m7p14 p6
rename m7p15 p7
rename m7p16 p8

forval i=4/8 {
recode p`i' (1=0) (2=1) (3=2) (4=3) (5=4) (.=0), gen(audit`i')
}

recode m7p17 (1=0) (2=2) (3=4) (.=0), gen(audit9)
tab m7p17 audit9, m

recode m7p18 (1=0) (2=2) (3=4) (.=0), gen(audit10)
tab m7p18 audit10, m

// SUM
gen sum_audit= .
replace sum_audit= audit1 + audit2 + audit3_women + audit4 + audit5 + audit6 + audit7 + audit8 + audit9 + audit10 if sex==0
replace sum_audit= audit1 + audit2 + audit3_men + audit4 + audit5 + audit6 + audit7 + audit8 + audit9 + audit10 if sex==1

tab sum_audit [fw=fw], m

// Dummy var
**0-7: Low risk
**8-15: Middle risk
**16-19: High risk
**20-40: Probable addiction
gen aud=.
replace aud=0 if sum_audit <8
replace aud=1 if sum_audit >=8 & sum_audit <=38
tab aud [fw=fw], m  // 11.48% AUD
label var aud "Risk consumption"
}
}

**--------------------
** [V] Comorbidities 
**--------------------
{

**-----------------------
** [14] OBESITY
**-----------------------
{
codebook imc // 24 missings
sum imc, d // max= 63.51065

gen obesity=.
replace obesity=0 if imc <30
replace obesity=1 if imc >= 30 & imc <= 64
label var obesity "Obesity"
tab obesity [fw=fw] // 33.92%
}

**-----------------------
** [15] DIABETES
**-----------------------
{// Diabetes
*** Diagnosis
tab di3, m // 596 YES (15.5%); 3,214 NO (83.57%); 36 DOESN'T REMEMBER (0.94%)
label list di3

gen diabetes=.
replace diabetes=1 if di3==1
replace diabetes=0 if di3==2
tab diabetes, m // 596 YES (15.5%); 3,214 NO (83.57%); 36 . (0.94%)

*** Glucose
tab glucosa, m // 255 missings
gen dia_aux=.
replace dia_aux=0 if glucosa < 126
replace dia_aux=1 if glucosa >= 126 & glucosa <= 498
tab dia_aux, m

tab dia_aux di3, cell m

replace diabetes=1 if dia_aux==1 & di3==2 // 85 no to yes: undiagnosed diabetes
replace diabetes=1 if dia_aux==1 & di3==3 // 4 doesn't remember to yes
replace diabetes=. if dia_aux==. & di3==3 // 3 (0.08%) missings

// Possible imputations: 235 (6,11%)
// (a) Within 206 test=. & diagnostic ==NO, 2.8% could be undiagnosed
//	diabetes: underestimation is low, keep as NO
// (b) Within (b) 29 test==0 & diagnosis==doesn't remember, 3 could be
//	treated diabetes; underestimation is low, replace to NO
replace diabetes=0 if dia_aux==0 & di3==3
tab diabetes, m // OK

drop dia_aux
}

**-----------------------
** [16] TOBACCO 
**-----------------------
{// Tobacco

{// (a) Quick indicator
label list ta3
tab ta3, m

gen tobacco=.
replace tobacco=1 if ta3==4
replace tobacco=2 if ta3==3
replace tobacco=3 if ta3==2
replace tobacco=4 if ta3==1
label var tobacco "Tobacco"
label define tobacco 1 "Non smoker" 2 "Former smoker" 3 "Occasional smoker" 4 "Daily smoker"
label values tobacco tobacco
tab tobacco, m
}

{// (b) Package-Year Index
}

{// (c) Dependency index
}
}

**-------------------------
** [17] METABOLIC SYNDROME
**-------------------------
{// Metabolic syndrome

*** Prefab variable:
tab sindrome_metabolico, m // 358 missings: 9.31%
tab sindrome_metabolico [fw=fw], m // missings: 8.8%

*** Guidelines of National Institutes of Health

{// (1) Elevated fasting blood glucose (255 missings)
//	Criterion: 100 mg/dl (5.6 mmol/l) or more
tab glucosa, m // 255 missings***
gen glucose=.
replace glucose=0 if glucosa < 100
replace glucose=1 if glucosa >= 100 & glucosa <= 498
tab glucose, m

*tab glucose hincpc_cat [fw=fw], m col 

}

{// (2) Waist circumference (26 missings)
//	Criterion: at least 35 inches (89 centimeters) for women
//	and 40 inches (102 centimeters) for men
tab m4p3, m // 26 missings; measure: centimeters
gen waist=.
replace waist=0 if sex==0 & m4p3 <89
replace waist=1 if sex==0 & (m4p3 >= 89 & m4p3 <=181)
replace waist=0 if sex==1 & m4p3 <102
replace waist=1 if sex==1 & (m4p3 >= 102  & m4p3 <=181)
tab waist, m

*drop if waist==.
}

{// (3) Elevated triglyceride levels (145 missings)
//	Criterion: 150 milligrams per deciliter (mg/dl) or 1.7 millimoles per liter
//	(mmol/l) or higher levels of this type of fat in the blood
tab trigliceridos, m // 145 missings; measure: milligrams
gen triglycerides=.
replace triglycerides= 0 if trigliceridos <150
replace triglycerides= 1 if trigliceridos >=150 & trigliceridos <=1216
tab triglycerides, m // 145 missings

*drop if triglycerides==.
}

{// (4) Reduced high-density lipoprotein or "good" cholesterol (145 missings)
// Criterion: less than 40 mg/dl (1.04 mmol/l) in men
//	and less than 50 mg/dl (1.3 mmol/l) in women

tab colesterol_hdl, m // 145 missings
gen cholesterol=. 
replace cholesterol = 1 if sex==1 & colesterol_hdl < 40
replace cholesterol = 0 if sex==1 & (colesterol_hdl >= 40 & colesterol_hdl <=125)
replace cholesterol = 1 if sex==0 & colesterol_hdl < 50
replace cholesterol = 0 if sex==0 & (colesterol_hdl >= 50 & colesterol_hdl <=125)
tab cholesterol, m // 145 missings

*drop if cholesterol==.
}

{// (5) Arterial hypertension (2 missings)
//	Criterion: 130/85 millimeters of mercury (mmHg) or more
codebook m2p11a_pas // 2 missings
sum m2p11a_pas, d
sum m2p11a_pad, d

gen hypertension=0
replace hypertension=1 if m2p11a_pas >= 130 & m2p11a_pad >=85
replace hypertension=. if m2p11a_pas==.
tab hypertension, m // 2 missings

*drop if hypertension==.
}

{// Indicator: three or more
gen smet=0
label var smet "Metabolic syndrome"
replace smet=1 if ///
(glucose==1 & waist==1 & triglycerides==1 & cholesterol==1 & hypertension==1) | ///
(glucose==1 & waist==1 & triglycerides==1 & cholesterol==1 & hypertension==0) | ///
(glucose==1 & waist==1 & triglycerides==1 & cholesterol==0 & hypertension==1) | ///
(glucose==1 & waist==1 & triglycerides==0 & cholesterol==1 & hypertension==1) | ///
(glucose==1 & waist==0 & triglycerides==1 & cholesterol==1 & hypertension==1) | ///
(glucose==0 & waist==1 & triglycerides==1 & cholesterol==1 & hypertension==1) | ///
(glucose==0 & waist==0 & triglycerides==1 & cholesterol==1 & hypertension==1) | ///
(glucose==0 & waist==1 & triglycerides==0 & cholesterol==1 & hypertension==1) | ///
(glucose==0 & waist==1 & triglycerides==1 & cholesterol==0 & hypertension==1) | ///
(glucose==0 & waist==1 & triglycerides==1 & cholesterol==1 & hypertension==0) | ///
(glucose==1 & waist==0 & triglycerides==0 & cholesterol==1 & hypertension==1) | ///
(glucose==1 & waist==0 & triglycerides==1 & cholesterol==0 & hypertension==1) | ///
(glucose==1 & waist==0 & triglycerides==1 & cholesterol==1 & hypertension==0) | ///
(glucose==1 & waist==1 & triglycerides==0 & cholesterol==0 & hypertension==1) | ///
(glucose==1 & waist==1 & triglycerides==0 & cholesterol==1 & hypertension==0) | ///
(glucose==1 & waist==1 & triglycerides==1 & cholesterol==0 & hypertension==0)
}

{// Handling missing data
// Possible bias by underestimation: to code as 0 some individuals that doesn't 
//	meet 3 conditions because they have missing values in some covariates

count if glucose==. | waist==. | triglycerides==. | /// 290 possible cases
					cholesterol==. | hypertension==.

// The problem does not involve those who meet 3 conditions indepent of 
//	missing values. Explore the others:

count if smet==0 &	///
		(glucose==. | waist==. | triglycerides==. | /// 290; bad scenario,
		cholesterol==. | hypertension==.) // all of them are smet==0

// The simple solution is to:
replace smet=. if smet!=1 & (glucose==. | waist==. | triglycerides==. | cholesterol==. | hypertension==.)
		
*** But we can go one step furher, and arrive at a less bad scenario:
// How many missings have those individuals that have missings?
egen miss_sum= rowmiss(glucose waist triglycerides cholesterol hypertension)
tab miss_sum if miss_sum != 0
tab miss_sum [fw=fw]
// 1 missing:	143	(39.37% - 2.60% of population)
// 2 missing:	12	(3.08% - 0.20% of population)
// 3 missing:	134	(57.42% - 3.79% of population)
// 4 missing:	1	(0,13% - 0.01% of population)

// An interesting option would be to:
// 	Encode as . those with more than 3 and 4, because:
//		3 and 4 missings make it impossible to assess whether  
//		the individual has metabolic syndrome or not
//	Explore more deeply what happens with the other cases, because:
//		1 and 2 missing only makes it more difficult, but not impossible

// How much more difficult? The key is that the criterion having metabolic
//	syndrome (sm==0) is always the acomplishment of 3 criterions, but the
//	criterion for not having it varies according to the number of missings.

// With 1 or 2 missings, one can always recode smet==. as smet==1 if the
//	individual still meets 3 criterions.

// To recode smet==. as smet==0, we always will assume that missings in
//	criterions are 1, but we are going to recode smet to 0 depending
// on how the other criteria behave

// Explore how many accomplishments the individuals meet:
egen accomplish_n= anycount(glucose waist triglycerides ///
					cholesterol hypertension), values(1)

// With 1 missing we observe 4 critreions, and assume that 1 is accomplished;
//	so, with 0 or 1 accomplishment, smet==0.
tab accomplish_n if miss_sum== 1 

/* 	  Freq.     Percent        Cum.  
   0 | 41       28.67       28.67		// 41 
   1 | 39       27.27       55.94		// + 39 (80) don't have metabolic syndrome
   2 | 36       25.17       81.12		// DARK MATTER
   3 | 24       16.78       97.90		// 24 
   4 | 3        2.10      100.00 		// +3 = 27 have metabolic syndrome */
 
replace smet=0 if smet==. & miss_sum== 1 & (accomplish_n>=0 & accomplish_n <= 1)
replace smet=1 if smet==. & miss_sum== 1 & (accomplish_n>=3 & accomplish_n <= 4)

// With 2 missing we observe 3 critreions, and assume that 2 are accomplished;
//	so, with 0 accomplishments, smet==0
tab accomplish_n if miss_sum== 2

/*    Freq.     Percent        Cum.
   0 | 6       50.00       50.00		// 6 have metabolic syndrome
   1 | 3       25.00       75.00		// DARK MATTER
   2 | 1        8.33       83.33		// DARK MATTER
   3 | 2       16.67      100.00		//	2 have metabolic syndrome */
   
replace smet=0 if smet==. & miss_sum== 2 & accomplish_==0
replace smet=1 if smet==. & miss_sum== 2 & accomplish_n==3
}

tab smet, m // 175 missings (4.55% of sample)
tab smet [fw=fw], m // (4.64% of population)

*** We recover half of the missing values, but we still have 175
***	What to do, then?

// We can assess possible remaining biases
// There is bias if the underestimation is explained by some relevant
// variable for the analysis of the paradox
}
}

**--------------------
** [VI] Sport & Food
**--------------------
{
**-----------------------
** [18] SPORT
**-----------------------
{// Sport
codebook a10
tab a10, m
tab a10 hincpc_cat [fw=fw], col m

codebook a11 // 1 extra missing
tab a11
tab a10 a11, m

gen sport=.
replace sport=0 if a10==2
replace sport=1 if a10==1
tab sport, m

gen sport2=.
replace sport2=0 if a10==2
replace sport2=0 if a10==1 & a11 <3
replace sport2=0 if a10==1 & a11==.
replace sport2=1 if a10==1 & (a11>=3 & a11<= 7)
tab sport2, m

tab obesity sport if sex==1, col
tab obesity sport2 if sex==1, col
}

**-----------------------
** [19] FOOD
**-----------------------
{// Food
// Pescado die1
// Lacteos die2 die3
// Cereales die4
// Legumbres die5
// Frutas die6 die7
// Verduras die8 die9
// Agua die11
// Bebidas con azucar die12
// Jugos con azucar die13

// Sellos die10

*** Compliance with dietary guidelines (ENCA_INFORME_FINAl)
// Three servings of daily dairy, equivalent to 600 ml.
codebook die2

gen die_guide1=.
replace die_guide1=1 if die2 == 1
replace die_guide1=0 if die2 != 1
tab die_guide1, m 

// Five daily servings of fruits or vegetables
codebook die6
codebook die7

sum die7, d
gen die_guide2=.
replace die_guide2=0 if die6 != 7
replace die_guide2=0 if die6 == 7 & die7 < 5
replace die_guide2=1 if die6 == 7 & (die7 >=5 & die7 < 70)
tab die_guide2, m

// Legumes twice a week
codebook die5

gen die_guide3=.
replace die_guide3=1 if die5 == 1
replace die_guide3=0 if die5 != 1
tab die_guide3, m

// Fish twice a week
codebook die1a

gen die_guide4=.
replace die_guide4=1 if die1a == 1
replace die_guide4=0 if die1a != 1
tab die_guide4, m

// Water, a daily intake of 1.5 liters (6-8 glasses daily)
codebook die11 // 4 missings
sum die11, d

gen die_guide5=.
replace die_guide5=1 if die11 >= 6 & die11 <= 8
replace die_guide5=0 if die11 < 6 | (die11 > 8 & die11 <=40)
tab die_guide5, m

// Indicator
egen die_guide_sum= rowtotal(die_guide*)
tab die_guide_sum [fw=fw], m

gen die_guide=.
replace die_guide=1 if die_guide_sum >=2
replace die_guide=0 if die_guide_sum < 2
tab die_guide hincpc_cat [fw=fw], m col


**Indice de alimentación saludable (ENCA_INFORME_FINAl)
// Cereales die4
// Verduras die8 die9
// Frutas die6 die7
// Lacteos die2 die3

// Pescado die1
// Legumbres die5

// Bebidas con azucar die12
// Jugos con azucar die13

/*10. Variedad
2 puntos si cumple cada una de las recomendaciones diarias;
1 punto si consume cada una de las recomendaciones semanales. */

tab die8, m
tab die8 hincpc_cat, col

tab die12_unidad, m

tab die14
}
}

**-----------------------
** [VII] Psychological factors
**-----------------------
{
**-----------------------
** [20] DEPRESSION
**-----------------------
{// Suicidal ideation
codebook sd13_f1
codebook sd25_f1

tab sd13_f1 sd25_f1, m

gen suicidal_ideation=0
replace suicidal_ideation=1 if sd13_f1==1 | sd25_f1==1
tab suicidal_ideation
}

**-----------------------
** [] 
**-----------------------
{// Depression
*** Stage 1: for depre1==1
{
{// CRITERION 1: Depressed mood most of the day and nearly every day
codebook sd1_f1 // 0 missings; 2,521 NO; 1,314 YES; 11 TAKING ANTIDEPRESSANTS
codebook sd2_f1 // 2,521 missings; 643 3 o 4; 326 ALL DAY; 356 NOST OF THE DAY
tab sd1_f1 sd2_f1, m // missings at sd2_f1 are NO at sd1_f1
codebook sd3_f1 // 3,164 missings; 284 EVERY DAY, 323 MOST DAYS
tab sd2_f1 sd3_f1, m  // missings: 2,521 NO at sd1_f1 + 643 3 o 4 at sd2_f1

gen depre1=0 
replace depre1=1 if sd1_f1==1 & (sd2_f1==1 | sd2_f1==2) & (sd3_f1==1 | sd3_f1==2)
replace depre1=1 if sd1_f1==3 & (sd2_f1==1 | sd2_f1==2) & (sd3_f1==1 | sd3_f1==2)
tab depre1, m // 607==1
tab depre1 [fw=fw], m // 18.31%
}

{// CRITERION 2: Markedly decreased interest or pleasure in all or almost all
//	activities for most of the day, almost every day
codebook sd4_f1 // 3,239 missings; 495 YES; 112 NO
gen depre2=0
replace depre2=1 if sd4_f1==1
}

{// CRITERION 3: Fatigue or loss of energy almost every day
codebook sd5_f1
gen depre3=0
replace depre3=1 if sd5_f1==1
}

{// CRITERION 4: Significant weight loss without dieting or significant
//	weight gain (eg, 5% change in body weight in a month), or decreased or
//	increased appetite nearly every day
codebook sd6_f1
codebook sd7_f1
codebook sd8_f1
gen depre4=0
replace depre4=1 if sd8_f1==1
}

{// CRITERION 5: Insomnia or hypersomnia almost every day
codebook sd9_f1
codebook sd10_f1
gen depre5=0
replace depre5=1 if sd10_f1==1 | sd10_f1==2
}

{// CRITERION 6: Reduced ability to think or concentrate, or indecisiveness
//	nearly every day
codebook sd11_f1
gen depre6=0
replace depre6=1 if sd11_f1==1
}

{// CRITERION 7: Inappropriate feelings of worthlessness or excessive guilt 
//	(which may be delusional) nearly every day
codebook sd12_f1
gen depre7=0
replace depre7=1 if sd12_f1==1
}

{// CRITERION 8: Recurrent thoughts of death (not just fear of dying), 
//	recurrent suicidal ideation with no specific plan, or a suicide attempt
//	or a specific suicide plan
codebook sd13_f1
gen depre8=0
replace depre8=1 if sd13_f1==1
}

// SUM
egen depre_sum= rowtotal(depre2 depre3 depre4 depre5 depre6)
tab depre_sum, m

// DEPRESSION
gen depression=0
replace depression=1 if depre1==1 & depre_sum >= 4
tab depression, m // 360
tab depression [fw=fw], m // 11.02%
}

{// Stage 2: for depre1==0

{// CRITERION 2: Markedly decreased interest or pleasure in all or almost all
//	activities for most of the day, almost every day
codebook sd14_f1 // 607 missings (depre==1); 2,853 NO; 379 YES; 7 TAKING ANTIDEPRESSANTS
codebook sd15_f1 // 3460 missings (607 . + 2,853 NO at sd14_f1)
codebook sd16_f1 // 3699 missings (3460 . + 164 "3" + 75 "4" at sd15_f1)

replace depre2=1 if sd14_f1==1 & (sd15_f1==1 | sd15_f1==2) & (sd16_f1==1 | sd16_f1==2)
replace depre2=1 if sd14_f1==3 & (sd15_f1==1 | sd15_f1==2) & (sd16_f1==1 | sd16_f1==2)
}

{// CRITERION 3: Fatigue or loss of energy almost every day
codebook sd17_f1
replace depre3=1 if sd17_f1==1
}

{// CRITERION 4: Significant weight loss without dieting or significant
//	weight gain (eg, 5% change in body weight in a month), or decreased or
//	increased appetite nearly every day
codebook sd18_f1
codebook sd19_f1
codebook sd20_f1
replace depre4=1 if sd20_f1==1
}

{// CRITERION 5: Insomnia or hypersomnia almost every day
codebook sd21_f1
codebook sd22_f1
replace depre5=1 if sd22_f1==1 | sd22_f1==2
}

{// CRITERION 6: Reduced ability to think or concentrate, or indecisiveness
//	nearly every day
codebook sd23_f1
replace depre6=1 if sd23_f1==1
}

{// CRITERION 7: Inappropriate feelings of worthlessness or excessive guilt 
//	(which may be delusional) nearly every day
codebook sd24_f1
replace depre7=1 if sd24_f1==1
}

{// CRITERION 8: Recurrent thoughts of death (not just fear of dying), 
//	recurrent suicidal ideation with no specific plan, or a suicide attempt
//	or a specific suicide plan
codebook sd25_f1
replace depre8=1 if sd24_f1==1
}

// SUM
replace depre_sum= 	depre3 + depre4 + depre5 + depre6 + ///
					depre7 + depre8 if depre1==0

// DEPRESSION
replace depression=1 if depre1==0 & depre2==1 & depre_sum >= 4
tab depression, m // 435
tab depression [fw=fw], m // 13.46%
tab depression if sex==0 [fw=fw], m // 18.87%
tab depression if sex==1 [fw=fw], m // 7.86%

*tab depression hincpc_cat [fw=fw], col
}

// Diagnosis of depression 
codebook sd28_f1
gen depression_diagnosed=0
replace depression_diagnosed=1 if sd28_f1==1
tab depression_diagnosed [fw=fw] // 21.68%
tab depression_diagnosed hincpc_cat [fw=fw], col


// Comparison: healed, depressed in the past, actually depressed, both
tab depression_diagnosed depression, col

tab hincpc_cat if depression_diagnosed==1 & depression==0 [fw=fw]

// Lifetime prevalence of depression
gen depression_life=0
replace depression_life=1 if depression==1 | depression_diagnosed==1

tab depression_life [fw=fw] // 27.3%
tab depression_life hincpc_cat [fw=fw], col
}

**-----------------------
** [] 
**-----------------------
{// Stress
codebook ps7_f1
codebook ps8
tab ps7_f1 ps8, m col

gen stress1=0
replace stress1=1 if ps7_f1==4

gen stress2=0
replace stress2=1 if ps8==3

gen stress=0
replace stress=1 if stress1==1 & stress2==1

}

**-----------------------
** [] 
**-----------------------
{// Care
codebook ps9_f1 // could be interesting in the case of women
tab ps9_f1, m
gen care=0
replace care=1 if ps9_f1==1
}
}

**------------------
** [VIII] Social networks
**------------------
{
**-----------------------
** [21] 
**-----------------------
{// Marital status
codebook as4_1 // estado civil
tab as4_1, m

gen partner=.
replace partner=1 if as4_1==1 | as4_1==2 | as4_1==3
replace partner=2 if as4_1==4 | as4_1==5| as4_1==6| as4_1==7
replace partner=3 if as4_1==8

label var partner "In a relationship?"
label define partner 1 "Yes" 2 "Ended relationship" 3 "Single"
label values partner partner

tab partner, m

bysort sex: tab prev_week partner [fw=fw], col
}

**-----------------------
** [22] 
**-----------------------
{// Social capital

// Psychosocial support
codebook ps3_f1 // Cuando tiene problemas, ¿tiene Ud. alguna persona en quien confiar, pedir ayuda o consejo?
tab ps3_f1, m
gen support_ps=0
replace support_ps=1 if ps3_f1== 1 | ps3_f1== 2 | ps3_f1== 3
tab support_ps [fw=fw], m

// Economical support
codebook ps4_f1 // ¿Puede recurrir confiadamente a alguien cuando tiene un gasto imprevisto, emergencia económica u otra situación grave o catastrófica?
tab ps4_f1, m
gen support_ec=0
replace support_ec=1 if ps4_f1== 1 | ps4_f1== 2 | ps4_f1== 3
tab support_ec [fw=fw], m

tab support_ps support_ec

gen support=1
replace support=0 if support_ps==1 & support_ec==1
label var support "Lack of socio-economic support"
tab support [fw=fw], m
*bysort hincpc_cat: tab support [fw=fw], m

// Groups
codebook ps6_f1 // ¿Pertenece Ud. a alguno de los siguientes grupos?

codebook ps6a // ¿Cuántas veces en promedio, se junta con su(s) grupo(s) en un mes?

}
}

**---------------------------------------
** Dependent variable: Liver Damage (LD)
**---------------------------------------
{
**-----------------------
** [23] 
** [24] 
** [25] 
**-----------------------
{// Liver damage
gen alt = transaminasa_glutamico_piruvica
label var alt "Alanine transaminase (ALT)"
tab alt, m // missings: 158 (4.11%)
histogram alt, norm
histogram alt [fw=fw], norm

// Three indicators as in [2] (Tejos et al, 2013)
// (a) Women > 19 UI/L, Men > 30 UI/L
gen ld1 = . 
replace ld1 = 0 if alt <= 19 & sex==0
replace ld1 = 0 if alt <= 30 & sex==1
replace ld1 = 1 if alt > 19 & alt <= 225 & sex==0
replace ld1 = 1 if alt > 30 & alt <= 225 & sex==1
label var ld1 "Liver Damage"
label define ld1 0 "No" 1 "Yes"
label values ld1 ld1
tab ld1, m // missings: 158
tab ld1 // Yes= 1399 (36.4%)
tab ld1 [fw=fw] // Yes= 4,963,995 (35.76%)

// (b) 	Women IMC ≤ 23: >31 UI/L;
// 		Women IMC > 23: > 44 UI/L 
//		Men IMC ≤ 23: > 42 UI/L 
//		Men IMC > 23: > 66 UI/L
gen ld2 = . 
replace ld2 = 0 if (sex==0 & imc <= 23) & (alt <= 31)
replace ld2 = 1 if (sex==0 & imc <= 23) & (alt > 31 & alt <= 225)
replace ld2 = 0 if (sex==0 & imc > 23) & (alt <= 44)
replace ld2 = 1 if (sex==0 & imc > 23) & (alt > 44 & alt <= 225)

replace ld2 = 0 if (sex==1 & imc <= 23) & (alt <= 42)
replace ld2 = 1 if (sex==1 & imc <= 23) & (alt > 42 & alt <= 225)
replace ld2 = 0 if (sex==1 & imc > 23) & (alt <= 66)
replace ld2 = 1 if (sex==1 & imc > 23) & (alt > 44 & alt <= 225)

label var ld2 "Liver Damage"
label define ld2 0 "No" 1 "Yes"
label values ld2 ld2
tab ld2, m // missings: 158
tab ld2 // Yes= 364 (9.87%)
bysort sex: tab ld2 [fw=fw] // Yes= 1,449,745 (10.44%)

// (c)  Women 10-18 years: > 20 UI/L 
//		Women > 18 años: > 30 UI/L
//		Men 10-18 years: > 30 UI/L 
// 		Men 10-18 years: > 55 UI/L
gen ld3 = . 
replace ld3 = 0 if (sex==0 & age <= 18) & (alt <= 20)
replace ld3 = 1 if (sex==0 & age <= 18) & (alt > 20 & alt <= 225)
replace ld3 = 0 if (sex==0 & age > 18) & (alt <= 30)
replace ld3 = 1 if (sex==0 & age > 18) & (alt > 30 & alt <= 225)

replace ld3 = 0 if (sex==1 & age <= 18) & (alt <= 30)
replace ld3 = 1 if (sex==1 & age <= 18) & (alt > 30 & alt <= 225)
replace ld3 = 0 if (sex==1 & age > 18) & (alt <= 55)
replace ld3 = 1 if (sex==1 & age > 18) & (alt > 55 & alt <= 225)

label var ld3 "Liver Damage"
label define ld3 0 "No" 1 "Yes"
label values ld3 ld3
tab ld3, m // missings: 158
tab ld3 // Yes= 507 (13.74%)
bysort sex: tab ld3 [fw=fw] // Yes= 1,721,864 (12.40%)
}

**-----------------------
** [26] 
**-----------------------
{// Diagnosis of liver cirrhosis, fatty liver or chronic liver damage
des m9p15a 
label list m9p15A
tab m9p15a, m // 1 missing; 15 doesn't know; 231 YES; 3,600 NO
tab m9p15a [fw=fw] // YES: 729,230 (5%)
gen dld = .
replace dld=1 if m9p15a==1
replace dld=0 if m9p15a==2 | m9p15a== -8888
label var dld "Diagnosis of LD"
tab dld, m // OK
tab dld [fw=fw] // 5% YES

tab dld ld3, m cell
tab dld ld3 [fw=fw], m cell
** 3,025 (79.95%) have no diagnosis or liver damage
** Among those diagnosed (dld==1; N:231) (5%):
//	(i) 157 have no liver damage (3.81%) -> They may have been treated (***RELEVANT)
//	(ii) 69 have liver damage (1.1%) -> Recently diagnosed, or received no treatment
//	(iii) 5 no exam (0.12%)
** Among those with LD (ld3==1; N: 507) (11.86%):
//	(i) 437 have no diagnosis (10,73%) -> probably asymptomatic  
//	(ii) 69 have diagnosis (1.1%) -> Recently diagnosed, or received no treatment
** 153 (4.27%) not examined have no diagnosis -> some of them could have LD 

*-------------------------------------------------------------------------------
**	WARNING!
*-------------------------------------------------------------------------------
** (a) Those 153 not examined with no diagnosis (dld==0 & ld3==.; 4.27%) can 
** 		produce an underestimation of the paradox. Some of them are likely to
** 		have asymptomatic LD; and this likelihood is greater within low income
** (b) Those 157 diagnosed with no liver damage (dld==1 & ld3==0; 3.81%) could
** 		be people healed from disease. Lack of information about their history
**		makes it complicated to hypothesize how their omission in the
**		construction of the dependent variable may affect the evaluation of the
**		paradox (which would depend on the variables that make it more likely 
**		to be healed of liver disease)
*-------------------------------------------------------------------------------

*--------------------
* Exploration of (a)
*--------------------
{
// What's the probability of having undiagnosed LD if diagnosis==0
tab ld3 if dld==0 // 12.62%

// Does this probability vary by income level?
tab ld3 hincpc_cat if dld==0, col	// 1: 14.35%
									// 2: 11.87%
									// 3: 11.84%

// What is the income-composition of dld==0 & ld3==.
tab hincpc_cat // In sample, 1: 31%, 2: 32%, 3:37% 
tab hincpc_cat if dld==0 & ld3==.	// Within nondiagnosed-nonexamined,
									// 1: 43.14%, 2: 29.41%, 3: 27.45%

// We could be leaving out approximately:
	dis 43.14*66/100 	// 28 lowest-income individuals with ld==1
	dis 66-28			// and 38 ld==0
	dis 29.41*45/100 	// 13 middle-income individuals with ld==1
	dis 45-13			// and 32 ld==0
	dis 27.45*42/100 	// 12 highest-income individuals with ld==1
	dis 42-12			// and 30 ld==0
	
// If we simulate their inclusion...
tab ld3 hincpc_cat, col
// In lowest income, ld==1 would be 176+28; and ld==0 would be 953+38
// In middle income, ld==1 would be 154+13; and ld==0 would be 1,040+32
// In highest income, ld==1 would be 177+12; and ld==0 would be 1,189+30

/*	Liver	/ Per capita household income, terciles	/
	Damage	/	Lowest	/	Middle	/	Highest		/
-------------------------------------------------------
		No	/	991		/	1,072	/	1,219		/
			/ (82.92%)	/ (86.52%)	/	(86.58)		/
		Yes /	204		/	167		/	189			/
			/ (17.08%)	/ (13.48%)	/	(13.42%)	/
-------------------------------------------------------
	Total	/ 1,195		/ 	1,239	/	1,408
-------------------------------------------------------
*/

// LD is underestimated by... 
	dis 15.59-17.08 // -1.49% within hincpc_cat==1
	dis 12.9-13.48	// -0.58% within hincpc_cat==2
	dis 12.96-13.42 // -0.46% within hincpc_cat==3
	
// So, (a) can produce an underestimation of the paradox
// *** However, these calculations do not consider expansion weights
}

*--------------------
* Exploration of (b)
*--------------------
{
** Those with dld==1 & ld3==0 are likely to be
	// (i) people misdiagnosed, 
	// (ii) people with unusual results in laboratory tests, or
	// (iii) people healed from disease

** If (i) were the case, there's no problem, because we are coding as healthy
//	some people that are actually healthy. 

** If (ii) were the case, we would be underestimating the prevalence of liver
//	damage; because we are coding as healthy some people that have liver damage,
//	even if the tests say otherwise. Now, we could assume randomness in the
//	causes of a failed test, so this underestimation would be the same for the
//	different income levels. In this scenario the analysis of the paradox would
//	not be significantly affected

** If (iii) were the case, we would be coding as healthy some people who
//	were sick in the past. As we are interested in the variation in the
//	probability of presenting liver damage, this underestimates the proportion
//	of people with LD; and, if the probability of being diagnosed, treated
//	and/or healed varies by income level, we could be introducing a bias in
//	the analysis of the paradox.

// In an ideal study, we would seek to get information about the clinical 
//	history of the individuals, so we could control for any diagnosis or 
//	treatment. In a first stage we would focus on the probability of presenting
//	liver damage **before any diagnosis or treatment**. In a second stage, we
//	would focus on the probability of presenting acute symptoms or episodes
//	that lead to diagnosis, treatment, eventual stabilization, healing or death 
//	(probability or time until death are posible outcomes in stage 2)

*** But we are not in the ideal study, and data is what it is. So, what can we
//	do with what we have? We can...
//		(i) Drop those individuals with dld==1 (5%)
//			or those with dld==1 & ld3==0 (3.81)
//		(ii) Code them as ld3==0
// 		(iii) Code them as ld3==1

// The option (i.i) simulates what the ideal study points to (probably doesn't
//	produce underestimations of the LD nor introduce bias in the effects of
//	covariates. The option (i.ii) ignores that probably the probability of
//	being diagnosed and treated vary by income.
{

// should start by analyzing if diagnosis reduce LD. 
tab ld3 dld, col	// Data says the contrary, but there is a selection bias: 
					// those who are ill are more likely to be diagnosed.
					// In addition, some people (depending on their
					// characteristics) are more likely to stick with medical
					// indications to heal

// However, in the most conservative scenario, we could assume that all those
//	with dld==1 & ld3==0 were healed. So we are underestimating the proportion
//	of people with LD. 

// Collider: ¿equivale a estar controlando por un outcome del outcome?
					
{// Other factors to consider (UNCOMPLETE)
// Time since diagnosis if ld3==0:
tab m9p15b if dld==1 & ld3==0, m	// 7.64% diagnosed before 30
									// 57.33% diagnosed between 30 and 59
									// 29.3% diagnosed between 60 and 81
									// 5.73% missing

// How long ago were they diagnosed?
sort age
list age m9p15b if dld==1 & ld3==0

gen age_diag= age-m9p15b
tab age_diag if dld==1 & ld3==0	// 35.81% 1 year ago or less
									// 64.19% more than 1 year ago}
					
// How many of them (dld==1 & ld3==0) received treatment?
tab m9p15c if dld==1 & ld3==0, m // 88, 56%
tab m9p15c if dld==1 & ld3==0 [fw=fw] , m // 53.42%

// Does treatment reduce LD?
tab ld3 m9p15c if dld==1 [fw=fw], col // Yes

// Does the probability of having been treated vary according to income?
tab m9p15c hincpc_cat if dld==1 [fw=fw], col // Yes

** surgery, ever
tab m9p15d, m // 2 doesn't know;8 YES; 236 NO
tab m9p15d dld3, m // data problems, but the relevant is that...
tab m9p15d if dld3==1, m // 8 YES; 223 NO
tab m9p15d m9p15c, m // and 7/8 cirguries were for treated

tab ld3 m9p15d if dld3==1 [fw=fw], col // cirgury reduces LD

** medication or treatment, last 2 weeks
tab m9p15e, m // 2 doesn't know; 49 YES; 195 NO
tab m9p15e dld3, m // data problems, but the relevant is that...
tab m9p15e if dld3==1, m // 49 YES; 182 NO 
tab m9p15e m9p15c, m // and 47/2 medications are for treated

tab ld3 m9p15e if dld3==1 [fw=fw], col // medication reduces LD

gen healed=0
replace healed=1 if dld==1 & ld3==0
tab healed [fw=fw], m (3.98%)

gen ld4=0
replace ld4=1 if ld3==1
replace ld4=1 if ld3==0 & dld==1
tab ld4, m
}
}

// To discern between (ii) and (iii), we should charaterize the expected bias
// 	for each option and made some hypotheses about their effects on our results
{
*** If we follow (i), we would be coding as healthy some people who
//	were sick in the past. There are two relevant things to assess:

//	First, as we are interested in the variation in the probability of
//		presenting LD, this may underestimate the proportion of people with LD;
//		and, if the probability of being diagnosed, treated and/or healed
//		varies by income level, becoming an explanation of the paradox, we
//		could be introducing a bias in the analysis

{// How important is the underestimation?

// With ld3==0
tab ld3 [fw=fw] // 12.40%

// With ld3==1
gen ld3_2= ld3
replace ld3_2=1 if dld==1 & ld3==0
tab ld3_2 [fw=fw] // 16.38%

// Underestimation:
dis 12.40-16.38 // -3.98%
}

{// What's the distribution of the bias by income?

// Income distribution in full sample:
tab hincpc_cat [fw=fw] // 1: 34%, 2: 34%, 3: 32% 

// Income distribution in dld==1 & ld3==0:
tab hincpc_cat if dld==1 & ld3==0 [fw=fw] // 1: 23%, 2: 53%, 3: 23% 

// It is likey that bias will be similar to 1 and 3, and greater to 2.

// What data says:
tab ld3 hincpc_cat [fw=fw], col // 1: 12.98%, 2: 11.79%, 3:  12.44%
tab ld3_2 hincpc_cat [fw=fw], col // 1: 15.68%, 2: 18.10%, 3: 15.33%
dis 12.98-15.68 // -2,7 for income==1
dis 11.79-18.10 // -6.31 for income==2
dis 12.44-15.33 // -2,89 for income==3
}

//	Second, if being diagnosed in the past leads to loss of weight, alcohol
//		abstinence, and other changes in relevant variables, the coding of LD
//		as 0 doesn't introduce bias: dld==1 & ld3==0 are not different than 
//		those who never presented LD. But, if diagnosis doesn't lead to such 
//		changes, the coding of LD as 0 could introduce bias to the estimated 
//		effect for those variables.

// Comparison with dld==0 ld3==0
sum prev_* aud obesity [fw=fw] if dld==0 & ld3==0
sum prev_* aud obesity [fw=fw] if dld==1 & ld3==0

// Comparisons by hincpc_cat
foreach var in prev_* aud obesity {
	tab `var' hincpc_cat [fw=fw], col
}

bysort hincpc_cat: sum prev_* aud obesity [fw=fw]
}
}
}
}

**----------------------
** (VII) Guardar BBDD
**----------------------
{
save NHS2016-17_newvars, replace

*keep daño_hepatico enfermedad_hepatica daño_enf household_income2 ingpc tingpc nedu nse prev_anio prev_mes prev_semana consumo_neto aud_medio hed obesidad diabetes fuma smet sexo edad tedad fexp rfexp pfexp

*save pda, replace

/* VOTAR MISSINGS
codebook
drop if nedu ==.
drop if nse ==.
drop if obesity ==.
drop if diabetes ==. */

}

*-------------
* REFERENCES
*-------------
{
/* [] “Manual de uso de la base de datos de la Encuesta Nacional de Salud 2016-2017”
** [] CEPAL. (2017). MEDICIÓN DE LOS INGRESOS Y LA POBREZA EN CHILE, ENCUESTA
		CASEN 2017. "The methodology requires expressing the indigence and 
		poverty thresholds in “adult equivalent” units. For this, the
		equivalence scale n^0.7 is used, where n is the number of household
		members". See footnote 8, page 29.
		Link: http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2017/Medicion_de_la_pobreza_en_Chile_2017_17082018.pdf

** [] Tejos et al. (2013). Niveles séricos de alaninoaminotransferasa en 
		población chilena. Análisis de los resultados de la encuesta nacional
		de salud 2009-2010
*/
}
}





