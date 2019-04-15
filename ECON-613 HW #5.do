log using "/Users/minchel93/Desktop/ECON-613/ECON-613 HW#5.txt", replace

clear

******
*HW 2*
******

** Exercise 1

set seed 1
set obs 10000 
gen X1 = runiform(1, 3) 
gen X2 = rgamma(3, 2)
gen X3 = rbinomial(1, 0.3)
gen eps = rnormal(2, 1)

gen Y = 0.5 + 1.2 * X1 - 0.9 * X2 + 0.1 * X3 + eps
egen Y_mean = mean(Y)
gen ydum = (Y > Y_mean)

** Exercise 2

*2.1
corr Y X1
*** Correlation is 0.2036. This quantity is different from 1.2.

*2.2
*** Regressing Y on X, Storing the Coefficients, and Making a Table
reg Y X1 X2 X3
est sto linear_coef
est tab linear_coef

*2.3
*** Bootstrapping 49 and 499 Times
bootstrap, rep(49): reg Y X1 X2 X3
est sto bootstrap_49
est tab bootstrap_49

bootstrap, rep(499): reg Y X1 X2 X3
est sto bootstrap_499
est tab bootstrap_499

* Exercise 3
*** Implementing Probit Model
global xvars "X1 X2 X3"

probit ydum $xvars
est sto Probit_model
est tab Probit_model

* Exercise 4
*** Implmenting Probit, Logit, and Linear Models

probit ydum $xvars
est sto Probit_model2
est tab Probit_model2

logit ydum $xvars
est sto Logit_model
est tab Logit_model

reg ydum $xvars
est sto Linear_model
est tab Linear_model

* Exercise 5
*** Computing the Marginal Effect of X on Y (Probit)

bootstrap, rep(499): probit ydum $xvars
margins, dydx(*)
est sto Marginal_Bootstrap_Probit
est tab Marginal_Bootstrap_Probit

probit ydum $xvars
margins, dydx(*) vce(delta)
est sto Marginal_Delta_Probit
est tab Marginal_Delta_Probit
** Bootstrap standard deviation calc under Margins is not specified in help func

bootstrap, rep(499): logit ydum $xvars
margins, dydx(*)
est sto Marginal_Bootstrap_Logit
est tab Marginal_Bootstrap_Logit

logit ydum $xvars
margins, dydx(*) vce(delta)
est sto Marginal_Delta_Logit
est tab Marginal_Delta_Logit
** Bootstrap standard deviation calc under Margins is not specified in help func

******
*HW 3*
******
* Importing Demos.csv
insheet using "/Users/minchel93/Desktop/ECON-613/Demos.csv", clear
sort hhid
save Demos, replace

* Importing ChoicePrice.csv
insheet using"/Users/minchel93/Desktop/ECON-613/ChoicePrice.csv", clear
sort hhid
save ChoicePrice, replace

* Merging Two Datasets
merge hhid using Demos ChoicePrice
save MergedData, replace

* Exercise 1
* Average and dispersion in product characteristics
su ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk /*
*/ pss_tub ppk_tub pfl_tub phse_tub

* Market Share by Choice
tab choice

* Market Share by Tub vs. Stk
gen market_char = cond(choice == 7 | choice == 8 | choice == 9 | /*
*/ choice == 10, "tub", "stk")
tab market_char

* Market Share by Brand
gen market_brand = "ppk" if choice == 1 | choice == 8 
replace market_brand = "pbb" if choice == 2
replace market_brand = "pfl" if choice == 3 | choice == 9
replace market_brand = "phse" if choice == 4 | choice == 10
replace market_brand = "pgen" if choice == 5
replace market_brand = "pimp" if choice == 6
replace market_brand = "pss" if choice == 7
tab market_brand

* Exercise 2
* Conditional Logit

* Renaming Column Names
rename (ppk_stk  pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub /*
*/ppk_tub pfl_tub phse_tub) (Price1 Price2 Price3 Price4 Price5 Price6 Price7 /*
*/Price8 Price9 Price10)

* Reshaping Wide-Format Price to Long-Format Price
reshape long Price, i(v1)

* Renaming First Two Columns
rename (v1 _j) (Individual Brand)

* Generating Dummy Variable
gen dummy = (Brand == choice)

* Implementing Conditional Logit
asclogit dummy Price, case(Individual) alternatives(Brand)
est sto conditional_logit
est tab conditional_logit

* Exercise 3
* Multinomial Logit

* Implementing Multinomial Logit
asclogit dummy, case(Individual) alternatives(Brand) casevar(income)
est sto multinomial_logit
est tab multinomial_logit

* Exercise 4
asclogit dummy Price, case(Individual) alternatives(Brand)
estat mfx

asclogit dummy, case(Individual) alternatives(Brand) casevar(income)
estat mfx

* Exercise 5
* Mixed Logit 

* Implementing Mixed Logit
asclogit dummy Price, case(Individual) alternatives(Brand) casevar(income)
est sto mixed_logit
est tab mixed_logit
gen F_logLikelihood = e(ll)

asclogit dummy Price if Brand != 10, case(Individual) alternatives(Brand) ///
casevar(income)
est sto removed_mixed_logit
est tab removed_mixed_logit
gen R_logLikelihood = e(ll)

* Test Statitiscs
gen MTT = -2 * (F_logLikelihood - R_logLikelihood)

* Testing IIA
hausman mixed_logit removed_mixed_logit
di r(p)
*** We conclude that the IIA holds.

******
*HW 4*
******

* Exercise 1

*Importing Koop-Tobias Data
insheet using "/Users/minchel93/Desktop/ECON-613/Koop-Tobias.csv", clear

* Setting Panel 
xtset personid timetrnd

* Investigating the Panel Dimension of Data
xtdes

* Exercise 2

* Declaring Global Variables
global yvar "logwage"
global xvars "educ potexper"

* Random Effects Model
xtreg $yvar $xvars, re
est sto RE_model
est tab RE_model

* Exercise 3

* Between Model
xtreg $yvar $xvars, be
est sto BE_model
est tab BE_model

* Within Model
xtreg $yvar $xvars, fe
est sto Within_model
est tab Within_model

* First-Difference Model
gen educ_diff = educ - L.educ
gen potexper_diff = potexper - L.potexper
gen logwage_diff = logwage - L.logwage

reg logwage_diff educ_diff potexper_diff, nocon
est sto Difference_model
est tab Difference_model



* Exercise 4
preserve

** Question 1
* Finding unique PERSONID
tempfile unique_id
bysort personid: keep if _n == 1

* Sampling 100 Individuals
sample 100, count
sort personid
save `unique_id', replace

* Subsetting Data
restore
merge m:1 personid using `unique_id'
keep if _merge == 3
drop _merge

* Updating Global Variables
global yvar2 "logwage"
global xvars2 "educ potexper"

* Estimating Fixed Effects
reg $yvar2 i.personid $xvars2 , nocon vce(robust) 
est sto Reduced_Fixed_Effects_Model
est tab Reduced_Fixed_Effects_Model

* Saving the Fixed Effect Coefficients
matrix coef = e(b)
matrix coef = coef'
matrix coef = coef[1..100, 1]

** Question 2

* Obtaining One Time-Invariant Variable for Each PersonID
bysort personid: keep if _n == 1
svmat double coef, names(coefficients)
drop if coefficients == 0

* Running Regression on Invariant Variables
reg coefficients ability mothered fathered brknhome siblings
est sto Individual_Fmodel
* Running Saved Results to Generate Tables

** HW 2
est tab linear_coef bootstrap_49 bootstrap_499, se 
est tab Probit_model Probit_model2 Logit_model Linear_model
est tab Marginal_Bootstrap_Probit Marginal_Delta_Probit /*
*/ Marginal_Bootstrap_Logit  Marginal_Delta_Logit, se

** HW 3
est tab conditional_logit multinomial_logit mixed_logit, se

** HW 4
est tab RE_model BE_model Within_model Difference_model, se
est tab Reduced_Fixed_Effects_Model Individual_Fmodel, se

log close
