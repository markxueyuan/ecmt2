#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignment1"

if c(username)=="xueqingyan" {
	cd "/Users/xueqingyan/Desktop/ECMT 676 Assignments/Assignment 1"
}

capture prog drop endog

*****************************************************
**                                                 ** 
**    (1)         X = 0.8Z + 0.6u                  **
**                                                 **
*****************************************************

program endog, rclass
	syntax , beta2(real)
	capture drop _all
	set obs 5000
	local beta1 .2
	tempvar z u e x y
	gen `z' = rnormal(0,1)
	gen `u' = rnormal(0,1)
	gen `e' = rnormal(0,1)
	gen `x' = 0.8*`z' + 0.6*`u'
	gen `y' = .5 + `beta1'*`x' + `beta2'*`u' + `e'
	regress `y' `x'
	return scala beta1_hat = _b[`x']	
end

********** simulate ***********************

local i 1
forvalues b2=-1.0(0.1)1.0{
	tempfile f`i'
	simulate estbeta1 = r(beta1_hat) , reps(30) saving(`f`i''): endog, beta2(`b2')
	gen beta2 = `b2'
	save `f`i'' , replace
	local ++i
}

********* append data *********************
drop _all

set obs 0
gen estbeta1 = .
gen beta2 = .

local j 1
forvalues b2=-1.0(0.1)1.0{
	append using `f`j''
	local ++j
}

********* draw graph ********************

scatter estbeta1 beta2 , yline(.2) name("graph06") title("X = 0.8Z + 0.6u")



*****************************************************
*****************************************************
**                                                 ** 
**     (2)        X = 0.8Z + 0.1u                  **
**                                                 **
*****************************************************
*****************************************************
clear

program endog2, rclass
	syntax , beta2(real)
	capture drop _all
	set obs 5000
	local beta1 .2
	tempvar z u e x y
	gen `z' = rnormal(0,1)
	gen `u' = rnormal(0,1)
	gen `e' = rnormal(0,1)
	gen `x' = 0.8*`z' + 0.1*`u'
	gen `y' = .5 + `beta1'*`x' + `beta2'*`u' + `e'
	regress `y' `x'
	return scala beta1_hat = _b[`x']	
end

********** simulate ***********************

local i 1
forvalues b2=-1.0(0.1)1.0{
	tempfile f`i'
	simulate estbeta1 = r(beta1_hat) , reps(30) saving(`f`i''): endog2, beta2(`b2')
	gen beta2 = `b2'
	save `f`i'' , replace
	local ++i
}

********* append data *********************
drop _all

set obs 0
gen estbeta1 = .
gen beta2 = .

local j 1
forvalues b2=-1.0(0.1)1.0{
	append using `f`j''
	local ++j
}

********* draw graph ********************

scatter estbeta1 beta2 , yline(.2) name("graph01") title("X = 0.8Z + 0.1u")


log close _all

