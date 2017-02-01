#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignment2"

if c(username)=="xueqingyan" {
	cd "/Users/xueqingyan/Desktop/ECMT 676 Assignments/Assignment 2"
}


program endog, rclass
	syntax , method(string) [beta2(real 1)]
	capture drop _all
	set obs 100
	local beta0 .3
	local beta1 1.5
	tempvar z u e v x y
	gen `z' = rnormal(0,1)
	gen `u' = rnormal(0,1)
	gen `e' = rnormal(0,1)
	gen `v' = rnormal(0,1)
	gen `x' = 0.4*`z' + 0.7*`u' + `v'
	gen `y' = `beta0' + `beta1'*`x' + `beta2'*`u' + `e'
	
	display "we output `method'"
	
	if "`method'" == "ols" {
		regress `y' `x'
		test _b[`x'] = `beta1'
		return scala beta1_hat = _b[`x']
		return scala p_value = `r(p)'
	}
	else if "`method'" == "2sls" {
		tempvar predict
		regress `x' `z'
		predict `predict'
		regress `y' `predict'
		test _b[`predict'] = `beta1'
		return scala beta1_hat = _b[`predict']
		return scala p_value = `r(p)'
	}
	else if "`method'" == "iv" {
		ivregress 2sls `y' (`x' = `z')
		test _b[`x'] = `beta1'
		return scala beta1_hat = _b[`x']
		return scala p_value = `r(p)'	
	}
end

endog , method("iv")

********** simulate ***********************

local methods ols iv 2sls

foreach m of local methods{
	simulate estbeta1 = r(beta1_hat) p_value = r(p_value) ///
	, reps(1000) : endog, method("`m'") beta2(1)
	summ estbeta1
	local est = r(mean)
	summ p_value
	local p = r(mean)
	display as text "`m':  " as result `est' as text " " as result `p'
}


log close _all


/*

Final results
--------------

method  estbeta1       Pr{estbeta1 = 1}
----------------------------------------
ols:    1.9269887       .00396258
iv:      .80695495      .50754814
2sls:   1.4228056       .68970508
----------------------------------------

*/
