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
	syntax [ , beta2(real 1)]
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
	
	* Method is ols:
		regress `y' `x'
		test _b[`x'] = `beta1'
		if `r(p)' < .01{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala b1_ols = _b[`x']
		return scala p_ols = `r(p)'
		return scala h1_ols = `h1'
	* Method is 2sls
		tempvar predict
		regress `x' `z'
		predict `predict'
		regress `y' `predict'
		test _b[`predict'] = `beta1'
		if `r(p)' < .01{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala b1_2sls = _b[`predict']
		return scala p_2sls = `r(p)'
		return scala h1_2sls = `h1'
	* Method is iv
		ivregress 2sls `y' (`x' = `z')
		test _b[`x'] = `beta1'
		if `r(p)' < .01{
			local h1 = 1
		}
		else {
			local h1 = 0
		}
		return scala b1_iv = _b[`x']
		return scala p_iv = `r(p)'
		return scala h1_iv = `h1'
end


********** simulate ***********************

simulate ///
    b1_ols  = r(b1_ols)  p_ols  = r(p_ols)  h1_ols  = r(h1_ols)  ///
	b1_2sls = r(b1_2sls) p_2sls = r(p_2sls) h1_2sls = r(h1_2sls) ///
	b1_iv   = r(b1_iv)   p_iv   = r(p_iv)   h1_iv   = r(h1_iv)   ///
	, reps(1000) : endog
	

********* print result ********************

program print_result
	display as text _dup(66) "-"
	display "method"  _col(10) "estbeta1" ///
			_col(22) "Pr{estbeta1 = 1}"   ///
			_col(46) "Pr{H0 is rejected}"
	display as text _dup(66) "-"
	
	local methods ols 2sls iv

	foreach m of local methods{
		quietly summ b1_`m'
		local b1 = r(mean)
		quietly summ p_`m'
		local p = r(mean)
		quietly summ h1_`m'
		local h1 = r(mean)
		display as text "`m':" ///
				_col(10)  as result %5.3f `b1' ///
				_col(22) as result %5.3f `p'   ///
				_col(46) as result %5.3f `h1'
	}
	display as text _dup(66) "-"
end

print_result

log close _all


/*

Final results
--------------

method   estbeta1    Pr{estbeta1 = 1}        Pr{H0 is rejected}
------------------------------------------------------------------
ols:     1.920       0.004                   0.914
2sls:    1.446       0.689                   0.000
iv:      1.446       0.508                   0.008
------------------------------------------------------------------



*/
