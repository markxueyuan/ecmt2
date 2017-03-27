#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignments/Assignment4"

if c(username)=="xueqingyan" {
	cd "C:\Users\yanxueqing\Desktop\ECMT 676 Assignments\Assignment 4"
}

** Implement part I.

program endog, rclass
	*syntax [ , trend(real 0)]
	capture drop _all
	
	tempvar id a_i r Tr t a_t Post Treat u_it e_it y
	
	local n 200
	set obs `n'
	
	gen `id' = _n
	
	gen `a_i' =rnormal(5,sqrt(8))
	
	gen `r' = runiform()
	sort `r'
	generate `Tr' = _n <= .5*`n'

	
	expand 15
	bysort `id' : gen `t' = _n
	
	xtset `id' `t'
	
	gen `Post' = 1 if `t' > 6
	replace `Post' = 0 if `t' < 7
	
	gen `Treat' = `Tr'*`Post'

	gen `a_t' = .
	forvalues i = 1/15{
		local r = rnormal(1,sqrt(2))
		replace `a_t' = `r' if `t' == `i'
	}
	
	gen `u_it' = rnormal(0,sqrt(2))
	
	gen `e_it' = 0
	levelsof `id', local(num_id)
	levelsof `t', local(num_t)
	foreach ii of local num_id{
		replace `e_it' = `u_it' ///
				if `id' == `ii' & `t' == 1
		foreach tt of local num_t{
			if `tt' > 1 {
				replace `e_it' = .25*L.`e_it' /// 
				+ .5*`u_it' if `id' == `ii' & `t' == `tt'
			}
		}
	}
	
	
	local theta = 1.5
	
	gen `y' = `a_i' + `a_t' + `theta'*`Treat' + `e_it'
	
	* Regress assuing iid:
		xtreg `y' i.`t' `Treat' , fe
		test _b[`Treat'] = `theta'
		if `r(p)' < .05{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala theta_1 = _b[`Treat']
		return scala std_1 = _se[`Treat']
		return scala p_1 = `r(p)'
		return scala h1_1 = `h1'
		
	* Regress clustering on id
	
		xtreg `y'  i.`t' `Treat' , fe cluster(`id')
		test _b[`Treat'] = `theta'
		if `r(p)' < .05{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala theta_2 = _b[`Treat']
		return scala std_2 = _se[`Treat']
		return scala p_2 = `r(p)'
		return scala h1_2 = `h1'
		
	* Regress hetero-robust
		local dummy
		local dummyt
		levelsof `id', local(num_id)
		levelsof `t', local(num_t)
		foreach ii of local num_id{
			if `ii' > 1 {
				gen v`ii' = 1 if `id' == `ii'
				replace v`ii' = 0 if v`ii' == .
				local dummy `dummy' v`ii'
			}
		}
		foreach tt of local num_t{
			if `tt' > 1 {
				gen vt`tt' = 1 if `t' == `tt'
				replace vt`tt' = 0 if vt`tt' == .
				local dummyt `dummyt' vt`tt'
			}
		}
		regress `y' `dummy' `dummyt' `Treat' , vce(robust)
		test _b[`Treat'] = `theta'
		if `r(p)' < .05{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala theta_3 = _b[`Treat']
		return scala std_3 = _se[`Treat']
		return scala p_3 = `r(p)'
		return scala h1_3 = `h1'
		drop `dummy' `dummyt'

		
end

** Implement part II and part III

program endog2, rclass
	syntax [ , indicator(real 0)]
	capture drop _all
	
	tempvar id a_i r Tr t a_t Post Treat u_it e_it y outcome
	
	local n 200
	set obs `n'
	
	gen `id' = _n	
	gen `a_i' =rnormal(5,sqrt(8))
	expand 15
	bysort `id' : gen `t' = _n
	
	xtset `id' `t'
	
	gen `Post' = 1 if `t' > 6
	quietly replace `Post' = 0 if `t' < 7

	gen `a_t' = .
	forvalues i = 1/15{
		local r = rnormal(1,sqrt(2))
		quietly replace `a_t' = `r' if `t' == `i'
	}
	
	gen `u_it' = rnormal(0,sqrt(2))
	
	gen `e_it' = 0
	levelsof `id', local(num_id)
	levelsof `t', local(num_t)
	foreach ii of local num_id{
		replace `e_it' = `u_it' ///
				if `id' == `ii' & `t' == 1
		foreach tt of local num_t{
			if `tt' > 1 {
				replace `e_it' = .25*L.`e_it' /// 
				+ .5*`u_it' if `id' == `ii' & `t' == `tt'
			}
		}
	}

	gen `outcome' = `a_i' + `a_t' + `e_it'
	summ `outcome' if `t' == 6 , detail
	gen `Tr' =  `outcome' < `r(p50)'
	
	gen `Treat' = `Tr'*`Post'
	
	
	local theta = 1.5
	
	gen `y' = `a_i' + `a_t' + `theta'*`Treat' + `e_it'
		if `indicator' == 1 {
			xtreg `y'  i6.`t'#c.`Treat' i.`t' `Treat' , fe cluster(`id')
			test _b[i6.`t'#c.`Treat'] = 0
			if `r(p)' < .05{
				local h1 = 1
			} 
			else {
				local h1 = 0
			}
		}
		else {
			xtreg `y'  i.`t' `Treat' , fe cluster(`id')
			test _b[`Treat'] = `theta'
			if `r(p)' < .05{
				local h1 = 1
			} 
			else {
				local h1 = 0
			}
		}
		return scala theta_2 = _b[`Treat']
		return scala std_2 = _se[`Treat']
		return scala p_2 = `r(p)'
		return scala h1_2 = `h1'
		
end


********** simulate ***********************

* For part I

simulate ///
    theta_iid  = r(theta_1)  h1_iid  = r(h1_1)  std_iid  = r(std_1)  ///
	theta_cluster  = r(theta_2)  h1_cluster  = r(h1_2) /// 
					 std_cluster  = r(std_2)  ///
	theta_robust  = r(theta_3)  h1_robust  = r(h1_3) /// 
					 std_robust  = r(std_3)  ///
	, reps(1000) : endog
	
* For part II
simulate ///
	theta_cluster  = r(theta_2)  h1_cluster  = r(h1_2) /// 
					 std_cluster  = r(std_2)  ///
	, reps(500) : endog2
	
* For part III
simulate ///
	theta_cluster  = r(theta_2)  h1_cluster  = r(h1_2) /// 
					 std_cluster  = r(std_2)  ///
	, reps(500) : endog2 , indicator(1)
	

********* print result ********************

program print_result
	display as text _dup(66) "-"
	display "method"  _col(10) "estTheta" ///
			_col(22) "Pr{H0 is rejected}" ///
			_col(46) "Standard Error"
	display as text _dup(66) "-"
	
	local methods iid cluster robust

	foreach m of local methods{
		capture quietly summ theta_`m'
		local theta = r(mean)
		capture quietly summ h1_`m'
		local h1 = r(mean)
		capture quietly summ std_`m'
		local std = r(mean)
		display as text "`m':" ///
				_col(10)  as result %5.3f `theta' ///
				_col(22) as result %5.3f `h1'   ///
				_col(46) as result %5.3f `std'
	}
	display as text _dup(66) "-"
end

print_result

log close _all


/*

Final results
--------------

Part I
-------

H0: Theta = 1.5
------------------------------------------------------------------
method   estTheta    Pr{H0 is rejected}      Standard Error
------------------------------------------------------------------
iid:     1.499       0.147                   0.058
cluster: 1.499       0.049                   0.076
robust:  1.499       0.129                   0.060
------------------------------------------------------------------

I expect the iid assumption should lead to higher standard errors

because its not efficient. But my results don't support this intuition.

But we can see that cluster method is the least likely to make H0

be rejected.

Part II
--------

Ashenfeler Dip
H0: Theta = 1.5
------------------------------------------------------------------
method   estTheta    Pr{H0 is rejected}      Standard Error
------------------------------------------------------------------
cluster: 1.055       0.999                   0.061
------------------------------------------------------------------

Part III
--------

H0: i6.t#c.Treat = 0
------------------------------------------------------------------
method   estTheta    Pr{H0 is rejected}      Standard Error
------------------------------------------------------------------
cluster: 0.457       0.000                   0.081
------------------------------------------------------------------
It seems unlikely to reject the null hypothesis. The falsification model
never works.

Part IV
--------

According to III, the control doesn't work. The reason lies in the
autoregression of the errors. This pre-treatment dip effect spreads to
other periods so that still confound the effects of the treatment.



*/
