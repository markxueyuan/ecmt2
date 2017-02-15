
#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignment3"

if c(username)=="xueqingyan" {
	cd "C:\Users\yanxueqing\Desktop\Metrics"
}


program endog, rclass
	syntax [ , trend(real 0)]
	capture drop _all
	
	tempvar y id r Treat t Post e TrPos
	
	local n 400
	set obs `n'
	gen `id' = _n
	gen `r' = runiform()
	sort `r'
	generate `Treat' = _n <= .5*`n'
	expand 10
	bysort `id' : gen `t' = _n 
	gen `Post' = 1 if `t' > 5
	replace `Post' = 0 if `t' < 6
	local b0 1.2
	local b1 2
	local b2 1.5
	local b3 2
	
	gen `e' = rnormal(0,2)
	gen `TrPos' = `Treat'*`Post'
	gen `y' = `b0' + `b1'*`Treat' + `b2'*`Post'  /// 
	+ `b3'*`Treat'*`Post'+ `trend'*`t'*`Treat' + `e'
	
	* Method is ols:
		regress `y' `Treat' `Post' `TrPos'
		test _b[`TrPos'] = `b3'
		if `r(p)' < .05{
			local h1 = 1
		} 
		else {
			local h1 = 0
		}
		return scala b3 = _b[`TrPos']
		return scala p = `r(p)'
		return scala h1 = `h1'
		
end


********** simulate ***********************

program siml, rclass
	syntax [ , trend(real 0)]
	capture drop _all
	
	simulate ///
	b3  = r(b3)  p  = r(p)  h1  = r(h1)  ///
	, reps(500): endog, trend(`trend')
	
	return scala trend = `trend'
	quietly summ b3
	return scala b3 = r(mean)
	quietly summ p
	return scala p = r(mean)
	quietly summ h1
	return scala h1 = r(mean)
	
end




********* print result ********************

program print_result
	display as result "Thanks for your patience!"
	display as result "I set the simulate function implemented quietly,"
	display as result "so that the table can be clearly printed."
	display as text _dup(66) "-"
	display "Trend"  _col(10) "estbeta3" ///
			_col(22) "Pr{estbeta3 = 2}"   ///
			_col(46) "Pr{H0 is rejected}"
	display as text _dup(66) "-"

	forvalues t=-.6(.2).7{
		quietly siml , trend(`t')
		display as result %5.1f `t' ///
				_col(10)  as result %5.3f `r(b3)' ///
				_col(22) as result %5.3f `r(p)'   ///
				_col(46) as result %5.3f `r(h1)'
	}
	display as text _dup(66) "-"
end

print_result

log close _all


/*

Final results
--------------

------------------------------------------------------------------
Trend    estbeta3    Pr{estbeta3 = 2}        Pr{H0 is rejected}
------------------------------------------------------------------
 -0.6    -0.999      0.000                   1.000
 -0.4    -0.001      0.000                   1.000
 -0.2    1.000       0.000                   1.000
  0.0    2.007       0.506                   0.050
  0.2    3.002       0.000                   1.000
  0.4    3.999       0.000                   1.000
  0.6    4.997       0.000                   1.000
------------------------------------------------------------------




*/
