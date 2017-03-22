#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignments/Assignment5"

if c(username)=="xueqingyan" {
	cd "C:\Users\yanxueqing\Desktop\ECMT 676 Assignments\Assignment 5"
}

use "runandjump_sample1500g.dta"

*normalize birth weight
gen bwtnorm = bweight - 1500


***************
**     A1    **
***************

* bingen generates a variable indicating bins that radiate out from 0.
* the bins are represented by their middle values.

program bingen
	syntax  , varname(string) binname(string) [binlen(real 10)]
	tokenize `varname'
	local vn `1'
	tokenize `binname'
	local bn `1'
	gen `bn' = `binlen' / 2 + `binlen' * floor(`vn' / `binlen')
end

* generate bins of length 1, 10, 25
bingen , varname(bwtnorm) binname(bin01) binlen(1)  
bingen , varname(bwtnorm) binname(bin10) binlen(10)
bingen , varname(bwtnorm) binname(bin25) binlen(25)

tempfile raw
save "`raw'"

tempfile bin01
tempfile bin10
tempfile bin25

collapse (count) freq=bwtnorm , by(bin01)
graph twoway scatter freq bin01 , ytitle(freq) ///
      name("graph01") title("Frequencies, bin width=1")
save `bin01'

use `raw' , clear
collapse (count) freq=bwtnorm , by(bin10)
graph twoway scatter freq bin10 , ytitle(freq) ///
      name("graph10") title("Frequencies, bin width=10")
save `bin10'

use `raw' , clear
collapse (count) freq=bwtnorm , by(bin25)
graph twoway scatter freq bin25 , ytitle(freq) ///
      name("graph25") title("Frequencies, bin width=25")
save `bin25'

***************
**     A2    **
***************

program myreg
	args bin
	gen cut = 1 if `bin' < 0
	replace cut = 0 if cut ==.
	gen inter = cut * `bin'
	forvalues bw = 150(-50)50 {
		display "`bin', bandwith=`bw':"
		regress freq cut `bin' inter if abs(`bin') < `bw'
	}
end


local bins bin01 bin10 bin25

foreach bin of local bins{
	use ``bin'' , clear
	myreg `bin'
}



