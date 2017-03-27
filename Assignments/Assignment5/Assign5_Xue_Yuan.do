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

local binlens 01 10 25

foreach l of local binlens{
	bingen , varname(bwtnorm) binname(bin`l') binlen(`l')
}

* save main data
tempfile raw
save "`raw'"

tempfile bin01
tempfile bin10
tempfile bin25

local bins bin01 bin10 bin25

foreach bin of local bins{
	use `raw' , clear
	collapse (count) freq=bwtnorm , by(`bin')
	graph twoway scatter freq `bin' , ytitle(freq) ///
      name("graph_`bin'") title("Frequencies for `bin'")
save ``bin''
	
}

***************
**     A2    **
***************

foreach bin of local bins{

	use ``bin'' , clear
	gen cut = 1 if `bin' < 0
	replace cut = 0 if cut ==.
	gen inter = cut * `bin'
	
	local bws
	
	forvalues bw = 150(-50)50 {
		local bws `bws' bandwidth:`bw'
		display "`bin', bandwith=`bw':"
		eststo : regress freq cut `bin' inter if abs(`bin') < `bw'
	}
	
	* print estimate table to latex format
	esttab using A2_`bin'.tex , mtitles(`bws') ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Test continuity via frequency, `bin'") nonumber replace 

	eststo clear
}




***************
**     A3    **
***************
use `raw' , clear

gen mom_white = 1 if mom_race == "white":racelbl
replace mom_white = 0 if mom_white == .

gen mom_high = 1 if mom_ed < 12
replace mom_high = 0 if mom_high ==.

gen cut = 1 if bwtnorm >= 0
replace cut = 0 if cut ==.

gen inter = bwtnorm * cut

save `raw' , replace

local chars mom_white mom_high

forvalues bw = 90(-30)30{
	tempvar wght
	gen `wght' = 1 - abs(bwtnorm / `bw')
	
	local cs
	
	foreach c of local chars{
		
		display "reg: `c', bandwith: `bw', Triangular kernel"
		local cs `cs' "`c'_TK"
		eststo : regress `c' bwtnorm cut inter [pweight=`wght'] ///
		if abs(bwtnorm) <= `bw'
		display "reg: `c', bandwith: `bw', Rectangular kernel"
		local cs `cs' "`c'_RK"
		eststo : regress `c' bwtnorm cut inter ///
		if abs(bwtnorm) <= `bw'
	}
	
	* print estimate table to latex format
	esttab using A3_bw`bw'.tex , mtitles(`cs') ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Test continuity via characteristics, bandwidth: `bw'") ///
	nonumber replace 

	eststo clear
}


***************
**     A4    **
***************

** jump on ounces
local grams
drop if bweight == 1500
tempfile raw2
save `raw2'

forvalues oz = 51(1)54{
	local gram = round(`oz'*28.3495)
	local grams `grams' `gram'
	gen oz`oz' = 1 if bweight == `gram'
	replace oz`oz' = 0 if oz`oz' == .
	gen inter`oz' = oz`oz' * bweight
	
	local cs
	foreach c of local chars{
		local cs `cs' `c'
		display "reg: `c', bandwidth: 25, jump: `oz'oz"
		eststo : regress `c' bweight inter`oz' if abs(bweight-`gram')<= 25
	}	
	
	* print estimate table to latex format
	esttab using A4_oz`oz'.tex , mtitles(`cs') ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Test jump on oz `oz', bandwidth: 25") ///
	nonumber replace 
	eststo clear
}


** jump on 1500

use `raw' , clear

gen is1500 = 1 if bweight==1500
replace is1500=0 if is1500==.
gen inter2 = is1500 * bweight

foreach g of local grams{
	drop if bweight == `g'
}

foreach c of local chars{
		display "reg: `c', bandwith: 25 jump: 1500g"
		eststo : regress `c' bweight inter2 if abs(bweight- 1500)<= 25
}

* print estimate table to latex format
esttab using A4_z1500.tex , mtitles(`cs') ///
star(* 0.10 ** 0.05 *** 0.01) ///
title("Test jump on 1500, bandwidth: 25") ///
nonumber replace 
eststo clear

***************
**     B1    **
***************

use `raw' , clear

forvalues bw = 90(-30)30{
	tempvar wght
	gen `wght' = 1 - abs(bwtnorm / `bw')
	display "reg: one-year mortality, bandwidth: `bw', triangular kernel"
	eststo : regress agedth5 bwtnorm cut inter [pweight=`wght'] ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	display "reg: one-year mortality, bandwidth: `bw', Rectangular kernel"
	eststo : regress agedth5 bwtnorm cut inter ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	
	* print estimate table to latex format
	esttab using B1_bw`bw'.tex , mtitles("Triangular" "Rectangular") ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Birth weight class on mortality, bandwidth: `bw'") ///
	nonumber replace 
	eststo clear
}

***************
**     B2    **
***************

use `raw2' , clear

forvalues bw = 90(-30)30{
	tempvar wght
	gen `wght' = 1 - abs(bwtnorm / `bw')
	display "reg: one-year mortality, bandwith: `bw', triangular kernel" ///
	", birth weight 1500 dropped"
	eststo : regress agedth5 bwtnorm cut inter [pweight=`wght'] ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	display "reg: one-year mortality, bandwith: `bw', Rectangular kernel" ///
	", birth weight 1500 dropped"
	eststo : regress agedth5 bwtnorm cut inter ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	
	* print estimate table to latex format
	esttab using B2_bw`bw'.tex , mtitles("Triangular" "Rectangular") ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Birth weight class on mortality, bandwidth: `bw'", 1500 dropped) ///
	nonumber replace 
	eststo clear
}

***************
**     B3    **
***************

foreach g of local grams{
	drop if bweight == `g'
}

forvalues bw = 90(-30)30{
	tempvar wght
	gen `wght' = 1 - abs(bwtnorm / `bw')
	display "reg: one-year mortality, bandwith: `bw', triangular kernel" ///
	", birth weight 1500 dropped, exact ounces dropped"
	eststo : regress agedth5 bwtnorm cut inter [pweight=`wght'] ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	display "reg: one-year mortality, bandwith: `bw', Rectangular kernel" ///
	", birth weight 1500 dropped, exact ounces dropped"
	eststo : regress agedth5 bwtnorm cut inter ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	
	* print estimate table to latex format
	esttab using B3_bw`bw'.tex , mtitles("Triangular" "Rectangular") ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Birth weight class on mortality, bandwidth: `bw'", ///
	       "1500 and ounce heaps dropped") ///
	nonumber replace 
	eststo clear
}


***************
**     B4    **
***************
use `raw2' , clear

local heaps
local heapinters

foreach g of local grams{
	gen heap_`g' = 1 if bweight == `g'
	replace heap_`g' = 0 if heap_`g' == .
	gen heapinter_`g' = heap_`g' * bwtnorm
	local heaps `heaps' heap_`g'
	local heapinters `heapinters' heapinter_`g'
}


forvalues bw = 90(-30)30{
	tempvar wght
	gen `wght' = 1 - abs(bwtnorm / `bw')
	display "reg: one-year mortality, bandwith: `bw', triangular kernel" ///
	", birth weight 1500 dropped, ounce heap controlled"
	eststo : regress agedth5 bwtnorm cut inter `heaps' `heapinters' ///
	[pweight=`wght'] if abs(bwtnorm) <= `bw' , vce(robust)
	display "reg: one-year mortality, bandwith: `bw', Rectangular kernel" ///
	", birth weight 1500 dropped, ounce heap controlled"
	eststo : regress agedth5 bwtnorm cut inter `heaps' `heapinters' ///
	if abs(bwtnorm) <= `bw' , vce(robust)
	
	* print estimate table to latex format
	esttab using B4_bw`bw'.tex , mtitles("Triangular" "Rectangular") ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	title("Birth weight class on mortality, bandwidth: `bw'", ///
	       "1500 and ounce heaps controlled") ///
	nonumber replace 
	eststo clear
}



log close _all



