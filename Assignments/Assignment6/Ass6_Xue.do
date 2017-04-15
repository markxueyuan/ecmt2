#delimit cr
capture log close
capture log using mylog , replace
capture program drop _all
capture estimates drop _all
clear all
set more off

capture cd "D:/Lindo/Assignments/Assignment6"

if c(username)=="xueqingyan" {
	cd "C:\Users\yanxueqing\Desktop\ECMT 676 Assignments\Assignment 6"
}

capture mkdir Output_Xue


** import data

use "stardata.dta"

** change data to long panel form

local panel star ctype stype hdeg clad totexp trace read math ses sysid ///
schid tchid csize

reshape long `panel' , i(newid) j(grade) string
* denote kindergarten's grade to 0
replace grade = "0" if grade == "k"
destring , replace

****************
*    1.b       *
****************

** replace missing data to .

program recode_miss
	args v
	local missing 999999 9999 999 99 9
	foreach m of local missing {
		display "try: `m'"
		quietly count if `v' == `m'
		display "match: `r(N)'"
		if `r(N)' != 0{
			replace `v' = . if `v' == `m'
			continue , break
		}
		else {
			continue
		}
	}
end

foreach v of varlist _all {
	display "`v': replace miss"
	recode_miss `v'
}



** generate new variables

*free lunch
gen frln = 1 if ses == 1
replace frln = 0 if ses == 2
label variable frln "free lunch"

* white or asian
gen whasn = 1 if race == 1 | race == 3
replace whasn = 2 if race == .
replace whasn = 0 if whasn == .
replace whasn = . if whasn == 2
label variable whasn "white/asian"

* month of birth
gen mob = 1.5 if qob == 1
replace mob = 4.5 if qob == 2
replace mob = 7.5 if qob == 3
replace mob = 10.5 if qob == 4

* age in 1985
gen agein1985 = 1985-yob+(9-mob)/12
label variable agein1985 "age in 1985"

* attrition indicator
local panel `panel' frln
reshape wide `panel' , i(newid) j(grade)
gen attr = 0 if star0 >= star1 & star1 >= star2 & star2 >= star3
replace attr = 2 if star0 == 2 & star1 == 2 & star2 == 2 & star3 == 1
replace attr = 1 if attr == .
replace attr = . if attr == 2
label variable attr "attrition"

reshape long

*average SAT score

local sat math read

gen mathp = .
gen readp = .

foreach s of local sat{
	forvalues g = 0(1)3{
		quietly summ `s' if grade == `g' & (ctype == 2 | ctype == 3)
		replace `s'p = 100 *(`s' - `r(min)') ///
		/ (`r(max)' - `r(min)') if grade ==`g'
	}
	replace `s'p = round(`s'p, 1) if `s'p != .
	replace `s'p = 100 if `s'p >= 100 & `s'p < .
	replace `s'p = 0 if `s'p <= 0
}


gen percent = (mathp + readp) / 2
replace percent = mathp if readp == . & mathp != .
replace percent = readp if mathp == . & readp != .
label variable percent "average score percentile"

** descriptive statistics

bysort grade: summ frln
bysort grade: summ percent

local panel `panel' mathp readp percent
reshape wide `panel', i(newid) j(grade)
summ whasn agein1985 attr

****************
*    1.b       *
****************

** generate first enter variable

generate enter1st = . 

replace enter1st = 0 if ctype0 != .
replace enter1st = 1 if ctype0 == . & ctype1 != .
replace enter1st = 2 if ctype0 == . & ctype1 == . & ctype2 != .
replace enter1st = 3 if ctype0 == . & ctype1 == . & ctype2 == . & ctype3 != .

tempfile long
capture save `long' , replace


** export results similar to table 1 in the paper

forvalues i = 0(1)3 {
	local vars frln`i' whasn agein1985 attr csize`i' percent`i'
	foreach v of local vars {
		eststo : xi , noomit : ///
		quietly regress `v' i.ctype`i' if enter1st == `i', noconstant
		testparm _I*, equal
		return list
		estadd scalar p_F = r(p)
	}
	
	esttab using "Output_Xue/q1b.csv" ///
	, scalars(p_F) plain append
	eststo clear
}

return list

****************
*     1.c      *
****************

* generate teacher white covariate
reshape long
gen twhite = 1 if trace == 1
replace twhite = 0 if trace == 2

* generate teacher master covariate
gen tmaster = 1 if hdeg >= 3
replace tmaster = 0 if hdeg < 3

* summarize twhite tmaster totexp
bysort grade :summ twhite tmaster totexp


** export results similar to table 1 in the paper

local panel `panel' twhite tmaster
reshape wide `panel' , i(newid) j(grade)

forvalues i = 0(1)3 {
	local vars twhite`i' tmaster`i' totexp`i'
	foreach v of local vars {
		eststo : xi , noomit : ///
		quietly regress `v' i.ctype`i' if enter1st == `i' , noconstant
		testparm _I*, equal
		return list
		estadd scalar p_F = r(p)
	}
	
	esttab using "Output_Xue/q1c.csv", scalars(p_F) plain append
	eststo clear
}

****************
*     1.d      *
****************
reshape long

local student frln whasn agein1985 attr csize percent
local teacher twhite tmaster totexp
local all `student' `teacher'

forvalues i = 0(1)3 {
	foreach v of local all {
		eststo : xi , noomit : ///
		quietly regress `v' i.ctype if enter1st == `i' ///
		, noconstant cluster(grade)
		testparm _I*, equal
		return list
		estadd scalar p_F = r(p)
	}
	esttab using "Output_Xue/q1d.csv", scalars(p_F) plain append
	eststo clear
}


****************
*     2.b      *
****************

forvalues i = 0(1)3 {
	foreach v of local all {
		* control on school id
		eststo : xi , noomit : ///
		quietly regress `v' schid i.ctype if enter1st == `i' ///
		, noconstant cluster(grade)
		testparm _I*, equal
		return list
		estadd scalar p_F = r(p)
	}
	esttab using "Output_Xue/q2b.csv", scalars(p_F) plain append
	eststo clear
}

****************
*     3.a      *
****************

gen smallc = 0 if ctype == 2
replace smallc = 1 if ctype == 1
replace smallc = 2 if ctype == 3

gen girl = 1 if sex == 2
replace girl = 0 if sex == 1

local st whasn girl frln
local tch twhite tmaster totexp


local rg1 i.smallc
local rg2  `rg1' schid
local rg3 `rg2' `st'
local rg4 `rg3' `teacher'


forvalues g = 0(1)3 {
	forvalues i = 1/4 {
		eststo : xi : quietly regress percent `rg`i'' if grade == `g'
	}
	esttab using "Output_Xue/q3a.csv", append
	eststo clear
}

drop _Ismallc_1
drop _Ismallc_2


****************
*     4.a      *
****************

local panel `panel' smallc
reshape wide `panel' , i(newid) j(grade)

* generate initial allocated class size  variable

gen initc = .

forvalues e = 0(1)3 {
	replace initc = ctype`e' if enter1st == `e'
}

reshape long

** regression of ctype over initc

local rg1 initc
local rg2  `rg1' schid
local rg3 `rg2' `st'
local rg4 `rg3' `teacher'

forvalues i = 1/4 {
	eststo : quietly regress ctype `rg`i''  if enter1st == 0
}
	esttab using "Output_Xue/q4a.csv", append
	eststo clear

****************
*     4.b      *
****************

gen s = 1 if initc == 1
replace s = 0 if initc == 2 | initc == 3
gen r = 1 if initc == 2
replace r = 0 if initc == 1 | initc == 3

local rg1	
local rg2  `rg1' schid
local rg3 `rg2' `st'
local rg4 `rg3' `teacher'

forvalues g = 0(1)3 {
	forvalues i = 1/4 {
		eststo : quietly ivreg percent `rg`i'' (csize = s r)
	}
	esttab using "Output_Xue/q4b.csv", append
	eststo clear
}
