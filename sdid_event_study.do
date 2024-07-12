cd "C:\Users\jonas\Documents\Studium\Bachelorarbeit"
use "basque_event_study.dta", clear

qui sdid gdpcap regionno year treated, vce(noinference) graph
matrix lambda=e(lambda)[1..15,1]
matrix yco=e(series)[1..15,2]
matrix ytr=e(series)[1..15,3]
matrix aux=lambda'*(ytr-yco)
scalar meanpre_o= aux[1,1]

matrix difference=e(difference)[1..43,1..2]
svmat difference
rename (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate


xtset regionno year


//very big command, did not run yet :(
local b = 1
local B = 20
while `b' <= `B' {
preserve
bsample, cluster(regionno) idcluster(newcl)
qui count if treated == 0
local r1 = r(N)
qui count if treated != 0
local r2 = r(N)
if (`r1'!=0 & `r2'!=0) {
qui sdid gdpcap regionno year treated, vce(noinference) graph
matrix lambda_b = e(lambda)[1..15,1] //save lambda weight
matrix yco_b = e(series)[1..15,2] //control baseline
matrix ytr_b = e(series)[1..15,3] //treated baseline
matrix aux_b = lambda_b´*(ytr_b - yco_b) //calculate the pre-treatment mean
matrix meanpre_b = J(43,1,aux_b[1,1])
matrix d`b' = e(difference)[1..43,2] - meanpre_b
local ++b
}
restore
}

preserve
keep time d
keep if time!=.
forval b=1/`B' {
svmat d`b' // import each bootstrap replicate of difference between trends
}
egen rsd = rowsd(d11 - d`B´1)
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot
tw rarea UCI LCI time, color(gray%40) || 
scatter d time if time>=1969
graph export "event_sdid.pdf", replace
restore


webuse set www.damianclarke.net/stata/
. webuse quota_example.dta, clear
