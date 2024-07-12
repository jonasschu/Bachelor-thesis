cd "C:\Users\jonas\Documents\Studium\Bachelorarbeit"
use basque_without_spain.dta, clear

ssc install synth, replace 
ssc install estout, replace
* for regression output code see RD paper *

gen treat = (regionno==17)
bysort post treat: egen gdpcap_mean = mean(gdpcap) //does not make sense
gen post = (year>=1970)
gen treated = (post*treat==1)

* Summary statistics * 
estpost sum gdpcap invest popdens sec_* school_* if treat==0 & year<=1969
est store a
estpost sum gdpcap invest popdens sec_* school_* if treat==1 & year<=1969
est store b
esttab a b using descriptive.tex, replace cells("mean")

* DiD *
bysort post: sum gdpcap if treat==0
bysort post: sum gdpcap if treat==1
di (7.620381-4.848286)-(6.435492-3.232592)
*reg or xtreg? serial correlation of errors within units? if yes -> panel data models
xtset regionno year
xtreg gdpcap post##treat, fe vce(robust) //DiD estimate, wrong standard errors (see Bertrand et al. (2004))
xtreg gdp_avg post##treat, fe vce(robust)

xtreg gdpcap c.year##treat if year<=1969, fe vce(robust)
margins treat, dydx(year) //test parallel trends assumption

line gdpcap year if inlist(regionno, 3,4,5,7,8,10,14,15,16,18), lcolor(gs8) lwidth(thin) connect(L) || line gdpcap year if treat, lc(navy) lwidth(thick) scheme(s1color) legend(order(1 "Select Spanish regions" 2 "Basque Country")) //works better
xtline gdpcap if inlist(regionno, 3,4,5,7,8,10,14,15,16,18,17), overlay xscale(range(1955 1997)) //simple graph with trends of all regions
egen gdpcap_avg = mean(gdpcap), by(treat year)
twoway (line gdpcap_avg year if treat==1, sort xline(1970) legend(label(1 "Basque Country"))) (line gdpcap_avg year if treat==0, sort ytitle("GDP per Capita") legend(label(2 "other Spanish regions"))) //graph of unweighted trends for DiD

//plain trend in first differences
gen gdp_diff = d.gdpcap
egen gdp_diff_avg = mean(gdp_diff) if regionno!=1, by(treat year)
twoway (line gdp_diff_avg year if treat==1, sort xline(1970) legend(label(1 "Basque Country"))) (line gdp_diff_avg year if treat==0, sort ytitle("First difference of GDP per Capita") legend(label(2 "other Spanish regions")))

//plain trend in growth rates
gen gdp_growth = gdp_diff/gdpcap
egen gdp_growth_avg = mean(gdp_growth) if regionno!=1, by(treat year)
twoway (line gdp_growth_avg year if treat==1, sort xline(1970) legend(label(1 "Basque Country"))) (line gdp_growth_avg year if treat==0, sort ytitle("First difference of GDP per Capita") legend(label(2 "other Spanish regions")))

//event study style DiD
gen event_date = 1968 
gen event = year-event_date
replace event=0 if year<=1968
gen treat_event = treat*event
xtreg gdpcap treat i.event ib1.treat_event, fe vce(robust)
estimates store did_es

coefplot, keep(*.treat_event) vertical noci rename(*.treat_event = .) //event study plot


xtset regionno year
xtreg gdp_diff i.year##treat if year<=1969, re vce(robust)

* SCM * 
//region 1 is whole of spain -> exclude!
tsset regionno year
synth gdpcap gdpcap sec_agriculture invest popdens sec_energy sec_industry sec_construction sec_svc_venta, trunit(17) trperiod(1970)
synth_runner gdpcap gdpcap sec_agriculture invest popdens sec_energy sec_industry sec_construction sec_svc_venta, trunit(17) trperiod(1970) gen_vars
effect_graphs, trlinediff(-1)
single_treatment_graphs, trlinediff(0) effects_ylabels(-2(1)2) effects_ymax(2) effects_ymin(-2)
drop gdpcap_synth effect lead post_rmspe pre_rmspe //school variables give poor fit
matrix synth_weights_cov=e(W_weights)
svmat synth_weights_cov
scatter synth_weights_cov2 synth_weights_cov1, xlabel(0(1)20) xtitle("Spanish regions (except Basque Country)") ytitle("Weights assigned") xlabel(2(1)18) ylabel(0(.5)1) //weights of SCM cov
matrix RMSPE_cov=e(RMSPE)

synth gdpcap gdpcap(1969) gdpcap(1968) gdpcap(1967) gdpcap(1966) gdpcap(1965) gdpcap(1964) gdpcap(1963) gdpcap(1962) gdpcap(1961) gdpcap(1960) gdpcap(1959) gdpcap(1958) gdpcap(1957) gdpcap(1956) gdpcap(1955), trunit(17) trperiod(1970)  //all values of lagged outcome variable as predictors, not just mean
matrix synth_weights_out=e(W_weights)
svmat synth_weights_out
scatter synth_weights_out2 synth_weights_out1, xlabel(0(1)20) xtitle("Spanish regions (except Basque Country)") ytitle("Weights assigned") xlabel(2(1)18) ylabel(0(.5)1) //weights of SCM outcome
synth_runner gdpcap gdpcap(1969) gdpcap(1968) gdpcap(1967) gdpcap(1966) gdpcap(1965) gdpcap(1964) gdpcap(1963) gdpcap(1962) gdpcap(1961) gdpcap(1960) gdpcap(1959) gdpcap(1958) gdpcap(1957) gdpcap(1956) gdpcap(1955), trunit(17) trperiod(1970) gen_vars
effect_graphs, trlinediff(-1)
single_treatment_graphs, trlinediff(-1) effects_ylabels(-2(2)2) effects_ymax(2) effects_ymin(-2)
drop gdpcap_synth effect lead post_rmspe pre_rmspe
 //synth_runner does inference (placebo)
ereturn list
matrix event_scm=e(b)
matrix event_scm_transposed = event_scm'
matrix list event_scm_transposed
svmat event_scm_transposed //how to display this in event study style?
line event_scm_transposed time //has different format 
sum event_scm_transposed //mean equal to estimate using SDiD command
 
* weights based on covariates are .87 for unit 10 and .13 for unit 14
gen w_scm_cov=0
replace w_scm_cov=1 if regionno==17
replace w_scm_cov=.87 if regionno==10
replace w_scm_cov=.13 if regionno==14
xtreg gdpcap post##treat [aw=w_scm_cov], fe vce(robust) //wrong standard errors

* weights based on pre-treatment periods: .311 for unit 5, .483 for unit 14 and .206 for unit 18
gen w_scm_out=0 //correct them because of new weights!
replace w_scm_out=1 if regionno==17
replace w_scm_out=0.311 if regionno==5
replace w_scm_out=0.483 if regionno==14
replace w_scm_out=0.206 if regionno==18
reg gdpcap post##treat [aw=w_scm_out], r //reg or xtreg, DiD-regression using weights SCM weights
bysort post: sum gdpcap_synth_scm if treat==0 //pre- and post-treatment averages for 
bysort post: sum gdpcap_treat_scm if treat==1
di (7.845454-5.282476)-(8.523997-5.278423) //ATT of DiD using synthetic control group
xtreg gdpcap_all_scm post treat post##treat, fe vce(robust) //no standard errors?

scul gdpcap, ahead(3) treat(treated) obscol(black) cfcol("170 19 15") legpos(11)
 
* SDiD * 
ssc install sdid, replace
//regress gdpcap on covariates first, then use residuals for sdid command (circumvent problem of missing values)
sdid gdpcap regionno year treated, vce(placebo) method(sdid)
estimates store sdid_sdid
sdid gdpcap regionno year treated, vce(placebo) graph g1on
* graph placebo treatment effects to check whether pre-treatment effect is poor 
ereturn list
matrix omega=e(omega)
svmat omega //gives variables with unit weight and corresponding regionno
gen w_sdid = 0
replace w_sdid=1 if regionno==17
replace w_sdid=.0830578 if regionno==3
replace w_sdid=.057794 if regionno==4
replace w_sdid=.1743213 if regionno==5
replace w_sdid=.0392851 if regionno==7
replace w_sdid=.0337174 if regionno==8
replace w_sdid=.1503419 if regionno==10
replace w_sdid=.1343746 if regionno==14
replace w_sdid=.0882058 if regionno==15
replace w_sdid=.1197117 if regionno==16
replace w_sdid=.1191903 if regionno==18

xtreg gdpcap post##treat [aw=w_sdid], fe vce(cluster regionno) //SDiD without time weights in DiD regression -> do "real" DiD with averages of pre- and post-treatment periods over synthetic control and treatment unit

matrix lambda=e(lambda)
svmat lambda //only one period with positive time weight (1969)
* compute average in pre- and post-treatment periods manually

// Event study style output
qui sdid gdpcap regionno year treated, vce(noinference) graph
matrix lambda=e(lambda)[1..15,1]
matrix yco=e(series)[1..15,2]
matrix ytr=e(series)[1..15,3]
matrix aux=lambda'*(ytr-yco)
scalar meanpre_o= aux[1,1]

matrix difference=e(difference)[1..43,1..2]
svmat difference
rename (difference1 difference2) (time d)
replace d = d - meanpre_o
scatter d time if time>=1969, , xtitle("Year") ytitle("Treatment effect estimator") yscale(r(.5 -1.5)) ylabel(-1.5(0.5)0.5)

qui sdid gdpcap regionno year treated, vce(noinference) method(did) graph //DiD event study
matrix lambda_did=e(lambda)[1..15,1]
matrix yco_did=e(series)[1..15,2]
matrix ytr_did=e(series)[1..15,3]
matrix aux_did=lambda_did'*(ytr_did-yco_did)
scalar meanpre_o_did= aux_did[1,1]

matrix difference_did=e(difference)[1..43,1..2]
svmat difference_did
rename (difference_did1 difference_did2) (time_did d_did)
replace d_did = d_did - meanpre_o_did
scatter d_did time_did if time_did>=1969, xtitle("Year") ytitle("Treatment effect estimator") yscale(r(.5 -1.5)) ylabel(-1.5(0.5)0.5)

qui sdid gdpcap regionno year treated, vce(noinference) method(sc) graph // SCM event study
matrix lambda_scm=e(lambda)[1..15,1]
matrix yco_scm=e(series)[1..15,2]
matrix ytr_scm=e(series)[1..15,3]
//matrix aux_scm=lambda_scm'*(ytr_scm-yco_scm) scalar meanpre_o_scm= aux_scm[1,1]
matrix difference_scm=e(difference)[1..43,1..2]
svmat difference_scm
rename (difference_scm1 difference_scm2) (time_scm d_scm)
//replace d_scm = d_scm - meanpre_o_scm
scatter d_scm time_scm if time_scm>=1969, xtitle("Year") ytitle("Treatment effect estimator") yscale(r(.5 -1.5)) ylabel(-1.5(0.5)0.5)

sdid gdpcap regionno year treated, vce(placebo) method(did) //DiD with SDiD command
estimates store sdid_did
sdid gdpcap regionno year treated, vce(placebo) method(did) graph

sdid gdpcap regionno year treated, vce(placebo) method(sc) //SCM with SDiD command
estimates store sdid_scm
sdid gdpcap regionno year treated, vce(placebo) method(sc) graph


sdid gdpcap regionno year treated if inlist(regionno, 2,3,4,5,6,8,9,10,11,12,13,14,15,17) , vce(placebo) method(sdid) graph //run estimates without regions 7, 16, 18 (neighboring regions) to avoid spillover effects (robustness check)
gen treated_early = (treat==1 & year>=1963)
sdid gdpcap regionno year treated_early if year<=1969, vce(placebo) method(sdid) //placebo-in-time
sdid gdpcap regionno year treated, vce(placebo) reps(16) method(sdid) //less placebo replications

sdid gdpcap regionno year treated if inlist(regionno, 2,3,4,5,6,8,9,10,11,12,13,14,15,17), vce(placebo) method(sc) graph g1on //spillover effects control for SCM

esttab sdid_* using sdid_command.tex, se label title(Treatment effect estimates) mtitles("SDiD" "DiD" "SCM") star (* 0.10 ** 0.05 *** 0.01)

sdid gdpcap regionno year treated if inlist(regionno, 3,4,5,7,8,10,14,15,16,18,17), vce(placebo) method(sdid)
ereturn list
matrix omega_exclude=e(omega)
matrix list omega_exclude

reg gdpcap i.regionno i.year sec_agriculture sec_energy sec_industry sec_construction sec_svc_venta sec_svc_nonventa school_illit school_prim school_med, vce(cluster regionno) //to many missing values
predict resid_sdid, residuals
sdid resid_sdid regionno year treated if regionno!=1, vce(placebo) //does not work because of missing values

xtreg gdpcap treat i.event ib1.treat_event[aw=w_sdid], fe vce(robust)
estimates store sdid_es
coefplot, keep(*.treat_event) vertical

sdid gdpcap regionno year treated, vce(noinference) graph
matrix omega=e(omega)
qui svmat omega
scatter omega1 omega2 if omega1<=1969, xlabel(0(1)20) xtitle("Spanish regions (except Basque Country)") ytitle("Weights assigned") xlabel(2(1)18) ylabel(0(.1).5) //SDiD weights graph

sdid gdpcap regionno year treated, vce(noinference) method(did) graph
matrix omega_did=e(omega)
qui svmat omega_did
scatter omega_did1 omega_did2 if omega_did1<=1969, xlabel(0(1)20) xtitle("Spanish regions (except Basque Country)") ytitle("Weights assigned") xlabel(2(1)18) ylabel(0(.1).5) //weights of DiD 

sdid gdpcap regionno year treated, vce(noinference) method(sc) graph
matrix omega_scm=e(omega)
qui svmat omega_scm
scatter omega_scm1 omega_scm2 if omega_scm1<=1969, xlabel(0(1)20) xtitle("Spanish regions (except Basque Country)") ytitle("Weights assigned") xlabel(2(1)18) ylabel(0(.1).5) //weights of SCM