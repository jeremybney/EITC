*JNF 2-5-2013
* This code creates the datasets specified below and produces all of the tables and figures in the manuscript
* I use the AER submission Table and Figure numbering, and then specify tables or figures only in the NBER version
* Input files from IRS:
	* Cross-Sectional Collapses and Derivatives:
		* IRS_zip3_collapse_masked_final.dta (income distributions collapsed to zipcode level)
		* IRS_zip3_bunching_saez_final.dta (bunching measures at zipcode/year level)
	* Movers Collapses:
		* movers_eventstudy_final.dta
		* movers_dists_final.dta
		* movers_db_bin_averages_final.dta
	* Child Birth:
		* Tables and figures for child birth created on the inside at the IRS.  Figures read into Stata point-by-point.
	
* Input files from other sources:
	* 2000census_data_zip3.dta (from Census website)
	* zip3_state_crosswalk.dta (from IRS collapse)
	* googletaxsearch.dta (from Google Trends)
	* state_eitc.dta (State EITC Top-Ups)
	* nrp_compliance.dta (from IRS NRP Tabulations)
	* app_figure_schedules.dta (from EITC and CTC law parameters)
	
	
**************************  OUTLINE  ****************************
*1. Read in IRS datasets and set up analysis datasets
*2. Create Figures (including NBER-version-only Figures)
*3. Create Tables (including NBER-version-only Tables)
*4. Calculate other numbers referenced in paper
*****************************************************************

*******************************************
****** SETUP CODE BLOCK - ALWAYS RUN ******
*******************************************

global root /n/chetty/IRS/EITC/empirics_final
global datadir $root/data_final

global logdir $root/logs_final
global figdir $root/figs_final
global kiddir $datadir/kidbig37_final

global date final
global IRSdate final
global bunchingdate final
global moversdate final

clear all
set matsize 800
set more off
set mem 5g

run $datadir/ado/discscatter.ado
run $datadir/ado/disclog.ado
run $datadir/ado/binscatter.ado


****************************************
****** SET UP COLLAPSE DATASET *********
****************************************

*First define weights for each zip3 and year to define quantiles as in micro data
*Define weights based on wage earners to calculate quintile cut points

*Open dataset, keeping only obs at highest level
*Note that there is one obs for each incbin/zip3/tax_yr/eic_child in this dataset
*Number of kids ranges from 0 to 2 in all yrs except 2009, when there is a bigger EITC for 3+ kids
use $datadir/IRS_zip3_collapse_masked_$IRSdate.dta if taxprep ==. & married_female == . & firmsize==., clear
drop if eic_child_law == 0 
drop if zip3 == -1
collapse (sum) count_w2_we, by(zip3 tax_yr)
rename count_w2_we count_we_zip3_tax_yr
sort zip3 tax_yr
save $datadir/wage_earner_counts_zip3_tax_yr.dta, replace

*Now make vingtiles of b using these weights
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
drop if zip3 == -1
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta
*keep if eic_child_law == 123
xtile b_q_post99 = b_zip3 [w=count_we_zip3_tax_yr] if tax_yr>=1999, nq(20) 
keep zip3 tax_yr b_q* b_zip3 count_we_zip3_tax_yr b_zip3selfemp
sort zip3 tax_yr
save $datadir/zip3_vingtiles.dta, replace

*Merge b_q back onto main collapsed data
use $datadir/IRS_zip3_collapse_masked_$IRSdate.dta, clear
drop if eic_child_law == 0 
drop if zip3 == -1
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/zip3_vingtiles.dta, nokeep
tab _merge
drop _merge
*Define married indicator
g married = 0 if (married_female==0|married_female==1)
replace married = 1 if married_female ==2

*Calculate EITC Refund
g incbin_exact= incbin+8950 if eic_child_law ==1
replace incbin_exact = incbin +12550 if eic_child_law ==2
replace incbin_exact = 0 if incbin_exact < 0

*Simulate EITC refund
g c1 = 12550 if eic_child_law >= 2
replace c1 = 8950 if eic_child_law == 1
g c2 = 16450
g p1 = 0.34 if eic_child_law == 1
replace p1 = 0.4 if eic_child_law >= 2
replace p1 = 0.45 if eic_child_law == 3 & tax_yr == 2009
g p2 = 0.21 if eic_child_law >= 2
replace p2 = 0.16 if eic_child_law == 1
 
g married_bonus = 0
replace married_bonus = 1000 if inrange(tax_yr,2002,2004)
replace married_bonus = 2000 if inrange(tax_yr,2005,2007)
replace married_bonus = 3000 if tax_yr==2008
replace married_bonus = 5000 if tax_yr==2009
 
g cpi = .7374 if tax_yr == 1996
replace cpi = .7577 if tax_yr == 1997
replace cpi = .7785 if tax_yr == 1998
replace cpi = .7916 if tax_yr == 1999
replace cpi = .8061 if tax_yr == 2000
replace cpi = .8311 if tax_yr == 2001
replace cpi = .8585 if tax_yr == 2002
replace cpi = .8721 if tax_yr == 2003
replace cpi = .8920 if tax_yr == 2004
replace cpi = .9126 if tax_yr == 2005
replace cpi = .9409 if tax_yr == 2006
replace cpi = .9776 if tax_yr == 2007
replace cpi = 1 if tax_yr == 2008
replace cpi = 1.0423 if tax_yr == 2009
g cpi2010 = 1.0440
 
replace married_bonus = married_bonus*cpi2010/cpi
g c2_single = c2
g p1_single = min(p1,0.4)
replace c2 = c2 + married_bonus if married == 1
g eic_refund = max(p1*min(incbin_exact,c1) - p2*max(incbin_exact-c2,0),0)
g eic_refund_single = max(p1_single*min(incbin_exact,c1) - p2*max(incbin_exact-c2_single,0),0)
g incbin_eic = incbin_exact + eic_refund

g c1_1kidsingle = 8950
g c2_1kidsingle = c2_single
g p1_1kidsingle = 0.34
g p2_1kidsingle = 0.1598
g eic_refund_1kidsingle = max(p1_1kidsingle*min(incbin_exact,c1_1kidsingle) - p2_1kidsingle*max(incbin_exact-c2_1kidsingle,0),0)
 
drop cpi*

save $datadir/cross_sec_collapse_work_$date.dta, replace


*****************************************
******* SET UP CORRELATES DATASET *******
*****************************************

*Generate tax preparer fraction dataset (fraction using a tax preparer among EITC filers with kids)
*Note: using 0 kids to get a split-sample estimate of se and tp frac yields very similar results
use $datadir/cross_sec_collapse_work_$date.dta if taxprep~=. & married_female==. & firmsize==. & eic_child>0, clear
drop if incbin == -20000
collapse (sum) count count_se count_w2_we, by(zip3 tax_yr taxprep)
bys zip3 tax_yr: egen count_zip3_tax_yr = sum(count)
bys zip3 tax_yr: egen count_se_zip3_tax_yr = sum(count_se)
g se_frac = count_se_zip3_tax_yr/count_zip3_tax_yr
g tp_frac = count/count_zip3_tax_yr if taxprep==1
keep if taxprep==1
keep zip3 tax_yr se_frac tp_frac
sort zip3 tax_yr
save $datadir/taxprep_se_frac_$date.dta, replace

*Construction of Google data
*Compute means by zip code (to account for multiple media markets)
use $datadir/googletaxsearch, clear
g temp = weight*taxsearch
bys zip: egen zip_taxsearch = sum(temp)

collapse (mean) zip_taxsearch (sum) pop, by(zip)

*Compute means by zip3 aggregating over zip codes
g zip3 = substr(zip,1,3)
destring zip3, replace
g temp = pop*zip_taxsearch
bys zip3: egen temp_num = sum(temp)
bys zip3: egen temp_denom = sum(pop)
g zip3_taxsearch = temp_num/temp_denom
bys zip3: keep if _n==1
keep zip3 zip3_taxsearch
rename zip3_taxsearch google_tax
sort zip3
save $datadir/googledata_work_$date, replace


** Get Population variables from 2000 Census
use $datadir/2000census_data_zip3.dta, clear
keep zip3 perc_foreignborn perc_white perc_black perc_hisp perc_asian perc_other tot_pop pop_density
sort zip3

** Get state from zip3-state crosswalk
sort zip3
merge zip3 using $datadir/zip3_state_crosswalk.dta
tab _merge
drop _merge
drop if zip3==.

keep zip3 state nostate perc_* tot_pop pop_density
sort zip3
save $datadir/correlates.dta, replace

***Merge various covariates onto bunching dataset at zip3 level

*1. Merge census covariates, density of EITC filers from Brookings
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3
merge zip3 using $datadir/correlates.dta
drop _merge
drop if nostate==1
drop if zip3==-1

*2. Merge State EITC data
sort state tax_yr
merge state tax_yr using $datadir/state_eitc
drop _merge
replace state_topup = 0 if state_topup == .

*3. Merge Google data, available only post-2004
sort zip3
merge zip3 using $datadir/googledata_work_$date
drop _merge
replace goog = . if tax_yr<2004

*4. Merge frac self-emp and tax prep indicators
*Note: merge is not 100% because tax prep data is missing in 1996
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/taxprep_se_frac_$date.dta
tab _merge
drop _merge

*Merge NRP data from IRS
sort state
merge state using $datadir/nrp_compliance.dta
tab _merge
drop _merge

g eitc_density = pop_dens*count/tot_pop

save $datadir/cross_sec_corr_work_$date.dta, replace

************************
******* FIGURES ********
************************
	
***Figure 1a: Income distributions for individuals with children in 2008, by # of kids

use $datadir/cross_sec_collapse_work_$date.dta  if tax_yr == 2008 & (eic_child_law == 1 | eic_child_law == 2) & taxprep==. & firmsize==. & married_female==., clear
collapse (sum) count, by(eic_child_law incbin_exact)
bys eic_child_law: egen temp = sum(count)
g frac = count/temp
replace frac = frac*100
drop temp
sort incbin

twoway connected frac incbin if eic_child_law == 1 &  incbin<=35000 & incbin> 0 ///
	|| connected frac incbin if eic_child_law == 2 & incbin<=40000 & incbin> 0, ///
	xline(8950, lpattern(dash) lcolor(navy))  xline(12550, lpattern(dash) lcolor(maroon)) ///
		graphregion(fcolor(white)) legend(off) msymbol(T) yla(, ang(h)) ///
		title("Income Distributions for Individuals with Children in 2008" ,size(small)) ///
		ytitle("Percent of Individuals" ,size(small)) xtitle("Taxable Income (Real 2010$)" ,size(small))
graph save $figdir/1a, replace
graph export $figdir/1b.tif, replace


***Figure 1b: Income distributions for wage earners with EITC schedule

use $datadir/cross_sec_collapse_work_$date.dta if tax_yr == 2008 & taxprep==. & firmsize==. & married_female==., clear
collapse (sum) count_w2_we, by (incbin_exact eic_child)
bys eic_child_law: egen temp = sum(count)
g frac = count_w2_we/temp
replace frac = frac*100
drop temp
sort incbin_exact

g number = _n
g d1 = 0 if number == 1
g a1 = 0 if number == 1
replace d1 = 8950	if number ==2
replace a1 = 3050 if number == 2
replace d1 = 16449 if number ==3
replace a1 = 3050 if number ==3
replace d1 = 35534 if number ==4
replace a1 = 0 if number ==4

g d2 = 0 if number == 1
g a2 = 0 if number == 1
replace d2 = 12550	if number ==2
replace a2 = 5036 if number == 2
replace d2 = 16449 if number ==3
replace a2 = 5036 if number ==3
replace d2 = 40362 if number ==4
replace a2 = 0 if number ==4

twoway	(connected frac incbin if eic_child_law == 1 &  incbin<=35000 & incbin> 0) ///
	(connected frac incbin if eic_child_law == 2 & incbin<=40000 & incbin> 0, msymbol(T)) ///
	(connected a1 d1, yaxis(2) lcolor(navy) msymbol(none)) ///
	(connected a2 d2 , yaxis(2) lcolor(maroon) msymbol(none)), ///
	ylabel(0(3000)15000, axis(2) angle(h))	xlabel(0(5000)35000) ///
	xline(8950, lpattern(dash) lcolor(navy)) xline(12550, lpattern(dash) lcolor(maroon)) ///
		graphregion(fcolor(white)) legend(off) yla(0(1)5, ang(h)) yscale(range(0 5)) ///
		title("Wage-Earners with Children in 2008" ,size(small)) /// 
		ytitle("Percent of Wage-Earners" ,size(small)) xtitle("W-2 Wage Earnings (Real 2010$)" ,size(small))
graph export $figdir/1b.tif, replace
graph save $figdir/1b, replace

***Figure 2: Maps of sharp bunching

*2a: Full US Map with zip3 bunching

use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

xtile b_q_2008 = b_zip3 [w=count] if tax_yr==2008, nq(10)

set more off
forval i=1/10 {
	sum b_zip3 if b_q_2008==`i'
}

cap drop _m
keep if tax_yr==2008

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

drop if (zip3>=995 & zip3<=999) | (zip3>=967 & zip3<=968) | (zip3>=006&zip3<=009)

bys id: gen dup = _n
drop if dup>1
drop dup

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.9 1.2 1.4 1.6 1.9 2.3 2.6 3.4 4.4 30.6) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2a_2008.tif, replace
graph export $figdir/2a_2008.png, replace
graph export $figdir/2a_2008low.png, width(650) replace

** Alaska

use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

cap drop _m
keep if tax_yr==2008

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

keep if (zip3>=995 & zip3<=999)
drop if zip3==995&id>130

bys id: gen dup = _n
drop if dup>1
drop dup

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.9 1.2 1.4 1.6 1.9 2.3 2.6 3.4 4.4 30.6) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2a_2008_alaska.png, replace

** Hawaii
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

cap drop _m
keep if tax_yr==2008

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

keep if (zip3>=967 & zip3<=968)

bys id: gen dup = _n
drop if dup>1
drop dup

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.9 1.2 1.4 1.6 1.9 2.3 2.6 3.4 4.4 30.6) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2a_2008_hawaii.png, replace


** 2 - Appendix Maps

foreach year of numlist 1996 1999 2002 2005 2008 {
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

xtile b_q_full = b_zip3 [w=count], nq(10)

set more off
forval i=1/10 {
	sum b_zip3 if b_q_full==`i'
}

cap drop _m
keep if tax_yr==`year'

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

drop if (zip3>=995 & zip3<=999) | (zip3>=967 & zip3<=968) | (zip3>=006&zip3<=009)

bys id: gen dup = _n
drop if dup>1
keep if count!=.

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.7 0.9 1.1 1.2 1.5 1.8 2.1 2.8 4.1 42.7) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2appx_`year'.png, width(1400) replace
graph export $figdir/2appx_`year'low.png, width(650) replace
}

** Alaska

foreach year of numlist 1996 1999 2002 2005 2008 {
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

xtile b_q_full = b_zip3 [w=count], nq(10)

set more off
forval i=1/10 {
	sum b_zip3 if b_q_full==`i'
}

cap drop _m
keep if tax_yr==`year'

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

keep if (zip3>=995 & zip3<=999)
drop if zip3==995&id>130

bys id: gen dup = _n
drop if dup>1
drop dup

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.7 0.9 1.1 1.2 1.5 1.8 2.1 2.8 4.1 42.7) graphr(fc(white)) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2appx_`year'_alaska.png, replace
}

** Hawaii
foreach year of numlist 1996 1999 2002 2005 2008 {
set more off
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta

*keep if eic_child_law>0
keep if count!=.

cap drop _m
keep if tax_yr==`year'

sort zip3
tempfile 01
save `01'

use $datadir/maps/zip3_database.dta
keep zip3 id
destring zip3, replace

sort zip3
merge m:m zip3 using `01'

drop _merge

keep if (zip3>=967 & zip3<=968)

bys id: gen dup = _n
drop if dup>1
drop dup

replace b_zip3 = b_zip3*100
format b_zip3 %2.1f

count
sort b_zip3


spmap b_zip3 using $datadir/maps/zip3_coords, id(id) clmethod(custom) clnumber(10) ///
	clbreaks(0 0.7 0.9 1.1 1.2 1.5 1.8 2.1 2.8 4.1 42.7) ///	
	fcolor(sandb*0.4 sandb*0.6 sandb gold dkorange orange_red red cranberry*1.1 cranberry*1.5 cranberry*1.8) ///
	oc(black) os(vthin vthin vthin vthin vthin vthin vthin vthin vthin vthin) legenda(off)
graph export $figdir/2appx_`year'_hawaii.png, replace
}

*** Figure 3 - Movers Figures
* 3a - Event Study (NBER Figure 4a)
* Note that coefficients displayed on graphs were calculated at IRS, and appear as coefficents
*   in eic_movers_collapse_v6.lst in the first regression
* The "first stage" impact of moving, which is the change in neighborhood bunching experienced
*    by the movers, is reported in eic_movers_collapse_v6.lst in the third regression
use $datadir/movers/movers_eventstudy_$moversdate, clear

rename bx_orig_dec o_dec
rename bx_new_dec n_dec

twoway connected bunch event_yr if o_dec == 5 & n_dec == 1, msymbol(O) yla(0(0.01)0.05) ///
 	|| connected bunch event_yr if o_dec == 5 & n_dec == 5, msymbol(T) ///
	|| connected bunch event_yr if o_dec == 5 & n_dec == 10, msymbol(S) xline(-0.5, lpattern(dash) lcolor(gs12)) ///
 		graphregion(fcolor(white)) title("Event Study of Sharp Bunching Around Moves" ,size(small)) ///
 		yla( ,angle(h)) ytitle("Sharp Bunching for Movers" ,size(small)) xtitle("Event Year" ,size(small)) ///
 		legend(region(lwidth(none)) col(1)label(1 "Movers to Lowest-Bunching Decile") ///
		label(2 "Movers to Middle-Bunching Decile") label(3 "Movers to Highest-Bunching Decile"))
graph export $figdir/3a.tif, replace
graph save $figdir/3a, replace

*Figure 3b: Asymmetry (NBER Figure 6)
use $datadir/movers/movers_db_bin_averages_$moversdate.dta, clear
rename eic_refund_count count
qui reg d_eic db_bin if db_bin <= 0 [w=count]
g fit_low = _b[db_bin]*db_bin+_b[_cons] if db_bin <= 0
di "DB < 0 Slope: " round(_b[db_bin],0.1) " (" round(_se[db_bin],0.1) ")"
qui reg d_eic db_bin if db_bin >= 0 [w=count]
g fit_high = _b[db_bin]*db_bin+_b[_cons] if db_bin >= 0
di "DB > 0 Slope: " round(_b[db_bin],0.1) " (" round(_se[db_bin],0.1) ")"
qui reg d_eic db_bin up treat [w=count], cluster(db_bin)
global pval = round(ttail(e(df_r),_b[treat]/_se[treat])*2,0.0001)
if $pval == 0 {
	di "P-Value for Asymmetry < 0.0001"
	}
else {
	di "P-Value for Asymmetry = " $pval
	}
twoway scatter d_eic db_bin || scatter fit_low db_bin if db_bin <= 0, c(l) m(i) lc(maroon) || ///
	scatter fit_high db_bin if db_bin >= 0, c(l) m(i) lc(maroon) xline(0, lpattern(dash) lcolor(gs12)) ///
	graphregion(fcolor(white)) title("Change in EITC Refunds vs. Change in Sharp Bunching for Movers" ,size(small)) ///
	yla( ,angle(h)) ytitle("Change in EITC Refund" ,size(small)) ///
	xtitle("Change in ZIP-3 Sharp Bunching" ,size(small)) legend(off)
graph save $figdir/3b, replace
graph export $figdir/3b.tif, replace


***Figure 4: Self-Employment Income and Child Birth (NBER Figure 8)
* Fig 4a
use $kiddir/fig_kid5_eicdensity_pre.dta, clear
	gen pre=1
	keep if group==1
	save $kiddir/eicdensity_pre, replace

	use $kiddir/fig_kid5_eicdensity_post.dta, clear
	gen pre=0
	append using $kiddir/eicdensity_pre.dta

	replace count=count*15 if group==10&pre==0
	replace count=count*7.6 if group==1&pre==0
	replace count=count*6.4 if group==1&pre==1
	replace count=count/100
	twoway connected count earningsr if group==1&pre==1 || ///
		 connected count earningsr if group==1&pre==0, leg(off) ms(t) || ///
		 connected count earningsr if group==10&pre==0 , title(" eicdensity_post") ms(s)
graph save $figdir/4a, replace
capture graph export $figdir/4a.wmf, replace

* Fig 4b
use $kiddir/fig_kid5_fracseinc, clear
scatter se_inc age if bd_zip3==1, c(l) ytitle("Income") ///
	|| scatter se_inc age if bd_zip3==5, c(l) xtitle("Age") xline(-0.5, lp(dash) lc(gs8)) ///
	|| scatter se_inc age if bd_zip3==10, c(l) graphreg(fc(white)) ///
		 title("Fraction of Individuals Reporting Self-Emp. Income Around Child Birth") ///
		 ytitle("Percent Reporting Self-Employment Income")
graph save $figdir/4b, replace
capture graph export $figdir/4b.wmf, replace

*** Figure 5: Cross-Sectional Wage Earning Distributions, by Bunching Decile (NBER Figure 9)
use $datadir/cross_sec_collapse_work_$date.dta if inlist(eic_child_law,1,2) & taxprep==. & firmsize==. & married_female==. & tax_yr>=1999, clear
*Deciles
g b_dec = ceil(b_q_post99/2)
collapse (sum) count_w2_we, by (b_dec incbin_exact eic_child_law)
bys b_dec eic_child_law: egen temp = sum(count)
g frac = count/temp
replace frac=100*frac
drop temp
sort incbin

g number = _n
g d1 = 0 if number == 1
g a1 = 0 if number == 1
replace d1 = 8950	if number ==2
replace a1 = 3050 if number == 2
replace d1 = 16449 if number ==3
replace a1 = 3050 if number ==3
replace d1 = 35534 if number ==4
replace a1 = 0 if number ==4

g d2 = 0 if number == 1
g a2 = 0 if number == 1
replace d2 = 12550	if number ==2
replace a2 = 5036 if number == 2
replace d2 = 16449 if number ==3
replace a2 = 5036 if number ==3
replace d2 = 40362 if number ==4
replace a2 = 0 if number ==4

twoway connected frac incbin if b_dec == 1 & incbin<=(26000+8450) & incbin>0 & eic_child==1, yla(0(0.5)3.5 ,angle(h)) xline(8950, lp(dash) lc(black)) /// 
	|| connected frac incbin if b_dec == 10 & incbin<=(26000+8450) & incbin>0 & eic_child==1, graphregion(fcolor(white)) ///
		ytitle("Percent of Wage-Earners", axis(1)) xtitle("Income ($)") msymbol(T) xline(16449, lp(dash) lc(black)) ///
	|| connected a1 d1  , yaxis(2) ylabel(0(1000)4000, axis(2) angle(h)) lcolor(gs8) msymbol(none) xlabel(0(5000)35000) ///
		ytitle("EITC Amount ($)", axis(2)) title("W-2 Earnings Distributions in High vs. Low Information Areas", size(small)) ///
		subtitle("Wage Earners with One Child", size(small)) ///
		legend(region(lwidth(none)) order(1 2) col(1) label(1 "Low Information Neighborhoods") label(2 "High Information Neighborhoods"))
graph save $figdir/5a, replace
graph export $figdir/5a.tif, replace

twoway connected frac incbin if b_dec == 1 & incbin<=39550 & incbin>0 & eic_child==2, yla(0(0.5)3.5 ,angle(h)) xline(12550, lp(dash) lc(black)) /// 
	|| connected frac incbin if b_dec == 10 & incbin<=39550 & incbin>0 & eic_child==2, msymbol(T) ///
		ytitle("Percent of Wage-Earners", axis(1)) xtitle("Income ($)") ///
	|| connected a2 d2 , yaxis(2) ylabel(0(2000)6000, axis(2) angle(h)) lcolor(gs8) msymbol(none) graphregion(fcolor(white)) ///
		ytitle("EITC Amount ($)", axis(2)) title("W-2 Earnings Distributions in High vs. Low Information Areas", size(small)) ///
		subtitle("Wage Earners with Two Children", size(small)) xline(16449, lp(dash) lc(black)) ///
		legend(region(lwidth(none)) order(1 2) col(1) label(1 "Low Information Neighborhoods") label(2 "High Information Neighborhoods"))
graph export $figdir/5b.tif, replace
graph save $figdir/5b, replace

*Related number for text and Figs 5a/b for difference between 10th and 1st decile in EITC refund
use $datadir/cross_sec_collapse_work_$date.dta if taxprep==. & married_female~=. & firmsize==. & tax_yr>=1999, clear
g b_dec = ceil(b_q_post99/2)
g top_decile=(b_dec==10)
g clustervar = tax_yr + zip3/1000
reg eic_refund top_decile if inlist(b_dec,1,10) & eic_child_law == 1 [w=count_w2_we], cl(clustervar)
reg eic_refund top_decile if inlist(b_dec,1,10) & eic_child_law > 1 [w=count_w2_we], cl(clustervar)
g kids = eic_child>1
tab kids b_dec if inlist(b_dec,1,10) [w=count_w2_we], sum(eic_refund) nost nof noobs
drop clustervar top_decile

*** Figure 6: Pre- and Post-Birth Wage Earnings Distributions (NBER Figure 12)
use $kiddir/fig_kid1_eicdensity_pre.dta, clear
scatter count wagesr if group==1, c(l) title("Year Before Child Birth", size(s)) legend(off) ylab(0(2)6) ///
	xline(8950 16500, lc(black black)) ///
	|| scatter count wagesr if group==5, c(l) graphr(color(white)) ms(T) ///
	|| scatter count wagesr if group==10, c(l) ytitle("Count") xtitle("Wages") ms(S)
capture graph export $figdir/6a.tif, replace
graph save $figdir/6a, replace

use $kiddir/fig_kid1_eicdensity_post.dta, clear
scatter count wagesr if group==1, c(l) title("Year of First Child Birth", size(s)) legend(off) ylab(0(2)6) ///
	xline(8950 16500, lc(black black)) ///
	|| scatter count wagesr if group==5, c(l) graphr(color(white)) ms(T) ///
	|| scatter count wagesr if group==10, c(l) ytitle("Count") xtitle("Wages") ms(S)
capture graph export $figdir/6b.tif, replace
graph save $figdir/6b, replace


*** Figure 7: Child-Birth Event Study (NBER Figure 13)
use $kiddir/kids_collapse_new_11, clear
g up = (bd_zip3_0 == 10)
g post = (age >= 0)
g treat = up*post
reg eicamtw1 up post treat [w=count] if inlist(bd_zip3_0,1,10) & inlist(age,-1,0)
local beta_treat=_b[treat]
* Take coefficient and SE on treat
collapse (mean) eicamtw1 (count) count [fw=count], by(bd_zip3_0 age)
keep if inlist(bd_zip3_0,1,5,10)
g temp = eic if age == -4
bys bd_zip3_0: egen correct = mean(temp)
drop temp
sum eic if age == -4 [w=count]
local perc_change=`beta_treat'/r(mean)*100
* number reported in text:
local perc_change=string(`perc_change',"%12.1f")
local beta_treat=string(`beta_treat',"%12.1f")
di "Treatment effect: $`beta_treat' (`perc_change'%)"
* gen figure
replace eicamtw1 = eicamtw1 - correct + r(mean)
discscatter eicamtw1 age if inlist(bd_zip3_0,1,5,10), by(bd_zip3_0) bynum(3) weight(count) ///
      graphregion(fcolor(white)) legend(region(lwidth(none)) order(1 2 3) ///
      label(1 "Lowest Decile") label(2 "Middle Decile") label(3 "Highest Decile")) yla( ,angle(h)) ///
      title("Event Study of Simulated Credit Around Child Birth: All Wage Earners" ,size(small)) ///
      ytitle("EITC Credit Amount for Wage Earners ($)" ,size(small)) xtitle("Age of Child" ,size(small)) ///
      xline(-0.5, lpattern(dash) lcolor(gs12))
graph save $figdir/7, replace
capture graph export $figdir/7.tif, replace

*** Figure 8: Changes in EITC Refunds Around Child Birth (NBER Figure 14)
* Fig 8a: Wage Earners Only
use $kiddir/fig_kid3_scatter.dta, clear
	gen n=_n
	tsset n
	gen d_eicamt = (eicamtw1-l.eicamtw1)
	keep if age==0
	gen id=1
	save $kiddir/eicamtscatter_d, replace

	use $kiddir/fig_kid3_scatterplacebo_2.dta, clear
	gen n=_n
	tsset n
	gen d_eicamt = (eicamtw1-l.eicamtw1)
	keep if age==0
	gen id=2
	
	append using $kiddir/eicamtscatter_d
	keep d_eicamt b_zip3_0 countjj id

twoway scatter d_eicamt b_zip3_0 if id==1 || lfit d_eicamt b_zip3_0 if id==1 [w=countjj] || ///
	scatter d_eicamt b_zip3_0 if id==2, ms(t) || lfit d_eicamt b_zip3_0 if id==2 [w=countjj], ///
	title("Changes in EITC Refund Amounts Around Child Birth vs. Sharp Bunching Rates" ///
	"(Wage Earners Only)", size(small)) xtitle("ZIP-3 Self-Employed Sharp Bunching",size(small)) ///
	ytitle("Change in Simulated One-Child EITC Refund ($)",size(small)) ylab(-100(100)200) ///
	legend(col(4) label(1 "0 to 1 Child") label(3 "2 to 3 Children"))
graph save $figdir/8a, replace
capture graph export $figdir/8a.tif, replace

* 8a coefficients computed using microdata

* Fig 8b: Full Sample
use $kiddir/fig_kid3_scatter_se.dta, clear
	gen n=_n
	tsset n
	gen d_eicamt = (eicamtw1-l.eicamtw1)
	keep if age==0
	gen id=1
	save $kiddir/eicamtscatter_d_se, replace

	use $kiddir/fig_kid3_scatterplacebo_2_se.dta, clear
	gen n=_n
	tsset n
	gen d_eicamt = (eicamtw1-l.eicamtw1)
	keep if age==0
	gen id=2
	
	append using $kiddir/eicamtscatter_d_se
	keep d_eicamt b_zip3_0 countjj id

twoway scatter d_eicamt b_zip3_0 if id==1 || lfit d_eicamt b_zip3_0 if id==1 [w=countjj] || ///
	scatter d_eicamt b_zip3_0 if id==2, ms(t) || lfit d_eicamt b_zip3_0 if id==2 [w=countjj], ///
	title("Changes in EITC Refund Amounts Around Child Birth vs. Sharp Bunching Rates" ///
	"(Full Sample)", size(small)) xtitle("ZIP-3 Self-Employed Sharp Bunching",size(small)) ///
	ytitle("Change in Simulated One-Child EITC Refund ($)",size(small)) ylab(-100(100)200) ///
	legend(col(4) label(1 "0 to 1 Child") label(3 "2 to 3 Children"))
graph save $figdir/8b, replace
capture graph export $figdir/8b.tif, replace

* 8b coefficients computed using microdata

*** Figure 9: Phase-In, Phase-Out, and Extensive Margin Responses (NBER Figure 15)
* Note that coefficients in both figures are from Table 2
* Fig 9a: Phase-In/Phase-Out 
* Note: Releveled so that y-intercepts of both best-fit lines are 0
use $kiddir/fig_kid4_phasein_scatter_se.dta, clear
	gen n=_n
	tsset n
	gen d_eicamt = (eicamtphaseinw1-l.eicamtphaseinw1)
	keep if age==0
	gen id=1
	save $kiddir/eicscatter4alt_se, replace

use $kiddir/fig_kid4_phaseout_scatter_se, clear
	gen n=_n
	tsset n
	gen d_eicamt = ( eicamtphaseoutw1-l. eicamtphaseoutw1)
	keep if age==0
	gen id=2
	append using $kiddir/eicscatter4alt_se

sum d_eicamt if id==1
gen d_eicamt_c1 = d_eicamt - r(mean) if id==1
sum d_eicamt if id==2
gen d_eicamt_c2 = d_eicamt - r(mean) if id==2

sum d_eicamt
replace d_eicamt_c1 = d_eicamt_c1 + r(mean)
replace d_eicamt_c2 = d_eicamt_c2 + r(mean)

*y intercept through 0
regress d_eicamt_c1 b_zip3_0 if id==1 
matrix c1 = e(b)
local c1int = c1[1,2]
regress d_eicamt_c2 b_zip3_0 if id==2
matrix c2 = e(b)
local c2int = c2[1,2]
replace d_eicamt_c1 = d_eicamt_c1 - `c1int'
replace d_eicamt_c2 = d_eicamt_c2 - `c2int'	

keep d_eicamt_c* b_zip3_0 countjj id
twoway scatter d_eicamt_c1 b_zip3_0 if id==1, mc(navy) || lfit d_eicamt_c1 b_zip3_0 if id==1, lc(navy) || ///
	scatter d_eicamt_c2 b_zip3_0 if id==2, mc(maroon) m(t) || lfit d_eicamt_c2 b_zip3_0 if id==2, lc(maroon) ///
	title("Phase-In and Phase-Out Responses" ,size(small)) ytitle("Changes in Simulated EITC Refund($)", size(small)) ///
	graphr(fc(white)) legend(col(4) label(2 "Phase In") label(4 "Phase Out")) ylab(-50(50)200) ///
	xtitle("ZIP-3 Self-Employed Sharp Bunching",size(small))
capture graph export $figdir/9a.tif, replace
graph save $figdir/9a, replace

* Fig 9b: Extensive Margin
use $kiddir/fig_kid4_extensive_work_se.dta, clear
	gen n=_n
	tsset n
	gen d_workw = (workw-l.workw)
	keep if age==0

keep d_workw b_zip3_0 countjj

twoway scatter d_workw b_zip3_0, yla(-0.044(0.044)0.176) || lfit d_workw b_zip3_0  [w=countjj], graphr(fc(white)) ///
	title("Extensive Margin Responses", size(small)) legend(off) xtitle("ZIP-3 Self-Employed Sharp Bunching",size(small)) ///
	ytitle("Change in Percent of Individuals with Positive W-2 Earnings", size(small))
capture graph export $figdir/9b_se.wmf, replace
graph save $figdir/9b_se, replace

** Appendix Figure 1: EITC and Total Tax Liability Schedule
use $datadir/app_figure_schedules.dta, clear
sort incbin
twoway line eic_refund incbin, ylabel(-2000(1000)4000) lc(maroon) || line total_refund incbin, graphreg(fc(white)) lp(dash) ///
	title("EITC Refund Schedule vs. Total Tax Liabilities for Single Filers with Child", size(small)) lc(navy) ///
	ytitle("Tax Refund") xtitle("Wage Earnings") legend(lab(1 "EITC Refund") lab(2 "Tax Refund"))
graph export $figdir/appx1.tif, replace
graph save $figdir/appx1, replace

*** Appendix Figure 2a: Results with Alternative Measure of Sharp Bunching, Cross-Sectional Scatter
use $datadir/cross_sec_collapse_work_$date.dta if (eic_child_law == 1 | eic_child_law==2) & taxprep==. & married_female~=. & firmsize==. & tax_yr>=1999, clear
drop if married == .
replace incbin = incbin+8950 if eic_child_law ==1
replace incbin = incbin +12550 if eic_child_law ==2
replace b_zip3 = b_zip3*100
replace b_zip3selfemp = 100*b_zip3selfemp

binscatter eic_refund b_zip3selfemp if tax_yr>=1999, weight(count_w2_we) legend(off) yla( ,angle(h)) ///
	title("EITC Credit Amount for Wage Earners with One Child vs. Neighborhood Self-Employed Sharp Bunching" ,size(small)) ///
	ytitle("EITC Credit Amount for Wage Earners ($)" ,size(small)) xtitle("Neighborhood Self-Emp. Sharp Bunching (%)" ,size(small)) ///
	reportreg method(tempvars)
graph export $figdir/appx2a.tif, replace
graph save $figdir/appx2a, replace

* Figures in discussion of AFig 2: Scaling from regular b to alternative b
sum b_zip3 [w=count]
global temp = r(mean)
sum b_zip3se [w=count]
di "Scaling from Regular B to Alternative B: " round(r(mean)/${temp},0.1)

*** Appendix Figure 2b: Results with Alternative Measure of Sharp Bunching, Child Birth Event-Study
use $kiddir/kids_collapse_new_11, clear
g zip3 = zip3_0
g tax_yr = birth_yr
sort zip3 tax_yr
merge zip3 tax_yr using $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, uniqusing keep(b_zip3selfemp)
rename b_zip3se b_zip3_se_0
xtile bd_zip3_se_0 = b_zip3_se_0 [w=count], nq(10)
g up = (bd_zip3_se_0 == 10)
g post = (age >= 0)
g treat = up*post
reg eicamtw1 up post treat [w=count] if inlist(bd_zip3_se_0,1,10) & inlist(age,-1,0)
* Take coefficient and SE on treat
collapse (mean) eicamtw1 (count) count [fw=count], by(bd_zip3_se_0 age)
keep if inlist(bd_zip3_se_0,1,5,10)
g temp = eic if age == -4
bys bd_zip3_se_0: egen correct = mean(temp)
drop temp
sum eic if age == -4 [w=count]
replace eicamtw1 = eicamtw1 - correct + r(mean)
discscatter eicamtw1 age if inlist(bd_zip3_se_0,1,5,10), by(bd_zip3_se_0) bynum(3) weight(count) ///
      graphregion(fcolor(white)) legend(region(lwidth(none)) order(1 2 3) ///
      label(1 "Lowest Decile") label(2 "Middle Decile") label(3 "Highest Decile")) yla( ,angle(h)) ///
      title("Event Study of Simulated Credit Around Child Birth: All Wage Earners" ,size(small)) ///
      ytitle("EITC Credit Amount for Wage Earners ($)" ,size(small)) xtitle("Age of Child" ,size(small)) ///
      xline(-0.5, lpattern(dash) lcolor(gs12))
graph save $figdir/appx2b, replace
graph export $figdir/appx2b.tif, replace




*** Appendix Figs 3 - MAPS (see maps code above)

*** Appendix Figure 4 (NBER Figure 3): Earnings Distribution in Top vs. Bottom Decile ZIPs
* Note: uses different deciles here since include self-employed
use $datadir/cross_sec_collapse_work_$date.dta if (eic_child_law >= 1) & taxprep==. & firmsize==. & married_female==., clear
xtile b_q_full = b_zip3 [w=count], nq(20) 
g b_dec = ceil(b_q_full/2)
collapse (sum) count, by (b_dec incbin)
bys b_dec: egen temp = sum(count)
g frac = count/temp
replace frac=100*frac
drop temp
sort incbin

twoway connected frac incbin if b_dec == 1 & incbin<=26000 & incbin>-20000 ///
 	|| connected frac incbin if b_dec == 10 &  incbin<=26000 & incbin>-20000, msymbol(t) xline(0, lpattern(dash) lcolor(gs12)) ///
 		graphregion(fcolor(white)) title("Income Distributions in Lowest vs. Highest Decile Neighborhoods" ,size(small)) ///
 		yla( ,angle(h)) ytitle("Percent of Individuals" ,size(small)) xtitle("Income Relative to First EITC Kink" ,size(small)) ///
 		legend(region(lwidth(none)) col(1) label(1 "Bottom Decile Bunching") label(2 "Top Decile Bunching"))
graph export $figdir/appx4.tif, replace
graph save $figdir/appx4, replace

*** Appendix Figure 5 (NBER Figure 4b): Event Study of EITC Refund for Movers 
* Note that coefficients displayed on graphs were calculated at IRS, and appear as coefficients
*   in eic_movers_collapse_v6.lst in the second regression
use $datadir/movers/movers_eventstudy_$moversdate, clear

rename bx_orig_dec o_dec
rename bx_new_dec n_dec

twoway connected eic_refund event_yr if o_dec == 5 & n_dec == 1, msymbol(O) ///
 	|| connected eic_refund event_yr if o_dec == 5 & n_dec == 5, msymbol(T) ///
	|| connected eic_refund event_yr if o_dec == 5 & n_dec == 10, msymbol(S) xline(-0.5, lpattern(dash) lcolor(gs12)) ///
 		graphregion(fcolor(white)) title("Event Study of EITC Refund Around Moves" ,size(small)) ///
 		yla( ,angle(h)) ytitle("EITC Refund for Movers" ,size(small)) xtitle("Event Year" ,size(small)) ///
 		legend(region(lwidth(none)) col(1) label(1 "Movers to Lowest-Bunching Decile") ///
		label(2 "Movers to Middle-Bunching Decile") label(3 "Movers to Highest-Bunching Decile"))
graph export $figdir/appx5.tif, replace
graph save $figdir/appx5, replace


*** Appendix Figure 6 (NBER Figure 5) - Pre/Post Move Income Distributions
use $datadir/movers/movers_dists_$moversdate, clear

rename bx_orig_dec o_dec
rename bx_new_dec n_dec
keep if o_dec == 5 & inlist(n_dec,1,5,10)
g post = (event_yr >= 0)
bys n_dec post: egen totcount = sum(count)
bys n_dec post incbin: egen postcount = sum(count)
g frac = postcount/totcount*100
keep if inlist(event_yr,-1,0)
sort incbin

* Appx 6a: Before Move
twoway connected frac incbin if post == 0 & n_dec == 1, msymbol(O) ///
 	|| connected frac incbin if post == 0 & n_dec == 5, msymbol(T) ///
	|| connected frac incbin if post == 0 & n_dec == 10, msymbol(S) ///
 		graphregion(fcolor(white)) title("Total Earnings Distribution in Years Before Move" ,size(small)) ///
 		yla( ,angle(h)) ytitle("Percent of Movers" ,size(small)) ///
		xtitle("Total Earnings Relative to First Kink" ,size(small)) ///
 		legend(region(lwidth(none)) col(3) label(1 "Movers to Lowest-Bunching Decile") ///
		label(2 "Movers to Middle-Bunching Decile") label(3 "Movers to Highest-Bunching Decile")) ///
		yscale(range(0 8)) ylab(0(2)8)
graph export $figdir/appx6a.tif, replace
graph save $figdir/appx6a, replace

* Appx 6b: After Move
twoway connected frac incbin if post == 1 & n_dec == 1, msymbol(O) ///
 	|| connected frac incbin if post == 1 & n_dec == 5, msymbol(T) ///
	|| connected frac incbin if post == 1 & n_dec == 10, msymbol(S) ///
 		graphregion(fcolor(white)) title("Total Earnings Distribution in Years After Move" ,size(small)) ///
 		yla( ,angle(h)) ytitle("Percent of Movers" ,size(small)) ///
		xtitle("Total Earnings Relative to First Kink" ,size(small)) ///
 		legend(region(lwidth(none)) col(1) label(1 "Movers to Lowest-Bunching Decile") ///
		label(2 "Movers to Middle-Bunching Decile") label(3 "Movers to Highest-Bunching Decile")) ///
		yscale(range(0 8)) ylab(0(2)8)
graph export $figdir/appx6b.tif, replace
graph save $figdir/appx6b, replace


*** Appendix Figure 7 (NBER Figure 7): Correlates of Sharp Bunching
* Appx Fig 7a: Evolution of sharp bunching 
use $datadir/cross_sec_corr_work_$date, clear
replace b_zip3 = 100*b_zip3
g temp = eitc_density if tax_yr==1996
bys zip3: egen eitc_density96 = mean(temp)
sum eitc_density96 [w=count],d
g highdens = eitc_density96>r(p50) if eitc_density96<.

collapse (mean) b_zip3 [w=count], by(tax_yr highdens)
drop if highdens==.

twoway (scatter b_zip3 tax_yr if highdens==0, c(l)) (scatter b_zip3 tax_yr if highdens==1, c(l) msymbol(T)), ///
	ylab(0(1)4) ///
	graphregion(fcolor(white)) ///
	title("Evolution of Self-Emp. Bunching in Low vs. High EITC-Density Neighborhoods", size(medium)) ///
	ytitle("Self-Employed Sharp Bunching") ///
	xtitle("Year") ///
	legend(label(1 "Below-Median EITC Density") label(2 "Above-Median EITC Density"))
graph export $figdir/appx7a.tif, replace
graph save $figdir/appx7a, replace

*Appx Fig 7b: Bunching vs fraction of tax prepared returns
use $datadir/cross_sec_collapse_work_$date.dta if taxprep~=. & married_female==. & firmsize==. & tax_yr==2008 & eic_child>0, clear
drop if incbin == -20000
replace count_se = . if incbin ~= 0
collapse (sum) count count_se count_w2_we, by(zip3 tax_yr taxprep)
g b = count_se/count
g temp = b if taxprep==1
bys zip3 tax_yr: egen b_t = mean(temp)
drop temp
g b_nt = b if taxprep==0
bys zip3 tax_yr: egen count_zip3_tax_yr = sum(count)
g temp = count/count_zip3_tax_yr if taxprep==1
bys zip3 tax_yr: egen tp_frac = mean(temp)
drop temp b
bys zip3 tax_yr: egen numer = sum(count_se)
g b = numer/count_zip3_tax_yr
drop if taxprep == 1
drop taxprep

replace b=100*b
replace b_nt=100*b_nt
replace b_t = 100*b_t

binscatter b_nt b_t tp_frac, title("Sharp Bunching vs. Paid Prepared Returns in ZIP code, by Preparation Status" ,size(small)) ///
		ytitle("Sharp Bunching by Self-Employed Taxpayers (%)" ,size(small)) legend(off) symbols(O T) ///
		xtitle("Fraction of Tax Prepared Returns in 3-Digit Zip Code" ,size(small)) ///
		weight(count_zip3_tax_yr) reportreg method(tempvars)

graph export $figdir/appx7b.tif, replace
graph save $figdir/appx7b, replace

***Appendix Figure 8 (NBER Figure 10): Difference in wage earnings distributions in top vs bottom decile, large vs. all firms
use $datadir/cross_sec_collapse_work_$date.dta if inlist(eic_child_law,1,2) & taxprep==. & married_female==. & tax_yr>=1999, clear

g b_dec = floor((1+b_q_post99)/2)
replace firmsize = 0 if firmsize==.
*Combine firmsize 2 (101-1000) and 3 (1001+) to get firms with >100 employees
replace firmsize =2 if firmsize==3
collapse (sum) count_w2_we, by (b_dec incbin_exact firmsize eic_child_law)
bys b_dec firmsize eic_child_law: egen temp = sum(count)
g frac = count/temp
drop temp

keep if b_dec==1|b_dec==10
bys eic_child_law incbin firmsize (b_dec): g d_frac = frac - frac[_n-1] if b_dec==10 & incbin==incbin[_n-1]
replace d_frac=d_frac*100

g number = _n
g d1 = 0 if number == 1
g a1 = 0 if number == 1
replace d1 = 8950	if number ==2
replace a1 = 3050 if number == 2
replace d1 = 16449 if number ==3
replace a1 = 3050 if number ==3
replace d1 = 35534 if number ==4
replace a1 = 0 if number ==4

g d2 = 0 if number == 1
g a2 = 0 if number == 1
replace d2 = 12550	if number ==2
replace a2 = 5036 if number == 2
replace d2 = 16449 if number ==3
replace a2 = 5036 if number ==3
replace d2 = 40362 if number ==4
replace a2 = 0 if number ==4

sort incbin
twoway connected d_frac incbin if b_dec == 10 &incbin<=34450 & incbin>=1000 & firmsize==0 & eic_child==1, yla( ,angle(h)) ///
	|| connected d_frac incbin if b_dec == 10 & incbin<=34450 & incbin>=1000 & firmsize==2 & eic_child==1, ///
		ytitle("Difference in Income Densities", axis(1)) xtitle("Income ($)") msymbol(T) ///
	|| connected a1 d1, sort(d1) yaxis(2) yla(0(1000)4000, axis(2) angle(h)) lcolor(gs8) msymbol(none) xlabel(0(5000)35000) ///
		ytitle("EITC Amount ($)", axis(2)) title("W-2 Earnings Distributions in High vs. Low Information Areas", size(small)) ///
		subtitle("Wage Earners with One Child", size(small))	graphregion(fcolor(white)) ///
		legend(region(lwidth(none)) order(1 2) label(1 "All Firms") label(2 ">100 Employees"))
graph export $figdir/appx8a.tif, replace
graph save $figdir/appx8a, replace

sort incbin
twoway connected d_frac incbin if b_dec == 10 & incbin<=39550 & incbin>=1000 & firmsize==0 & eic_child==2, yla( ,angle(h)) ///
	|| connected d_frac incbin if b_dec == 10 & incbin<=39550 & incbin>=1000 & firmsize==2 & eic_child==2, ///
		graphregion(fcolor(white)) title("W-2 Earnings Distributions in High vs. Low Information Areas", size(small)) ///
		subtitle("Wage Earners with Two Children", size(small)) ytitle("Difference in Income Densities", axis(1)) ///
		xtitle("Income ($)") ytitle("EITC Amount ($)", axis(2)) msymbol(T)  xlabel(0(10000)40000) ///
	|| connected a2 d2 , sort(d2) yaxis(2) yla(0(1000)6000, axis(2) angle(h)) lcolor(gs8) msymbol(none) ///
		legend(region(lwidth(none)) order(1 2) label(1 "All Firms") label(2 ">100 Employees"))
graph export $figdir/appx8b.tif, replace
graph save $figdir/appx8b, replace


*** Appendix Figure 9 (NBER Figure 11): Wage Earners' EITC Amounts vs. Self-Employed Sharp Bunching Rates
*Appx Fig 9a: EITC credit amount for wage earners vs Neighborhood Self-Employed Sharp Bunching
use $datadir/cross_sec_collapse_work_$date.dta if taxprep==. & married_female~=. & firmsize==. & tax_yr>=1999, clear

drop if married == .
replace b_zip3 = 100*b_zip3

binscatter eic_refund b_zip3 if (eic_child_law == 1 | eic_child_law==2), weight(count_w2_we) legend(off) yla( ,angle(h)) ///
	title("EITC Credit Amount for Wage Earners with One Child vs. Neighborhood Self-Employed Sharp Bunching" ,size(small)) ///
	ytitle("EITC Credit Amount for Wage Earners ($)" ,size(small)) xtitle("Neighborhood Self-Emp. Sharp Bunching (%)" ,size(small)) ///
	method(tempvars) linetype(lfitscatter)
graph export $figdir/appx9a.tif, replace
graph save $figdir/appx9a, replace

* Regression for figure
reg eic_refund b_zip3 if (eic_child_law == 1 | eic_child_law==2) [w=count_w2_we]

* Regression using single-earner EITC schedule only
reg eic_refund_single b_zip3 if tax_yr >= 1999 [w=count_w2_we]

* Regression using single-earner with 1 kid EITC schedule only
reg eic_refund_1kidsingle b_zip3 if tax_yr >= 1999 [w=count_w2_we]

* Numbers in text
xtile dec=b_zip3 [w=count_w2_we], nq(10)
g top_decile = (dec==10)
reg eic_refund top_decile if inlist(dec,1,10) & inlist(eic_child,1,2) [w=count_w2_we]

sum eic_refund if dec==1 & inlist(eic_child,1,2) [w=count_w2_we]
local low_mean=r(mean)
sum eic_refund if dec==10 & inlist(eic_child,1,2) [w=count_w2_we]
local high_mean=r(mean)
local diff=`high_mean'-`low_mean'
local perc_change=`diff'/`low_mean'*100

local diff=string(`diff',"%12.0f")
local perc_change=string(`perc_change',"%12.1f")
di "Treatment effect: $`diff' (`perc_change'%)"

* Appx Fig 9b: Wage Earner Asymmetry in EITC Refund 
use $datadir/movers/movers_db_bin_averages_$moversdate.dta, clear
rename eic_refund_w2_we_count count
qui reg d_eic_w2_we db_bin if db_bin <= 0 [w=count]
g fit_low = _b[db_bin]*db_bin+_b[_cons] if db_bin <= 0
di "DB < 0 Slope: " round(_b[db_bin],0.1) " (" round(_se[db_bin],0.1) ")"
qui reg d_eic_w2_we db_bin if db_bin >= 0 [w=count]
g fit_high = _b[db_bin]*db_bin+_b[_cons] if db_bin >= 0
di "DB > 0 Slope: " round(_b[db_bin],0.1) " (" round(_se[db_bin],0.1) ")"
qui reg d_eic_w2_we db_bin up treat [w=count], cluster(db_bin)
global pval = round(ttail(e(df_r),_b[treat]/_se[treat])*2,0.0001)
if $pval == 0 {
	di "P-Value for Asymmetry < 0.0001"
	}
else {
	di "P-Value for Asymmetry = " $pval
	}
twoway scatter d_eic_w2_we db_bin || scatter fit_low db_bin if db_bin <= 0, c(l) m(i) lc(maroon) || ///
	scatter fit_high db_bin if db_bin >= 0, c(l) m(i) lc(maroon) xline(0, lpattern(dash) lcolor(gs12)) ///
	graphregion(fcolor(white)) title("Effect of Changes in Neighborhood Sharp Bunching for Wage-Earner Movers" ,size(small)) ///
	yla( ,angle(h)) ytitle("Change in EITC Refund for Wage Earners" ,size(small)) ///
	xtitle("Change in ZIP-3 Sharp Bunching" ,size(small)) legend(off)
graph save $figdir/appx9b, replace
graph export $figdir/appx9b.tif, replace


*** NBER Appendix Figure 4: Phase-In and Phase-Out Schedules
* 4a
use $kiddir/fig_kidapp1_1ststage_kids, clear
gen a=0
gen b=0
gen n=_n
replace a=3 if _n>=2
replace b=8950 if a==3
replace b=35000 if _n>=3

sort b

scatter a b, c(l) yla(0(1)4, angle(ho)) xla(0(5000)35000) xline(8950, lp(dash) lc(black)) ///
	xline(16449, lp(dash) lc(black)) ms(none) title("Simulated Phase-In Credit") ///
	xtitle("Total Income") ytitle("Phase-in Simulated Credit Amount")
graph save $figdir/NBER_appx4a, replace

** Appx Fig 4b
use $kiddir/fig_kidapp1_1ststage_kids, clear
gen a=3
gen b=0
gen n=_n
replace b=16449 if _n==2
replace b=35000 if _n>=3
replace a=0 if b==35000

sort b

scatter a b, c(l) yla(0(1)4, angle(ho)) xla(0(5000)35000) xline(8950, lp(dash) lc(black)) ///
	xline(16449, lp(dash) lc(black)) ms(none) title("Simulated Phase-Out Credit") ///
	xtitle("Total Income") ytitle("Phase-out Simulated Credit Amount") lc(maroon)
graph save $figdir/NBER_appx4b, replace


** Slides Only Figures
* Income Distribution for Single Wage Earners with One Child: "Is the EITC having an effect on this distribution?"
use $datadir/cross_sec_collapse_work_$date.dta if (eic_child_law == 1) & taxprep==. & firmsize==. & married_female==. & tax_yr>=1999, clear
collapse (sum) count_w2_we, by(incbin_exact)
egen b = sum(count)
gen frac = count/b

g number = _n
g d1 = 0 if number == 1
g a1 = 0 if number == 1
replace d1 = 8950	if number ==2
replace a1 = 3050 if number == 2
replace d1 = 16449 if number ==3
replace a1 = 3050 if number ==3
replace d1 = 35534 if number ==4
replace a1 = 0 if number ==4

sort incbin

twoway scatter frac incbin if incbin>0&incbin<=35000, c(l) ylabel(0(.005).035) ///
	|| connected a1 d1 , yaxis(2) ylabel(0(1000)4000, axis(2) angle(h)) lcolor(gs8) msymbol(none) graphregion(fcolor(white)) ///
	xlabel(0(5000)35000) ///
	ytitle("EITC Amount ($)", axis(2)) ///
	title("Income Distribution for Individuals with One Child", size(small)) ///
	xline(8950) xline(16449)
graph save $figdir/inc_dist_we_1kid, replace

use $datadir/cross_sec_collapse_work_$date.dta if (eic_child_law == 2) & taxprep==. & firmsize==. & married_female==. & tax_yr>=1999, clear
g incbin_shifted= incbin +12550
collapse (sum) count_w2_we, by(incbin_shifted)
egen b = sum(count)
gen frac = count/b

g number = _n
g d2 = 0 if number == 1
g a2 = 0 if number == 1
replace d2 = 12550	if number ==2
replace a2 = 5036 if number == 2
replace d2 = 16449 if number ==3
replace a2 = 5036 if number ==3
replace d2 = 40362 if number ==4
replace a2 = 0 if number ==4

sort incbin
twoway scatter frac incbin if incbin>=0&incbin<=35000, c(l) ylabel(0(.01).04) ///
	|| connected a2 d2 , yaxis(2) ylabel(0(2000)6000, axis(2) angle(h)) lcolor(gs8) msymbol(none) graphregion(fcolor(white)) ///
		ytitle("EITC Amount ($)", axis(2)) title("Income Distribution for Individuals with Two Children", size(small)) xline(12550)
graph save $figdir/inc_dist_we_2kid, replace

* Google Figure
use $datadir/cross_sec_corr_work_$date, clear
	replace b_zip3 = b_zip3*100
	replace eitc_density = eitc_density/1000

	*Scale google measure relative to variation in 2008
	sum google_tax [w=count] if tax_yr==2008,d
	g goog_norm = (google-r(mean))/r(sd)

binscatter b_zip3 google_tax, ytitle("Zip3 Bunching") title("Google Searches") ///
	xtitle("Google Search Intensity for 'Tax' in Zip Code (%)") 
graph save $figdir/google, replace

* Texas and Kansas Earnings Distributions
use $datadir/cross_sec_collapse_work_$date.dta  if (zip3>=749 & zip3<800) & taxprep==. & firmsize==. & married_female==., clear
collapse (sum) count, by(incbin)
egen temp = sum(count)
g frac = count/temp
replace frac = frac*100
drop temp
sort incbin

twoway connected frac incbin if incbin<=25500&incbin>=-13000, xline(0) ///
		graphregion(fcolor(white)) legend(off) yla(0(1)5, ang(h)) ///
		title("Earnings Distribution in Texas" ,size(small)) ///
		ytitle("Percent of Filers" ,size(small)) xtitle("Income Relative to 1st Kink" ,size(small))
graph save $figdir/texas_distrn, replace

use $datadir/cross_sec_collapse_work_$date.dta  if (zip3>659 & zip3<680) & taxprep==. & firmsize==. & married_female==., clear
collapse (sum) count, by(incbin)
egen temp = sum(count)
g frac = count/temp
replace frac = frac*100
drop temp
sort incbin

twoway connected frac incbin if incbin<=25500&incbin>=-13000 , xline(0) ///
		graphregion(fcolor(white)) legend(off) yla(0(1)5, ang(h)) ///
		title("Earnings Distribution in Kansas" ,size(small)) ///
		ytitle("Percent of Filers" ,size(small)) xtitle("Income Relative to 1st Kink" ,size(small))
graph save $figdir/kansas_distrn, replace


		 
****************************
********  TABLES  **********
****************************

*** Table 1 (NBER Table 2): Cross-Sectional Correlates of Bunching

use $datadir/cross_sec_corr_work_$date, clear

replace b_zip3=b_zip3*100
replace b_zip3selfemp = b_zip3selfemp*100
replace eitc_density = eitc_density/1000
qui sum goog [w=count] if tax_yr==2008,d
g goog_norm = (google-r(mean))/r(sd)

** AER Version
log using $logdir/table1_cross_sec_corrs_AER.smcl, replace
*AER1. Include tax prep, and density/demogs together.
reg b_zip3 tp_frac eitc_density perc* [w=count] if tax_yr==2000
*Related reg mentioned in text
reg b_zip3 eitc_density [w=count] if tax_yr == 2000

*AER2. Include state fixed effects
areg b_zip3 tp_frac eitc_density perc*  [w=count] if tax_yr==2000, a(state)

*AER3. State EITC has no impact.  Cluster by state since that is the level of variation
reg b_zip3 state_topup tp_frac eitc_density perc* [w=count] if tax_yr==2000, cl(state)
* Check which is referenced from text
reg b_zip3 state_topup tp_frac eitc_density perc* [w=count] if tax_yr==2008, cl(state)

*AER4. Run #1 at the state level, with compliance added as a variable.
g temp = eitc_dens*count
bys state tax_yr: egen temp1 = sum(temp)
by state tax_yr: egen temp2 = sum(count)
g eitc_dens_st = temp1/temp2
drop temp*
g temp = tp_frac *count
bys state tax_yr: egen temp1 = sum(temp)
by state tax_yr: egen temp2 = sum(count)
g tp_st = temp1/temp2
drop temp*
g prod_count = count*count_noclaim
reg b_zip3 eitc_dens_st frac_noclaim tp_frac perc* [w=prod_count] if tax_yr == 2000, cl(state)
log close

** NBER Version
log using $logdir/table2_cross_sec_corrs_NBER.smcl, replace
set more off
*NBER1: R-squared of 60% with just density
reg b_zip3 eitc_density if tax_yr==2000  [w=count]

*NBER2: R-squared of 80% with just two types of variables: density and demographic (racial) background.
reg b_zip3 eitc_density perc* if tax_yr==2000  [w=count]

*NBER3: Frac tax prep
reg b_zip3 tp_frac if tax_yr==2008  [w=count], robust

*NBER4: Google Data.
reg b_zip3 goog_norm if tax_yr==2008 [w=count]

*NBER5: Include google, tax prep, and density/demogs together.
reg b_zip3 tp_frac goog_norm eitc_density perc* [w=count] if tax_yr==2008

*NBER6: Include state fixed effects
areg b_zip3 tp_frac goog_norm eitc_density perc* [w=count] if tax_yr==2008, a(state)

*NBER7: State EITC has no impact. (Cluster by state since no within-state variation)
reg b_zip3 state_topup if tax_yr==2000 [w=count], cluster(state)

*NBER8: No correlation of bunching with NRP tax compliance measure
* (Run at state-level since NRP data at state level)
collapse (mean) b_zip3 frac_noclaim_1k count_noclaim (count) count [fw=count], by(state tax_yr)
g prod_count = count*count_noclaim
reg b_zip3 frac_noclaim_1k if tax_yr==2000 [w=prod_count]

log close


*** Table 2 (NBER Table 3): Impacts of EITC on Wage Earnings: Regression Estimates from Child Birth Design
*Output from kidbig37.lst, regressions run on the inside to avoid small count problems.

*Means in year before birth from outside collapses
*Col 1,3:
use $kiddir/kids_collapse_new_2.dta, clear
egen id = group(zip3_0 birth_yr)
tsset id age
sum eicamtw1 if age == -1 [w=count]

* Col 5
sum eicamtphaseinw1 if age == -1 [w=count]

* Col 6
sum eicamtphaseoutw1 if age == -1 [w=count]

*Col 7:
sum workw if age == -1 [w=count]

*Col 8:
sum w2num if age == -1 [w=count]

*Col 2:
use $kiddir/kids_collapse_new_8.dta, clear
egen id = group(zip3_0 birth_yr)
tsset id age
sum eicamtw1 if age == -1 [w=count]

*Col 4:
use $kiddir/kids_collapse_new_4.dta, clear
egen id = group(zip3_0 birth_yr)
tsset id age
sum eicamtw1 if age == -1 [w=count]

*** Table 3 (NBER Table 4): Elasticity Calculations

* Estimates from Excel worksheet "elasticity_calc" in main Tables file
* Includes a number of mean estimates from child birth sample:
use $kiddir/kids_collapse_new_2.dta, clear

g temp = b_zip3_0 if age==0
xtile dec=temp [w=count], nq(10)
xtile vin=temp [w=count], nq(20)
g top_decile = (dec==10)
g bottom_decile = (dec==1)
drop temp

egen id = group(birth_yr zip3_0)
tsset id age

* Calculate unconditional mean minus bottom decile by regression on bottom decile dummy and multiplying by 0.9
* Overall Effect
	* EITC Refund from Wages
	qui sum d.eicamtw1 if age==0 & dec==1 [w=count]
	di "Mean in Bottom Decile = " round(r(mean),0.1)
	qui sum d.eicamtw1 if age==0 [w=count]
	di "Unconditional Mean = " round(r(mean),0.1)

	qui reg d.eicamtw1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile EITC Refund (Wages): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"
	* EITC Refund from Income
	qui reg d.eicamt1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile EITC Refund (Income): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"

* Phase-In
	* EITC Refund from Wages
	qui reg d.eicamtphaseinw1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Phase-In (Wages): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"
	* EITC Refund from Income
	qui reg d.eicamtphasein1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Phase-In (Income): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"

* Phase-Out
	* EITC Refund from Wages
	qui reg d.eicamtphaseoutw1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Phase-Out (Wages): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"
	* EITC Refund from Income
	qui reg d.eicamtphaseout1 bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Phase-Out (Income): " round(-_b[bottom]*0.9,0.1) " (" round(_se[bottom]*0.9,0.1) ")"

* Extensive Margin
	* Note that we multiply these results by 1075 (average eic refund for wage earners), which comes
	*    from the kids inside log file as the dep var mean in regressions (exact number is 1074.4)
	* EITC Refund from Wages
	qui reg d.workw bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Extensive Margin (Wages): " round(-_b[bottom]*0.9*100,0.1) "% (" round(_se[bottom]*0.9*100,0.1) "%)"
	* EITC Refund from Income
	qui reg d.work bottom_decile if age==0 [w=count], robust
	di "Mean - Bottom Decile Extensive Margin (Income): " round(-_b[bottom]*0.9*100,0.1) "% (" round(_se[bottom]*0.9*100,0.1) "%)"

* Other numbers for Table 3 (z_X, phi_X, and frac SE) were calculated on the inside and can be found in kidbig37.lst:
*	First and second proc means in the output is for people between 0 and middle of the plateau
*		First cuts on earnings in that range, second on wages in that range (only for wage earners)
*	Third and fourth proc means in the output is for people between middle of the plateau and the end of the phase-out
*		Third cuts on earnings in that range, second on wages in that range (only for wage earners)
*	Fifth includes everyone, used to get denominator for % in phase-in and phase-out (multiplied by .88 for wages)
*	Sixth cuts on EITC_elig to get % of EITC-eligible sample is self-employed   
	
*** Table 4 (NBER Table 5): Poverty Rate Under Counterfactual Policies
* Calculate behavioral impact

use $kiddir/fig_kid1_eicdensity_pre.dta, clear
g post = 0
append using $kiddir/fig_kid1_eicdensity_post.dta
replace post = 1 if post == .
replace count = count/100
rename wagesr incbin
bys incbin post: egen mean_frac = mean(count)
g _temp = count if group == 10
bys incbin post: egen high_frac = mean(_temp)
rename count low_frac
keep if group == 1
drop group _temp

* Reshape wide and calculate effects
reshape wide low_frac mean_frac high_frac, i(incbin) j(post)
g EITC_behav_PDF_mean = (mean_frac1 - mean_frac0) - (low_frac1 - low_frac0)
g EITC_behav_PDF_high = (high_frac1 - high_frac0) - (low_frac1 - low_frac0)
sort incbin
g EITC_behav_CDF_mean = sum(EITC_behav_PDF_mean)
g EITC_behav_CDF_high = sum(EITC_behav_PDF_high)
keep incbin EITC*
save $kiddir/EITC_behavioral_effect.dta, replace

** Calculate aggregate distribution
use $datadir/cross_sec_collapse_work_$date.dta if (eic_child_law == 1 | eic_child_law==2) & taxprep==. & married_female!=. & firmsize==. & inrange(tax_yr,2000,2005), clear
drop if married == .
replace incbin = incbin+8950 if eic_child_law ==1
replace incbin = incbin +12550 if eic_child_law ==2
replace incbin = (mod(8950/1000,1)*1000 - 500)/2 if inrange(incbin,-1000,0) & eic_child == 1
replace incbin = (mod(12570/1000,1)*1000 - 500)/2 if inrange(incbin,-1000,0) & eic_child >= 2 
drop if incbin < 0
collapse (sum) count_w2_we (mean) incbin_eic, by(incbin married eic_child tax_yr)
rename count_w2_we count
rename incbin incbin_exact
g incbin = round(incbin_exact/1000)*1000


* Calculate aggregate CDF
sort incbin_exact
egen totcount = sum(count)
g frac = count/totcount
bys incbin: egen agg_pdf = sum(frac)

* Merge on Behavioral Impact (in terms of aggregate PDF) and calculate new distribution
merge incbin using $kiddir/EITC_behavioral_effect.dta, uniqusing
replace EITC_behav_PDF_mean = 0 if _m == 1 
replace EITC_behav_PDF_high = 0 if _m == 1 
drop if _m == 2
drop _m EITC_behav_CDF*
g new_frac_mean = frac - (frac/agg_pdf)*EITC_behav_PDF_mean
g new_frac_high = frac - (frac/agg_pdf)*(EITC_behav_PDF_mean-EITC_behav_PDF_high)

* Calc poverty line
* See Poverty Rates Data Worksheet from http://www.census.gov/hhes/www/poverty/data/threshld/index.html
sort married eic_child tax_yr
merge married eic_child_law tax_yr using $kiddir/povline_data.dta, uniqusing sort
g cpi2010 = 1.0440
replace povline = povline*cpi2010/cpi
drop _m cpi*

* Perform poverty rate calculations
di "Table 4: Impact of EITC on Wage-Earnings Distribution of EITC-Eligible Households"
foreach pov of numlist 0.5 1 1.5 2 {
      cap drop temp
      qui g temp = (incbin_exact < (`pov'*povline))
      qui sum temp [w=new_frac_mean]
      di "Fraction Below " `pov' " X Poverty Rate, No EITC Counterfactual: " round(r(mean)*10000)/100 "%"
 
      cap drop temp
      qui g temp = (incbin_eic < (`pov'*povline))
      qui sum temp [w=new_frac_mean]
      di "Fraction Below " `pov' " X Poverty Rate, With EITC Payments: " round(r(mean)*10000)/100 "%"
 
      qui sum temp [w=frac]
      di "Fraction Below " `pov' " X Poverty Rate, With EITC Behavioral Response (U.S. Avg.): " round(r(mean)*10000)/100 "%"
 
      qui sum temp [w=new_frac_high]
      di "Fraction Below " `pov' " X Poverty Rate, With EITC Behavioral Response (Top Decile): " round(r(mean)*10000)/100 "%"
      drop temp
      di ""
      }

*** Appendix Table 1 (NBER Table 1)

* Summary statistics run on the inside.  
* Output in collapse_sumstats.lst


*** NBER Appendix Table 1

* Run on the inside.  Output in kidbig37.lst

	  
************************
*** NUMBERS IN PAPER ***
************************

*** Examples of low and high information cities by population
use $datadir/cross_sec_collapse_work_$date.dta  if tax_yr == 2008 & (eic_child_law == 1 | eic_child_law == 2) & taxprep==. & firmsize==. & married_female==., clear
sort zip3
merge zip3 using $datadir/correlates.dta, nokeep
drop if nostate==1

* High Bunching (use Chicago, zip3 == 606)
sum b_zip3 if zip3 == 606 & tax_yr == 2008
di "Example of High Bunching City: Chicago,IL (b = " round(r(mean)*100,0.1) "%)"

* Low Bunching (use Rapid City, zip3 == 577)
sum b_zip3 if zip3 == 577 & tax_yr == 2008
di "Example of Low Bunching City: Rapid City, SD (b = " round(r(mean)*100,0.1) "%)"

*** Detailed study of specific areas in Figure 2
use $datadir/IRS_zip3_bunching_saez_$bunchingdate.dta, clear
keep if tax_yr==2008

merge 1:1 zip3 tax_yr using $datadir/wage_earner_counts_zip3_tax_yr.dta
keep if _merge==3
drop _merge
keep if count!=.

merge 1:1 zip3 using $datadir/zip3_state_crosswalk.dta
keep if _merge==3
drop _merge

replace b_zip3 = b_zip3*100

* North and South Dakota
tab b_zip3 if state=="ND"
tab b_zip3 if state=="SD"

* Texas and Florida
set more off
tab b_zip3 if state=="TX"
set more off
tab b_zip3 if state=="FL"

* Rio Grande Valley, TX
sum b_zip3 if zip3==785

* Corpus Christi, TX
sum b_zip3 if zip3==784 

	
*** Growth of Bunching Over the Years 
use $datadir/cross_sec_collapse_work_$date.dta if inlist(eic_chil,1,2) & taxprep==. & married_female==. & firmsize==., clear
tab tax_yr [w=count], sum(b_zip3)

*** Numbers for text discussing Fig 8b
use $kiddir/fig_kid3_scatter_se.dta, clear
gen n=_n
tsset n
gen d_eicamt = (eicamtw1-l.eicamtw1)
keep if age==0
g dec = ceil(n/4)

sum d_eicamt if dec==1 [w=count]
global temp = r(mean)	
sum d_eicamt [w=count]
di "Fig 8b Difference Between Mean and Bottom Decile: " round(r(mean) - ${temp},0.1)
sum d_eicamt if dec==10 [w=count]
di "Fig 8b Difference Between Top and Bottom Decile: " round(r(mean) - ${temp},0.1)

*** Numbers for text discussing Fig 9
use $kiddir/fig_kid4_phasein_scatter_se.dta, clear
gen n=_n
tsset n
gen d_eicamt = (eicamtphaseinw1-l.eicamtphaseinw1)
keep if age==0
g dec = ceil(n/4)

sum d_eicamt if dec==1 [w=count]
global temp = r(mean)	
sum d_eicamt [w=count]
di "Fig 9a Difference Between Mean and Bottom Decile: " round(r(mean) - ${temp},0.1)
sum d_eicamt if dec==10 [w=count]
di "Fig 9a Difference Between Top and Bottom Decile: " round(r(mean) - ${temp},0.1)

*** Mean Fraction of Individuals Working Before/After Birth
use $kiddir/kids_collapse_new_2.dta, clear
tab age if inlist(age,-1,0) [w=count], sum(workw) noobs nof nost

*** Number of W2s per individual in phase-in and phase-out
*  From kidbig37.lst
*  The third and fourth proc means with w2num in it covers people with 0 kids in the pre-period
*    and who are EITC eligible.
*  Estimates of W2 / person use w2num - w2num_spouse
*  Estimates of Wages / W2 take wagesr_own divided by the previous number 
