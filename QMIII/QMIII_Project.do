*Leo Do
* --------------------------------------------------------
* QM3 Project!
* --------------------------------------------------------
clear
eststo clear
capture log close
set more off

global dir "/Users/Leodo/Desktop/QM III/Final Data Exercise 2020" 
cd "/Users/Leodo/Desktop/QM III/Final Data Exercise 2020"

log using "$dir/Posterlog.smcl", replace

use "$dir/traffic_fat.dta"

encode name, gen(state2)
label var state2 "State"
*finding first year of implementation
/*
li sbprim name if year==1995
*/
*--------------------------------------------------------Finding order ------------------------------------------
preserve
drop if intlck1 ==0
sort year
sort name year
collapse (max) intlck1 (min)year, by(name)
egen Order = rank(year) ,track
order name year Order, first
sort year
restore

*-----------------------------------------------Order of Policy Implementation-------------------------------------------------
gen Order=.
order state2 year Order, first
replace Order=1 if name=="Iowa"
replace Order=2 if name=="New Mexico"
replace Order=3 if name=="Louisiana"
replace Order=4 if name=="Oregon"|name=="West Virginia"|name=="Arizona"
replace Order=5 if name=="Alaska"|name=="Washington"|name=="Arkansas"|name=="Utah"|name=="Colorado"|name=="Nebraska"|name=="Illinois"
replace Order=6 if name=="Kansas"|name=="New York"|name=="Michigan"|name=="Hawaii"
replace Order=7 if name=="Virginia"|name=="Connecticut"
order name year totfat,first

*------------------------------------------------Exogeneity of Sequencing Check--------------------------------------------
xtset state2 year

gen Fatality = (totfat/totpop)*100000
label var Fatality "Traffic Fatality Rates"

bysort  name (year): g pregrowth_rate=(Fatality [_n]-Fatality[_n-1])/Fatality[_n-1] if _n!=1 & Fatality!=. & year<1995
egen preavg= mean(pregrowth_rate), by(state2)

bysort  name (year): g postgrowth_rate=(Fatality [_n]-Fatality[_n-1])/Fatality[_n-1] if _n!=1 & Fatality!=. & year>=1995
egen postavg= mean(postgrowth_rate), by(state2)

*avg death growths before first implementation

tw lfitci preavg Order || (scatter preavg Order,mlabel(state) msize(small) caption("Iowa, New Mexico, Louisiana, Arizona, Oregon, West Virginia, Washington," "Nebraska, Colorado, Alaska, Illinois, Arkansas, Utah, Hawaii, Michigan, Kansas," "New York, Virginia, Connecticut enacted",size(small)) ytitle("Average Fatality Growth" "Rates pre-1995",orientation(horizontal) size(vsmall)) mcolor(maroon) legend(  label(1 "95% Confidence Interval") label(2 "Fitted Values") label(3 "States")) mlabsize(small) xlabel(1(1)7) title(Evaluating Exogeneity of Sequencing) subtitle("Is there a correlation between pre-treatment fatality rates and the order of policy adoption?",size(small))) 
*looks pretty good since it is flat
graph export "$dir/ExogenitySequence.pdf", replace

*------------------------------------------Exogenity of Timing Check--------------------------------------------------------
gen treatyear = year if intlck1 > 0 & intlck1!=.
bysort name: egen mintreatyear = min(treatyear) //first treatment year for each ZCTA considered only
gen tau = year - mintreatyear //tau is our lead lag indicator (defined only for treatment groups)
order Order name tau, first
sort name year

preserve
keep if tau>=-6 & tau<=6
*Baseline Creation
char tau[omit] -1 //omit -1st lag when we create dummies for tau so that it is the baseline constant to be compared ie. with this treatment this is the effect of the policy relative to right before implementation of the policy moving forward in time
xi i.tau //create dummies for each value of tau

xtreg Fatality intlck1 i.year _Itau*, fe i(state2) cluster(state2)
coefplot, vertical yline(0) xline(5, lpattern(dash)lcolor(orange)) keep(_Itau*) ///
		 levels(95) legend(order(1 "95% CI" 2 "Point estimate")) ciopts(recast(rcap) lcolor(blue%80)) mcolor(blue)	///
		 graphregion(color(white)) xlabel(0 "-6" 2 "-4" 4 "-2" 6 "0" 8"2" 10 "4" 12 "6") subtitle("Lead-and-Lag Regression Estimates w/Robust Standard Errors",size(small)) title("Evaluating Exogeneity of the Timing of Policy Adoption" ) xtitle("Year Before or After Policy Begins") ytitle("Beta Coefficient Estimates")  relocate(_Itau_5=0) 
	 *random significance with 2 years before policy implementation effects of the Ignition interlock mandatory, first DUI offense
	 

restore

*Full Model Building
corr intlck1 intlck2 intlck15
*fuck intlck2
eststo m1: xtreg Fatality intlck1 i.year, fe cluster(state2) robust 
outreg2 using "$dir/DoTest.doc", replace keep(intlck1) label
*simple 2way FE
eststo m2: xtreg Fatality intlck1 i.year bac08 pbt admin intlck15 intlck2 mlda21 avgage unemprte pcinc,fe cluster(state2) robust 
* other alcohol controls and people demographics(age unemployment rate)
outreg2 using "$dir/DoTest.doc", append keep(intlck1) label
eststo m3: xtreg Fatality intlck1 i.year bac08 pbt admin intlck15 intlck2 mlda21 avgage unemprte pcinc vcrimerte pcrimerte rurdense urbdense,fe cluster(state2) robust 
*Urban vs Rural (roads and crime propensities)
outreg2 using "$dir/DoTest.doc", append keep(intlck1) label
eststo m4: xtreg Fatality intlck1 i.year bac08 pbt admin intlck15 intlck2 mlda21 avgage unemprte pcinc vcrimerte pcrimerte rurdense urbdense sbprim sbsec txt hha hhlearn lim70,fe cluster(state2) robust
outreg2 using "$dir/DoTest.doc", append keep(intlck1) label
*Stopping reckless driving and stupid habits when in car (speed limits, seatbelts, License revocation)

*---------------------First Difference
xtset state2 year

gen ydif=Fatality-Fatality[_n-1] if name==name[_n-1]

order name Fatality ydif, first
foreach x in Fatality intlck1 {
	gen d`x' = D.`x' if name==name[_n-1]
	}
	*other way to get first differences of Fatality Rates
*gen DeathFirstDif=D.Fatality
*order name Fatality DeathFirstDif, first
label var dFatality "First Diffs: Traffic Fatality Rates"

order name year dFatality dintlck1,first
replace dintlck1 = 0 if dintlck1 ==.

eststo m5: reg dFatality intlck1 i.year bac08 pbt admin intlck15 intlck2 mlda21 avgage unemprte pcinc vcrimerte pcrimerte rurdense urbdense sbprim sbsec txt hha hhlearn lim70,cluster(state2) robust
outreg2 using "$dir/DoTest.doc", append keep(intlck1 dintlck1) label

esttab m1 m2 m3 m4, se ar2 nomtitles label keep(intlck1)


*predictions although not significant 
su tau
tab tau
*Event Study
preserve
keep if tau>=-6 & tau<=4
char tau[omit] -1 
xtreg Fatality intlck1 i.year bac08 pbt admin intlck15 intlck2 mlda21 avgage unemprte pcinc rurdense urbdense sbprim sbsec txt hha hhlearn lim70,fe cluster(state2) robust
predict yhat
collapse (mean) yhat Fatality, by(tau)	
scatter yhat tau || line yhat tau, ytitle("Predicted Fatality Rates",size(small)) xtitle("Years Before and After Policy Implementation") title(Prediction of Fatality Rates Before & After Implementation of Interlock Policy,size(Medium)) subtitle("Fatality Rates per 100,000 People",size(mediu ))  legend(off) xlabel(-6(1)4) xsc(r(-6 4))
graph export "$dir/Predicted FatlityRates.pdf",replace
*the prediction of fatality rates appear to already be downward trending showing that the treatment mean nothing in terms of its diff in diff effects.
restore


*Final Model Table Build
*Basically a Diff in Diff analysis since treatment is dummy

label var dintlck1 "First Diff. Mandatory Ignition Lock Law"
label var intlck1 "Mandatory Ignition Lock Law"
eststo m7: xtreg Fatality intlck1 i.year, fe i(state2) cluster(state2) robust 
outreg2 using "$dir/DoMain2.doc", replace keep(intlck1) label addtext(State FE, Yes, Year FE,Yes, Contr. Other Alcoholic policies,No,Demographic Controls,No, Contr. Urban/Rural, No, Contr. Speeding/Distracted Driving&Seat Belt Laws, No) title("Regressions of Traffic Fatality Rates per 100,000 Persons in State Populations on the Implementation of Mandatory Ignition Interlock Systems after First Driving Under The Influenced Offense") 
*simple 2way FE

eststo m8: xtreg Fatality intlck1 i.year intlck15 bac08 pbt admin mlda21 avgage unemprte pcinc,fe i(state2) cluster(state2) robust 
outreg2 using "$dir/DoMain2.doc", append keep(intlck1) label addtext(State FE, Yes, Year FE,Yes, Contr. Other Alcoholic policies,Yes,Demographic Controls,Yes, Contr. Urban/Rural, No, Contr. Speeding/Distracted Driving&Seat Belt Laws, No)
* other alcohol controls and people demographics(age unemployment rate)

eststo m9: xtreg Fatality intlck1 i.year intlck15 bac08 pbt admin  mlda21 avgage unemprte pcinc rurdense urbdense,fe i(state2) cluster(state2) robust 
outreg2 using "$dir/DoMain2.doc", append keep(intlck1) label addtext(State FE, Yes, Year FE,Yes, Contr. Other Alcoholic policies,Yes,Demographic Controls,Yes, Contr. Urban/Rural, Yes, Contr. Speeding/Distracted Driving&Seat Belt Laws, No)
*Urban vs Rural (roads and crime propensities)

eststo m10: xtreg Fatality intlck1 i.year intlck15 bac08 pbt admin  mlda21 avgage unemprte pcinc rurdense urbdense sbprim sbsec txt hha hhlearn lim70,fe i(state2) cluster(state2) robust
outreg2 using "$dir/Domain2.doc",append keep(intlck1) label addtext(State FE, Yes, Year FE, Yes, Contr. Other Alcoholic policies, Yes,Demographic Controls,Yes, Contr. Urban/Rural, Yes, Contr. Speeding/Distracted Driving&Seat Belt Laws, Yes)
*Stopping reckless driving and stupid habits when in car (speed limits, seatbelts, License revocation)

*eststo m11: reg dFatality dintlck1 i.year intlck15 bac08 pbt admin mlda21 avgage unemprte pcinc  rurdense urbdense sbprim sbsec txt hha hhlearn lim70, cluster(state2) robust
*outreg2 using "$dir/DoMain2.doc", append keep(intlck1) label addtext(State FE, No, Year FE,Yes, Contr. Other Alcoholic policies,Yes,Demographic Controls,Yes, Contr. Urban/Rural, Yes, Contr. Speeding/Distracted Driving&Seat Belt Laws, Yes)
*How much outcome slope changes after treatment begins first difference of Y

esttab m7 m8 m9 m10 ,se ar2 nomtitles label keep(intlck1) constant lines compress title("Regressions of Traffic Fatality Rates per 100,000 Persons in State Populations on the Implementation of Mandatory Ignition Interlock Systems after First Driving Under The Influenced Offense")

 *vcrimerte pcrimerte Contr. Violent&Property Crime Rates,No,
 
*graph creation for background
*tw lfitci preavg Order ||lfitci postavg Order||scatter postavg Order,mlabel(state) msize(small) mlabsize(tiny) || (scatter preavg Order,mlabel(state) msize(small)  mcolor(maroon) legend(  label(1 "95% Confidence Interval") label(2 "Fitted Values Pre1995") label(3 "Fitted Values Post 1995") label(4 "Post-State") label(5 "Pre-State")) mlabsize(tiny) xlabel(1(1)7) title(Evaluating Exogeneity of Sequencing) subtitle("Is there a correlation between pre-treatment fatality rates and the order of policy adoption?",size(vsmall))) 

*eststo m12 : xtreg Fatality intlck1 i.year intlck15 bac08 pbt admin mlda21 avgage unemprte pcinc  rurdense urbdense sbprim sbsec txt hha hhlearn lim70,fe i(state2) cluster(state2) robust
 
*esttab m7 m8 m9 m10 m12,se ar2 nomtitles label keep(intlck1) constant lines compress title("Regressions of Traffic Fatality Rates per 100,000 Persons in State Populations on the Implementation of Mandatory Ignition Interlock Systems after First Driving Under The Influenced Offense")


*tw lfitci Fatality year || (scatter Fatlity year,mlabel(state) msize(small) caption("Iowa, New Mexico, Louisiana ,Arizona, Oregon, West Virginia, Washington, Nebraska, Colorado, Alaska, Illinois, Arkansas, Utah, Hawaii, Michigan, Kansas, New York, Virginia, Connecticut enacted",size(tiny)) mcolor(maroon) legend(  label(1 "95% Confidence Interval") label(2 "Fitted Values") label(3 "States")) mlabsize(tiny)  title(Evaluating Exogeneity of Sequencing) subtitle("Is there a correlation between pre-treatment fatality rates and the order of policy adoption?",size(vsmall))) 


* --------------------------------------------------------
* clean up
* --------------------------------------------------------
clear
capture log close
