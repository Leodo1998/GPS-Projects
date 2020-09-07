*Leo Do
* --------------------------------------------------------
* QM2 IAP in STATA
* --------------------------------------------------------
clear
eststo clear
capture log close
set more off

*cd "/Users/Leodo/Desktop/QM III/Homework"


*Alternatively, imagine that an international organization wants to understand the best way to enhance firm development in certain African countries. They have contracted you to examine the factors that affect this outcome in the manufacturing sector, so that the organization can identify relevant policy levers and ultimately have a meaningful impact.
use hcbase.dta, replace
save myhcbase.dta, replace


log using "IAP_Project.smcl",replace


*Leo theory 101

*Factors to enhance firm performance?
*	Human based capital?
		//aka education and experience?


* ---------------- Get to Know Data ----------------
* What is the unit of observation?

*The individual Firms 

*log of value added vs value added? makes more sense on the margins


*how many countries 

codebook country
* Ghana Cameroon Ghana Kenya Zambia Zimbabwe

* Number of firms
//743 unique firms

*initial thoughts and theory that education always has good returns for development


su ledutot

cor lvadppp ledutot


*-------------------Graphic creation for Initial Motivation of the study: Figure 1

tw (lfitci lvadppp ledutot) (scatter lvadppp ledutot, ylabel(,angle(horizontal)) ylabel(,format(%9.2fc))   graphregion(color(white)) ytitle("Value Added per Employee") xtitle(Log of Total Years of Education) legend(off) title("Logged Total Education vs Value Added Per Employee") subtitle(Value Added in terms of US$ PPP) caption(Increasing Education in a firm should result in higher economic returns in manufacturing, size(vsmall)))

graph export "Figure 1.pdf",replace

*Checking to see if people are entering higher education for these returns

 mean ledutot
	//low percentage of firms that have university graduates
		//why?
			//Returns to education are clearly evident why are more people not entering higher education for skilled labor returns?


*-----------Inital Regression tests prior to checking heteroskedasticity and colinearness----------


*SLR model

reg lvadppp ledutot
outreg2 using "test.doc", replace label ctitle(simple model) 

*experience having an effect?
reg lvadppp ledutot ltentot
outreg2 using "test.doc", append label ctitle(Working as opportunity cost Addressed)

*interaction between experience and intelligence?
reg lvadppp c.ledutot##c.ltentot
outreg2 using "test.doc", append label ctitle(Interaction)

*country Difference?
	//a country's infrastrature at the time 
reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi
outreg2 using "test.doc", append label ctitle(Country Differences)

*checking if sector makes a difference to see if sectors are atypical in education bias
reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi wood1 textile1 metal1
outreg2 using "test.doc", append label ctitle(Sector Differences)



*----------------------------------------Collineraty Tests---------------------------------------------*
*testing VIF on last model
reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi wood1 textile1 metal1
vif
	//vif 6.17 = good since im using 10 as threshold


*------------------------------------Outlier test----------------------------------------------------------*
reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi wood1 textile1 metal1

* Potential Outliers
predict yhat, xb

predict rstud, rstudent
predict lev, leverage
predict cooksd, cooksd
predict dfits, dfit

* use stored values so you never have to change this!
ereturn list
scalar kk=e(df_m)
scalar nn=e(N)

di (2*kk+2)/nn		/* leverage */
di 4/nn				/* Cook's Distance */
di 2*sqrt(kk/nn)		/* DFFIT */

gen absrstud=abs(rstud)
label var absrstud "Absolute Value of Studentized Residuals"
gen id=_n

*-------------------------------Creates Figure 2: Outliers Graph

tw (scatter lev absrstud if (rstud!=. & lev!=.)) (scatter lev absrstud if (abs(rstud)>2 & lev>(2*kk+2)/nn), xli(2) yli(`=(2*kk+2)/nn') mlabel(country) ylabel(, angle(horizontal)) legend(order(1 "Any Outlier" 2 "Egregious Outlier")) graphregion(color(white))title(Leverage vs Residuals)caption(only 3 firms were found to be egregious outliers)) 
graph export "Figure2.pdf", replace 


* leverage v. residuals plot
li country if (abs(rstud)>2 & lev>(2*k+2)/nn) & (rstud!=. & lev!=.)

count if abs(rstud)>2 & rstud!=.				
count if lev>(2*kk+2)/nn & lev!=.	
count if cooksd>4/nn & cooksd!=.	
count if abs(dfits)>2*sqrt(kk/nn) & dfits!=.

* Critical values (arrange in nice table in writeup).

count if (abs(rstud)>2 & lev>(2*kk+2)/nn) & (rstud!=. & lev!=.)
count if (abs(rstud)>2 & lev>(2*kk+2)/nn & cooksd>4/nn) & (rstud!=. & lev!=. & cooksd!=.)
count if (abs(rstud)>2 & lev>(2*kk+2)/nn & abs(dfits)>2*sqrt(kk/nn)) & (rstud!=. & lev!=. & dfits!=.)
count if (abs(rstud)>2 & lev>(2*kk+2)/nn & cooksd>4/nn & abs(dfits)>2*sqrt(kk/nn)) & (rstud!=. & lev!=. & cooksd!=. & dfits!=.)
* Should explore sensitivity of results to excluding some of these potential outliers
* Reassuring that no repeat offenders here



*-------------------------------Heteroeskadasticity testing------------------------------------------------*

reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi wood1 textile1 metal1

estat hettest 

//"A low chi-square (and correspondingly high p-value) imply that the error variance is not heteroskedastic enough to cause problems with your test statistics." However, we found a very low p-value and high chi-square -- evidence of heteroskedasticity. (Source:Reed College)

// rejecting the null of which homoskedasticity exists
	// chi2(1)= 5.12 Prob > chi2  = 0.0237

	
	// what is wanted for homoskedasticity would be a low chisquare value and a high p-value
	
estat imtest, white //"low chi-square and a high p-value indicate nonworrisome heteroskedasticity." would be the goal (Source: Reed College)
	// chi2= 156 Prob > chi2  = 0.00000

		//heteroskedasticity exists
		

*viual heteroskedastic check		
rvfplot, mlabel(n) yline(0)

	//x axis is y values predicted
	
graph save rvfplot.gph, replace

	*want it to be centered around 0 and fanning out is not as dramatic
	
* Interesting to see that some of the big outliers are big offenders in rvpplots. *


*----------------------------------------------Generate Table 1---------------------------------------------
*------------------------- Main Reg Model Tests after Heterofix using robust--------------------------------*


*SLR model

reg lvadppp ledutot, robust
outreg2 using "Figure2.doc", replace label addnote(Returns to education are deemed to be highly significant in the simple model and to a similar degree at the final progression of the model. The additional effects that Kenyan has when interacted lowers the effects of education in comparison to Cameroon hence aid should be focused on them to improve education and skilled labor within the metal manufacturing sector.)

*experience having an effect?
reg lvadppp ledutot ltentot, robust
outreg2 using "Figure2.doc", append label 

*interaction between experience and intelligence?
reg lvadppp c.ledutot##c.ltentot, robust
outreg2 using "Figure2.doc", append label 

*country Difference?
	//a country's infrastrature at the time 
 reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi, robust
outreg2 using "Figure2.doc", append label  

*checking if sector makes a difference to see if sectors are atypical in education bias
reg lvadppp c.ledutot##c.ltentot ghana kenya zimba zambi wood1 textile1 metal1, robust
outreg2 using "Figure2.doc", append label  

reg lvadppp c.ledutot##c.ltentot  c.ledutot##i.ghana c.ledutot##i.kenya c.ledutot##i.zimba c.ledutot##i.zambi wood1 textile1 metal1, robust
outreg2 using "Figure2.doc", append label  



encode(country),gen(Country)

*--------------------------Creating Figure 3 Education By Country"
margins, at(ledutot=(0(.5)1)) by(Country)
marginsplot, ytitle("Predicted Log Value Added per employee")  						///
	xtitle("Log of Total Years of Education in Firm")	title("Education Interaction by Country") caption(Only Kenya and Cameroon are deemed to be significant)											///
	legend(bplace(n) ring(6) col(9) size(vsmall) symxsize(1))				///
	graphregion(color(white))
graph export "Figure 3.pdf",replace



* --------------------------------------------------------
* clean up
* --------------------------------------------------------
save myhcbase.dta, replace
clear
capture log close
