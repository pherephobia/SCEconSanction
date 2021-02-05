/******************************************************/
Social capital and the success of economic sanctions

Data Collection
Variables - IV and DV
Analysis

Taehee whang
02/09/2016
/******************************************************/
**********
** Data **
**********
* = sanctions + sc + KimYW_NGO data (mclean2 cannot be used because it ends in 2000.

/* Merge SC data to the sanctions data
1. Concatenate variables to make an id variable. 
2. It will generage a string variable.
3. Change from string to numeric using destring option.
4. Format and sort the numeric variable.
5. merge two data using the numeric id variable.
*/

* sanctions + sc

* open baseline data
use "C:\Users\TW\Dropbox\KimH_prj\data\sanctions.dta", clear
* merge this one to the baseline data
sort identifier /*need to sort both data*/
merge identifier using "C:\Users\TW\Dropbox\KimH_prj\data\sc.dta" 

* (sanctions + sc) + KimYW_NGO data

* Kim_YW_NGO prj: RealMain3_20150509.dta
use "C:\Users\TW\Dropbox\KimH_prj\data\RealMain3_20150509.dta", clear
egen idyearA = concat(year target)
destring  idyearA, generate(idyearA_destring) 
format  idyearA_destring %12.0g
sort  idyearA_destring
save "C:\Users\TW\Dropbox\KimH_prj\data\RealMain3_20150509.dta", replace

* main data
use "C:\Users\TW\Dropbox\KimH_prj\data\sanctions_sc_KimYW_merge.dta", clear
egen idyearA = concat(startyear targetstate)
drop if idyearA == ".."
destring  idyearA, generate(idyearA_destring) 
format  idyearA_destring %12.0g
sort  idyearA_destring
save "C:\Users\TW\Dropbox\KimH_prj\data\sanctions_sc_KimYW_merge.dta", replace

* open baseline data
use "C:\Users\TW\Dropbox\KimH_prj\data\sanctions_sc_KimYW_merge.dta", clear
* merge this one to the baseline data
merge idyearA_destring using "C:\Users\TW\Dropbox\KimH_prj\data\RealMain3_20150509.dta" 


***************
** Variables **
***************

** DV = finaloutcome 

gen sanctionoutcome = 0
replace sanctionoutcome = 1 if finaloutcome == 6 | finaloutcome == 7 | finaloutcome == 10
replace sanctionoutcome = . if finaloutcome == .
label var sanctionoutcome "= 1 if finaloutcome == 6 | finaloutcome == 7 | finaloutcome == 10"

gen sanctionoutcome2 = 0
replace sanctionoutcome2 = 1 if finaloutcome == 6 | finaloutcome == 7
replace sanctionoutcome2 = . if finaloutcome == .
label var sanctionoutcome2 "= 1 if finaloutcome == 6 | finaloutcome == 7;  Negotiated Settlement following sanctions imposition removed"

*Final outcome의 10가지 기준 중, 1 - 3번의 기준을 6, 7, 10번과 같이 성공으로 코딩했을 경우에 독립변수와 종속변수 간의 상관관계는 변하지 
*않았다. 또한 4, 5번까지 성공으로 코딩했을 경우 기존의 6, 7, 10번만 성공으로 코딩했을 때와 비교하여 독립변수와 종속변수 간의 상관관계가 0.02 낮게 도출되었다. 
*또한, 1 - 5번을 성공으로 코딩하고, 6 - 10을 실패로 코딩했을 경우, 데이터는 845개 중 4개에 불과하다.

** IV

global membership1 m_religious1 m_educartsmusiccul1 m_laborunion1 m_polparty1 m_humanrights1 m_conservenvecol1 m_profassociation1 m_sportsrec1
global membership2 m_religious2 m_educartsmusiccul2 m_laborunion2 m_polparty2 m_humanrights2 m_conservenvecol2 m_profassociation2 m_sportsrec2
global confidence1 c_churches1 c_armedforces1 c_justicelegalsyscourts1 c_press1 c_laborunion1 c_police1 c_parliament1 c_civilservice1 c_polparty1 c_government1
global confidence2 c_churches2 c_armedforces2 c_justicelegalsyscourts2 c_press2 c_laborunion2 c_police2 c_parliament2 c_civilservice2 c_polparty2 c_government2

gen ussanctions = 0
replace ussanctions = 1 if  sender1==2 | sender2==2 | sender3==2 | sender4==2 | sender5==2 | primarysender==2
replace ussanctions = . if finaloutcome == .
label var ussanctions "=1 if sender1==2 | sender2==2 | sender3==2 | sender4==2 | sender5==2 | primarysender==2"

**************
** Analysis **
**************

set more off

* Descriptive statistics: All observations 
sum sanctionoutcome trust ///
m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem

corr sanctionoutcome trust ///
m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem

* Descriptive statistics: Observations in our results 
sum sanctionoutcome trust ///
m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem ///
if sanctionoutcome != . & trust != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

corr sanctionoutcome trust ///
m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem ///
if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

* IV
sum $membership1 if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .
sum $membership2 if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

* DV
tab finaloutcome if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .
tab sanctionoutcome if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .


***********
** Trust **
***********

*probit sanctionoutcome trust salience_dummy2 alliance2 targetdem , robust /*political model*/
*probit sanctionoutcome trust contig distance lntarget_gdppc_gle , robust /*economic model*/
probit sanctionoutcome trust contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem , robust /*full model*/
est store reg1
* possible regressor: targettradeopen

margins, at(trust=(4.9(3)59.4)) 


****************
** Membership **
****************

probit sanctionoutcome m_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg2

margins, at(m_polparty1=(0(3)45.5)) 

probit sanctionoutcome m_profassociation1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg3

margins, at(m_profassociation1=(0(1.5)25.7))

*probit sanctionoutcome m_humanrights1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome m_religious1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust


esttab reg1 reg2 reg3 using "C:\Users\user\Dropbox\KimH_prj\data\turst_membership", ///
replace modelwidth(10) addnote ("Probit Models Using Robust Standard Error. DV = Sanction Outcome.")  ///
title("Effect of Social Capital (Trust and Membership) on the Success of Econoimc Sanctions") starlevels (* 0.10 ** 0.05)  ///
numbers("Model ")  mlabels("Trust" "Membership: Political Party" "Membership: Prof. Association")  ///
coeflabels(Trust "Trust" m_profassociation1 "Membership: Prof. Association" m_polparty1 "Membership: Political Party" ///
contig "Contiguity" distance "Distance" lntarget_gdppc_gle "Target ln(GDPPC)" /// 
salience_dummy2 "Issue Salience" alliance2 "Alliance" targetdem "Target Democracy" _constant "Constant")  ///
legend cells(b(star fmt(2)) se(par)) stats(N pr2, labels  ("Observations" "Pseudo R-squared")) label varwidth(30) compress 


****************
** Confidence **
****************

probit sanctionoutcome c_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg4

margins, at(c_polparty1=(0.9(3)31.1))

probit sanctionoutcome c_government1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg5

margins, at(c_government1=(1.9(3)54.5))

probit sanctionoutcome c_parliament1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg6

margins, at(c_parliament1=(0.9(3)46.1))

probit sanctionoutcome c_justicelegalsyscourts1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg7

margins, at(c_justicelegalsyscourts1=(6.7(2)37.3))

*probit sanctionoutcome c_armedforces1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome c_churches1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome c_police1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust


esttab reg4 reg5 reg6 reg7 using "C:\Users\user\Dropbox\KimH_prj\data\confidence", ///
replace modelwidth(10) addnote ("Probit Models Using Robust Standard Error. DV = Sanction Outcome.")  ///
title("Effect of Social Capital (Confidence) on the Success of Econoimc Sanctions") starlevels (* 0.10 ** 0.05)  ///
numbers("Model ")  mlabels("Confidence: Political Party" "Confidence: Government" "Confidence: Parliament" "Confidence: Courts")  ///
coeflabels(c_polparty1 "Confidence in Political Party" c_government1 "Confidence in Government" c_parliament1 "Confidence in Parliament" c_justicelegalsyscourts1 "Confidence in Courts" ///
contig "Contiguity" distance "Distance" lntarget_gdppc_gle "Target ln(GDPPC)" /// 
salience_dummy2 "Issue Salience" alliance2 "Alliance" targetdem "Target Democracy" _constant "Constant")  ///
legend cells(b(star fmt(2)) se(par)) stats(N pr2, labels  ("Observations" "Pseudo R-squared")) label varwidth(30) compress 


** END

