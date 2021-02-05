
*******************************
*** Main Dataset
*******************************
*** Import
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\merge_data.dta" 
*** order necessary variables
order idyearA idyearA_destring sanctionoutcome trust m_polparty1 m_profassociation1 m_humanrights1 m_religious1 c_government1 c_parliament1 /*
	*/ c_polparty1  c_justicelegalsyscourts1  c_armedforces1 c_churches1  c_police1 contig  distance  lntarget_gdppc_gle  /*
	*/ salience_dummy2  alliance2  targetdem, after(identifier)
sort idyearA_destring
save, replace

*******************************
*** V-dem Dataset
*******************************
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\Country_Year_V-Dem_Core_STATA_v10\V-Dem-CY-Core-v10.dta"
keep COWcode year v2x_polyarchy v2x_libdem v2x_partipdem v2x_delibdem /*
	*/ v2x_egaldem v2x_freexp_altinf v2x_suffr v2x_civlib /*
	*/ v2csreprss v2csprtcpt v2mecenefm v2mecenefi v2meslfcen /*
	*/ v2xnp_client v2x_corr
drop if COWcode==.
egen idyearA = concat(year COWcode)
destring idyearA, gen(idyearA_destring)
sort idyearA_destring
save, replace
	
*******************************
*** Main Dataset + V-dem
*******************************
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\merge_data.dta" 
sort idyearA_destring
rename _merge _merge_old2
merge idyearA_destring using "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\Country_Year_V-Dem_Core_STATA_v10\V-Dem-CY-Core-v10.dta"
	
keep identifier idyearA idyearA_destring caseid startmonth startday startyear wave endmonth endday endyear /*
	*/ sanctionoutcome trust m_polparty1 m_profassociation1 m_humanrights1 /* 
	*/ m_religious1 c_government1 c_parliament1 c_polparty1 /*
	*/ c_justicelegalsyscourts1  c_armedforces1 c_churches1  c_police1 contig /*
	*/ distance  lntarget_gdppc_gle  /*
	*/ salience_dummy2  alliance2  targetdem /*
	*/ v2x_polyarchy v2x_libdem v2x_partipdem v2x_delibdem v2x_egaldem  /*
	*/ v2x_freexp_altinf v2x_suffr v2csreprss v2csprtcpt v2mecenefm v2mecenefi /* 
	*/ v2meslfcen v2xnp_client v2x_civlib v2x_corr
*** V-dem에서 가져온 변수들: 민주주의 지표(5개; 선거 민주주의, 자유민주주의, 참여민주주의, 심의민주주의, 평등민주주의)
*** 표현의 자유와 대안정보에 대한 접근성 지수, 인구 대비 선거권 보유자 비율, 시민단체 억압도, 시민단체 참여 환경 등
*** V-dem 변수들은 stata에 변수 Label로 설명이 달려 있습니다.
*** 그리고 이전 Rcode.R에서 살펴본 분석을 바탕으로 실제 분석에 사용했던 변수들 + 합친 V-dem 변수들 + 
*** 기본 식별변수(제재 시작 연도, 국가코드 등)만 남김.
sort idyearA_destring
save, replace

*******************************************
*** Merged Dataset + Quality of Government
*******************************	***********
*** import QoG
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\qog_std_ts_jan20.dta"
drop if ccodecow ==.
egen idyearA = concat(year ccodecow)
destring idyearA, gen(idyearA_destring)
keep idyearA idyearA_destring ccodecow year wdi_internet gle_trade wdi_trade
save "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\qog_sub.dta"
clear 
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\qog_sub.dta"
sort idyearA_destring
save, replace

*** merge
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\merge_data.dta" 
sort idyearA_destring
merge idyearA_destring using "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\qog_sub.dta"
save, replace
exit, clear


******************************
*** Targeted Sanction Data 
******************************
*** 일단 R로 불러와서 stata로 만듦.
***** 알카에다, 탈리반과 같은 비국가단체는 제외되었음.
*** spsstostata.R 파일 참조.
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\tsc.dta"
sort idyearA_destring
save, replace

*******************************************
*** Merged Dataset + TSC data
*******************************	***********
clear
use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\merge_data.dta" 
rename _merge _merge_old3
sort idyearA_destring
merge idyearA_destring using "C:\Users\phere\Dropbox\Scholar\2_Graduates\2020_02_Summer\Hur_data\tsc.dta"

save, replace


	
	
	
	
	
	
	
	
	
