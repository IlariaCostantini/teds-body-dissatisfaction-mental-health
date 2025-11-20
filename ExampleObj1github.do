*Author: Ilaria Costantini
*Project: Association between body dissatisfaction and EDs, MFQ, and BMI
*Analysis: Longitudinal association between body dissatisfaction and ED symptoms in imputed dataset 
/*Description: Example of script for primary analysis (imputed dataset) in Objective 1 for eating disorder symptoms.
This script includes the imputation model as well as the mixed-effect analysis across levels of confounders
*/

*==============================================================================*
*                IMPUTE DATA AND USE AS PRIMARY ANALYSES                       *
*==============================================================================*

use "<local_path>/data/", clear

*keep a subset of variables for imputation // add difference in scores? or create after imputing?? 
keep twinid pairid random age16 age21 age26 ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 bodydissatisf16 edscale21_full edscale21_specific MFQ1c21 MFQc26 BMI21 BMI26 sex astress amumagetw adadagetw amumage adadage mated pated dlifeev EPDSm4 PRS_AN_std PRS_SCHZ_std chaostotc_std12 parent_disciptotc12 parent_feelc12 MZ zygos birthorder random sex zygos sexzyg x3zygos  PRS_ADHD_std PRS_ASD_std PRS_BipD_std PRS_crossDis_std PRS_DEP_std PRS_NEU_std totalpuberty16 totalpuberty14 u1ceats11_rc zBMI16 zBMI12 zBMI7 zBMI4 zBMI3 MZ depPHQ26 bingeeating26 bulimianerv26 anorexianerv26 BMIlowest26 zmhagoradiag zmhpanicdiag zmhsocphdiag zmhspephdiag Depatypical26 MDDc26 bodydissatisf26 mhqbingeeating26 mhqpsychoverating26 mhqbulimia26 mhqanorexia26 MFQcovid4c21 MFQcovid3c21 MFQcovid2c21 MFQcovid1c21 MFQ2c21 bingeeating21 bulimianerv21 anorexianerv21 bodypreocctot21 bingeatingtot21 eatingoveralltot21 eatingunhealthytot21 eatinghealthytot21 risk_ACEc21 SDQc_prosoc16 SDQc_cond16 SDQc_hyper16 SDQc_peer16 SDQc_emot16 SDQpl_prosoc16 SDQpl_cond16 SDQpl_hyper16 SDQp_total16 SDQp_prosoc16 SDQp_cond16 SDQp_hyper16 bully_totc16 chaostotc_16 MFQlc16 MFQc16 MFQlp16 MFQp16 selfesteem_aca16 selfesteem_opt16 pcbhaqt pcbhaqdett pcbhaqsoct pubertyall16 BMI16 bullyingc14 chaostotc_std14 chaostotp_std14 MFQec12 SDQt_emot12 SDQt_peer12 SDQt_cond12 SDQt_hyper12 SDQt_prosoc12 SDQc_emot12 SDQc_peer12 SDQc_cond12 SDQc_hyper12 SDQc_prosoc12 SDQp_total12 SDQp_emot12 SDQp_peer12 SDQp_cond12 SDQp_hyper12 SDQp_prosoc12 chaostotp_std12 SDQt_total9 SDQt_emot9 SDQt_peer9 SDQt_cond9 SDQt_hyper9 SDQt_prosoc9 SDQc_total9 SDQc_emot9 SDQc_peer9 SDQc_cond9 SDQc_hyper9 SDQc_prosoc9 SDQp_emot9 SDQp_peer9 SDQp_cond9 SDQp_hyper9 SDQp_prosoc9 financchange9 chaostotc_std9 chaostotp_std9 lifeevents9 SDQt_emot7 SDQt_peer7 SDQt_cond7 SDQt_hyper7 SDQt_prosoc7 SDQp_total7 SDQp_emot7 SDQp_peer7 SDQp_cond7 SDQp_hyper7 SDQp_prosoc7 lifeevents7 financchange7 SDQp_total4 SDQp_peer4 SDQp_prosoc4 SDQp_hyper4 SDQp_emot4 SDQp_cond4 BMIf4 BMIm4 BMI4 chaostot_std4 chaosfam_std4 enviro_riskc4 enviro_riskf4 SDQp_prosoc3 SDQp_peer3 SDQp_hyper3 SDQp_emot3 SDQp_cond3 BMI3 enviro_riskc3 enviro_riskf3 chaostot_std3 chaoscalmfam_std3 chaosfam_std3 cmstatus chospmo chospfa chospsib cillsib cillfa cillmo SDQp_total2 SDQp_prosoc2 SDQp_peer2 SDQp_hyper2 SDQp_emot2 SDQp_cond2 birthweight bchstatus btwsick afaclas afasoc afawork afaspq afajob afahqual amoclas amosoc amowork amohqual amagechl apartage arespage 

*install ice 
ssc install ice

* Drop rows with missing ethnicity (as no auxiliary vars available)
drop if ethnicity == .

* Generate interaction terms and quadratic term
gen bdsex = bodydissatisf16 * sex 
gen bdzyg = bodydissatisf16 * MZ 
gen bdage = bodydissatisf16 * age21
gen z2BMI14 = zBMI14 * zBMI14 

*reshape wide before imputing so to appropriately address the nested nature of the dataset
drop twinid
reshape wide ///
age16 age21 age26 bdsex bdzyg bdage zBMI14 z2BMI14 ///
    totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 bodydissatisf16 ///
    edscale21_full edscale21_specific MFQ1c21 MFQc26 BMI21 BMI26 sex  ///
     dlifeev PRS_AN_std PRS_SCHZ_std chaostotc_std12 parent_disciptotc12 ///
    parent_feelc12 random PRS_ADHD_std PRS_ASD_std PRS_BipD_std PRS_crossDis_std ///
    PRS_DEP_std PRS_NEU_std totalpuberty16 totalpuberty14 u1ceats11_rc zBMI16 zBMI12 zBMI7 zBMI4 zBMI3 ///
    depPHQ26 bingeeating26 bulimianerv26 anorexianerv26 BMIlowest26 zmhagoradiag zmhpanicdiag zmhsocphdiag zmhspephdiag ///
    Depatypical26 MDDc26 bodydissatisf26 mhqbingeeating26 mhqpsychoverating26 mhqbulimia26 mhqanorexia26 ///
    MFQcovid4c21 MFQcovid3c21 MFQcovid2c21 MFQcovid1c21 MFQ2c21 bingeeating21 bulimianerv21 anorexianerv21 ///
    bodypreocctot21 bingeatingtot21 eatingoveralltot21 eatingunhealthytot21 eatinghealthytot21 risk_ACEc21 ///
    SDQc_prosoc16 SDQc_cond16 SDQc_hyper16 SDQc_peer16 SDQc_emot16 SDQpl_prosoc16 SDQpl_cond16 SDQpl_hyper16 ///
    SDQp_total16 SDQp_prosoc16 SDQp_cond16 SDQp_hyper16 bully_totc16 chaostotc_16 MFQlc16 MFQc16 MFQlp16 MFQp16 ///
    selfesteem_aca16 selfesteem_opt16 pcbhaqt pcbhaqdett pcbhaqsoct pubertyall16 BMI16 bullyingc14 chaostotc_std14 ///
    MFQec12 SDQt_emot12 SDQt_peer12 SDQt_cond12 SDQt_hyper12 SDQt_prosoc12 SDQc_emot12 SDQc_peer12 ///
    SDQc_cond12 SDQc_hyper12 SDQc_prosoc12 SDQp_total12 SDQp_emot12 SDQp_peer12 SDQp_cond12 SDQp_hyper12 SDQp_prosoc12 ///
    chaostotp_std12 SDQt_total9 SDQt_emot9 SDQt_peer9 SDQt_cond9 SDQt_hyper9 SDQt_prosoc9 SDQc_total9 SDQc_emot9 ///
    SDQc_peer9 SDQc_cond9 SDQc_hyper9 SDQc_prosoc9 SDQp_emot9 SDQp_peer9 SDQp_cond9 SDQp_hyper9 SDQp_prosoc9 ///
    financchange9 chaostotc_std9 chaostotp_std9 lifeevents9 SDQt_emot7 SDQt_peer7 SDQt_cond7 SDQt_hyper7 SDQt_prosoc7 ///
    SDQp_total7 SDQp_emot7 SDQp_peer7 SDQp_cond7 SDQp_hyper7 SDQp_prosoc7 lifeevents7 financchange7 SDQp_total4 ///
    SDQp_peer4 SDQp_prosoc4 SDQp_hyper4 SDQp_emot4 SDQp_cond4 BMIf4 BMIm4 BMI4 chaostot_std4 chaosfam_std4 ///
    enviro_riskc4 enviro_riskf4 SDQp_prosoc3 SDQp_peer3 SDQp_hyper3 SDQp_emot3 SDQp_cond3 BMI3 enviro_riskc3 ///
    enviro_riskf3 chaostot_std3 chaoscalmfam_std3 chaosfam_std3 cmstatus chospmo chospfa chospsib cillsib cillfa ///
    cillmo SDQp_total2 SDQp_prosoc2 SDQp_peer2 SDQp_hyper2 SDQp_emot2 SDQp_cond2 birthweight btwsick , ///
	i(pairid) j(birthorder)

*now impute
ice ///
age161 age162 age211 age212 age261 age262 bdsex1 bdsex2 bdzyg1 bdzyg2 bdage1 bdage2 ///
ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 ///
zBMI141 zBMI142 z2BMI141 z2BMI142 totalpuberty121 totalpuberty122 autismc141 autismc142 MFQc121 MFQc122 ///
sdqc_ext121 sdqc_ext122 bullyingc121 bullyingc122 enjoy_sportc121 enjoy_sportc122 hrs_sportc121 hrs_sportc122 ///
bodydissatisf161 bodydissatisf162 edscale21_full1 edscale21_full2 edscale21_specific1 edscale21_specific2 ///
MFQ1c211 MFQ1c212 MFQc261 MFQc262 BMI211 BMI212 BMI261 BMI262 sex1 sex2 amumage adadage ///
EPDSm4 chaostotc_std121 chaostotc_std122 parent_disciptotc121 parent_disciptotc122 ///
parent_feelc121 parent_feelc122 MZ ///
PRS_AN_std1 PRS_AN_std2 PRS_SCHZ_std1 PRS_SCHZ_std2 PRS_ADHD_std1 PRS_ADHD_std2 ///
PRS_ASD_std1 PRS_ASD_std2 PRS_BipD_std1 PRS_BipD_std2 PRS_crossDis_std1 PRS_crossDis_std2 ///
PRS_DEP_std1 PRS_DEP_std2 PRS_NEU_std1 PRS_NEU_std2 ///
totalpuberty161 totalpuberty162 totalpuberty141 totalpuberty142 zBMI161 zBMI162 zBMI121 zBMI122 zBMI71 zBMI72 zBMI41 zBMI42 zBMI31 zBMI32 ///
MFQcovid1c211 MFQcovid1c212 MFQ2c211 MFQ2c212 bodypreocctot211 bodypreocctot212 risk_ACEc211 risk_ACEc212 ///
SDQc_prosoc161 SDQc_prosoc162 SDQc_cond161 SDQc_cond162 SDQc_hyper161 SDQc_hyper162 SDQc_peer161 SDQc_peer162 SDQc_emot161 SDQc_emot162 ///
bully_totc161 bully_totc162 MFQc161 MFQc162 BMI161 BMI162 bullyingc141 bullyingc142 chaostotc_std141 chaostotc_std142 chaostotp_std14  ///
birthweight1 birthweight2 btwsick1 btwsick2, ///
saving("<local_path>/data/", replace) ///
seed(765987) m(100) ///
match( ///
    edscale21_full1 edscale21_full2 edscale21_specific1 edscale21_specific2 MFQ1c211 MFQ1c212 MFQc261 MFQc262 ///
    MFQc121 MFQc122 bullyingc121 bullyingc122 bodydissatisf161 bodydissatisf162 ///
    BMI211 BMI212 BMI261 BMI262 parent_feelc121 parent_feelc122 parent_disciptotc121 parent_disciptotc122 ///
    chaostotc_std121 chaostotc_std122 hrs_sportc121 hrs_sportc122 enjoy_sportc121 enjoy_sportc122 ///
    sdqc_ext121 sdqc_ext122 autismc141 autismc142 totalpuberty121 totalpuberty122 EPDSm3 ///
) ///
passive( ///
    bdsex1: bodydissatisf161*sex1 \ bdsex2: bodydissatisf162*sex2 \  ///
    bdzyg1: bodydissatisf161*MZ \ bdzyg2: bodydissatisf162*MZ \  ///
    bdage1: bodydissatisf161*age211 \ bdage2: bodydissatisf162*age212 \  ///
    z2BMI141: zBMI141*zBMI141 \ z2BMI142: zBMI142*zBMI142 ///
) ///
cycles(20) genmiss(m_) persist clear

*now analyse data using the imputed dataset 
*changed directory
use "<local_path>/data/", clear

*import ice
mi import ice, automatic

*check dataset complete/imputed
mi describe

*check if an update is needed
mi update

*reshape to long so that you have one twin on each row
mi reshape long age16 age21 age26 bdsex bdzyg bdage ///
zBMI14 z2BMI14 totalpuberty12 autismc14 MFQc12 ///
sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
bodydissatisf16 edscale21_full edscale21_specific ///
MFQ1c21 MFQc26 BMI21 BMI26 sex chaostotc_std12 parent_disciptotc12 parent_feelc12 ///
PRS_AN_std PRS_SCHZ_std PRS_ADHD_std ///
PRS_ASD_std PRS_BipD_std PRS_crossDis_std ///
PRS_DEP_std PRS_NEU_std ///
totalpuberty16 totalpuberty14 zBMI16 zBMI12 zBMI7 zBMI4 zBMI3 ///
MFQcovid1c21 MFQ2c21 bodypreocctot21 risk_ACEc21 ///
SDQc_prosoc16 SDQc_cond16 SDQc_hyper16 SDQc_peer16 SDQc_emot16 ///
bully_totc16 MFQc16 BMI16 bullyingc14 chaostotc_std14  ///
birthweight btwsick, i(pairid) j(twinid)

*keep only if they had the exposure
mi xeq: keep if bodydissatisf16!=. 

save "<local_path>/data/", replace
use "<local_path>/data/", clear

*check distribution of variables in analytic model 
*exposure
mi xeq 0: su bodydissatisf16
mi xeq 1/5: su bodydissatisf16
*outcomes
mi xeq 0: su edscale21_full 
mi xeq 1/5: su edscale21_full 
mi xeq 0: su edscale21_specific 
mi xeq 1/5: su edscale21_specific 
mi xeq 0: su MFQ1c21
mi xeq 1/5: su MFQ1c21
mi xeq 0: su MFQc26
mi xeq 1/5: su MFQc26
mi xeq 0: su BMI21 
mi xeq 1/5: su BMI21 
mi xeq 0: su BMI26 
mi xeq 1/5: su BMI26 
*confounders
mi xeq 0: su age21 
mi xeq 1/5: su age21 
mi xeq 0: su age26 
mi xeq 1/5: su age26
mi xeq 0: su sex 
mi xeq 1/5: su sex
mi xeq 0: su MZ 
mi xeq 1/5: su MZ 
mi xeq 0: su ethnicity
mi xeq 1/5: su ethnicity 
mi xeq 0: su ases 
mi xeq 1/5: su ases
mi xeq 0: su clifeev_rc
mi xeq 1/5: su clifeev_rc 
mi xeq 0: su EPDSm3 
mi xeq 1/5: su EPDSm3
mi xeq 0: su BMIm3  
mi xeq 1/5: su BMIm3
mi xeq 0: su BMIf3 
mi xeq 1/5: su BMIf3
mi xeq 0: su zBMI14
mi xeq 1/5: su zBMI14
mi xeq 0: su totalpuberty12 
mi xeq 1/5: su totalpuberty12 
mi xeq 0: su autismc14 
mi xeq 1/5: su autismc14 
mi xeq 0: su MFQc12 
mi xeq 1/5: su MFQc12 
mi xeq 0: su sdqc_ext12
mi xeq 1/5: su sdqc_ext12 
mi xeq 0: su bullyingc12 
mi xeq 1/5: su bullyingc12 
mi xeq 0: su enjoy_sportc12 
mi xeq 1/5: su enjoy_sportc12  
mi xeq 0: su hrs_sportc12
mi xeq 1/5: su hrs_sportc12 
mi xeq 0: su chaostotc_std12 
mi xeq 1/5: su chaostotc_std12 
mi xeq 0: su parent_disciptotc12 
mi xeq 1/5: su parent_disciptotc12
mi xeq 0: su parent_feelc12 
mi xeq 1/5: su parent_feelc12 
mi xeq 0: su PRS_AN_std
mi xeq 1/5: su PRS_AN_std 
mi xeq 0: su PRS_SCHZ_std 
mi xeq 1/5: su PRS_SCHZ_std 
count

*===============================================================================*
* OBJECTIVE 1: Association between body dissatisfaction (age 16) and ED symptoms (age 21)
* Models estimated with MI dataset using random intercepts for twin pairs
*===============================================================================*

*------------------------------------------------------------------------------*
* PRIMARY OUTCOME: edscale21_full (includes all ED symptoms incl. body image) *
*------------------------------------------------------------------------------*

* Model 1: Unconditional association (no covariates)
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
     || pairid:, cov(unstructured) reml

* Model 2: Adjust for child-level covariates
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity ///
     || pairid:, cov(unstructured) reml

* Model 3: Add family-level covariates
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 ///
     || pairid:, cov(unstructured) reml

* Model 4: Add primary confounders
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 ///
    totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
   || pairid:, cov(unstructured) reml

* Model 5: Two-way interactions (BD × sex, BD × zygosity)
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity bdsex bdzyg ///
    ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 totalpuberty12 ///
    autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
    || pairid:, cov(unstructured) reml

* Model 6: Quadratic BMI term (non-linear confounding)
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity bdsex bdzyg ///
    ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 z2BMI14 ///
    totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
     || pairid:, cov(unstructured) reml
	
* Model 7: Sensitivity – Secondary confounders (expanded adjustment)
mi estimate, mcerror level(95): mixed edscale21_full bodydissatisf16 ///
    age21 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 ///
    totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
    chaostotc_std12 parent_disciptotc12 parent_feelc12 PRS_AN_std  PRS_SCHZ_std  ///
    || pairid:, cov(unstructured) reml
	
*------------------------------------------------------------------------------*
* SENSITIVITY OUTCOME: edscale21_specific (excludes body dissatisfaction items)
*------------------------------------------------------------------------------*

* Sensitivity Model 1: Adjusted for all primary confounders
mi estimate, mcerror level(95): mixed edscale21_specific bodydissatisf16 ///
    age21 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 ///
    totalpuberty12 autismc14 MFQc12 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 ///
    || pairid:, cov(unstructured) reml
	

