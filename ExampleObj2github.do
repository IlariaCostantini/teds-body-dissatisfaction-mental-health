*Author: Ilaria Costantini
*Project: Association between body dissatisfaction and EDs, MFQ, and BMI
*Analysis: Differences-in-scores in imputed dataset 

*==============================================================================*
*                              OBJECTIVE 2                                     *
*==============================================================================*
* Difference-in-score analyses using imputed dataset

use "<local_path>/data/", clear

* Declare imputations
mi import ice, automatic
mi describe

* Keep only pairs with exposure data
mi xeq: keep if bodydissatisf16 != .

*recode random so that it is 1 and 2 instead of 0 and 1 and use for reshaping  // try this out
mi passive: gen random_twinid = 1 if random==0
mi passive: replace random_twinid = 2 if random==1

* Generate non-passive version
gen random_twinid_nonpassive = random_twinid if _mi_m == 0
replace random_twinid_nonpassive = random_twinid

* Keep variables needed for analysis
keep _mi_id _mi_miss _mi_m edscale21_full edscale21_specific bodydissatisf16 age21 age26 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 z2BMI14 totalpuberty12 autismc14 MFQc12 MFQ1c21 MFQc26 BMI21 BMI26 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 chaostotc_std12 parent_disciptotc12 parent_feelc12 PRS_AN_std PRS_SCHZ_std m_edscale21_full m_edscale21_specific m_bodydissatisf16 m_age21 m_age26 m_sex m_MZ m_ethnicity m_ases m_clifeev_rc m_EPDSm3 m_BMIm3 m_BMIf3 m_zBMI14 m_z2BMI14 m_totalpuberty12 m_autismc14 m_MFQc12 m_MFQ1c21 m_MFQc26 m_BMI21 m_BMI26 m_sdqc_ext12 m_bullyingc12 m_enjoy_sportc12 m_hrs_sportc12 m_chaostotc_std12 m_parent_disciptotc12 m_parent_feelc12 m_PRS_AN_std m_PRS_SCHZ_std random_twinid_nonpassive pairid 

* Reshape to wide by twin order so that you can then create the difference in scores
mi reshape wide edscale21_full edscale21_specific bodydissatisf16 age21 age26 sex MZ ethnicity ases clifeev_rc EPDSm3 BMIm3 BMIf3 zBMI14 z2BMI14 totalpuberty12 autismc14 MFQc12 MFQ1c21 MFQc26 BMI21 BMI26 sdqc_ext12 bullyingc12 enjoy_sportc12 hrs_sportc12 chaostotc_std12 parent_disciptotc12 parent_feelc12 PRS_AN_std PRS_SCHZ_std m_edscale21_full m_edscale21_specific m_bodydissatisf16 m_age21 m_age26 m_sex m_MZ m_ethnicity m_ases m_clifeev_rc m_EPDSm3 m_BMIm3 m_BMIf3 m_zBMI14 m_z2BMI14 m_totalpuberty12 m_autismc14 m_MFQc12 m_MFQ1c21 m_MFQc26 m_BMI21 m_BMI26 m_sdqc_ext12 m_bullyingc12 m_enjoy_sportc12 m_hrs_sportc12 m_chaostotc_std12 m_parent_disciptotc12 m_parent_feelc12 m_PRS_AN_std m_PRS_SCHZ_std , i(pairid) j(random_twinid_nonpassive)

save "<local_path>/data/", replace 
use "<local_path>/data/", clear

* Define twin variable lists
local twin1 "edscale21_full1 edscale21_specific1 MFQ1c211 MFQc261 BMI211 BMI261 bodydissatisf161 age211 age261 sex1 MZ1 ethnicity1 ases1 clifeev_rc1 EPDSm31 BMIm31 BMIf31 zBMI141 z2BMI141 totalpuberty121 autismc141 MFQc121 sdqc_ext121 bullyingc121 enjoy_sportc121 hrs_sportc121 chaostotc_std121 parent_disciptotc121 parent_feelc121 PRS_AN_std1 PRS_AN_std2 PRS_SCHZ_std1 PRS_SCHZ_std2"

local twin2 "edscale21_full2 edscale21_specific2 MFQ1c212 MFQc262 BMI212 BMI262 bodydissatisf162 age212 age262 sex2 MZ2 ethnicity2 ases2 clifeev_rc2 EPDSm32 BMIm32 BMIf32 zBMI142 z2BMI142 totalpuberty122 autismc142 MFQc122 sdqc_ext122 bullyingc122 enjoy_sportc122 hrs_sportc122 chaostotc_std122 parent_disciptotc122 parent_feelc122 PRS_AN_std1 PRS_AN_std2 PRS_SCHZ_std1 PRS_SCHZ_std2"

* Loop through variables to create differences and averages
forval i = 1/`: word count `twin1'' {
    local varname1 : word `i' of `twin1'
    local varname2 : word `i' of `twin2'
    * Extract the base name by removing the last character
    local varname_base = substr("`varname1'", 1, length("`varname1'") - 1)
    
    * Generate relative differences, averages
	*relative differences highlight the magnitude of change or discrepancy between pairs of twins.
    mi passive: gen rel_diff_`varname_base' = (`varname2' - `varname1') // this is actually a relative difference in score so I am going to change the name 
    mi passive: gen avg_`varname_base' = (`varname1' + `varname2') / 2
}

*now check it worked correctly 
order pairid MZ1 MZ2 sex1 sex2 rel_diff_sex age211 age212 rel_diff_age21 age261 age262 rel_diff_age26 bodydissatisf161 bodydissatisf162 rel_diff_bodydissatisf16 edscale21_full1 edscale21_full2 rel_diff_edscale21_full edscale21_specific1 edscale21_specific2 rel_diff_edscale21_specific MFQ1c211 MFQ1c212 rel_diff_MFQ1c21 MFQc261 MFQc262 rel_diff_MFQc26 BMI211 BMI212 rel_diff_BMI21 BMI261 BMI262 rel_diff_BMI26  

save "<local_path>/data/", replace
use "<local_path>/data/", clear

*-------------------------------------------------------------------------------*	  
*                         EATING DISORDER AGE 21                                *
*-------------------------------------------------------------------------------*
*check if any update is needed
mi update

mi passive: gen exposure=1 if bodydissatisf161!=. & bodydissatisf162!=.  // not needed as dropped those without exposure

*-------------------------------------------------------------------------------*
*                               DYZIGOTIC                                       *
*-------------------------------------------------------------------------------*
 
* Objective 1: Unconditional Model 1
* Unconditional model 1 - Relative Differences
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 if MZ1==0 , noconstant 

*Objective 1: Model 2 with child characteristics
*no need for this as covariate are fixed, so it's commented out
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 rel_diff_sex rel_diff_age21 if MZ1==0, noconstant 

*Objective 1: Model 4 with all primary confounding factors
* Model 4 - Relative Differences
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 rel_diff_sex rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 if MZ1==0, noconstant 

* Sensitivity analysis objective 1: Model 6 secondary confounders - Relative Differences
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16  rel_diff_sex  rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 rel_diff_chaostotc_std12 rel_diff_parent_disciptotc12 rel_diff_parent_feelc12 rel_diff_PRS_AN_std rel_diff_PRS_SCHZ_std if MZ1==0, noconstant 

* Sensitivity analysis - Remove body dissatisfaction from the outcome - Relative Differences
mi estimate: regress rel_diff_edscale21_specific rel_diff_bodydissatisf16 rel_diff_sex rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 if MZ1==0, noconstant  

*-------------------------------------------------------------------------------*
*                               MONOZYGOTIC                                     *
*-------------------------------------------------------------------------------*
 
* Objective 1: Unconditional Model 1
* Unconditional model 1 - Relative Differences
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 if MZ1==1, noconstant 

*Objective 1: Model 2 with child characteristics
*no need for this as covariate are fixed, so it's commented out  
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 rel_diff_age21 if MZ1==1, noconstant 

*Objective 1: Model 4 with all primary confounding factors *I don't need sex1 and sex2 and will be the same, use only one 
* Model 4 - Relative Differences
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 if MZ1==1, noconstant 

* Sensitivity analysis objective 1: Model 6 secondary confounders - Relative Differences - no need of PRS as should be the same
mi estimate: regress rel_diff_edscale21_full rel_diff_bodydissatisf16 rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 rel_diff_chaostotc_std12 rel_diff_parent_disciptotc12 rel_diff_parent_feelc12 if MZ1==1, noconstant 

* Sensitivity analysis - Remove body dissatisfaction from the outcome - Relative Differences
mi estimate: regress rel_diff_edscale21_specific rel_diff_bodydissatisf16 rel_diff_age21 rel_diff_zBMI14 rel_diff_totalpuberty12 rel_diff_autismc14 rel_diff_MFQc12 rel_diff_sdqc_ext12 rel_diff_bullyingc12 rel_diff_enjoy_sportc12 rel_diff_hrs_sportc12 if MZ1==1, noconstant 
