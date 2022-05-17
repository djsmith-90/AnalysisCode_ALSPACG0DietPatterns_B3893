***** Analysis code for 'religion and diet' study (B3893) - Exploring potential differences in dietary patterns by religiosity - G0 mother age 4 (J file).
*** Created 25/10/2021 by Dan Major-Smith
*** Stata version 17.0


**********************************************************************************
**** Set working directory, start a log file, read in dataset, and add numlabels

cd "X:\Groups\ARC\DanS\DietaryPatterns_B3893"

capture log close
log using "B3893_RSBB_Diet_G0MumJ_log", replace text

use "Diet_RSBB_B3893.dta", clear

numlabel, add


** Drop mums with non-live-birth, as not very much pregnancy data for them anyhow (most are early miscarriages)
tab mz012, m

tab c645a if mz012 < 2, m
tab mz028b if mz012 < 2, m

drop if mz012 < 3
tab mz012, m

** Remove one pregnancy of mult mums with two or more core-enrolled pregancies (to avoid issues of non-independence due to repeating data by same mother related to a different pregnancy)
tab mult_mum_Y, m

drop if mult_mum_Y == 1
tab mult_mum_Y, m

** Will also drop those who withdrew consent, as is not data for complete-case analysis, and probably shouldn't try to imput their data of using multiple imputation (plus, they have no data, so imputation would have little information to go on). Drop both mum and partner WoCs
tab1 a006 pa065, m

drop if a006 == .a | pa065 == .c
tab1 a006 pa065, m


**********************************************************************************
**** Check variables and tidy if necessary

*** Exposures (RSBB)

** G0 mums
tab1 d810 d813 d816, m

* Code negative values as missing
foreach var of varlist d810 d813 d816 {
	replace `var' = . if `var' < 0
	tab `var', m
}

* Recode denomination to none vs Christian vs other
recode d813 (0 = 1) (1/6 = 2) (7/13 = 3), gen(d813_grp)
label define relig_lb 1 "None" 2 "Christian" 3 "Other"
numlabel relig_lb, add
label values d813_grp relig_lb
tab d813_grp, m

* Also recode church attendance to combine once a week and once a month
recode d816 (1 2 = 1) (3 = 2) (4 = 3), gen(d816_grp)
label define attend_lb 1 "Min once a month" 2 "Min once a year" 3 "Not at all"
numlabel attend_lb, add
label values d816_grp attend_lb
tab d816_grp, m


** G0 partners/fathers
tab1 pb150 pb153 pb155, m

* Code negative values as missing
foreach var of varlist pb150 pb153 pb155 {
	replace `var' = . if `var' < 0
	tab `var', m
}

* Recode denomination to none vs Christian vs other
recode pb153 (0 = 1) (1/6 = 2) (7/13 = 3), gen(pb153_grp)
label values pb153_grp relig_lb
tab pb153_grp, m

* Also recode church attendance to combine once a week and once a month
recode pb155 (1 2 = 1) (3 = 2) (4 = 3), gen(pb155_grp)
label values pb155_grp attend_lb
tab pb155_grp, m


*** Outcomes (dietary patterns)

** G0 mothers
sum c3800-c3844 j1000-j1103

* For nutrients, recode negative values as missing
foreach var of varlist c3800-c3836 j1000-j1036 {
	replace `var' = . if `var' < 0
}
sum c3800-c3836 j1000-j1036

* For dietary PCA patterns, recode values less than -100 as missing (as some 'true' values are negative)
foreach var of varlist c3840-c3844 j1100-j1103 {
	replace `var' = . if `var' < -10
}
sum c3840-c3844 j1100-j1103


** G0 partners/fathers
sum pg2501-pg2543

* For nutrients, recode negative values as missing
foreach var of varlist pg2501-pg2536 {
	replace `var' = . if `var' < 0
}
sum pg2501-pg2536

* For dietary PCA patterns, recode values less than -100 as missing (as some 'true' values are negative)
foreach var of varlist pg2540-pg2543 {
	replace `var' = . if `var' < -10
}
sum pg2540-pg2543


*** Covariates/confounders

** G0 mother

* Age at birth
tab mz028b, m

replace mz028b = . if mz028b < 0
tab mz028b, m

* Age at C questionnaire - If missing, will use age at birth, as will be very similar
tab c994, m

replace c994 = . if c994 < 0
replace c994 = mz028b if c994 == . & mz028b < .
tab c994, m

* Age at J questionnaire - If missing, will add 4 years to age at birth, as J file completed approximately 4 years post-birth
tab j912, m

replace j912 = . if j912 < 0
replace j912 = mz028b + 4 if j912 == . & mz028b < .
tab j912, m

* Highest education
tab c645a, m

replace c645a = . if c645a < 0
tab c645a, m

* Occupational social class - Will split into low (III manual, IV and V) and high (!, II and III non-manual))
tab c755, m

replace c755 = . if c755 < 0 | c755 == 65
tab c755, m

recode c755 (1 2 3 = 1) (4 5 6 = 0), gen(c755_grp)
label define occ_lb 1 "High (I/II/III-manual)" 0 "Low (III-manual/IV/V)"
numlabel occ_lb, add
label value c755_grp occ_lb
tab c755_grp, m

* IMD
tab dimd2010q5, m

replace dimd2010q5 = . if dimd2010q5 < 0
tab dimd2010q5, m

* Urban/rural status - combine into urban vs rural
tab dur01ind, m

replace dur01ind = . if dur01ind < 0
tab dur01ind, m

recode dur01ind (1 = 1) (2 3 4 = 0), gen(dur01ind_grp)
label define urban_lb 1 "Urban" 0 "Town/village/Hamlet"
numlabel urban_lb, add
label value dur01ind_grp urban_lb
tab dur01ind_grp, m

* Housing status - Recode into owned vs rented vs council/HA vs other
tab a006, m

replace a006 = . if a006 < 0
tab a006, m

recode a006 (0 1 = 1) (3 4  = 2) (2 5 = 3) (6 = 4), gen(a006_grp)
label define housing_lb 1 "Owned/Mortgaged" 2 "Renting" 3 "Council/HA" 4 "Other"
numlabel housing_lb, add
label values a006_grp housing_lb
tab a006_grp, m

* Financial difficulties - Combine into yes vs no
tab b594, m

replace b594 = . if b594 < 0
tab b594

recode b594 (5 = 0) (1/4 = 1), gen(b594_grp)
label define fin_lb 0 "No" 1 "Yes"
numlabel fin_lb, add
label values b594_grp fin_lb
tab b594_grp, m

* Ethnicity - Combine into white vs other than white
tab c800, m

replace c800 = . if c800 < 0
tab c800, m

recode c800 (1 = 0) (2/9 = 1), gen(c800_grp)
label define white_lb 0 "White" 1 "Other than white"
numlabel white_lb, add
label values c800_grp white_lb
tab c800_grp, m

* Marital status - Combine widowed/divorced/separated, as well as 1st and 2nd/3rd marriage
tab a525, m

replace a525 = . if a525 < 0
tab a525, m

recode a525 (1 = 1) (5 6 = 2) (2 3 4 = 3), gen(a525_grp)
label define marital_lb 1 "Never married" 2 "Married" 3 "Widowed/Divorced/Separated"
numlabel marital_lb, add
label values a525_grp marital_lb
tab a525_grp, m

* Parity - Recode into 0, 1 and 2 or more
tab b032, m

replace b032 = . if b032 < 0
tab b032, m

recode b032 (3/22 = 2), gen(b032_grp)
label define parity_lb 2 "2 or more"
numlabel parity_lb, add
label values b032_grp parity_lb
tab b032_grp, m

* Smoking status - Combine into 'never' vs 'smoked pre-preg' vs 'smoked in preg'
tab1 b650 b665 b667, m

replace b650 = . if b650 < 0
replace b665 = . if b665 < 0
replace b665 = . if b650 == .
replace b665 = 2 if b665 > 2 & b665 < .
replace b667 = . if b667 < 0
replace b667 = . if b650 == .
replace b667 = 2 if b667 > 2 & b665 < .

replace b650 = 1 if b650 == 2 & (b665 == 2 | b667 == 2)

tab1 b650 b665 b667, m

tab b650 b665, m
tab b650 b667, m
tab b665 b667, m

gen mum_smk = .
replace mum_smk = 0 if b650 == 2
replace mum_smk = 1 if b650 == 1 & b665 == 1 & b667 == 1
replace mum_smk = 2 if b650 == 1 & (b665 == 2 | b667 == 2)

label define smk_lb 0 "Never smoked" 1 "Smoked prior to preg" 2 "Smoked in preg"
numlabel smk_lb, add
label values mum_smk smk_lb
tab mum_smk, m

* Depression/MH (using EPDS total score)
tab b371, m

replace b371 = . if b371 < 0
tab b371, m

* Self-reported general health prior to pregnancy - Combine 'sometimes', 'often' and 'always' unwell together
tab b040, m

replace b040 = . if b040 < 0
tab b040, m

recode b040 (3/5 = 3), gen(b040_grp)
label define health_lb 1 "Always well" 2 "Usually well" 3 "Sometimes/often/always unwell"
numlabel health_lb, add
label values b040_grp health_lb
tab b040_grp, m


** G0 fathers/partners

* Age at birth (will use age at PB questionnaire in pregnancy, as don't have G0 partner/father age at birth)
tab pb910, m

replace pb910 = . if pb910 < 0
tab pb910, m

* Any dads with PG data but not PB data to try and backfill? Are 439 dads, so will estimate age based on age of child
tab pg9996a, m

replace pg9996a = . if pg9996a < 0
tab pg9996a, m

count if pb910 >= . & pg9996a < .

list aln pg9996a pg9991a pg9991b if pb910 >= . & pg9996a < . in 1/200

gen dad_age = pb910
replace dad_age = pg9996a - round(pg9991a / 12) if dad_age >= . & pg9996a < .
tab dad_age, m

* Age at PG questionnaire - If missing, will use PB data plus 4 years
tab pg9996a, m

replace pg9996a = pb910 if pg9996a == . & pb910 < .
tab pg9996a, m

* Highest education
tab c666a, m

replace c666a = . if c666a < 0
tab c666a, m

* Occupational social class - Will split into low (III manual, IV and V) and high (!, II and III non-manual))
tab c765, m

replace c765 = . if c765 < 0 | c765 == 65
tab c765, m

recode c765 (1 2 3 = 1) (4 5 6 = 0), gen(c765_grp)
label value c765_grp occ_lb
tab c765_grp, m

* Ethnicity - Combine into white vs other than white
tab c801, m

replace c801 = . if c801 < 0
tab c801, m

recode c801 (1 = 0) (2/9 = 1), gen(c801_grp)
label values c801_grp white_lb
tab c801_grp, m

* Smoking status - Combine into 'never' vs 'smoked pre-preg' vs 'smoked in preg'
tab1 pb071 pb077, m
tab pb071 pb077, m

replace pb071 = . if pb071 < 0
replace pb077 = . if pb077 < 0
replace pb077 = . if pb071 == .
replace pb077 = 2 if pb077 > 2 & pb077 < .
replace pb071 = . if pb071 > 0 & pb071 < . & pb077 == .

tab1 pb071 pb077, m
tab pb071 pb077, m

gen dad_smk = .
replace dad_smk = 0 if pb071 == 2
replace dad_smk = 1 if pb071 == 1 & pb077 == 1
replace dad_smk = 2 if pb071 == 1 & pb077 == 2

label values dad_smk smk_lb
tab dad_smk, m

* Depression/MH (using EPDS total score)
tab pb260, m

replace pb260 = . if pb260 < 0
tab pb260, m

* Self-reported general health prior to pregnancy (reported by G0 mother) - Combine 'sometimes', 'often' and 'always' unwell together
tab a524, m

replace a524 = . if a524 < 0
tab a524, m

recode a524 (3/5 = 3), gen(a524_grp)
label values a524_grp health_lb
tab a524_grp, m


**********************************************************************************
**** Analysis of G0 mothers at age 4 (J file)

describe j1100 j1101 j1102 j1103


*** 'Healthy' PCA score
sum j1100

* The distribution is fairly normally distributed, although are some high outliers (7 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist j1100, freq
count if j1100 > 5 & j1100 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen belief_CCA = 1
foreach var of varlist j1100 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* Descriptive statistics
oneway j1100 d810, tab
oneway j1100 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress j1100 ib3.d810
regress j1100 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1100 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1100 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1100 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & j1100 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen denomination_CCA = 1
foreach var of varlist j1100 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denomination_CCA = 0 if denomination_CCA == 1 & `var' >= .
}
tab denomination_CCA, m

* Descriptive statistics
oneway j1100 d813_grp, tab
oneway j1100 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress j1100 i.d813_grp
regress j1100 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1100 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1100 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1100 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & j1100 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen attend_CCA = 1
foreach var of varlist j1100 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* Descriptive statistics
oneway j1100 d816_grp, tab
oneway j1100 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress j1100 ib3.d816_grp
regress j1100 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1100 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1100 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1100 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & j1100 < 5


*** As with pregnancy results, get some odds patterns, as belief in God is associated with more 'healthy' diet, but being christian associated with less healthy diet compared to non-believers and other faiths, while attending church associated with healthier diet. I wonder if belief and church attendance effects might explain this - So will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress j1100 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress j1100 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Perhaps lapsed Christians who no longer believe but are 'nominal' Christians could account for this effect? Will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab1 d810 d813_grp, m
tab d810 d813_grp, m

gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other"
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure
gen lapsed_CCA = 1
foreach var of varlist j1100 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m

regress j1100 i.lapsed_Xian if lapsed_CCA == 1

regress j1100 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if lapsed_CCA == 1

* Formal test that Christian non-believers have a less healthy diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian



************************************************************************************
*** Now repeat for 'processed' PCA
sum j1101

* The distribution is relatively normally distributed, although are some high outliers (14 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist j1101, freq
count if j1101 > 5 & j1101 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway j1101 d810, tab
oneway j1101 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress j1101 ib3.d810
regress j1101 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1101 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1101 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1101 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & j1101 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway j1101 d813_grp, tab
oneway j1101 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress j1101 i.d813_grp
regress j1101 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1101 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1101 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1101 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & j1101 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway j1101 d816_grp, tab
oneway j1101 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress j1101 ib3.d816_grp
regress j1101 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1101 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1101 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1101 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & j1101 < 5


*** For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress j1101 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress j1101 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure
regress j1101 i.lapsed_Xian if lapsed_CCA == 1

regress j1101 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'processed' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'Confectionary' PCA
sum j1102

* The distribution has a long right/positive tail, and are some high outliers (10 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist j1102, freq
count if j1102 > 5 & j1102 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway j1102 d810, tab
oneway j1102 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress j1102 ib3.d810
regress j1102 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1102 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1102 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1102 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & j1102 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway j1102 d813_grp, tab
oneway j1102 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress j1102 i.d813_grp
regress j1102 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1102 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1102 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1102 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & j1102 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway j1102 d816_grp, tab
oneway j1102 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress j1102 ib3.d816_grp
regress j1102 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1102 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1102 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1102 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & j1102 < 5


*** For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress j1102 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress j1102 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure
regress j1102 i.lapsed_Xian if lapsed_CCA == 1

regress j1102 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'confectionary' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'vegetarian' PCA
sum j1103

* The distribution is kinda normally distributed, although are some high outliers (18 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist j1103, freq
count if j1103 > 5 & j1103 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway j1103 d810, tab
oneway j1103 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress j1103 ib3.d810
regress j1103 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1103 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1103 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1103 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & j1103 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway j1103 d813_grp, tab
oneway j1103 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress j1103 i.d813_grp
regress j1103 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1103 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1103 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1103 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & j1103 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway j1103 d816_grp, tab
oneway j1103 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress j1103 ib3.d816_grp
regress j1103 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress j1103 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress j1103 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress j1103 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & j1103 < 5


*** Pretty consistent effects that Christiantity associated with less 'vegetarian' diet ('other' beliefs much more likely to be vegetarian than non-believers, though, so seems Christian-specific). For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress j1103 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress j1103 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure
regress j1103 i.lapsed_Xian if lapsed_CCA == 1

regress j1103 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'vegetarian' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian



*************************************************************************************
*************************************************************************************
*** To think about multiple imputation, as of ~14,000 mums in total, only ~6,000 (~45%) are included in the complete case pregnancy analysis (for reference, ~9,000 mums have FFQ PCA data at age 4). MI definitely could be useful here, as missingness pattern is of many items with little missing data (rather than whole waves of data missing).

** Missingness patterns of outcome, exposure and confounders
misstable sum j1100 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp c666a c765_grp mum_smk b371 b040_grp, all

misstable patterns j1100 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp c666a c765_grp mum_smk b371 b040_grp, freq


*** To also think about choice of auxiliary variables, as need some to improve precision relative to CCA analysis (if just use covariates, not adding any extra information). E.g., using other sociodemographic variables could help, as could using later RSBB and diet data if missing at this time point but collected at later questionnaires.


** Explore whether exposure and outcome associated with missingness (as CCA only biased if outcome or outcome and exposure associated with selection). 

** Outcome: Dietary PCAs. Using pregnancy diet data, can test whether outcome (diet) predicts selection

* Missingness marker
gen miss_PCA = 1
replace miss_PCA = 0 if j1100 < .
tab miss_PCA, m

* Does pregnancy diet predict missing J diet data? Yes in unadjusted analyses, although mostly null in adjusted models controlling for socioeconomic factors (although issues of interpretation here, as many of these have missing data too). May still be a weak adjusted association with diet and missingness, though.
logistic miss_PCA c3800-c3804
logistic miss_PCA c3800-c3804 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Possible to use C questionnaire dietary patterns as auxiliary variables to predict missing J diet values? Definitely, as ~3,000 mums with C data but missing J diet data
sum c3840
count if miss_PCA == 1 & c3840 != .

* Check association between C and J PCA components (although 'traditional' PCA not found in J file). Not super-strong, but r = 0.4-0.5, so is some association. So altogether, including both C file diet PCAs and other SEP covariates in imputations likely to make MAR assumption more reasonable (although not certain, if other unmeasured factors associated with selection)
describe c3840-c3844 j1100-j1103

corr c3840 j1100
corr c3842 j1101
corr c3843 j1102
corr c3844 j1103


** Exposure 1) Belief in God - As above, very difficult to properly assess predictors of missingness due to lots of missing values in covariates, but are some associations (age, housing status, marital status, parity)
gen miss_belief = 1
replace miss_belief = 0 if d810 < .
tab miss_belief, m

tab miss_belief if j912 != . & a006_grp != . & a525_grp != . & b032_grp != .
logistic miss_belief j912 a006_grp a525_grp b032_grp, or

* Possible to gain missing information from RSBB in K quest (asked at age 5)
tab1 k6240 k6243 k6247, m

* Code negative values as missing
foreach var of varlist k6240 k6243 k6247 {
	replace `var' = . if `var' < 0
	tab `var', m
}

* Recode denomination to none vs Christian vs other
recode k6243 (0 = 1) (1/6 = 2) (7/13 = 3), gen(k6243_grp)
label values k6243_grp relig_lb
tab k6243_grp, m

* Also recode church attendance to combine once a week and once a month (this variable also have 'occassional worship', which will code with 'once a year' [as only 11 cases, not going to have much impact either way])
recode k6247 (1 2 = 1) (3 5 = 2) (4 = 3), gen(k6247_grp)
label values k6247_grp attend_lb
tab k6247_grp, m

* Any extra RSBB data missing from D file (~300-400 extra cases, so could be useful)
count if d810 == . & k6240 < .
count if d813_grp == . & k6243_grp < .
count if d816_grp == . & k6247_grp < .

* Do RSBB measures correlate? Yes, quite storngly.
tab d810 k6240, row chi
tab d813_grp k6243_grp, row chi
tab d816_grp k6247_grp, row chi


** For other auxiliary variables, will include partner's education and partner's social class (as may help predict missing confounders/covariates)
tab1 c666a c765_grp, m

* Will also include smoking status, depression/MH and general health as auxiliary variables, as these are likely to predict both diet and missingness (e.g., via health-seeking behaviours), and may make the MAR assumption more plausible. Will check that each are associated with both missingness and the diet outcomes

* These variables predict missing diet data
logistic miss_PCA i.mum_smk, or
logistic miss_PCA b371, or
logistic miss_PCA i.b040_grp, or

* And are associated with diet variables (although less so after adjusting for other potential confounders - but interpretation issues as changes in sample size)
regress j1100 i.mum_smk b371 i.b040_grp
regress j1100 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress j1101 i.mum_smk b371 i.b040_grp
regress j1101 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress j1102 i.mum_smk b371 i.b040_grp
regress j1102 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress j1103 i.mum_smk b371 i.b040_grp
regress j1103 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp




***********************************************************************************
**** Now set up the multiple imputation

*** Save the dataset to can return back to it after running MI (for the nutrient analyses)
save "RSBB_Diet_JQuest_temp.dta", replace
	
* Drop variables not included in MI to speed up imputation and make file much smaller
keep aln c3840 c3841 c3842 c3843 c3844 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp j1100 j1101 j1102 j1103 k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Set up the imputation and register the variables with missing data (note there's no 'mi register regular' here, as all variables have some missing data)
mi set mlong
mi register imputed c3840 c3841 c3842 c3843 c3844 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp j1100 j1101 j1102 j1103 k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Test the imputation with a dry run first, to make sure looks okay - As some of the PCA's aren't quite normally distributed, will impute using predictive mean matching (based on 5 nearest neighbours)
mi impute chained ///
	(regress) j912 ///
	(pmm, knn(5)) c3840 c3841 c3842 c3843 c3844 j1100 j1101 j1102 j1103 b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp, ///
	add(10) burnin(10) rseed(98765) dryrun
	
	
** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about 1.5 hours. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010), and 'savetrace' to check convergence
mi impute chained ///
	(regress) j912 ///
	(pmm, knn(5)) c3840 c3841 c3842 c3843 c3844 j1100 j1101 j1102 j1103 b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp, ///
	add(50) burnin(10) rseed(98765) dots augment ///
	savetrace(imp_trace_PCA_age4_J, replace)


** Save this imputed dataset, so not have to run whole imputation again to access results
save "imp_PCA_Age4_J.dta", replace


** Check convergence and that imputation chains are well-mixed
* Read in the trace dataset
use "imp_trace_PCA_age4_J.dta", clear

sum 

* Save the mean value to add as a line in the plot - Do this for all outcomes and exposures
sum j1100_mean
local mean_healthyPCA = r(mean)
display `mean_healthyPCA'

sum j1101_mean
local mean_processedPCA = r(mean)
display `mean_processedPCA'

sum j1102_mean
local mean_confectPCA = r(mean)
display `mean_confectPCA'

sum j1103_mean
local mean_vegPCA = r(mean)
display `mean_vegPCA'

sum d810_mean
local mean_belief = r(mean)
display `mean_belief'

sum d813_grp_mean
local mean_denom = r(mean)
display `mean_denom'

sum d816_grp_mean
local mean_attend = r(mean)
display `mean_attend'


* Convert the data from long to wide format (is necessary to create the plots)
reshape wide *mean *sd, i(iter) j(m)

* Set the iteration variable as the 'time' variable
tsset iter

* Make the plots - These all look relatively well-mixed and converged
tsline j1100_mean*, yline(`mean_healthyPCA') legend(off)
tsline j1101_mean*, yline(`mean_processedPCA') legend(off)
tsline j1102_mean*, yline(`mean_confectPCA') legend(off)
tsline j1103_mean*, yline(`mean_vegPCA') legend(off)
tsline d810_mean*, yline(`mean_belief') legend(off)
tsline d813_grp_mean*, yline(`mean_denom') legend(off)
tsline d816_grp_mean*, yline(`mean_attend') legend(off)



******************************************************************************
*** Now run the models on the imputed data and combine using Rubin's Rules
use "imp_PCA_age4_J.dta", clear

** Check descriptive stats of observed vs imputed data

* Outcome 1) Healthy PCA - Imputed data seems quite a bit lower healthy PCA values
sum j1100 if _mi_m == 0
mi estimate: mean j1100

* Specifically check the observed data vs previously-missing data in first few imputed datasets
mi convert wide

gen miss_PCA = 1
replace miss_PCA = 0 if j1100 < .
tab miss_PCA, m

sum j1100
mi estimate: mean j1100 if miss_PCA == 1

* Outcome 2) Processed PCA (imputed data a lot more processed)
sum j1101
mi estimate: mean j1101 if miss_PCA == 1

* Outcome 3) Confectionary PCA (imputed data a bit lower for confectionary)
sum j1102
mi estimate: mean j1102 if miss_PCA == 1

* Outcome 4) Vegetarian PCA (imputed data is somewhat more vegetarian)
sum j1103
mi estimate: mean j1103 if miss_PCA == 1

* Exposure 1) Belief in God (imputed have less belief and more disbelief - not sure is the same)
gen miss_belief = 1
replace miss_belief = 0 if d810 < .
tab miss_belief, m

tab d810
mi estimate: proportion d810 if miss_belief == 1

* Exposure 2) Religious affiliation (imputed more likely to have no faith and less likely to be Christian)
gen miss_denom = 1
replace miss_denom = 0 if d813_grp < .
tab miss_denom, m

tab d813_grp
mi estimate: proportion d813_grp if miss_denom == 1

* Exposure 3) Church attendance (imputed more likely to attend not at all and less likely to visit once a month or once a year)
gen miss_attend = 1
replace miss_attend = 0 if d816_grp < .
tab miss_attend, m

tab d816_grp
mi estimate: proportion d816_grp if miss_attend == 1


** Reset the data by reading in the imputed data again and making sure is in flong format
use "imp_PCA_age4_J.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist j1100 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist j1100 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist j1100 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* 4) Lapsed Christians
gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other"
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m
tab lapsed_Xian if _mi_m == 0, m

gen lapsed_CCA = 1 if _mi_m == 0
foreach var of varlist j1100 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose j_diet
postfile j_diet str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "age4_J_diet_results.dta", replace
	
* Will also save another postfile, which just has the overall p-value testing for an overall effect of the exposure on the outcome
capture postclose j_diet_p
postfile j_diet_p str20 outcome str30 exposure str5 model p_exp ///
	using "age4_J_diet_p_results.dta", replace

	
foreach var of varlist j1100-j1103 {
	
	// Save the outcome variable as a macro
	if "`var'" == "j1100" {
		local outcome = "Healthy PCA"
	}
	else if "`var'" == "j1101" {
		local outcome = "Processed PCA"
	}
	else if "`var'" == "j1102" {
		local outcome = "Confectionary PCA"
	}
	else {
		local outcome = "Vegetarian PCA"
	}
	//local outcome = "`var'"
	
	//// Exposure 1) Belief in God
	local exp = "Belief (ref = no)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 if belief_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d810 2.d810
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
	
	//// Exposure 2) Denomination affiliation
	local exp = "Denomination (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp if denom_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 3) Church attendance
	local exp = "Church attendance (ref = not at all)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp if attend_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 4) 'Lapsed' Christians
	local exp = "Belief and religion (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian if lapsed_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	mi test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_diet_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
}

postclose j_diet
postclose j_diet_p


*** Save the results testing exposure on outcome as CSV file, as easier to work with
use "age4_J_diet_p_results.dta", clear

format p_exp %9.3f

outsheet using ".\G0Mum_Age4_Results\PCA_fullResults_p.csv", comma replace


****************************************************************************
*** Now to try and make some nice plots from this data
use "age4_J_diet_results.dta", clear
format %9.3f coef-p


** Convert string vars to numeric so can order on graph

* Outcome
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Processed PCA"
replace outcome_num = 2 if outcome == "Confectionary PCA"
replace outcome_num = 3 if outcome == "Vegetarian PCA"

label define out_lb 0 "Healthy PCA" 1 "Processed PCA" 2 "Confectionary PCA" 3 "Vegetarian PCA"
label values outcome_num out_lb
tab outcome_num

* Exposure
tab exposure

gen exp_num = 0
replace exp_num = 1 if exposure == "Denomination (ref = none)"
replace exp_num = 2 if exposure == "Church attendance (ref = not a"
replace exp_num = 3 if exposure == "Belief and religion (ref = non"

label define exp_lb 0 "Belief (ref = no)" 1 "Denomination (ref = none)" 2 "Church attendance (ref = not at all)" 3 "Belief and religion (ref = none)"
label values exp_num exp_lb
tab exp_num

* Model
tab model

gen model_num = 0
replace model_num = 1 if model == "adj"
replace model_num = 2 if model == "MI"

label define model_lb 0 "Unadjusted" 1 " Adjusted" 2 "Imputed"
label values model_num model_lb
tab model_num

* Level
tab exposure level

gen level_num = 0
replace level_num = 1 if exp_num == 0 & level == "Yes"
replace level_num = 3 if exp_num == 1 & level == "Christian"
replace level_num = 4 if exp_num == 1 & level == "Other"
replace level_num = 6 if exp_num == 2 & level == "Min once a month"
replace level_num = 7 if exp_num == 2 & level == "Min once a year"
replace level_num = 9 if exp_num == 3 & level == "Christian non-believ"
replace level_num = 10 if exp_num == 3 & level == "Christian believer"
replace level_num = 11 if exp_num == 3 & level == "Other"

label define level_lb 0 "Belief in God - Not sure (ref -= No)" 1 "Belief in God - Yes (ref = No)" 3 "Religion - Christian (ref = None)" 4 "Religion - Other (ref = None)" 6 "Church attendance - Min once a month (ref = Not at all)" 7 "Church attendance - Min once a year (ref = Not at all)" 9 "Belief and religion - Christian non-believer (ref = None)" 10 "Belief and religion - Christian believer (ref = None)" 11 "Belief and religion - Other (ref = None)"
label value level_num level_lb
tab level_num

* Split apart the models for each level
gen level_split = .
replace level_split = level_num - 0.3 if model_num == 0
replace level_split = level_num if model_num == 1
replace level_split = level_num + 0.3 if model_num == 2
tab level_split


** Make 4 plots (one for each outcome), then combine together

* Healthy PCA
twoway (scatter level_split coef if outcome_num == 0 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Health-conscious""', size(medium)) ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(vsmall) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(tiny) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(healthy, replace)
	
* Processed PCA
twoway (scatter level_split coef if outcome_num == 1 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Processed""', size(medium)) ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(vsmall) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(tiny) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(process, replace)
	
* Confectionary PCA
twoway (scatter level_split coef if outcome_num == 2 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Confectionery""', size(medium)) ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(vsmall) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(tiny) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(confect, replace)
	
* Vegetarian PCA
twoway (scatter level_split coef if outcome_num == 3 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Vegetarian""', size(medium)) ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(vsmall) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(tiny) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(veg, replace)

	
** Combine graphs together with a single legend using the 'grc1leg' user-written package
*ssc install grc1leg, replace

grc1leg healthy process confect veg, imargin(tiny)

* This isn't really the nicest plot, though...
graph export ".\G0Mum_Age4_Results\Age4_J_RSBB_DietPatterns.pdf", replace


** Will save separate plots as well, as individually they look nicer

* Healthy PCA
twoway (scatter level_split coef if outcome_num == 0 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 0 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Health-conscious""') ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "(ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small))
	
graph export ".\G0Mum_Age4_Results\Age4_J_RSBB_DietPatterns_Healthy.pdf", replace

	
* Processed PCA
twoway (scatter level_split coef if outcome_num == 1 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 1 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Processed""') ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "(ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small))
	
graph export ".\G0Mum_Age4_Results\Age4_J_RSBB_DietPatterns_Processed.pdf", replace

	
* Confectionary PCA
twoway (scatter level_split coef if outcome_num == 2 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 2 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Confectionery""') ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "(ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small))
	
graph export ".\G0Mum_Age4_Results\Age4_J_RSBB_DietPatterns_Confectionary.pdf", replace

	
* Vegetarian PCA
twoway (scatter level_split coef if outcome_num == 3 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 0, ///
		horizontal col(black)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 1, ///
		horizontal col(red)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rspike lci uci level_split if outcome_num == 3 & model_num == 2, ///
		horizontal col(blue)), ///
	ysc(reverse) title(`""Vegetarian""') ytitle("") ///
	xscale(range(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "(ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small))
	
graph export ".\G0Mum_Age4_Results\Age4_J_RSBB_DietPatterns_Vegetarian.pdf", replace

graph close _all


** Also want to save out data as a table in CSV format - Also add in marker to say if 'significant' (p <= 0.05) using bonferroni corrected p-values (as four PCAs, will divide by 4)
gen bon_sig = 0
replace bon_sig = 1 if p <= 0.05 / 4
tab bon_sig, m

* Now save table (plus separate tables for each PCA)
list outcome exposure level model n-p bon_sig in 1/10, clean
outsheet outcome exposure level model n-p bon_sig using ".\G0Mum_Age4_Results\PCA_fullResults.csv", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Healthy PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\G0Mum_Age4_Results\PCA_HealthyResults.csv" if outcome == "Healthy PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Processed PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\G0Mum_Age4_Results\PCA_ProcessedResults.csv" if outcome == "Processed PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Confectionary PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\G0Mum_Age4_Results\PCA_ConfectResults.csv" if outcome == "Confectionary PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Vegetarian PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\G0Mum_Age4_Results\PCA_VegResults.csv" if outcome == "Vegetarian PCA", comma replace



*********************************************************************************
**** As seems to be some association between RSBB and diet in the G0 mums at age 4, will explore this in more detail by looking at specific nutrient intake. (note that sample size for nutrients is slightly lower than for PCAs - This is because the PCAs and nutrient intakes were calculated separately, with the PCAs based on transformed raw FFQ items (not the nutrient intakes) and allowed for small amounts of missing data)

** Read in the temp dataset which has the cleaned variables
use "RSBB_Diet_JQuest_temp.dta", clear


*** Explore/describe the nutrient data, look at correlations and see if distributions normal-ish
sum j1000-j1036

* The age 4 data contains alcohol consumption, but as did not have this for pregnancy questionnaire (and alcohol intake will be the focus of other work), will drop this variable here
drop j1000

* Correlation matrix - j1007, j1008 and j1009 (omega-3 intake, DHA and EPA intake from fish) all pretty much perfectly correlated (as all based on the same fish data), so will just use variable j1007. Some other correlations are very high, but as not prefectly correlated will leave as is
corr j1001-j1036

drop j1008 j1009

* Histograms
foreach var of varlist j1001-j1036 {
	hist `var', freq
}

** The following variables have very odd multi-modal distributions. For these variables, will include in ordinary linear regression models along with all other variables for now, but explore them in more depth aferwards to ensure that results not biased due to odd distribution

* j1003 - Daily carotene intake (three peaks at 1000, 2000 and 5000 ug)
hist j1003, freq

* j1007 - Daily omega-3 fatty acid intake (peaks at 0, 0.2 and 0.4, and not even approaching a normal distribution - Because is only based on fish questions, not whole diet)
hist j1007, freq

* j1022 - Daily retinol intake (main peak at ~500, but then another small peak at 2000)
hist j1022, freq


** Will also drop trypt60, as the sum of trypt60 and niacin (not in this dataset) these makes niacin equivalent (which will keep)
desc j1015 j1030
sum j1015 j1030
corr j1015 j1030
list j1015 j1030 in 1/10

drop j1030


*** Make a marker for each of the exposures to say whether included in CCA or not

* Exposure 1) Belief in God
gen belief_CCA_nut = 1
foreach var of varlist j1001 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA_nut = 0 if belief_CCA_nut == 1 & `var' >= .
}
tab belief_CCA_nut, m

* Exposure 2) Religious affiliation
gen denom_CCA_nut = 1
foreach var of varlist j1001 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA_nut = 0 if denom_CCA_nut == 1 & `var' >= .
}
tab denom_CCA_nut, m

* Exposure 3) Church attendance
gen attend_CCA_nut = 1
foreach var of varlist j1001 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA_nut = 0 if attend_CCA_nut == 1 & `var' >= .
}
tab attend_CCA_nut, m

* Exposure 4) Lapsed Christians
gen lapsed_CCA_nut = 1
foreach var of varlist j1001 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA_nut = 0 if lapsed_CCA_nut == 1 & `var' >= .
}
tab lapsed_CCA_nut, m


*** Next, set up a loop to cycle through each nutrient outcome and for each exposure describe the raw data, run an unadjusted analysis (on the CCA sample size), then run adjusted analyses for all nutrients
foreach var of varlist j1001-j1036 {
	
	// Exposure 1) Belief in God (no belief in God as reference category)
	oneway `var' d810 if belief_CCA_nut == 1, tab // Description of diet
	regress `var' ib3.d810 if belief_CCA_nut == 1 // Unadjusted regression model
	regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 2) Religious affiliation (none as reference category)
	oneway `var' d813_grp if denom_CCA_nut == 1, tab // Description of diet
	regress `var' i.d813_grp if denom_CCA_nut == 1 // Unadjusted regression model
	regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 3) Church attendance (not at all as reference category)
	oneway `var' d816_grp if attend_CCA_nut == 1, tab // Description of diet
	regress `var' ib3.d816_grp if attend_CCA_nut == 1 // Unadjusted regression model
	regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 4) Lapsed Christians (Atheists as reference category)
	oneway `var' lapsed_Xian if lapsed_CCA_nut == 1, tab // Description of diet
	regress `var' i.lapsed_Xian if lapsed_CCA_nut == 1 // Unadjusted regression model
	regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
}


** Check the variables with a dodgy distribution by splitting in to deciles and compare results - Overall results are pretty similar, so not likely to be any major bias caused by these oddly-distributed variables

* j1003 - Daily carotene intake (three peaks at 1000, 2000 and 5000 ug)
hist j1003, freq

sum j1003

xtile j1003_dec = j1003, nq(10)
tab j1003_dec

* Belief in God
oneway j1003 d810 if belief_CCA_nut == 1, tab
regress j1003 ib3.d810 if belief_CCA_nut == 1
regress j1003 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1003_dec d810 if belief_CCA_nut == 1, tab
regress j1003_dec ib3.d810 if belief_CCA_nut == 1
regress j1003_dec ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway j1003 d813_grp if belief_CCA_nut == 1, tab
regress j1003 i.d813_grp if belief_CCA_nut == 1
regress j1003 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1003_dec d813_grp if belief_CCA_nut == 1, tab
regress j1003_dec i.d813_grp if belief_CCA_nut == 1
regress j1003_dec i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway j1003 d816_grp if belief_CCA_nut == 1, tab
regress j1003 ib3.d816_grp if belief_CCA_nut == 1
regress j1003 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1003_dec d816_grp if belief_CCA_nut == 1, tab
regress j1003_dec ib3.d816_grp if belief_CCA_nut == 1
regress j1003_dec ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway j1003 lapsed_Xian if belief_CCA_nut == 1, tab
regress j1003 i.lapsed_Xian if belief_CCA_nut == 1
regress j1003 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1003_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress j1003_dec i.lapsed_Xian if belief_CCA_nut == 1
regress j1003_dec i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp


* j1007 - Daily omega-3 fatty acid intake (peaks at 0, 0.2 and 0.4, and not even approaching a normal distribution - Because is only based on fish questions, not whole diet)
hist j1007, freq

sum j1007

xtile j1007_dec = j1007, nq(10)
tab j1007_dec

* Belief in God
oneway j1007 d810 if belief_CCA_nut == 1, tab
regress j1007 ib3.d810 if belief_CCA_nut == 1
regress j1007 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1007_dec d810 if belief_CCA_nut == 1, tab
regress j1007_dec ib3.d810 if belief_CCA_nut == 1
regress j1007_dec ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway j1007 d813_grp if belief_CCA_nut == 1, tab
regress j1007 i.d813_grp if belief_CCA_nut == 1
regress j1007 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1007_dec d813_grp if belief_CCA_nut == 1, tab
regress j1007_dec i.d813_grp if belief_CCA_nut == 1
regress j1007_dec i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway j1007 d816_grp if belief_CCA_nut == 1, tab
regress j1007 ib3.d816_grp if belief_CCA_nut == 1
regress j1007 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1007_dec d816_grp if belief_CCA_nut == 1, tab
regress j1007_dec ib3.d816_grp if belief_CCA_nut == 1
regress j1007_dec ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway j1007 lapsed_Xian if belief_CCA_nut == 1, tab
regress j1007 i.lapsed_Xian if belief_CCA_nut == 1
regress j1007 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1007_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress j1007_dec i.lapsed_Xian if belief_CCA_nut == 1
regress j1007_dec i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp


* j1022 - Daily retinol intake (main peak at ~500, but then another small peak at 2000)
hist j1022, freq

sum j1022

xtile j1022_dec = j1022, nq(10)
tab j1022_dec

* Belief in God
oneway j1022 d810 if belief_CCA_nut == 1, tab
regress j1022 ib3.d810 if belief_CCA_nut == 1
regress j1022 ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1022_dec d810 if belief_CCA_nut == 1, tab
regress j1022_dec ib3.d810 if belief_CCA_nut == 1
regress j1022_dec ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway j1022 d813_grp if belief_CCA_nut == 1, tab
regress j1022 i.d813_grp if belief_CCA_nut == 1
regress j1022 i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1022_dec d813_grp if belief_CCA_nut == 1, tab
regress j1022_dec i.d813_grp if belief_CCA_nut == 1
regress j1022_dec i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway j1022 d816_grp if belief_CCA_nut == 1, tab
regress j1022 ib3.d816_grp if belief_CCA_nut == 1
regress j1022 ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1022_dec d816_grp if belief_CCA_nut == 1, tab
regress j1022_dec ib3.d816_grp if belief_CCA_nut == 1
regress j1022_dec ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway j1022 lapsed_Xian if belief_CCA_nut == 1, tab
regress j1022 i.lapsed_Xian if belief_CCA_nut == 1
regress j1022 i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway j1022_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress j1022_dec i.lapsed_Xian if belief_CCA_nut == 1
regress j1022_dec i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp



**********************************************************************************
*** As lots of missing data, will do MI for nutrients. However, as there are such a large number of nutrients I will only impute exposures and covariates, not missing nutrient data, else with over 30 nutrients, plus exposures, covariates and auxiliary variables the imputation would take a very long time (with the MI on dietary patterns, even though there were differences between observed and imputed data, the overall results of the regression models did not materially change, suggesting that it's not necessary to impute the missing nutrient data).

* Drop variables not included in MI to speed up imputation and make file much smaller - Will still include later RSBB data to try and impute any of the missing pregnancy RSBB vars. Ditto for auxiliary SEP variables (partner education and occupational social class)
keep aln j1001-j1036 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

* Drop cases to only included those with non-missing nutrient data
drop if j1001 == .

* Check missing data
misstable sum j1001 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp, all

misstable patterns j1001 d810 d813_grp d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp, freq

** Set up the imputation and register the variables with missing data
mi set mlong
mi register regular j1001-j1036 j912
mi register imputed d810 d813_grp d816_grp c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Test the imputation with a dry run first, to make sure looks okay
mi impute chained ///
	(pmm, knn(5)) b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp ///
	= j1001-j1036 j912, ///
	add(10) burnin(10) rseed(56789) dryrun
	
	
** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about 2 hours. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010). As convergence okay for more complicated PCA imputation, wont check again here
mi impute chained ///
	(pmm, knn(5)) b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp ///
	= j1001-j1036 j912, ///
	add(50) burnin(10) rseed(56789) dots augment


** Save this imputed dataset, so not have to run whole imputation again to access results
save "imp_nutrients_age4_J.dta", replace


** Read in the imputed data (if skipping imputation) and make sure is in flong format
use "imp_nutrients_age4_J.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* 4) Lapsed Christians
gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other", replace
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m
tab lapsed_Xian if _mi_m == 0, m

gen lapsed_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose j_nut
postfile j_nut str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "age4_J_nutrient_results.dta", replace
	
* Will also save another postfile, which just has the overall p-value testing for an overall effect of the exposure on the outcome
capture postclose j_nut_p
postfile j_nut_p str20 outcome str30 exposure str5 model p_exp ///
	using "age4_J_nutrient_p_results.dta", replace

	
foreach var of varlist j1001-j1036 {
	
	// Save the outcome variable as a macro
	if "`var'" == "j1001" {
		local outcome = "Calcium (mg)"
	}
	else if "`var'" == "j1002" {
		local outcome = "Carbohydrate (g)"
	}
	else if "`var'" == "j1003" {
		local outcome = "Carotene (ug)"
	}
	else if "`var'" == "j1004" {
		local outcome = "Cholesterol (mg)"
	}
	else if "`var'" == "j1005" {
		local outcome = "Energy (kJ)"
	}
	else if "`var'" == "j1006" {
		local outcome = "Fat (g)"
	}
	else if "`var'" == "j1007" {
		local outcome = "Omega-3 (g)"
	}
	else if "`var'" == "j1010" {
		local outcome = "Folate (ug)"
	}
	else if "`var'" == "j1011" {
		local outcome = "Iodine (ug)"
	}
	else if "`var'" == "j1012" {
		local outcome = "Iron (mg)"
	}
	else if "`var'" == "j1013" {
		local outcome = "Magnesium (mg)"
	}
	else if "`var'" == "j1014" {
		local outcome = "Monounsaturated fat (mg)"
	}
	else if "`var'" == "j1015" {
		local outcome = "Niacin equivalent (mg)"
	}
	else if "`var'" == "j1016" {
		local outcome = "Non-milk sugars (g)"
	}
	else if "`var'" == "j1017" {
		local outcome = "Fibre (g)"
	}
	else if "`var'" == "j1018" {
		local outcome = "Phosphorus (mg)"
	}
	else if "`var'" == "j1019" {
		local outcome = "Polyunsaturated fat (g)"
	}
	else if "`var'" == "j1020" {
		local outcome = "Potassium (mg)"
	}
	else if "`var'" == "j1021" {
		local outcome = "Protein (g)"
	}
	else if "`var'" == "j1022" {
		local outcome = "Retinol (ug)"
	}
	else if "`var'" == "j1023" {
		local outcome = "Riboflavin (mg)"
	}
	else if "`var'" == "j1024" {
		local outcome = "Selenium (ug)"
	}
	else if "`var'" == "j1025" {
		local outcome = "Saturated fat (g)"
	}
	else if "`var'" == "j1026" {
		local outcome = "Sodium (mg)"
	}
	else if "`var'" == "j1027" {
		local outcome = "Starch (g)"
	}
	else if "`var'" == "j1028" {
		local outcome = "Sugar (g)"
	}
	else if "`var'" == "j1029" {
		local outcome = "Thiamin (mg)"
	}
	else if "`var'" == "j1031" {
		local outcome = "Vitamin C (mg)"
	}
	else if "`var'" == "j1032" {
		local outcome = "Vitamin B6 (mg)"
	}
	else if "`var'" == "j1033" {
		local outcome = "Vitamin B12 (ug)"
	}
	else if "`var'" == "j1034" {
		local outcome = "Vitamin D (ug)"
	}
	else if "`var'" == "j1035" {
		local outcome = "Vitamin E (mg)"
	}
	else {
		local outcome = "Zinc (mg)"
	}
	//local outcome = "`var'"
	
	//// Exposure 1) Belief in God
	local exp = "Belief (ref = no)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 if belief_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
	
	//// Exposure 2) Denomination affiliation
	local exp = "Denomination (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp if denom_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 3) Church attendance
	local exp = "Church attendance (ref = not at all)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp if attend_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	mi test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 4) 'Lapsed' Christians
	local exp = "Belief and religion (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian if lapsed_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
}

postclose j_nut
postclose j_nut_p


*** Save the results testing exposure on outcome as CSV file, as easier to work with (split by exposure, as this is how SI is arranged, and only include the MI results)
use "age4_J_nutrient_p_results.dta", clear

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Energy (kJ)"
replace outcome_num = 2 if outcome == "Carbohydrate (g)"
replace outcome_num = 3 if outcome == "Sugar (g)"
replace outcome_num = 4 if outcome == "Non-milk sugars (g)"
replace outcome_num = 5 if outcome == "Starch (g)"
replace outcome_num = 6 if outcome == "Fibre (g)"
replace outcome_num = 7 if outcome == "Fat (g)"
replace outcome_num = 8 if outcome == "Monounsaturated fat "
replace outcome_num = 9 if outcome == "Polyunsaturated fat "
replace outcome_num = 10 if outcome == "Saturated fat (g)"
replace outcome_num = 11 if outcome == "Omega-3 (g)"
replace outcome_num = 12 if outcome == "Cholesterol (mg)"
replace outcome_num = 13 if outcome == "Protein (g)"
replace outcome_num = 14 if outcome == "Thiamin (mg)"
replace outcome_num = 15 if outcome == "Riboflavin (mg)"
replace outcome_num = 16 if outcome == "Niacin equivalent (m"
replace outcome_num = 17 if outcome == "Vitamin B6 (mg)"
replace outcome_num = 18 if outcome == "Vitamin B12 (ug)"
replace outcome_num = 19 if outcome == "Folate (ug)"
replace outcome_num = 20 if outcome == "Vitamin C (mg)"
replace outcome_num = 21 if outcome == "Retinol (ug)"
replace outcome_num = 22 if outcome == "Carotene (ug)"
replace outcome_num = 23 if outcome == "Vitamin D (ug)"
replace outcome_num = 24 if outcome == "Vitamin E (mg)"
replace outcome_num = 25 if outcome == "Calcium (mg)"
replace outcome_num = 26 if outcome == "Phosphorus (mg)"
replace outcome_num = 27 if outcome == "Magnesium (mg)"
replace outcome_num = 28 if outcome == "Sodium (mg)"
replace outcome_num = 29 if outcome == "Potassium (mg)"
replace outcome_num = 30 if outcome == "Iron (mg)"
replace outcome_num = 31 if outcome == "Zinc (mg)"
replace outcome_num = 32 if outcome == "Selenium (ug)"
replace outcome_num = 33 if outcome == "Iodine (ug)"

label define out_lb 1 "Energy (kJ)" 2 "Carbohydrates (g)" 3 "Sugars (g)" 4 "Free sugars (g)" 5 "Starch (g)" 6 "NSP (Fibre; g)" 7 "Fat (g)" 8 "Monounsaturated fat (g)" 9 "Polyunsaturated fat (g)" 10 "Saturated fat (g)" 11 "Omega-3 (g; from fish)" 12 "Cholesterol (mg)" 13 "Protein (g)" 14 "Thiamin (mg)" 15 "Riboflavin (mg)" 16 "Niacin equivalent (mg)" 17 "Vitamin B6 (mg)" 18 "Vitamin B12 (ug)" 19 "Folate (ug)" 20 "Vitamin C (mg)" 21 "Retinol/Vitamin A (ug)" 22 "Carotene (ug)" 23 "Vitamin D (ug)" 24 "Vitamin E (mg)" 25 "Calcium (mg)" 26 "Phosphorus (mg)" 27 "Magnesium (mg)" 28 "Sodium (mg)" 29 "Potassium (mg)" 30 "Iron (mg)" 31 "Zinc (mg)" 32 "Selenium (ug)" 33 "Iodine (ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove

sort outcome_num
drop outcome_num

format p_exp %9.3f

outsheet using ".\G0Mum_Age4_Results\nut_belief_p.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_religion_p.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_attend_p.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_belief_relig_p.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace



********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "age4_J_nutrient_results.dta", clear
format %9.3f coef-p

* Transform the p-value by -log10 to get p-values on similar order of magnitude to be readable (0 = p-value of 1)
gen logp = -log10(p)
sum logp

* What's the Bonferroni correction here? Are 33 outcomes assessed, so 0.05 / 33 = 0.0015 (and -log10 of this is 2.82) - As nutrients not independent from one another perhaps the Bonferroni correction is too conservative, but will go with it for now.

** Plot -log10 of each variable and see if any reach Bonferroni significance (will use imputed results for this)

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Energy (kJ)"
replace outcome_num = 2 if outcome == "Carbohydrate (g)"
replace outcome_num = 3 if outcome == "Sugar (g)"
replace outcome_num = 4 if outcome == "Non-milk sugars (g)"
replace outcome_num = 5 if outcome == "Starch (g)"
replace outcome_num = 6 if outcome == "Fibre (g)"
replace outcome_num = 7 if outcome == "Fat (g)"
replace outcome_num = 8 if outcome == "Monounsaturated fat "
replace outcome_num = 9 if outcome == "Polyunsaturated fat "
replace outcome_num = 10 if outcome == "Saturated fat (g)"
replace outcome_num = 11 if outcome == "Omega-3 (g)"
replace outcome_num = 12 if outcome == "Cholesterol (mg)"
replace outcome_num = 13 if outcome == "Protein (g)"
replace outcome_num = 14 if outcome == "Thiamin (mg)"
replace outcome_num = 15 if outcome == "Riboflavin (mg)"
replace outcome_num = 16 if outcome == "Niacin equivalent (m"
replace outcome_num = 17 if outcome == "Vitamin B6 (mg)"
replace outcome_num = 18 if outcome == "Vitamin B12 (ug)"
replace outcome_num = 19 if outcome == "Folate (ug)"
replace outcome_num = 20 if outcome == "Vitamin C (mg)"
replace outcome_num = 21 if outcome == "Retinol (ug)"
replace outcome_num = 22 if outcome == "Carotene (ug)"
replace outcome_num = 23 if outcome == "Vitamin D (ug)"
replace outcome_num = 24 if outcome == "Vitamin E (mg)"
replace outcome_num = 25 if outcome == "Calcium (mg)"
replace outcome_num = 26 if outcome == "Phosphorus (mg)"
replace outcome_num = 27 if outcome == "Magnesium (mg)"
replace outcome_num = 28 if outcome == "Sodium (mg)"
replace outcome_num = 29 if outcome == "Potassium (mg)"
replace outcome_num = 30 if outcome == "Iron (mg)"
replace outcome_num = 31 if outcome == "Zinc (mg)"
replace outcome_num = 32 if outcome == "Selenium (ug)"
replace outcome_num = 33 if outcome == "Iodine (ug)"

label define out_lb 1 "Energy (kJ)" 2 "Carbohydrates (g)" 3 "Total sugars (g)" 4 "Free sugars (g)" 5 "Starch (g)" 6 "NSP (Fibre; g)" 7 "Fat (g)" 8 "Monounsaturated fat (g)" 9 "Polyunsaturated fat (g)" 10 "Saturated fat (g)" 11 "Omega-3 (g; from fish)" 12 "Cholesterol (mg)" 13 "Protein (g)" 14 "Thiamin (mg)" 15 "Riboflavin (mg)" 16 "Niacin equivalent (mg)" 17 "Vitamin B6 (mg)" 18 "Vitamin B12 (ug)" 19 "Folate (ug)" 20 "Vitamin C (mg)" 21 "Retinol/Vitamin A (ug)" 22 "Carotene (ug)" 23 "Vitamin D (ug)" 24 "Vitamin E (mg)" 25 "Calcium (mg)" 26 "Phosphorus (mg)" 27 "Magnesium (mg)" 28 "Sodium (mg)" 29 "Potassium (mg)" 30 "Iron (mg)" 31 "Zinc (mg)" 32 "Selenium (ug)" 33 "Iodine (ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove


*** Now make the plots and explore how RSBB is related to nutrient intake

** Starting with belief in God
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief in God (ref = no)") ///
	legend(order(1 "Not sure" 2 "Yes"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_BeliefInGod_pvalues.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome_num exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/33
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_belief_bon.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\G0Mum_Age4_Results\nut_belief_05.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\G0Mum_Age4_Results\nut_belief_full.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


** Next, religious affiliation
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(vsmall) angle(0)) ///
	title("Religious Affiliation (ref = none)") ///
	legend(order(1 "Christian" 2 "Other"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_Religion_pvalues.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/33
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_religion_bon.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\G0Mum_Age4_Results\nut_religion_05.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\G0Mum_Age4_Results\nut_religion_full.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


* Next to church attendance
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(vsmall) angle(0)) ///
	title("Church Attendance (ref = not at all)") ///
	legend(order(1 "Min once a month" 2 "Min once a year"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_ChurchAttendance_pvalues.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/33
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_attend_bon.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\G0Mum_Age4_Results\nut_attend_05.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\G0Mum_Age4_Results\nut_attend_full.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


* And finally belief and relgion
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief and religion (ref = none)") ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other"))
		
graph export ".\G0Mum_Age4_Results\Nutrients_BeliefAndReligion_pvalues.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/33
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_belief_relig_bon.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\G0Mum_Age4_Results\nut_belief_relig_05.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\G0Mum_Age4_Results\nut_belief_relig_full.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


* Combining plots together
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief in God (ref = no)", size(small)) ///
	legend(order(1 "Not sure" 2 "Yes") size(vsmall)) ///
	name(belief, replace)

local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Religious Affiliation (ref = none)", size(small)) ///
	legend(order(1 "Christian" 2 "Other") size(vsmall)) ///
	name(relig, replace)
	
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Church Attendance (ref = not at all)", size(small)) ///
	legend(order(1 "Min once a month" 2 "Min once a year") size(vsmall)) ///
	name(attend, replace)
	
local bon_threshold = -log10(0.05/33)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ///
	ytitle("") ysc(reverse) ///
	ylabel(1(1)33, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief and religion (ref = none)", size(small)) ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other") ///
	cols(3) size(vsmall)) ///
	name(belief_relig, replace)

graph combine belief relig attend belief_relig, imargin(0 0 0 0)
graph export ".\G0Mum_Age4_Results\Nutrients_combined_pvalues.pdf", replace	

graph close _all



***********************************************************************************
*** But are these differences in nutrient intake meaningful? There may be mean differences, but if everyone is getting enough nutrients, then there might not be any harm/impact on health. Ideally want to split into groups such as 'safe vs harmful' or 'meeting RNIs vs not' and see if results hold.

*** Have put together nutritional requirement intakes based on Scientific Advisory Committee on Nutrition (SACN) 2016 recommendations (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/618167/government_dietary_recommendations.pdf) and 1991 Committee on Medical Aspects of Food Policy (COMA) report (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/743790/Dietary_Reference_Values_-_A_Guide__1991_.pdf). Based on average adult female recommendations (age 19-49), plus extra intake if pregnant

*** Will go through each nutrient and code as whether meeting Recommended Nutrient Intakes (RNI) or Estimated Average Requirements (EAR), or whether consuming less/more than recommended min/max intakes

** Read in the imputed nutrient data and make sure is in flong format
use "imp_nutrients_age4_J.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* 4) Lapsed Christians
gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other"
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m
tab lapsed_Xian if _mi_m == 0, m

gen lapsed_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now go through each nutrient and code as binary variable whether meets recommended daily intakes

** Calcium (mg) - RNI of 700mg for adult women
gen calc_RNI = .
replace calc_RNI = 1 if j1001 < 700
replace calc_RNI = 0 if j1001 >= 700 & j1001 < .
tab calc_RNI if _mi_m == 0, m

label define calc_lb 0 ">= RNI (700mg)" 1 "< RNI (700mg)"
numlabel calc_lb, add
label values calc_RNI calc_lb
tab calc_RNI if _mi_m == 0, m

** Carbohydrate (g) - Minimum 267g for adult women
gen carb_min = .
replace carb_min = 1 if j1002 < 267
replace carb_min = 0 if j1002 >= 267 & j1002 < .
tab carb_min if _mi_m == 0, m

label define carb_lb 0 ">= min (267g)" 1 "< min (267mg)"
numlabel carb_lb, add
label values carb_min carb_lb
tab carb_min if _mi_m == 0, m

** Carotene (ug) - No recommendations for carotene

** Cholesterol (mg) - No recommendations for cholesterol

** Energy (kJ) - Estimated average requirements 8,400kJ for adult women (+800 for pregnancy)
gen energy_EAR = .
replace energy_EAR = 1 if j1005 < 8400
replace energy_EAR = 0 if j1005 >= 8400 & j1005 < .
tab energy_EAR if _mi_m == 0, m

label define energy_lb 0 ">= EAR (8400kJ)" 1 "< EAR (8400kJ)"
numlabel energy_lb, add
label values energy_EAR energy_lb
tab energy_EAR if _mi_m == 0, m

** Fat (g) - Maximum 78g for adult women
gen fat_max = .
replace fat_max = 1 if j1006 > 78 & j1006 < .
replace fat_max = 0 if j1006 <= 78
tab fat_max if _mi_m == 0, m

label define fat_lb 0 "<= max (78g)" 1 "> max (78g)"
numlabel fat_lb, add
label values fat_max fat_lb
tab fat_max if _mi_m == 0, m

** Omega-3 fatty acid from fish only (g) - No RNI, but more omega-3 recommended (esp. in pregnancy). As of potential importance, will code into low (<0.25g/day) and high (>0.25g/day) as this is where there is a natural split in the data for pregnancy omega-3 data (but not at age 4; but important to bear in mind that this is not based on any formal RNI recommendations, and omega-3 is only counted for fish, not sources from other foods)
sum j1007 if j1007 > 0.2 & j1007 < 0.25 & _mi_m == 0

gen omega3 = .
replace omega3 = 1 if j1007 < 0.25
replace omega3 = 0 if j1007 >= 0.25 & j1007 < .
tab omega3 if _mi_m == 0, m

label define omega3_lb 0 "High (>0.25g)" 1 "Low (<0.25g)"
numlabel omega3_lb, add
label values omega3 omega3_lb
tab omega3 if _mi_m == 0, m

** Folate (ug) - RNI of 200ug for adult women (+100 for pregnanct women)
gen folate_RNI = .
replace folate_RNI = 1 if j1010 < 200
replace folate_RNI = 0 if j1010 >= 200 & j1010 < .
tab folate_RNI if _mi_m == 0, m

label define folate_lb 0 ">= RNI (200ug)" 1 "< RNI (200ug)"
numlabel folate_lb, add
label values folate_RNI folate_lb
tab folate_RNI if _mi_m == 0, m

** Iodine (ug) - RNI of 140ug for adult women
gen iodine_RNI = .
replace iodine_RNI = 1 if j1011 < 140
replace iodine_RNI = 0 if j1011 >= 140 & j1011 < .
tab iodine_RNI if _mi_m == 0, m

label define iodine_lb 0 ">= RNI (140ug)" 1 "< RNI (140ug)"
numlabel iodine_lb, add
label values iodine_RNI iodine_lb
tab iodine_RNI if _mi_m == 0, m

** Iron (mg) - RNI of 14.8mg for adult women
gen iron_RNI = .
replace iron_RNI = 1 if j1012 < 14.8
replace iron_RNI = 0 if j1012 >= 14.8 & j1012 < .
tab iron_RNI if _mi_m == 0, m

label define iron_lb 0 ">= RNI (14.8mg)" 1 "< RNI (14.8mg)"
numlabel iron_lb, add
label values iron_RNI iron_lb
tab iron_RNI if _mi_m == 0, m

** Magnesium (mg) - RNI of 270mg for adult women
gen mag_RNI = .
replace mag_RNI = 1 if j1013 < 270
replace mag_RNI = 0 if j1013 >= 270 & j1013 < .
tab mag_RNI if _mi_m == 0, m

label define mag_lb 0 ">= RNI (270mg)" 1 "< RNI (270mg)"
numlabel mag_lb, add
label values mag_RNI mag_lb
tab mag_RNI if _mi_m == 0, m

** Mono-unsaturated fat (g) - RNI of 29g for adult women
gen mono_RNI = .
replace mono_RNI = 1 if j1014 < 29
replace mono_RNI = 0 if j1014 >= 29 & j1014 < .
tab mono_RNI if _mi_m == 0, m

label define mono_lb 0 ">= RNI (29g)" 1 "< RNI (29g)"
numlabel mono_lb, add
label values mono_RNI mono_lb
tab mono_RNI if _mi_m == 0, m

** Niacin equivalent (mg; niacin + tyrpt/60) - RNI of 13.2mg for adult women
gen niacinEq_RNI = .
replace niacinEq_RNI = 1 if j1015 < 13.2
replace niacinEq_RNI = 0 if j1015 >= 13.2 & j1015 < .
tab niacinEq_RNI if _mi_m == 0, m

label define niacinEq_lb 0 ">= RNI (13.2mg)" 1 "< RNI (13.2mg)"
numlabel niacinEq_lb, add
label values niacinEq_RNI niacinEq_lb
tab niacinEq_RNI if _mi_m == 0, m

** Non-milk extrinsic sugars (g; 'free sugars') - Maximum 27g for adult women
gen sugar_max = .
replace sugar_max = 1 if j1016 > 27
replace sugar_max = 0 if j1016 <= 27 & j1016 < .
tab sugar_max if _mi_m == 0, m

label define sugar_lb 0 "<= max (27g)" 1 "> max (27g)"
numlabel sugar_lb, add
label values sugar_max sugar_lb
tab sugar_max if _mi_m == 0, m

** Fibre (g) - RNI of 22.5g for adult women
gen fibre_RNI = .
replace fibre_RNI = 1 if j1017 < 22.5
replace fibre_RNI = 0 if j1017 >= 22.5 & j1017 < .
tab fibre_RNI if _mi_m == 0, m

label define fibre_lb 0 ">= RNI (22.5g)" 1 "< RNI (22.5g)"
numlabel fibre_lb, add
label values fibre_RNI fibre_lb
tab fibre_RNI if _mi_m == 0, m

** Phosphorous (mg) - RNI of 550mg for adult women
gen phos_RNI = .
replace phos_RNI = 1 if j1018 < 550
replace phos_RNI = 0 if j1018 >= 550 & j1018 < .
tab phos_RNI if _mi_m == 0, m

label define phos_lb 0 ">= RNI (550mg)" 1 "< RNI (550mg)"
numlabel phos_lb, add
label values phos_RNI phos_lb
tab phos_RNI if _mi_m == 0, m

** Poly-unsaturated fat (g) - RNI of 14g for adult women
gen poly_RNI = .
replace poly_RNI = 1 if j1019 < 14
replace poly_RNI = 0 if j1019 >= 14 & j1019 < .
tab poly_RNI if _mi_m == 0, m

label define poly_lb 0 ">= RNI (14g)" 1 "< RNI (14g)"
numlabel poly_lb, add
label values poly_RNI poly_lb
tab poly_RNI if _mi_m == 0, m

** Potassium (mg) - RNI of 3500mg for adult women
gen pot_RNI = .
replace pot_RNI = 1 if j1020 < 3500
replace pot_RNI = 0 if j1020 >= 3500 & j1020 < .
tab pot_RNI if _mi_m == 0, m

label define pot_lb 0 ">= RNI (3500mg)" 1 "< RNI (3500mg)"
numlabel pot_lb, add
label values pot_RNI pot_lb
tab pot_RNI if _mi_m == 0, m

** Protein (g) - RNI of 45.0g for adult women (+6 for pregnancy)
gen prot_RNI = .
replace prot_RNI = 1 if j1021 < 45
replace prot_RNI = 0 if j1021 >= 45 & j1021 < .
tab prot_RNI if _mi_m == 0, m

label define prot_lb 0 ">= RNI (45g)" 1 "< RNI (45g)"
numlabel prot_lb, add
label values prot_RNI prot_lb
tab prot_RNI if _mi_m == 0, m

** Retinol/Vitamin A (ug) - RNI of 600ug for adult women (+100 for pregnancy)
gen ret_RNI = .
replace ret_RNI = 1 if j1022 < 600
replace ret_RNI = 0 if j1022 >= 600 & j1022 < .
tab ret_RNI if _mi_m == 0, m

label define ret_lb 0 ">= RNI (600ug)" 1 "< RNI (600ug)"
numlabel ret_lb, add
label values ret_RNI ret_lb
tab ret_RNI if _mi_m == 0, m

** Riboflavin (mg) - RNI of 1.1mg for adult women (+0.3 for pregnancy)
gen ribo_RNI = .
replace ribo_RNI = 1 if j1023 < 1.1
replace ribo_RNI = 0 if j1023 >= 1.1 & j1023 < .
tab ribo_RNI if _mi_m == 0, m

label define ribo_lb 0 ">= RNI (1.1mg)" 1 "< RNI (1.1mg)"
numlabel ribo_lb, add
label values ribo_RNI ribo_lb
tab ribo_RNI if _mi_m == 0, m

** Saturated fat (g) - Maximum 24g for adult women
gen sat_max = .
replace sat_max = 1 if j1025 > 24 & j1025 < .
replace sat_max = 0 if j1025 <= 24
tab sat_max if _mi_m == 0, m

label define sat_lb 0 "<= max (24g)" 1 "> max (24g)"
numlabel sat_lb, add
label values sat_max sat_lb
tab sat_max if _mi_m == 0, m

** Selenium (ug) - RNI of 60ug for adult women
gen selen_RNI = .
replace selen_RNI = 1 if j1024 < 60
replace selen_RNI = 0 if j1024 >= 60 & j1024 < .
tab selen_RNI if _mi_m == 0, m

label define selen_lb 0 ">= RNI (60ug)" 1 "< RNI (60ug)"
numlabel selen_lb, add
label values selen_RNI selen_lb
tab selen_RNI if _mi_m == 0, m

** Sodium (mg) - RNI of 1600mg for adult women, but recommended max intake of 2400mg (6g salt)
gen sodium_RNI = .
replace sodium_RNI = 1 if (j1026 < 1600 | j1026 > 2400) & j1026 < .
replace sodium_RNI = 0 if j1026 >= 1600 & j1026 <= 2400
tab sodium_RNI if _mi_m == 0, m

label define sodium_lb 0 ">= RNI (1600mg) & < max (2400mg)" 1 "< RNI (1600mg) OR > max (2400mg)"
numlabel sodium_lb, add
label values sodium_RNI sodium_lb
tab sodium_RNI if _mi_m == 0, m

** Starch (g) - No recommendations for starch

** Total sugar (g) - No recommendations for total sugar

** Thiamin (mg) - RNI of 0.8mg for adult women (+0.1 for pregnancy)
gen thiamin_RNI = .
replace thiamin_RNI = 1 if j1029 < 0.8
replace thiamin_RNI = 0 if j1029 >= 0.8 & j1029 < .
tab thiamin_RNI if _mi_m == 0, m

label define thiamin_lb 0 ">= RNI (0.8mg)" 1 "< RNI (0.8mg)"
numlabel thiamin_lb, add
label values thiamin_RNI thiamin_lb
tab thiamin_RNI if _mi_m == 0, m

** Vitamin C (mg) - RNI of 40mg for adult women (+10 for pregnancy)
gen vitC_RNI = .
replace vitC_RNI = 1 if j1031 < 40
replace vitC_RNI = 0 if j1031 >= 40 & j1031 < .
tab vitC_RNI if _mi_m == 0, m

label define vitC_lb 0 ">= RNI (40mg)" 1 "< RNI (40mg)"
numlabel vitC_lb, add
label values vitC_RNI vitC_lb
tab vitC_RNI if _mi_m == 0, m

** Vitamin B6 (mg) - RNI of 1.2mg for adult women
gen vitB6_RNI = .
replace vitB6_RNI = 1 if j1032 < 1.2
replace vitB6_RNI = 0 if j1032 >= 1.2 & j1032 < .
tab vitB6_RNI if _mi_m == 0, m

label define vitB6_lb 0 ">= RNI (1.2mg)" 1 "< RNI (1.2mg)"
numlabel vitB6_lb, add
label values vitB6_RNI vitB6_lb
tab vitB6_RNI if _mi_m == 0, m

** Vitamin B12 (ug) - RNI of 1.5ug for adult women
gen vitB12_RNI = .
replace vitB12_RNI = 1 if j1033 < 1.5
replace vitB12_RNI = 0 if j1033 >= 1.5 & j1033 < .
tab vitB12_RNI if _mi_m == 0, m

label define vitB12_lb 0 ">= RNI (1.5ug)" 1 "< RNI (1.5ug)"
numlabel vitB12_lb, add
label values vitB12_RNI vitB12_lb
tab vitB12_RNI if _mi_m == 0, m

** Vitamin D (ug) - RNI of 10ug for adult women
gen vitD_RNI = .
replace vitD_RNI = 1 if j1034 < 10
replace vitD_RNI = 0 if j1034 >= 10 & j1034 < .
tab vitD_RNI if _mi_m == 0, m

label define vitD_lb 0 ">= RNI (10ug)" 1 "< RNI (10ug)"
numlabel vitD_lb, add
label values vitD_RNI vitD_lb
tab vitD_RNI if _mi_m == 0, m

** Vitamin E (mg) - Minimum of 3ug for adult women (+0.8 for pregnancy)
gen vitE_min = .
replace vitE_min = 1 if j1035 < 3
replace vitE_min = 0 if j1035 >= 3 & j1035 < .
tab vitE_min if _mi_m == 0, m

label define vitE_lb 0 ">= min (3ug)" 1 "< min (3ug)"
numlabel vitE_lb, add
label values vitE_min vitE_lb
tab vitE_min if _mi_m == 0, m

** Zinc (mg) - RNI of 7mg for adult women
gen zinc_RNI = .
replace zinc_RNI = 1 if j1036 < 7
replace zinc_RNI = 0 if j1036 >= 7 & j1036 < .
tab zinc_RNI if _mi_m == 0, m

label define zinc_lb 0 ">= RNI (7mg)" 1 "< RNI (7mg)"
numlabel zinc_lb, add
label values zinc_RNI zinc_lb
tab zinc_RNI if _mi_m == 0, m


*** Check this data, then run through same analyses as above exploring nutrient intake by RSBB
sum calc_RNI-zinc_RNI

* First, save summary stats of both raw nutrient intake and whether meeting RNIs. Store this data so easy to copy into table (install estout package if necessary)
*ssc install estout, replace
estpost summarize j1001-j1036 if _mi_m == 0
esttab . using ".\G0Mum_Age4_Results\MumAge4_nutrition_SumStats.csv", cells("count mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f))") nomtitle nonumber noobs replace

estpost summarize calc_RNI-zinc_RNI if _mi_m == 0
esttab . using ".\G0Mum_Age4_Results\MumAge4_nutritionRNI_SumStats.csv", cells("count sum mean(fmt(%9.3f))") nomtitle nonumber noobs replace


* As so few people above the RNI threshold for Vitamin D, will drop this variable (for power reasons, and because it causes the imputations below to crash as collinear cases are dropped)
drop vitD_RNI


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file if it's already open)
capture postclose j_nut_RNI
postfile j_nut_RNI str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "age4_J_nutrient_RNI_results.dta", replace
	
* Will also save another postfile, which just has the overall p-value testing for an overall effect of the exposure on the outcome
capture postclose j_nut_RNI_p
postfile j_nut_RNI_p str20 outcome str30 exposure str5 model p_exp ///
	using "age4_J_nutrient_RNI_p_results.dta", replace

	
foreach var of varlist calc_RNI-zinc_RNI {
	
	// Save the outcome variable as a macro
	local outcome = "`var'"
	
	//// Exposure 1) Belief in God
	local exp = "Belief (ref = no)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d810 if belief_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable - For some reason the 'mi estimate: logistic' command gives the output in log-odds, rather than odds ratios, so need to manually construct these from the log-odds coefficient, the log SE and the critical value
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef_log = res[1,1]
	local se = res[2,1]
	local crit = res[8,1]
	local p = res[4,1]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
	
	//// Exposure 2) Denomination affiliation
	local exp = "Denomination (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.d813_grp if denom_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 3) Church attendance
	local exp = "Church attendance (ref = not at all)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d816_grp if attend_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef_log = res[1,1]
	local se = res[2,1]
	local crit = res[8,1]
	local p = res[4,1]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 4) 'Lapsed' Christians
	local exp = "Belief and religion (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.lapsed_Xian if lapsed_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,4]
	local se = res[2,4]
	local crit = res[8,4]
	local p = res[4,4]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p ("`outcome'") ("`exp'") ("`model'") (`p_exp')
}

postclose j_nut_RNI
postclose j_nut_RNI_p


*** Save the results testing exposure on outcome as CSV file, as easier to work with (split by exposure, as this is how SI is arranged, and only include the MI results)
use "age4_J_nutrient_RNI_p_results.dta", clear

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "energy_EAR"
replace outcome_num = 2 if outcome == "carb_min"
replace outcome_num = 3 if outcome == "sugar_max"
replace outcome_num = 4 if outcome == "fibre_RNI"
replace outcome_num = 5 if outcome == "fat_max"
replace outcome_num = 6 if outcome == "mono_RNI"
replace outcome_num = 7 if outcome == "poly_RNI"
replace outcome_num = 8 if outcome == "sat_max"
replace outcome_num = 9 if outcome == "omega3"
replace outcome_num = 10 if outcome == "prot_RNI"
replace outcome_num = 11 if outcome == "thiamin_RNI"
replace outcome_num = 12 if outcome == "ribo_RNI"
replace outcome_num = 13 if outcome == "niacinEq_RNI"
replace outcome_num = 14 if outcome == "vitB6_RNI"
replace outcome_num = 15 if outcome == "vitB12_RNI"
replace outcome_num = 16 if outcome == "folate_RNI"
replace outcome_num = 17 if outcome == "vitC_RNI"
replace outcome_num = 18 if outcome == "ret_RNI"
*replace outcome_num = 18 if outcome == "vitD_RNI"
replace outcome_num = 19 if outcome == "vitE_min"
replace outcome_num = 20 if outcome == "calc_RNI"
replace outcome_num = 21 if outcome == "phos_RNI"
replace outcome_num = 22 if outcome == "mag_RNI"
replace outcome_num = 23 if outcome == "sodium_RNI"
replace outcome_num = 24 if outcome == "pot_RNI"
replace outcome_num = 25 if outcome == "iron_RNI"
replace outcome_num = 26 if outcome == "zinc_RNI"
replace outcome_num = 27 if outcome == "selen_RNI"
replace outcome_num = 28 if outcome == "iodine_RNI"

label define out_lb 1 "Energy < EAR (8,400kJ)" 2 "Carbohydrates < min (267g)" 3 "Free sugars > max (27g)" 4 "NSP (Fibre) < RNI (22.5g)" 5 "Fat > max (78g)" 6 "Monounsaturated fat < RNI (29g)" 7 "Polyunsaturated fat < RNI (14g)" 8 "Saturated fat > max (24g)" 9 "Omega-3 < 0.25g (from fish)" 10 "Protein < RNI (45g)" 11 "Thiamin < RNI (0.8mg)" 12 "Riboflavin < RNI (1.1mg)" 13 "Niacin equivalent < RNI (13.2mg)" 14 "Vitamin B6 < RNI (1.2mg)" 15 "Vitamin B12 < RNI (1.5ug)" 16 "Folate < RNI (200ug)" 17 "Vitamin C < RNI (40mg)" 18 "Retinol/Vitamin A < RNI (600ug)" 19 "Vitamin E < min (3ug)" 20 "Calcium < RNI (700mg)" 21 "Phosphorus < RNI (550mg)" 22 "Magnesium < RNI (270mg)" 23 "Sodium < RNI (1.6g) | > max (2.4g)" 24 "Potassium < RNI (3500mg)" 25 "Iron < RNI (14.8mg)" 26 "Zinc < RNI (7mg)" 27 "Selenium < RNI (60ug)" 28 "Iodine < RNI (140ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove

sort outcome_num
drop outcome_num

format p_exp %9.3f

outsheet using ".\G0Mum_Age4_Results\nutRNI_belief_p.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_religion_p.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_attend_p.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_belief_relig_p.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace



********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "age4_J_nutrient_RNI_results.dta", clear
format %9.3f coef-p

* Transform the p-value by -log10 to get p-values on similar order of magnitude to be readable (0 = p-value of 1)
gen logp = -log10(p)
sum logp

* What's the Bonferroni correction here? Are 27 outcomes assessed, so 0.05 / 28 = 0.0018 (and -log10 of this is 2.75; note that in C quest there were 29 comparisons, but vitamin D dropped here as low n meeting RNI and causes imputation issues) - As nutrients not independent from one another perhaps the Bonferroni correction is too conservative, but will go with it for now.

** Plot -log10 of each variable and see if any reach Bonferroni significance (will use imputed results for this)

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "energy_EAR"
replace outcome_num = 2 if outcome == "carb_min"
replace outcome_num = 3 if outcome == "sugar_max"
replace outcome_num = 4 if outcome == "fibre_RNI"
replace outcome_num = 5 if outcome == "fat_max"
replace outcome_num = 6 if outcome == "mono_RNI"
replace outcome_num = 7 if outcome == "poly_RNI"
replace outcome_num = 8 if outcome == "sat_max"
replace outcome_num = 9 if outcome == "omega3"
replace outcome_num = 10 if outcome == "prot_RNI"
replace outcome_num = 11 if outcome == "thiamin_RNI"
replace outcome_num = 12 if outcome == "ribo_RNI"
replace outcome_num = 13 if outcome == "niacinEq_RNI"
replace outcome_num = 14 if outcome == "vitB6_RNI"
replace outcome_num = 15 if outcome == "vitB12_RNI"
replace outcome_num = 16 if outcome == "folate_RNI"
replace outcome_num = 17 if outcome == "vitC_RNI"
replace outcome_num = 18 if outcome == "ret_RNI"
*replace outcome_num = 18 if outcome == "vitD_RNI"
replace outcome_num = 19 if outcome == "vitE_min"
replace outcome_num = 20 if outcome == "calc_RNI"
replace outcome_num = 21 if outcome == "phos_RNI"
replace outcome_num = 22 if outcome == "mag_RNI"
replace outcome_num = 23 if outcome == "sodium_RNI"
replace outcome_num = 24 if outcome == "pot_RNI"
replace outcome_num = 25 if outcome == "iron_RNI"
replace outcome_num = 26 if outcome == "zinc_RNI"
replace outcome_num = 27 if outcome == "selen_RNI"
replace outcome_num = 28 if outcome == "iodine_RNI"

label define out_lb 1 "Energy < EAR (8,400kJ)" 2 "Carbohydrates < min (267g)" 3 "Free sugars > max (27g)" 4 "NSP (Fibre) < RNI (22.5g)" 5 "Fat > max (78g)" 6 "Monounsaturated fat < RNI (29g)" 7 "Polyunsaturated fat < RNI (14g)" 8 "Saturated fat > max (24g)" 9 "Omega-3 < 0.25g (from fish)" 10 "Protein < RNI (45g)" 11 "Thiamin < RNI (0.8mg)" 12 "Riboflavin < RNI (1.1mg)" 13 "Niacin equivalent < RNI (13.2mg)" 14 "Vitamin B6 < RNI (1.2mg)" 15 "Vitamin B12 < RNI (1.5ug)" 16 "Folate < RNI (200ug)" 17 "Vitamin C < RNI (40mg)" 18 "Retinol/Vitamin A < RNI (600ug)" 19 "Vitamin E < min (3ug)" 20 "Calcium < RNI (700mg)" 21 "Phosphorus < RNI (550mg)" 22 "Magnesium < RNI (270mg)" 23 "Sodium < RNI (1.6g) | > max (2.4g)" 24 "Potassium < RNI (3500mg)" 25 "Iron < RNI (14.8mg)" 26 "Zinc < RNI (7mg)" 27 "Selenium < RNI (60ug)" 28 "Iodine < RNI (140ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove


*** Now make the plots and explore how RSBB is related to RNIs

** Starting with belief in God
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief in God (ref = no)") ///
	legend(order(1 "Not sure" 2 "Yes"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_BeliefInGod_pvalues.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome_num exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/28
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_belief_bon.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\G0Mum_Age4_Results\nutRNI_belief_05.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\G0Mum_Age4_Results\nutRNI_belief_full.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


** Next, religious affiliation
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(vsmall) angle(0)) ///
	title("Religious Affiliation (ref = none)") ///
	legend(order(1 "Christian" 2 "Other"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_Religion_pvalues.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/28
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_religion_bon.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\G0Mum_Age4_Results\nutRNI_religion_05.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\G0Mum_Age4_Results\nutRNI_religion_full.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


* Next to church attendance
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(vsmall) angle(0)) ///
	title("Church Attendance (ref = not at all)") ///
	legend(order(1 "Min once a month" 2 "Min once a year"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_ChurchAttendance_pvalues.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/28
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_attend_bon.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\G0Mum_Age4_Results\nutRNI_attend_05.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\G0Mum_Age4_Results\nutRNI_attend_full.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


* And finally belief and relgion
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief and religion (ref = none)") ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other"))
		
graph export ".\G0Mum_Age4_Results\NutrientsRNI_BeliefAndReligion_pvalues.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/28
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_belief_relig_bon.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\G0Mum_Age4_Results\nutRNI_belief_relig_05.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\G0Mum_Age4_Results\nutRNI_belief_relig_full.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


** Combining plots together
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief in God (ref = no)", size(small)) ///
	legend(order(1 "Not sure" 2 "Yes") size(vsmall)) ///
	name(belief, replace)
	

local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Religious Affiliation (ref = none)", size(small)) ///
	legend(order(1 "Christian" 2 "Other") size(vsmall)) ///
	name(relig, replace)
	
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Church Attendance (ref = not at all)", size(small)) ///
	legend(order(1 "Min once a month" 2 "Min once a year") size(vsmall)) ///
	name(attend, replace)
	
local bon_threshold = -log10(0.05/28)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)28, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief and religion (ref = none)", size(small)) ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other") ///
	cols(3) size(vsmall)) ///
	name(belief_relig, replace)

graph combine belief relig attend belief_relig, imargin(0 0 0 0)
graph export ".\G0Mum_Age4_Results\NutrientsRNI_combined_pvalues.pdf", replace	

graph close _all



******************************************************************************
*** Want to repeat these nutrient analyses, but this time adjusting for total energy intake. It's not super-clear whether energy intake is a mediator or a confounder of the RSBB-nutrient intake association, so will adjust for energy intake as a sensitivity analysis to see if results differ.

*** Starting with total nutrient intake

** Read in the imputed data (if skipping imputation) and make sure is in flong format
use "imp_nutrients_age4_J.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* 4) Lapsed Christians
gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other", replace
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m
tab lapsed_Xian if _mi_m == 0, m

gen lapsed_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose j_nut_adjIntake
postfile j_nut_adjIntake str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "age4_J_nutrient_adjIntake_results.dta", replace
	
* Will also save another postfile, which just has the overall p-value testing for an overall effect of the exposure on the outcome
capture postclose j_nut_p_adjIntake
postfile j_nut_p_adjIntake str20 outcome str30 exposure str5 model p_exp ///
	using "age4_J_nutrient_p_adjIntake_results.dta", replace

	
foreach var of varlist j1001-j1004 j1006-j1036 {
	
	// Save the outcome variable as a macro
	if "`var'" == "j1001" {
		local outcome = "Calcium (mg)"
	}
	else if "`var'" == "j1002" {
		local outcome = "Carbohydrate (g)"
	}
	else if "`var'" == "j1003" {
		local outcome = "Carotene (ug)"
	}
	else if "`var'" == "j1004" {
		local outcome = "Cholesterol (mg)"
	}
	else if "`var'" == "j1006" {
		local outcome = "Fat (g)"
	}
	else if "`var'" == "j1007" {
		local outcome = "Omega-3 (g)"
	}
	else if "`var'" == "j1010" {
		local outcome = "Folate (ug)"
	}
	else if "`var'" == "j1011" {
		local outcome = "Iodine (ug)"
	}
	else if "`var'" == "j1012" {
		local outcome = "Iron (mg)"
	}
	else if "`var'" == "j1013" {
		local outcome = "Magnesium (mg)"
	}
	else if "`var'" == "j1014" {
		local outcome = "Monounsaturated fat (mg)"
	}
	else if "`var'" == "j1015" {
		local outcome = "Niacin equivalent (mg)"
	}
	else if "`var'" == "j1016" {
		local outcome = "Non-milk sugars (g)"
	}
	else if "`var'" == "j1017" {
		local outcome = "Fibre (g)"
	}
	else if "`var'" == "j1018" {
		local outcome = "Phosphorus (mg)"
	}
	else if "`var'" == "j1019" {
		local outcome = "Polyunsaturated fat (g)"
	}
	else if "`var'" == "j1020" {
		local outcome = "Potassium (mg)"
	}
	else if "`var'" == "j1021" {
		local outcome = "Protein (g)"
	}
	else if "`var'" == "j1022" {
		local outcome = "Retinol (ug)"
	}
	else if "`var'" == "j1023" {
		local outcome = "Riboflavin (mg)"
	}
	else if "`var'" == "j1024" {
		local outcome = "Selenium (ug)"
	}
	else if "`var'" == "j1025" {
		local outcome = "Saturated fat (g)"
	}
	else if "`var'" == "j1026" {
		local outcome = "Sodium (mg)"
	}
	else if "`var'" == "j1027" {
		local outcome = "Starch (g)"
	}
	else if "`var'" == "j1028" {
		local outcome = "Sugar (g)"
	}
	else if "`var'" == "j1029" {
		local outcome = "Thiamin (mg)"
	}
	else if "`var'" == "j1031" {
		local outcome = "Vitamin C (mg)"
	}
	else if "`var'" == "j1032" {
		local outcome = "Vitamin B6 (mg)"
	}
	else if "`var'" == "j1033" {
		local outcome = "Vitamin B12 (ug)"
	}
	else if "`var'" == "j1034" {
		local outcome = "Vitamin D (ug)"
	}
	else if "`var'" == "j1035" {
		local outcome = "Vitamin E (mg)"
	}
	else {
		local outcome = "Zinc (mg)"
	}
	//local outcome = "`var'"
	
	//// Exposure 1) Belief in God
	local exp = "Belief (ref = no)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 if belief_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
	
	//// Exposure 2) Denomination affiliation
	local exp = "Denomination (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp if denom_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 3) Church attendance
	local exp = "Church attendance (ref = not at all)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp if attend_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	mi test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 4) 'Lapsed' Christians
	local exp = "Belief and religion (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian if lapsed_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
}

postclose j_nut_adjIntake
postclose j_nut_p_adjIntake


*** Save the results testing exposure on outcome as CSV file, as easier to work with (split by exposure, as this is how SI is arranged, and only include the MI results)
use "age4_J_nutrient_p_adjIntake_results.dta", clear

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Carbohydrate (g)"
replace outcome_num = 2 if outcome == "Sugar (g)"
replace outcome_num = 3 if outcome == "Non-milk sugars (g)"
replace outcome_num = 4 if outcome == "Starch (g)"
replace outcome_num = 5 if outcome == "Fibre (g)"
replace outcome_num = 6 if outcome == "Fat (g)"
replace outcome_num = 7 if outcome == "Monounsaturated fat "
replace outcome_num = 8 if outcome == "Polyunsaturated fat "
replace outcome_num = 9 if outcome == "Saturated fat (g)"
replace outcome_num = 10 if outcome == "Omega-3 (g)"
replace outcome_num = 11 if outcome == "Cholesterol (mg)"
replace outcome_num = 12 if outcome == "Protein (g)"
replace outcome_num = 13 if outcome == "Thiamin (mg)"
replace outcome_num = 14 if outcome == "Riboflavin (mg)"
replace outcome_num = 15 if outcome == "Niacin equivalent (m"
replace outcome_num = 16 if outcome == "Vitamin B6 (mg)"
replace outcome_num = 17 if outcome == "Vitamin B12 (ug)"
replace outcome_num = 18 if outcome == "Folate (ug)"
replace outcome_num = 19 if outcome == "Vitamin C (mg)"
replace outcome_num = 20 if outcome == "Retinol (ug)"
replace outcome_num = 21 if outcome == "Carotene (ug)"
replace outcome_num = 22 if outcome == "Vitamin D (ug)"
replace outcome_num = 23 if outcome == "Vitamin E (mg)"
replace outcome_num = 24 if outcome == "Calcium (mg)"
replace outcome_num = 25 if outcome == "Phosphorus (mg)"
replace outcome_num = 26 if outcome == "Magnesium (mg)"
replace outcome_num = 27 if outcome == "Sodium (mg)"
replace outcome_num = 28 if outcome == "Potassium (mg)"
replace outcome_num = 29 if outcome == "Iron (mg)"
replace outcome_num = 30 if outcome == "Zinc (mg)"
replace outcome_num = 31 if outcome == "Selenium (ug)"
replace outcome_num = 32 if outcome == "Iodine (ug)"

label define out_lb 1 "Carbohydrates (g)" 2 "Total sugars (g)" 3 "Free sugars (g)" 4 "Starch (g)" 5 "NSP (Fibre; g)" 6 "Fat (g)" 7 "Monounsaturated fat (g)" 8 "Polyunsaturated fat (g)" 9 "Saturated fat (g)" 10 "Omega-3 (g; from fish)" 11 "Cholesterol (mg)" 12 "Protein (g)" 13 "Thiamin (mg)" 14 "Riboflavin (mg)" 15 "Niacin equivalent (mg)" 16 "Vitamin B6 (mg)" 17 "Vitamin B12 (ug)" 18 "Folate (ug)" 19 "Vitamin C (mg)" 20 "Retinol/Vitamin A (ug)" 21 "Carotene (ug)" 22 "Vitamin D (ug)" 23 "Vitamin E (mg)" 24 "Calcium (mg)" 25 "Phosphorus (mg)" 26 "Magnesium (mg)" 27 "Sodium (mg)" 28 "Potassium (mg)" 29 "Iron (mg)" 30 "Zinc (mg)" 31 "Selenium (ug)" 32 "Iodine (ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove

sort outcome_num
drop outcome_num

format p_exp %9.3f

outsheet using ".\G0Mum_Age4_Results\nut_belief_p_adjIntake.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_religion_p_adjIntake.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_attend_p_adjIntake.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nut_belief_relig_p_adjIntake.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace



********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "age4_J_nutrient_adjIntake_results.dta", clear
format %9.3f coef-p

* Transform the p-value by -log10 to get p-values on similar order of magnitude to be readable (0 = p-value of 1)
gen logp = -log10(p)
sum logp

* What's the Bonferroni correction here? Are 33 outcomes assessed, so 0.05 / 32 = 0.0016 (and -log10 of this is 2.81) - As nutrients not independent from one another perhaps the Bonferroni correction is too conservative, but will go with it for now.

** Plot -log10 of each variable and see if any reach Bonferroni significance (will use imputed results for this)

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Carbohydrate (g)"
replace outcome_num = 2 if outcome == "Sugar (g)"
replace outcome_num = 3 if outcome == "Non-milk sugars (g)"
replace outcome_num = 4 if outcome == "Starch (g)"
replace outcome_num = 5 if outcome == "Fibre (g)"
replace outcome_num = 6 if outcome == "Fat (g)"
replace outcome_num = 7 if outcome == "Monounsaturated fat "
replace outcome_num = 8 if outcome == "Polyunsaturated fat "
replace outcome_num = 9 if outcome == "Saturated fat (g)"
replace outcome_num = 10 if outcome == "Omega-3 (g)"
replace outcome_num = 11 if outcome == "Cholesterol (mg)"
replace outcome_num = 12 if outcome == "Protein (g)"
replace outcome_num = 13 if outcome == "Thiamin (mg)"
replace outcome_num = 14 if outcome == "Riboflavin (mg)"
replace outcome_num = 15 if outcome == "Niacin equivalent (m"
replace outcome_num = 16 if outcome == "Vitamin B6 (mg)"
replace outcome_num = 17 if outcome == "Vitamin B12 (ug)"
replace outcome_num = 18 if outcome == "Folate (ug)"
replace outcome_num = 19 if outcome == "Vitamin C (mg)"
replace outcome_num = 20 if outcome == "Retinol (ug)"
replace outcome_num = 21 if outcome == "Carotene (ug)"
replace outcome_num = 22 if outcome == "Vitamin D (ug)"
replace outcome_num = 23 if outcome == "Vitamin E (mg)"
replace outcome_num = 24 if outcome == "Calcium (mg)"
replace outcome_num = 25 if outcome == "Phosphorus (mg)"
replace outcome_num = 26 if outcome == "Magnesium (mg)"
replace outcome_num = 27 if outcome == "Sodium (mg)"
replace outcome_num = 28 if outcome == "Potassium (mg)"
replace outcome_num = 29 if outcome == "Iron (mg)"
replace outcome_num = 30 if outcome == "Zinc (mg)"
replace outcome_num = 31 if outcome == "Selenium (ug)"
replace outcome_num = 32 if outcome == "Iodine (ug)"

label define out_lb 1 "Carbohydrates (g)" 2 "Total sugars (g)" 3 "Free sugars (g)" 4 "Starch (g)" 5 "NSP (Fibre; g)" 6 "Fat (g)" 7 "Monounsaturated fat (g)" 8 "Polyunsaturated fat (g)" 9 "Saturated fat (g)" 10 "Omega-3 (g; from fish)" 11 "Cholesterol (mg)" 12 "Protein (g)" 13 "Thiamin (mg)" 14 "Riboflavin (mg)" 15 "Niacin equivalent (mg)" 16 "Vitamin B6 (mg)" 17 "Vitamin B12 (ug)" 18 "Folate (ug)" 19 "Vitamin C (mg)" 20 "Retinol/Vitamin A (ug)" 21 "Carotene (ug)" 22 "Vitamin D (ug)" 23 "Vitamin E (mg)" 24 "Calcium (mg)" 25 "Phosphorus (mg)" 26 "Magnesium (mg)" 27 "Sodium (mg)" 28 "Potassium (mg)" 29 "Iron (mg)" 30 "Zinc (mg)" 31 "Selenium (ug)" 32 "Iodine (ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove


*** Now make the plots and explore how RSBB is related to nutrient intake

** Starting with belief in God
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief in God (ref = no)") ///
	legend(order(1 "Not sure" 2 "Yes"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_BeliefInGod_pvalues_adjIntake.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome_num exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/32
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_belief_bon_adjIntake.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\G0Mum_Age4_Results\nut_belief_05_adjIntake.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\G0Mum_Age4_Results\nut_belief_full_adjIntake.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


** Next, religious affiliation
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(vsmall) angle(0)) ///
	title("Religious Affiliation (ref = none)") ///
	legend(order(1 "Christian" 2 "Other"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_Religion_pvalues_adjIntake.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/32
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_religion_bon_adjIntake.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\G0Mum_Age4_Results\nut_religion_05_adjIntake.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\G0Mum_Age4_Results\nut_religion_full_adjIntake.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


* Next to church attendance
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(vsmall) angle(0)) ///
	title("Church Attendance (ref = not at all)") ///
	legend(order(1 "Min once a month" 2 "Min once a year"))
	
graph export ".\G0Mum_Age4_Results\Nutrients_ChurchAttendance_pvalues_adjIntake.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/32
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_attend_bon_adjIntake.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\G0Mum_Age4_Results\nut_attend_05_adjIntake.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\G0Mum_Age4_Results\nut_attend_full_adjIntake.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


* And finally belief and relgion
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief and religion (ref = none)") ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other"))
		
graph export ".\G0Mum_Age4_Results\Nutrients_BeliefAndReligion_pvalues_adjIntake.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/32
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nut_belief_relig_bon_adjIntake.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\G0Mum_Age4_Results\nut_belief_relig_05_adjIntake.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\G0Mum_Age4_Results\nut_belief_relig_full_adjIntake.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


* Combining plots together
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief in God (ref = no)", size(small)) ///
	legend(order(1 "Not sure" 2 "Yes") size(vsmall)) ///
	name(belief, replace)

local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Religious Affiliation (ref = none)", size(small)) ///
	legend(order(1 "Christian" 2 "Other") size(vsmall)) ///
	name(relig, replace)
	
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Church Attendance (ref = not at all)", size(small)) ///
	legend(order(1 "Min once a month" 2 "Min once a year") size(vsmall)) ///
	name(attend, replace)
	
local bon_threshold = -log10(0.05/32)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ///
	ytitle("") ysc(reverse) ///
	ylabel(1(1)32, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief and religion (ref = none)", size(small)) ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other") ///
	cols(3) size(vsmall)) ///
	name(belief_relig, replace)

graph combine belief relig attend belief_relig, imargin(0 0 0 0)
graph export ".\G0Mum_Age4_Results\Nutrients_combined_pvalues_adjIntake.pdf", replace	

graph close _all



***********************************************************************************
*** And now repeat energy intake adjustment for meeting RNIs

** Read in the imputed nutrient data and make sure is in flong format
use "imp_nutrients_age4_J.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d810 j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d813_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 d816_grp j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* 4) Lapsed Christians
gen lapsed_Xian = .
replace lapsed_Xian = 1 if (d810 == 3 | d810 == 2) & d813_grp == 1
replace lapsed_Xian = 2 if (d810 == 3 | d810 == 2) & d813_grp == 2
replace lapsed_Xian = 3 if d810 == 1 & d813_grp == 2
replace lapsed_Xian = 4 if lapsed_Xian == . & d810 != . & d813_grp != .

label define lapsed_lb 1 "Non-believer and no affiliation" 2 "Christian non-believer" 3 "Christian believer" 4 "Other"
numlabel lapsed_lb, add
label value lapsed_Xian lapsed_lb
tab lapsed_Xian, m
tab lapsed_Xian if _mi_m == 0, m

gen lapsed_CCA = 1 if _mi_m == 0
foreach var of varlist j1001 lapsed_Xian j912 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now go through each nutrient and code as binary variable whether meets recommended daily intakes

** Calcium (mg) - RNI of 700mg for adult women
gen calc_RNI = .
replace calc_RNI = 1 if j1001 < 700
replace calc_RNI = 0 if j1001 >= 700 & j1001 < .
tab calc_RNI if _mi_m == 0, m

label define calc_lb 0 ">= RNI (700mg)" 1 "< RNI (700mg)"
numlabel calc_lb, add
label values calc_RNI calc_lb
tab calc_RNI if _mi_m == 0, m

** Carbohydrate (g) - Minimum 267g for adult women
gen carb_min = .
replace carb_min = 1 if j1002 < 267
replace carb_min = 0 if j1002 >= 267 & j1002 < .
tab carb_min if _mi_m == 0, m

label define carb_lb 0 ">= min (267g)" 1 "< min (267mg)"
numlabel carb_lb, add
label values carb_min carb_lb
tab carb_min if _mi_m == 0, m

** Carotene (ug) - No recommendations for carotene

** Cholesterol (mg) - No recommendations for cholesterol

** Fat (g) - Maximum 78g for adult women
gen fat_max = .
replace fat_max = 1 if j1006 > 78 & j1006 < .
replace fat_max = 0 if j1006 <= 78
tab fat_max if _mi_m == 0, m

label define fat_lb 0 "<= max (78g)" 1 "> max (78g)"
numlabel fat_lb, add
label values fat_max fat_lb
tab fat_max if _mi_m == 0, m

** Omega-3 fatty acid from fish only (g) - No RNI, but more omega-3 recommended (esp. in pregnancy). As of potential importance, will code into low (<0.25g/day) and high (>0.25g/day) as this is where there is a natural split in the data for pregnancy omega-3 data (but not at age 4; but important to bear in mind that this is not based on any formal RNI recommendations, and omega-3 is only counted for fish, not sources from other foods)
sum j1007 if j1007 > 0.2 & j1007 < 0.25 & _mi_m == 0

gen omega3 = .
replace omega3 = 1 if j1007 < 0.25
replace omega3 = 0 if j1007 >= 0.25 & j1007 < .
tab omega3 if _mi_m == 0, m

label define omega3_lb 0 "High (>0.25g)" 1 "Low (<0.25g)"
numlabel omega3_lb, add
label values omega3 omega3_lb
tab omega3 if _mi_m == 0, m

** Folate (ug) - RNI of 200ug for adult women (+100 for pregnanct women)
gen folate_RNI = .
replace folate_RNI = 1 if j1010 < 200
replace folate_RNI = 0 if j1010 >= 200 & j1010 < .
tab folate_RNI if _mi_m == 0, m

label define folate_lb 0 ">= RNI (200ug)" 1 "< RNI (200ug)"
numlabel folate_lb, add
label values folate_RNI folate_lb
tab folate_RNI if _mi_m == 0, m

** Iodine (ug) - RNI of 140ug for adult women
gen iodine_RNI = .
replace iodine_RNI = 1 if j1011 < 140
replace iodine_RNI = 0 if j1011 >= 140 & j1011 < .
tab iodine_RNI if _mi_m == 0, m

label define iodine_lb 0 ">= RNI (140ug)" 1 "< RNI (140ug)"
numlabel iodine_lb, add
label values iodine_RNI iodine_lb
tab iodine_RNI if _mi_m == 0, m

** Iron (mg) - RNI of 14.8mg for adult women
gen iron_RNI = .
replace iron_RNI = 1 if j1012 < 14.8
replace iron_RNI = 0 if j1012 >= 14.8 & j1012 < .
tab iron_RNI if _mi_m == 0, m

label define iron_lb 0 ">= RNI (14.8mg)" 1 "< RNI (14.8mg)"
numlabel iron_lb, add
label values iron_RNI iron_lb
tab iron_RNI if _mi_m == 0, m

** Magnesium (mg) - RNI of 270mg for adult women
gen mag_RNI = .
replace mag_RNI = 1 if j1013 < 270
replace mag_RNI = 0 if j1013 >= 270 & j1013 < .
tab mag_RNI if _mi_m == 0, m

label define mag_lb 0 ">= RNI (270mg)" 1 "< RNI (270mg)"
numlabel mag_lb, add
label values mag_RNI mag_lb
tab mag_RNI if _mi_m == 0, m

** Mono-unsaturated fat (g) - RNI of 29g for adult women
gen mono_RNI = .
replace mono_RNI = 1 if j1014 < 29
replace mono_RNI = 0 if j1014 >= 29 & j1014 < .
tab mono_RNI if _mi_m == 0, m

label define mono_lb 0 ">= RNI (29g)" 1 "< RNI (29g)"
numlabel mono_lb, add
label values mono_RNI mono_lb
tab mono_RNI if _mi_m == 0, m

** Niacin equivalent (mg; niacin + tyrpt/60) - RNI of 13.2mg for adult women
gen niacinEq_RNI = .
replace niacinEq_RNI = 1 if j1015 < 13.2
replace niacinEq_RNI = 0 if j1015 >= 13.2 & j1015 < .
tab niacinEq_RNI if _mi_m == 0, m

label define niacinEq_lb 0 ">= RNI (13.2mg)" 1 "< RNI (13.2mg)"
numlabel niacinEq_lb, add
label values niacinEq_RNI niacinEq_lb
tab niacinEq_RNI if _mi_m == 0, m

** Non-milk extrinsic sugars (g; 'free sugars') - Maximum 27g for adult women
gen sugar_max = .
replace sugar_max = 1 if j1016 > 27
replace sugar_max = 0 if j1016 <= 27 & j1016 < .
tab sugar_max if _mi_m == 0, m

label define sugar_lb 0 "<= max (27g)" 1 "> max (27g)"
numlabel sugar_lb, add
label values sugar_max sugar_lb
tab sugar_max if _mi_m == 0, m

** Fibre (g) - RNI of 22.5g for adult women
gen fibre_RNI = .
replace fibre_RNI = 1 if j1017 < 22.5
replace fibre_RNI = 0 if j1017 >= 22.5 & j1017 < .
tab fibre_RNI if _mi_m == 0, m

label define fibre_lb 0 ">= RNI (22.5g)" 1 "< RNI (22.5g)"
numlabel fibre_lb, add
label values fibre_RNI fibre_lb
tab fibre_RNI if _mi_m == 0, m

** Phosphorous (mg) - RNI of 550mg for adult women
gen phos_RNI = .
replace phos_RNI = 1 if j1018 < 550
replace phos_RNI = 0 if j1018 >= 550 & j1018 < .
tab phos_RNI if _mi_m == 0, m

label define phos_lb 0 ">= RNI (550mg)" 1 "< RNI (550mg)"
numlabel phos_lb, add
label values phos_RNI phos_lb
tab phos_RNI if _mi_m == 0, m

** Poly-unsaturated fat (g) - RNI of 14g for adult women
gen poly_RNI = .
replace poly_RNI = 1 if j1019 < 14
replace poly_RNI = 0 if j1019 >= 14 & j1019 < .
tab poly_RNI if _mi_m == 0, m

label define poly_lb 0 ">= RNI (14g)" 1 "< RNI (14g)"
numlabel poly_lb, add
label values poly_RNI poly_lb
tab poly_RNI if _mi_m == 0, m

** Potassium (mg) - RNI of 3500mg for adult women
gen pot_RNI = .
replace pot_RNI = 1 if j1020 < 3500
replace pot_RNI = 0 if j1020 >= 3500 & j1020 < .
tab pot_RNI if _mi_m == 0, m

label define pot_lb 0 ">= RNI (3500mg)" 1 "< RNI (3500mg)"
numlabel pot_lb, add
label values pot_RNI pot_lb
tab pot_RNI if _mi_m == 0, m

** Protein (g) - RNI of 45.0g for adult women (+6 for pregnancy)
gen prot_RNI = .
replace prot_RNI = 1 if j1021 < 45
replace prot_RNI = 0 if j1021 >= 45 & j1021 < .
tab prot_RNI if _mi_m == 0, m

label define prot_lb 0 ">= RNI (45g)" 1 "< RNI (45g)"
numlabel prot_lb, add
label values prot_RNI prot_lb
tab prot_RNI if _mi_m == 0, m

** Retinol/Vitamin A (ug) - RNI of 600ug for adult women (+100 for pregnancy)
gen ret_RNI = .
replace ret_RNI = 1 if j1022 < 600
replace ret_RNI = 0 if j1022 >= 600 & j1022 < .
tab ret_RNI if _mi_m == 0, m

label define ret_lb 0 ">= RNI (600ug)" 1 "< RNI (600ug)"
numlabel ret_lb, add
label values ret_RNI ret_lb
tab ret_RNI if _mi_m == 0, m

** Riboflavin (mg) - RNI of 1.1mg for adult women (+0.3 for pregnancy)
gen ribo_RNI = .
replace ribo_RNI = 1 if j1023 < 1.1
replace ribo_RNI = 0 if j1023 >= 1.1 & j1023 < .
tab ribo_RNI if _mi_m == 0, m

label define ribo_lb 0 ">= RNI (1.1mg)" 1 "< RNI (1.1mg)"
numlabel ribo_lb, add
label values ribo_RNI ribo_lb
tab ribo_RNI if _mi_m == 0, m

** Saturated fat (g) - Maximum 24g for adult women
gen sat_max = .
replace sat_max = 1 if j1025 > 24 & j1025 < .
replace sat_max = 0 if j1025 <= 24
tab sat_max if _mi_m == 0, m

label define sat_lb 0 "<= max (24g)" 1 "> max (24g)"
numlabel sat_lb, add
label values sat_max sat_lb
tab sat_max if _mi_m == 0, m

** Selenium (ug) - RNI of 60ug for adult women
gen selen_RNI = .
replace selen_RNI = 1 if j1024 < 60
replace selen_RNI = 0 if j1024 >= 60 & j1024 < .
tab selen_RNI if _mi_m == 0, m

label define selen_lb 0 ">= RNI (60ug)" 1 "< RNI (60ug)"
numlabel selen_lb, add
label values selen_RNI selen_lb
tab selen_RNI if _mi_m == 0, m

** Sodium (mg) - RNI of 1600mg for adult women, but recommended max intake of 2400mg (6g salt)
gen sodium_RNI = .
replace sodium_RNI = 1 if (j1026 < 1600 | j1026 > 2400) & j1026 < .
replace sodium_RNI = 0 if j1026 >= 1600 & j1026 <= 2400
tab sodium_RNI if _mi_m == 0, m

label define sodium_lb 0 ">= RNI (1600mg) & < max (2400mg)" 1 "< RNI (1600mg) OR > max (2400mg)"
numlabel sodium_lb, add
label values sodium_RNI sodium_lb
tab sodium_RNI if _mi_m == 0, m

** Starch (g) - No recommendations for starch

** Total sugar (g) - No recommendations for total sugar

** Thiamin (mg) - RNI of 0.8mg for adult women (+0.1 for pregnancy)
gen thiamin_RNI = .
replace thiamin_RNI = 1 if j1029 < 0.8
replace thiamin_RNI = 0 if j1029 >= 0.8 & j1029 < .
tab thiamin_RNI if _mi_m == 0, m

label define thiamin_lb 0 ">= RNI (0.8mg)" 1 "< RNI (0.8mg)"
numlabel thiamin_lb, add
label values thiamin_RNI thiamin_lb
tab thiamin_RNI if _mi_m == 0, m

** Vitamin C (mg) - RNI of 40mg for adult women (+10 for pregnancy)
gen vitC_RNI = .
replace vitC_RNI = 1 if j1031 < 40
replace vitC_RNI = 0 if j1031 >= 40 & j1031 < .
tab vitC_RNI if _mi_m == 0, m

label define vitC_lb 0 ">= RNI (40mg)" 1 "< RNI (40mg)"
numlabel vitC_lb, add
label values vitC_RNI vitC_lb
tab vitC_RNI if _mi_m == 0, m

** Vitamin B6 (mg) - RNI of 1.2mg for adult women
gen vitB6_RNI = .
replace vitB6_RNI = 1 if j1032 < 1.2
replace vitB6_RNI = 0 if j1032 >= 1.2 & j1032 < .
tab vitB6_RNI if _mi_m == 0, m

label define vitB6_lb 0 ">= RNI (1.2mg)" 1 "< RNI (1.2mg)"
numlabel vitB6_lb, add
label values vitB6_RNI vitB6_lb
tab vitB6_RNI if _mi_m == 0, m

** Vitamin B12 (ug) - RNI of 1.5ug for adult women
gen vitB12_RNI = .
replace vitB12_RNI = 1 if j1033 < 1.5
replace vitB12_RNI = 0 if j1033 >= 1.5 & j1033 < .
tab vitB12_RNI if _mi_m == 0, m

label define vitB12_lb 0 ">= RNI (1.5ug)" 1 "< RNI (1.5ug)"
numlabel vitB12_lb, add
label values vitB12_RNI vitB12_lb
tab vitB12_RNI if _mi_m == 0, m

** Vitamin D (ug) - RNI of 10ug for adult women
gen vitD_RNI = .
replace vitD_RNI = 1 if j1034 < 10
replace vitD_RNI = 0 if j1034 >= 10 & j1034 < .
tab vitD_RNI if _mi_m == 0, m

label define vitD_lb 0 ">= RNI (10ug)" 1 "< RNI (10ug)"
numlabel vitD_lb, add
label values vitD_RNI vitD_lb
tab vitD_RNI if _mi_m == 0, m

** Vitamin E (mg) - Minimum of 3ug for adult women (+0.8 for pregnancy)
gen vitE_min = .
replace vitE_min = 1 if j1035 < 3
replace vitE_min = 0 if j1035 >= 3 & j1035 < .
tab vitE_min if _mi_m == 0, m

label define vitE_lb 0 ">= min (3ug)" 1 "< min (3ug)"
numlabel vitE_lb, add
label values vitE_min vitE_lb
tab vitE_min if _mi_m == 0, m

** Zinc (mg) - RNI of 7mg for adult women
gen zinc_RNI = .
replace zinc_RNI = 1 if j1036 < 7
replace zinc_RNI = 0 if j1036 >= 7 & j1036 < .
tab zinc_RNI if _mi_m == 0, m

label define zinc_lb 0 ">= RNI (7mg)" 1 "< RNI (7mg)"
numlabel zinc_lb, add
label values zinc_RNI zinc_lb
tab zinc_RNI if _mi_m == 0, m


*** Check this data, then run through same analyses as above exploring nutrient intake by RSBB
sum calc_RNI-zinc_RNI


* As so few people above the RNI threshold for Vitamin D, will drop this variable (for power reasons, and because it causes the imputations below to crash as collinear cases are dropped)
drop vitD_RNI


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file if it's already open)
capture postclose j_nut_RNI_adjIntake
postfile j_nut_RNI_adjIntake str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "age4_J_nutrient_RNI_adjIntake_results.dta", replace
	
* Will also save another postfile, which just has the overall p-value testing for an overall effect of the exposure on the outcome
capture postclose j_nut_RNI_p_adjIntake
postfile j_nut_RNI_p_adjIntake str20 outcome str30 exposure str5 model p_exp ///
	using "age4_J_nutrient_RNI_adjIntake_p_results.dta", replace

	
foreach var of varlist calc_RNI-zinc_RNI {
	
	// Save the outcome variable as a macro
	local outcome = "`var'"
	
	//// Exposure 1) Belief in God
	local exp = "Belief (ref = no)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d810 if belief_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable - For some reason the 'mi estimate: logistic' command gives the output in log-odds, rather than odds ratios, so need to manually construct these from the log-odds coefficient, the log SE and the critical value
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d810 j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef_log = res[1,1]
	local se = res[2,1]
	local crit = res[8,1]
	local p = res[4,1]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d810 2.d810
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
	
	//// Exposure 2) Denomination affiliation
	local exp = "Denomination (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.d813_grp if denom_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.d813_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.d813_grp 3.d813_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 3) Church attendance
	local exp = "Church attendance (ref = not at all)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d816_grp if attend_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d816_grp j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef_log = res[1,1]
	local se = res[2,1]
	local crit = res[8,1]
	local p = res[4,1]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 1.d816_grp 2.d816_grp
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
		
		
	//// Exposure 4) 'Lapsed' Christians
	local exp = "Belief and religion (ref = none)"
	
	// Univariable/unadjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.lapsed_Xian if lapsed_CCA == 1 & _mi_m == 0
	
	local n = e(N)
	local model = "uni"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005 if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
	
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.lapsed_Xian j912 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp j1005
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,4]
	local se = res[2,4]
	local crit = res[8,4]
	local p = res[4,4]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post j_nut_RNI_adjIntake ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	// Now test for overall effect of categorical exposure on outcome and save result
	mi test 2.lapsed_Xian 3.lapsed_Xian 4.lapsed_Xian
	local p_exp = r(p)
	post j_nut_RNI_p_adjIntake ("`outcome'") ("`exp'") ("`model'") (`p_exp')
}

postclose j_nut_RNI_adjIntake
postclose j_nut_RNI_p_adjIntake


*** Save the results testing exposure on outcome as CSV file, as easier to work with (split by exposure, as this is how SI is arranged, and only include the MI results)
use "age4_J_nutrient_RNI_adjIntake_p_results.dta", clear

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "carb_min"
replace outcome_num = 2 if outcome == "sugar_max"
replace outcome_num = 3 if outcome == "fibre_RNI"
replace outcome_num = 4 if outcome == "fat_max"
replace outcome_num = 5 if outcome == "mono_RNI"
replace outcome_num = 6 if outcome == "poly_RNI"
replace outcome_num = 7 if outcome == "sat_max"
replace outcome_num = 8 if outcome == "omega3"
replace outcome_num = 9 if outcome == "prot_RNI"
replace outcome_num = 10 if outcome == "thiamin_RNI"
replace outcome_num = 11 if outcome == "ribo_RNI"
replace outcome_num = 12 if outcome == "niacinEq_RNI"
replace outcome_num = 13 if outcome == "vitB6_RNI"
replace outcome_num = 14 if outcome == "vitB12_RNI"
replace outcome_num = 15 if outcome == "folate_RNI"
replace outcome_num = 16 if outcome == "vitC_RNI"
replace outcome_num = 17 if outcome == "ret_RNI"
*replace outcome_num = 18 if outcome == "vitD_RNI"
replace outcome_num = 18 if outcome == "vitE_min"
replace outcome_num = 19 if outcome == "calc_RNI"
replace outcome_num = 20 if outcome == "phos_RNI"
replace outcome_num = 21 if outcome == "mag_RNI"
replace outcome_num = 22 if outcome == "sodium_RNI"
replace outcome_num = 23 if outcome == "pot_RNI"
replace outcome_num = 24 if outcome == "iron_RNI"
replace outcome_num = 25 if outcome == "zinc_RNI"
replace outcome_num = 26 if outcome == "selen_RNI"
replace outcome_num = 27 if outcome == "iodine_RNI"

label define out_lb 1 "Carbohydrates < min (267g)" 2 "Free sugars > max (27g)" 3 "NSP (Fibre) < RNI (22.5g)" 4 "Fat > max (78g)" 5 "Monounsaturated fat < RNI (29g)" 6 "Polyunsaturated fat < RNI (14g)" 7 "Saturated fat > max (24g)" 8 "Omega-3 < 0.25g (from fish)" 9 "Protein < RNI (45g)" 10 "Thiamin < RNI (0.8mg)" 11 "Riboflavin < RNI (1.1mg)" 12 "Niacin equivalent < RNI (13.2mg)" 13 "Vitamin B6 < RNI (1.2mg)" 14 "Vitamin B12 < RNI (1.5ug)" 15 "Folate < RNI (200ug)" 16 "Vitamin C < RNI (40mg)" 17 "Retinol/Vitamin A < RNI (600ug)" 18 "Vitamin E < min (3ug)" 19 "Calcium < RNI (700mg)" 20 "Phosphorus < RNI (550mg)" 21 "Magnesium < RNI (270mg)" 22 "Sodium < RNI (1.6g) | > max (2.4g)" 23 "Potassium < RNI (3500mg)" 24 "Iron < RNI (14.8mg)" 25 "Zinc < RNI (7mg)" 26 "Selenium < RNI (60ug)" 27 "Iodine < RNI (140ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove

sort outcome_num
drop outcome_num

format p_exp %9.3f

outsheet using ".\G0Mum_Age4_Results\nutRNI_belief_p_adjIntake.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_religion_p_adjIntake.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_attend_p_adjIntake.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace
outsheet using ".\G0Mum_Age4_Results\nutRNI_belief_relig_p_adjIntake.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace



********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "age4_J_nutrient_RNI_adjIntake_results.dta", clear
format %9.3f coef-p

* Transform the p-value by -log10 to get p-values on similar order of magnitude to be readable (0 = p-value of 1)
gen logp = -log10(p)
sum logp

* What's the Bonferroni correction here? Are 27 outcomes assessed, so 0.05 / 27 = 0.0019 (and -log10 of this is 2.73; note that in C quest there were 29 comparisons, but vitamin D dropped here as low n meeting RNI and causes imputation issues) - As nutrients not independent from one another perhaps the Bonferroni correction is too conservative, but will go with it for now.

** Plot -log10 of each variable and see if any reach Bonferroni significance (will use imputed results for this)

* Need to recode the outcome variables to numeric and put in more sensible order (currently alphabetical)
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "carb_min"
replace outcome_num = 2 if outcome == "sugar_max"
replace outcome_num = 3 if outcome == "fibre_RNI"
replace outcome_num = 4 if outcome == "fat_max"
replace outcome_num = 5 if outcome == "mono_RNI"
replace outcome_num = 6 if outcome == "poly_RNI"
replace outcome_num = 7 if outcome == "sat_max"
replace outcome_num = 8 if outcome == "omega3"
replace outcome_num = 9 if outcome == "prot_RNI"
replace outcome_num = 10 if outcome == "thiamin_RNI"
replace outcome_num = 11 if outcome == "ribo_RNI"
replace outcome_num = 12 if outcome == "niacinEq_RNI"
replace outcome_num = 13 if outcome == "vitB6_RNI"
replace outcome_num = 14 if outcome == "vitB12_RNI"
replace outcome_num = 15 if outcome == "folate_RNI"
replace outcome_num = 16 if outcome == "vitC_RNI"
replace outcome_num = 17 if outcome == "ret_RNI"
*replace outcome_num = 18 if outcome == "vitD_RNI"
replace outcome_num = 18 if outcome == "vitE_min"
replace outcome_num = 19 if outcome == "calc_RNI"
replace outcome_num = 20 if outcome == "phos_RNI"
replace outcome_num = 21 if outcome == "mag_RNI"
replace outcome_num = 22 if outcome == "sodium_RNI"
replace outcome_num = 23 if outcome == "pot_RNI"
replace outcome_num = 24 if outcome == "iron_RNI"
replace outcome_num = 25 if outcome == "zinc_RNI"
replace outcome_num = 26 if outcome == "selen_RNI"
replace outcome_num = 27 if outcome == "iodine_RNI"

label define out_lb 1 "Carbohydrates < min (267g)" 2 "Free sugars > max (27g)" 3 "NSP (Fibre) < RNI (22.5g)" 4 "Fat > max (78g)" 5 "Monounsaturated fat < RNI (29g)" 6 "Polyunsaturated fat < RNI (14g)" 7 "Saturated fat > max (24g)" 8 "Omega-3 < 0.25g (from fish)" 9 "Protein < RNI (45g)" 10 "Thiamin < RNI (0.8mg)" 11 "Riboflavin < RNI (1.1mg)" 12 "Niacin equivalent < RNI (13.2mg)" 13 "Vitamin B6 < RNI (1.2mg)" 14 "Vitamin B12 < RNI (1.5ug)" 15 "Folate < RNI (200ug)" 16 "Vitamin C < RNI (40mg)" 17 "Retinol/Vitamin A < RNI (600ug)" 18 "Vitamin E < min (3ug)" 19 "Calcium < RNI (700mg)" 20 "Phosphorus < RNI (550mg)" 21 "Magnesium < RNI (270mg)" 22 "Sodium < RNI (1.6g) | > max (2.4g)" 23 "Potassium < RNI (3500mg)" 24 "Iron < RNI (14.8mg)" 25 "Zinc < RNI (7mg)" 26 "Selenium < RNI (60ug)" 27 "Iodine < RNI (140ug)"

numlabel out_lb, add
label value outcome_num out_lb
tab outcome_num

numlabel out_lb, remove


*** Now make the plots and explore how RSBB is related to RNIs

** Starting with belief in God
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief in God (ref = no)") ///
	legend(order(1 "Not sure" 2 "Yes"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_BeliefInGod_pvalues_adjIntake.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome_num exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/27
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_belief_bon_adjIntake.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\G0Mum_Age4_Results\nutRNI_belief_05_adjIntake.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\G0Mum_Age4_Results\nutRNI_belief_full_adjIntake.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


** Next, religious affiliation
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(vsmall) angle(0)) ///
	title("Religious Affiliation (ref = none)") ///
	legend(order(1 "Christian" 2 "Other"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_Religion_pvalues_adjIntake.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/27
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_religion_bon_adjIntake.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\G0Mum_Age4_Results\nutRNI_religion_05_adjIntake.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\G0Mum_Age4_Results\nutRNI_religion_full_adjIntake.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


* Next to church attendance
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(vsmall) angle(0)) ///
	title("Church Attendance (ref = not at all)") ///
	legend(order(1 "Min once a month" 2 "Min once a year"))
	
graph export ".\G0Mum_Age4_Results\NutrientsRNI_ChurchAttendance_pvalues_adjIntake.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/27
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_attend_bon_adjIntake.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\G0Mum_Age4_Results\nutRNI_attend_05_adjIntake.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\G0Mum_Age4_Results\nutRNI_attend_full_adjIntake.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


* And finally belief and relgion
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(small) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(small) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(dash)) ///
	xline(`threshold_05', lcol(black) lpattern(dot)) ///
	xtitle("-log10 of p-value") ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(vsmall) angle(0)) ///
	title("Belief and religion (ref = none)") ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other"))
		
graph export ".\G0Mum_Age4_Results\NutrientsRNI_BeliefAndReligion_pvalues_adjIntake.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/27
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\G0Mum_Age4_Results\nutRNI_belief_relig_bon_adjIntake.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\G0Mum_Age4_Results\nutRNI_belief_relig_05_adjIntake.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\G0Mum_Age4_Results\nutRNI_belief_relig_full_adjIntake.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


** Combining plots together
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Not sure", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief (ref = no)" & ///
		model == "MI" & level == "Yes", col(red) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief in God (ref = no)", size(small)) ///
	legend(order(1 "Not sure" 2 "Yes") size(vsmall)) ///
	name(belief, replace)
	

local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Christian", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Denomination (ref = none)" & ///
		model == "MI" & level == "Other", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Religious Affiliation (ref = none)", size(small)) ///
	legend(order(1 "Christian" 2 "Other") size(vsmall)) ///
	name(relig, replace)
	
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a month", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Church attendance (ref = not a" & ///
		model == "MI" & level == "Min once a year", col(black) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Church Attendance (ref = not at all)", size(small)) ///
	legend(order(1 "Min once a month" 2 "Min once a year") size(vsmall)) ///
	name(attend, replace)
	
local bon_threshold = -log10(0.05/27)
local threshold_05 = -log10(0.05)

twoway (scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian believer", col(red) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Christian non-believ", col(black) msize(vsmall) msym(D))  ///
	(scatter outcome_num logp if exposure == "Belief and religion (ref = non" & ///
		model == "MI" & level == "Other", col(blue) msize(vsmall) msym(D)),  ///
	xline(`bon_threshold', lcol(black) lpattern(shortdash) lwidth(thin)) ///
	xline(`threshold_05', lcol(black) lpattern(dot) lwidth(thin)) ///
	xtitle("-log10 of p-value", size(vsmall)) ytitle("") ysc(reverse) ///
	ylabel(1(1)27, valuelabel labsize(tiny) angle(0)) ///
	xlabel(, labsize(vsmall)) ///
	title("Belief and religion (ref = none)", size(small)) ///
	legend(order(1 "Christian believer" 2 "Christian non-believer" 3 "Other") ///
	cols(3) size(vsmall)) ///
	name(belief_relig, replace)

graph combine belief relig attend belief_relig, imargin(0 0 0 0)
graph export ".\G0Mum_Age4_Results\NutrientsRNI_combined_pvalues_adjIntake.pdf", replace	

graph close _all


log close

clear

