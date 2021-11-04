***** Analysis code for 'religion and diet' study (B3893) - Exploring potential differences in dietary patterns by religiosity - Pregnancy (32 weeks gest) C file.
*** Created 13/10/2021 by Dan Smith
*** Last updated 1/11/2021 by Dan Smith
*** Stata version 16.0


**********************************************************************************
**** Set working directory, start a log file, read in dataset, and add numlabels

cd "X:\Groups\ARC\DanS\DietaryPatterns_B3893"

capture log close
log using "B3893_RSBB_Diet_QuestC_log", replace text

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

* Proportion of Christian and other beliefs
tab d813 if d813_grp == 2
tab d813 if d813_grp == 3

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

* Proportion of Christian and other beliefs
tab pb153 if pb153_grp == 2
tab pb153 if pb153_grp == 3

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
sum c994

* Age at J questionnaire - If missing, will add 4 years to age at birth, as J file completed approximately 4 years post-birth
tab j912, m

replace j912 = . if j912 < 0
replace j912 = mz028b + 4 if j912 == . & mz028b < .
tab j912, m
sum j912

* Highest education
tab c645a, m

replace c645a = . if c645a < 0
tab c645a, m
tab c645a

* Occupational social class - Will split into low (III manual, IV and V) and high (!, II and III non-manual))
tab c755, m

replace c755 = . if c755 < 0 | c755 == 65
tab c755, m

recode c755 (1 2 3 = 1) (4 5 6 = 0), gen(c755_grp)
label define occ_lb 1 "High (I/II/III-non-manual)" 0 "Low (III-manual/IV/V)"
numlabel occ_lb, add
label value c755_grp occ_lb
tab c755_grp, m
tab c755_grp

* IMD
tab dimd2010q5, m

replace dimd2010q5 = . if dimd2010q5 < 0
tab dimd2010q5, m
tab dimd2010q5

* Urban/rural status - combine into urban vs rural
tab dur01ind, m

replace dur01ind = . if dur01ind < 0
tab dur01ind, m

recode dur01ind (1 = 1) (2 3 4 = 0), gen(dur01ind_grp)
label define urban_lb 1 "Urban" 0 "Town/village/Hamlet"
numlabel urban_lb, add
label value dur01ind_grp urban_lb
tab dur01ind_grp, m
tab dur01ind_grp

* Housing status - Recode into owned vs rented vs council/HA vs other
tab a006, m

replace a006 = . if a006 < 0
tab a006, m

recode a006 (0 1 = 1) (3 4  = 2) (2 5 = 3) (6 = 4), gen(a006_grp)
label define housing_lb 1 "Owned/Mortgaged" 2 "Renting" 3 "Council/HA" 4 "Other"
numlabel housing_lb, add
label values a006_grp housing_lb
tab a006_grp, m
tab a006_grp

* Financial difficulties - Combine into yes vs no
tab b594, m

replace b594 = . if b594 < 0
tab b594

recode b594 (5 = 0) (1/4 = 1), gen(b594_grp)
label define fin_lb 0 "No" 1 "Yes"
numlabel fin_lb, add
label values b594_grp fin_lb
tab b594_grp, m
tab b594_grp

* Ethnicity - Combine into white vs other than white
tab c800, m

replace c800 = . if c800 < 0
tab c800, m

recode c800 (1 = 0) (2/9 = 1), gen(c800_grp)
label define white_lb 0 "White" 1 "Other than white"
numlabel white_lb, add
label values c800_grp white_lb
tab c800_grp, m
tab c800_grp

* Marital status - Combine widowed/divorced/separated, as well as 1st and 2nd/3rd marriage
tab a525, m

replace a525 = . if a525 < 0
tab a525, m

recode a525 (1 = 1) (5 6 = 2) (2 3 4 = 3), gen(a525_grp)
label define marital_lb 1 "Never married" 2 "Married" 3 "Widowed/Divorced/Separated"
numlabel marital_lb, add
label values a525_grp marital_lb
tab a525_grp, m
tab a525_grp

* Parity - Recode into 0, 1 and 2 or more
tab b032, m

replace b032 = . if b032 < 0
tab b032, m

recode b032 (3/22 = 2), gen(b032_grp)
label define parity_lb 2 "2 or more"
numlabel parity_lb, add
label values b032_grp parity_lb
tab b032_grp, m
tab b032_grp

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
tab mum_smk

* Depression/MH (using EPDS total score)
tab b371, m

replace b371 = . if b371 < 0
tab b371, m
sum b371

* Self-reported general health prior to pregnancy - Combine 'sometimes', 'often' and 'always' unwell together
tab b040, m

replace b040 = . if b040 < 0
tab b040, m

recode b040 (3/5 = 3), gen(b040_grp)
label define health_lb 1 "Always well" 2 "Usually well" 3 "Sometimes/often/always unwell"
numlabel health_lb, add
label values b040_grp health_lb
tab b040_grp, m
tab b040_grp


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
sum pg9996a

* Highest education
tab c666a, m

replace c666a = . if c666a < 0
tab c666a, m
tab c666a

* Occupational social class - Will split into low (III manual, IV and V) and high (!, II and III non-manual))
tab c765, m

replace c765 = . if c765 < 0 | c765 == 65
tab c765, m

recode c765 (1 2 3 = 1) (4 5 6 = 0), gen(c765_grp)
label value c765_grp occ_lb
tab c765_grp, m
tab c765_grp

* Ethnicity - Combine into white vs other than white
tab c801, m

replace c801 = . if c801 < 0
tab c801, m

recode c801 (1 = 0) (2/9 = 1), gen(c801_grp)
label values c801_grp white_lb
tab c801_grp, m
tab c801_grp

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
tab dad_smk

* Depression/MH (using EPDS total score)
tab pb260, m

replace pb260 = . if pb260 < 0
tab pb260, m
sum pb260

* Self-reported general health prior to pregnancy (reported by G0 mother) - Combine 'sometimes', 'often' and 'always' unwell together
tab a524, m

replace a524 = . if a524 < 0
tab a524, m

recode a524 (3/5 = 3), gen(a524_grp)
label values a524_grp health_lb
tab a524_grp, m
tab a524_grp


**********************************************************************************
**** Analysis of G0 mothers during pregnancy

describe c3840 c3841 c3842 c3843 c3844


*** 'Healthy' PCA score
sum c3840

* The distribution is fairly normally distributed, although are some high outliers (7 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist c3840, freq
count if c3840 > 5 & c3840 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen belief_CCA = 1
foreach var of varlist c3840 d810 c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* Descriptive statistics
oneway c3840 d810, tab
oneway c3840 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress c3840 ib3.d810
regress c3840 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3840 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3840 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3840 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & c3840 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen denomination_CCA = 1
foreach var of varlist c3840 d813_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denomination_CCA = 0 if denomination_CCA == 1 & `var' >= .
}
tab denomination_CCA, m

* Descriptive statistics
oneway c3840 d813_grp, tab
oneway c3840 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress c3840 i.d813_grp
regress c3840 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3840 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3840 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3840 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & c3840 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Add a marker so that all models have the same number of observations in (else model output potentially not comparable)
gen attend_CCA = 1
foreach var of varlist c3840 d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA = 0 if attend_CCA == 1 & `var' >= .
}
tab attend_CCA, m

* Descriptive statistics
oneway c3840 d816_grp, tab
oneway c3840 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress c3840 ib3.d816_grp
regress c3840 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3840 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3840 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3840 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & c3840 < 5


*** This is a bit weird, as belief in God is associated with more 'healthy' diet, but being christian associated with less healthy diet compared to non-believers and other faiths, while attending church associated with healthier diet. I wonder if belief and church attendance effects might explain this - So will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress c3840 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress c3840 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


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

** Now repeat analysis again with this as exposure - Ah, so it seems that Christian non-believers have a less healthy diet than both non-religious non-believers and Chrstian believers (and that there's no difference between non-religious non-believers and Christian believers)
gen lapsed_CCA = 1
foreach var of varlist c3840 lapsed_Xian c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m

regress c3840 i.lapsed_Xian if lapsed_CCA == 1

regress c3840 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if lapsed_CCA == 1

* Formal test that Christian non-believers have a less healthy diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'traditional' PCA
sum c3841

* The distribution is not normally distributed as is a long positive tail, and are some high outliers (7 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist c3841, freq
count if c3841 > 5 & c3841 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway c3841 d810, tab
oneway c3841 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress c3841 ib3.d810
regress c3841 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3841 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3841 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3841 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & c3840 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway c3841 d813_grp, tab
oneway c3841 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress c3841 i.d813_grp
regress c3841 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3841 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3841 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3841 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & c3840 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway c3841 d816_grp, tab
oneway c3841 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress c3841 ib3.d816_grp
regress c3841 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3841 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3841 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3841 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & c3840 < 5


*** Pretty consistent effects that RSBB associated with more 'traditional' diet. For consistence with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress c3841 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress c3841 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure - Now both christian non-believers and Christian believers have more 'traditional' diet than non-believer
regress c3841 i.lapsed_Xian if lapsed_CCA == 1

regress c3841 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'traditional' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'processed' PCA
sum c3842

* The distribution is relatively normally distributed, although are some high outliers (28 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist c3842, freq
count if c3842 > 5 & c3842 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway c3842 d810, tab
oneway c3842 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress c3842 ib3.d810
regress c3842 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3842 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3842 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3842 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & c3840 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway c3842 d813_grp, tab
oneway c3842 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress c3842 i.d813_grp
regress c3842 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3842 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3842 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3842 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & c3840 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway c3842 d816_grp, tab
oneway c3842 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress c3842 ib3.d816_grp
regress c3842 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3842 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3842 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3842 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & c3840 < 5


*** Pretty weak effects that RSBB associated with more 'processed' diet. For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress c3842 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress c3842 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure - No strong evidence that Chrstians (believers or non-believers) have more 'processed' diet than non-believers
regress c3842 i.lapsed_Xian if lapsed_CCA == 1

regress c3842 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'processed' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'Confectionary' PCA
sum c3843

* The distribution has a long right/positive tail, and are some high outliers (18 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist c3843, freq
count if c3843 > 5 & c3843 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway c3843 d810, tab
oneway c3843 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress c3843 ib3.d810
regress c3843 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3843 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3843 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3843 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & c3840 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway c3843 d813_grp, tab
oneway c3843 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress c3843 i.d813_grp
regress c3843 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3843 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3843 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3843 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & c3840 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway c3843 d816_grp, tab
oneway c3843 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress c3843 ib3.d816_grp
regress c3843 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3843 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3843 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3843 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & c3840 < 5


*** Pretty weak effects that RSBB associated with more 'confectionary' diet. For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress c3843 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress c3843 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure - Weak evidence that Chrstians (believers or non-believers) have more 'confectionary' diet than non-believers
regress c3843 i.lapsed_Xian if lapsed_CCA == 1

regress c3843 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'confectionary' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian


************************************************************************************
*** Now repeat for 'vegetarian' PCA
sum c3844

* The distribution is kinda normally distributed, although are some high outliers (41 values > 5) - However, as is such a small number is unlikely to bias results to any major extent (could remove these cases as a sensitivity analysis, though, just to be sure?)
hist c3844, freq
count if c3844 > 5 & c3844 < .

** Outcome 1: Belief in God/divine power
tab d810, m

* Belief marker so that all models have the same number of observations in (else model output potentially not comparable)
tab belief_CCA, m

* Descriptive statistics
oneway c3844 d810, tab
oneway c3844 d810 if belief_CCA == 1, tab

* Unadjusted regression model (no belief in God as reference category)
regress c3844 ib3.d810
regress c3844 ib3.d810 if belief_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3844 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3844 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3844 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & c3840 < 5


** Outcome 2: Religious denomination
tab d813_grp, m

* Denomination marker so that all models have the same number of observations in (else model output potentially not comparable)
tab denomination_CCA, m

* Descriptive statistics
oneway c3844 d813_grp, tab
oneway c3844 d813_grp if denomination_CCA == 1, tab

* Unadjusted regression model ('None' as reference category)
regress c3844 i.d813_grp
regress c3844 i.d813_grp if denomination_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3844 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3844 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3844 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if denomination_CCA == 1 & c3840 < 5


** Outcome 3: Church/worship attendance
tab d816_grp, m

* Church attendance marker so that all models have the same number of observations in (else model output potentially not comparable)
tab attend_CCA, m

* Descriptive statistics
oneway c3844 d816_grp, tab
oneway c3844 d816_grp if attend_CCA == 1, tab

* Unadjusted regression model ('Not at all' as reference category)
regress c3844 ib3.d816_grp
regress c3844 ib3.d816_grp if attend_CCA == 1

* Adjusted regression model (for age, education, occupational social class, IMD, financial difficulties, housing status, urban/rural status, ethnicity, marital status and parity)
regress c3844 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_RSBB

* Likelihood ratio test to check whether including RSBB outcome improves model fit
regress c3844 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1

est store mod_noRSBB

lrtest mod_noRSBB mod_RSBB

* Check obtain similar results if exclude outliers (very similar results)
regress c3844 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & c3840 < 5


*** Pretty consistent effects that Christiantity associated with less 'vegetarian' diet ('other' beliefs much more likely to be vegetarian than non-believers, though, so seems Christian-specific). For consistency with 'healthy' PCA, will reduce sample to just those with none or Christian beliefs and repeat analyses 1 and 3 (note: this is exploratory and not pre-specified!)

** Belief in God, excluding 'other' religions
tab d813_grp, m

regress c3844 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if belief_CCA == 1 & d813_grp != 3

** Church attendance, excluding 'other' religions
regress c3844 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if attend_CCA == 1 & d813_grp != 3


*** Very little difference in results. Again, as with 'healthy' PCA will code data as 'non-believers (not identify as Christian and no/not sure belief in God)' vs 'Christian non-believers (nominally identify as Christian, but no/unsure belief in God)' vs 'Christian believers (identify as Christian and believe in God)
tab lapsed_Xian, m

** Now repeat analysis again with this as exposure - Strong evidence that Chrstians (believers or non-believers) have less 'vegetarian' diet than non-believers
regress c3844 i.lapsed_Xian if lapsed_CCA == 1

regress c3844 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Formal test that Christian non-believers no difference in 'vegetarian' diet than Chrstian believers
test 2.lapsed_Xian = 3.lapsed_Xian



*************************************************************************************
*************************************************************************************
*** To think about multiple imputation, as of ~14,000 mums in total, only ~8,000 (~55%) are included in the complete case pregnancy analysis (for reference, ~12,000 mums have FFQ PCA data). MI definitely could be useful here, as missingness pattern is of many items with little missing data (rather than whole waves of data missing).

** Missingness patterns of outcome, exposure, confounders and auxiliary variables
misstable sum c3804 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp c666a c765_grp mum_smk b371 b040_grp, all

misstable patterns c3804 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp c666a c765_grp mum_smk b371 b040_grp, freq


*** As most mums have diet data, could just impute up to that number, as that would save a lot of MI effort, and the amount of extra missing data is quite low. However, if mums without diet data are very different to mums with this data, then this could be an issue, so probably worth imputing whole sample...

*** To also think about choice of auxiliary variables, as need some to improve precision relative to CCA analysis (if just use covariates, not adding any extra information). E.g., using other sociodemographic variables could help, as could using later RSBB and diet data if missing at this time point but collected at later questionnaires.


** Explore whether exposure and outcome associated with missingness (as CCA only biased if outcome or outcome and exposure associated with selection). 

** Outcome: Dietary PCAs. Can't test whether outcome (diet) predicts selection (and no relevant auxilary variables to use), so am just assuming that missingness is MAR, rather than MNAR (can test MCAR assumption, but missing data wont be MCAR)

* Missingness marker
gen miss_PCA = 1
replace miss_PCA = 0 if c3840 < .
tab miss_PCA, m

* Hard to predict this accurately, as no exposures or confounders are fully observed, and many will be collinear with it if from C questionnaire. Will just explore a few variables with low missingness (sample size still drops considerably, though). Missingness associated with age, housing status and parity, but very difficult to interpret. Still, defo not MCAR, so will make MAR assumption
tab miss_PCA if c994 != . & d810 != . & a006_grp != . & a525_grp != . & b032_grp != .
logistic miss_PCA c994 d810 a006_grp a525_grp b032_grp, or

* Possible to use J questionnaire dietary patterns as auxiliary variables to predict missing C diet values? Are actually ~500, so could use these.
sum j1100
count if miss_PCA == 1 & j1100 != .

* Check association between C and J PCA components (although 'traditional' PCA not found in J file). Not super-strong, but r = 0.4-0.5, so is some association.
describe c3840-c3844 j1100-j1103

corr c3840 j1100
corr c3842 j1101
corr c3843 j1102
corr c3844 j1103


** Exposure 1) Belief in God - As above, very difficult to properly assess predictors of missingness due to lots of missing values in covariates, but are some associations (age, housing status, marital status, parity)
gen miss_belief = 1
replace miss_belief = 0 if d810 < .
tab miss_belief, m

tab miss_belief if c994 != . & a006_grp != . & a525_grp != . & b032_grp != .
logistic miss_belief c994 a006_grp a525_grp b032_grp, or

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


** For other auxiliary variables, will include partner's education and partner's social class (as may help predict missing SEP confounders/covariates)
tab1 c666a c765_grp, m

* Will also include smoking status, depression/MH and general health as auxiliary variables, as these are likely to predict both diet and missingness (e.g., via health-seeking behaviours), and may make the MAR assumption more plausible. Will check that each are associated with both missingness and the diet outcomes

* These variables predict missing diet data
logistic miss_PCA i.mum_smk, or
logistic miss_PCA b371, or
logistic miss_PCA i.b040_grp, or

* And are associated with diet variables (although less so after adjusting for other potential confounders - but interpretation issues as changes in sample size)
regress c3840 i.mum_smk b371 i.b040_grp
regress c3840 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress c3841 i.mum_smk b371 i.b040_grp
regress c3841 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress c3842 i.mum_smk b371 i.b040_grp
regress c3842 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress c3843 i.mum_smk b371 i.b040_grp
regress c3843 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

regress c3844 i.mum_smk b371 i.b040_grp
regress c3844 i.mum_smk b371 i.b040_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp


***********************************************************************************
**** Now set up the multiple imputation

*** Save the dataset to can return back to it after running MI (for the nutrient analyses)
save "RSBB_Diet_CQuest_temp.dta", replace

* Drop variables not included in MI to speed up imputation and make file much smaller
keep aln c3840 c3841 c3842 c3843 c3844 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp j1100 j1101 j1102 j1103 k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Set up the imputation and register the variables with missing data (note there's no 'mi register regular' here, as all variables have some missing data)
mi set mlong
mi register imputed c3840 c3841 c3842 c3843 c3844 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp j1100 j1101 j1102 j1103 k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Test the imputation with a dry run first, to make sure looks okay - As some of the PCA's aren't quite normally distributed, will impute using predictive mean matching (based on 5 nearest neighbours)
mi impute chained ///
	(regress) c994 ///
	(pmm, knn(5)) c3840 c3841 c3842 c3843 c3844 j1100 j1101 j1102 j1103 b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp, ///
	add(10) burnin(10) rseed(98765) dryrun
	
	
** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about 2 hours. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010), and 'savetrace' to check convergence
mi impute chained ///
	(regress) c994 ///
	(pmm, knn(5)) c3840 c3841 c3842 c3843 c3844 j1100 j1101 j1102 j1103 b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp, ///
	add(50) burnin(10) rseed(98765) dots augment ///
	savetrace(imp_trace_PCA_preg, replace)


** Save this imputed dataset, so not have to run whole imputation again to access results
save "imp_PCA_preg.dta", replace


** Check convergence and that imputation chains are well-mixed
* Read in the trace dataset
use "imp_trace_PCA_preg.dta", clear

sum 

* Save the mean value to add as a line in the plot - Do this for all outcomes and exposures
sum c3840_mean
local mean_healthyPCA = r(mean)
display `mean_healthyPCA'

sum c3841_mean
local mean_tradPCA = r(mean)
display `mean_tradPCA'

sum c3842_mean
local mean_processedPCA = r(mean)
display `mean_processedPCA'

sum c3843_mean
local mean_confectPCA = r(mean)
display `mean_confectPCA'

sum c3844_mean
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
tsline c3840_mean*, yline(`mean_healthyPCA') legend(off)
tsline c3841_mean*, yline(`mean_tradPCA') legend(off)
tsline c3842_mean*, yline(`mean_processedPCA') legend(off)
tsline c3843_mean*, yline(`mean_confectPCA') legend(off)
tsline c3844_mean*, yline(`mean_vegPCA') legend(off)
tsline d810_mean*, yline(`mean_belief') legend(off)
tsline d813_grp_mean*, yline(`mean_denom') legend(off)
tsline d816_grp_mean*, yline(`mean_attend') legend(off)



******************************************************************************
*** Now run the models on the imputed data and combine using Rubin's Rules
use "imp_PCA_preg.dta", clear

** Check descriptive stats of observed vs imputed data

* Outcome 1) Healthy PCA - Imputed data seems quite a bit lower healthy PCA values
sum c3840 if _mi_m == 0
mi estimate: mean c3840

* Specifically check the observed data vs previously-missing data in first few imputed datasets
mi convert wide

gen miss_PCA = 1
replace miss_PCA = 0 if c3840 < .
tab miss_PCA, m

sum c3840
mi estimate: mean c3840 if miss_PCA == 1

* Outcome 2) Traditional PCA (imputed data slightly less traditional, but v. small diff)
sum c3841
mi estimate: mean c3841 if miss_PCA == 1

* Outcome 3) Processed PCA (imputed data a lot more processed)
sum c3842
mi estimate: mean c3842 if miss_PCA == 1

* Outcome 4) Confectionary PCA (imputed data a bit higher for confectionary)
sum c3843
mi estimate: mean c3843 if miss_PCA == 1

* Outcome 5) Vegetarian PCA (imputed data is somewhat more vegetarian)
sum c3844
mi estimate: mean c3844 if miss_PCA == 1

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
use "imp_PCA_preg.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist c3840 d810 c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist c3840 d813_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist c3840 d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
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
foreach var of varlist c3840 lapsed_Xian c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose preg_diet
postfile preg_diet str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "preg_diet_results.dta", replace

	
foreach var of varlist c3840-c3844 {
	
	// Save the outcome variable as a macro
	if "`var'" == "c3840" {
		local outcome = "Healthy PCA"
	}
	else if "`var'" == "c3841" {
		local outcome = "Traditional PCA"
	}
	else if "`var'" == "c3842" {
		local outcome = "Processed PCA"
	}
	else if "`var'" == "c3843" {
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
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	
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
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_diet ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
}

postclose preg_diet


****************************************************************************
*** Now to try and make some nice plots from this data
use "preg_diet_results.dta", clear
format %9.3f coef-p


** Convert string vars to numeric so can order on graph

* Outcome
tab outcome

gen outcome_num = 0
replace outcome_num = 1 if outcome == "Traditional PCA"
replace outcome_num = 2 if outcome == "Processed PCA"
replace outcome_num = 3 if outcome == "Confectionary PCA"
replace outcome_num = 4 if outcome == "Vegetarian PCA"

label define out_lb 0 "Healthy PCA" 1 "Traditional PCA" 2 "Processed PCA" 3 "Confectionary PCA" 4 "Vegetarian PCA"
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


** Make 5 plots (one for each outcome), then combine together

* Healthy PCA
twoway (scatter level_split coef if outcome_num == 0 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Health-conscious PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
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
	
* Traditional PCA
twoway (scatter level_split coef if outcome_num == 1 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Traditional PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
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
	name(trad, replace)
	
* Processed PCA
twoway (scatter level_split coef if outcome_num == 2 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Processed PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
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
twoway (scatter level_split coef if outcome_num == 3 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Confectionary PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
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
twoway (scatter level_split coef if outcome_num == 4 & model_num == 0, ///
		col(black) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 1, ///
		col(red) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 2, ///
		col(blue) msize(vsmall) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Vegetarian PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
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

grc1leg healthy trad process confect veg

* This isn't really the nicest plot, though...
graph export ".\PregC_Results\Preg_RSBB_DietPatterns.pdf", replace


** Try combining 'healthy' and 'veg' into one plot, and 'confectionary' and 'processed' into another

* Healthy PCA
twoway (scatter level_split coef if outcome_num == 0 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Health-conscious PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(healthy, replace)
	
* Vegetarian PCA
twoway (scatter level_split coef if outcome_num == 4 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Vegetarian PCA", size(medium)) ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
	xline(0, lcol(black) lpattern(dash)) ///
	ylabel(0 `" "Not sure believes in God" "(ref = No)" "' ///
	1 `" "Believes in God" "(ref = No)" "' ///
	3 `" "Christian" "(ref = None)" "' ///
	4 `" "Other religion" "(ref = None)" "' ///
	6 `" "Church min 1 per/month" "(ref = Not at all)" "' ///
	7 `" "Church min 1 per/year" "(ref = Not at all)" "' ///
	9 `" "Christian non-believer" "(ref = No religion/belief)" "' ///
	10 `" "Christian believer" "ref = No religion/belief)" "' ///
	11 `" "Other" "(ref = No religion/belief)" "', labsize(vsmall) angle(0)) ///
	legend(order(1 "Unadjusted (CCA)" 3 "Adjusted (CCA)" 5 "Adjusted (MI)") ///
	cols(3) size(small)) ///
	name(veg, replace)

* And combine together
grc1leg healthy veg
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_HealthVeg.pdf", replace


* And for traditional, processed and oconfectionary
grc1leg trad process confect
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_TradConfProc.pdf", replace



** Will save separate plots as well, as individually they look nicer

* Healthy PCA
twoway (scatter level_split coef if outcome_num == 0 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 0 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 0 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Health-conscious PCA") ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
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
	
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_Healthy.pdf", replace


* Traditional PCA
twoway (scatter level_split coef if outcome_num == 1 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 1 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 1 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Traditional PCA") ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
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
	
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_Traditional.pdf", replace

	
* Processed PCA
twoway (scatter level_split coef if outcome_num == 2 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 2 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 2 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Processed PCA") ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
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
	
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_Processed.pdf", replace

	
* Confectionary PCA
twoway (scatter level_split coef if outcome_num == 3 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 3 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 3 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Confectionary PCA") ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
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
	
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_Confectionary.pdf", replace

	
* Vegetarian PCA
twoway (scatter level_split coef if outcome_num == 4 & model_num == 0, ///
		col(black) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 0, ///
		horizontal col(black) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 1, ///
		col(red) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 1, ///
		horizontal col(red) msize(vtiny)) ///
	(scatter level_split coef if outcome_num == 4 & model_num == 2, ///
		col(blue) msize(small) msym(D)) ///
	(rcap lci uci level_split if outcome_num == 4 & model_num == 2, ///
		horizontal col(blue) msize(vtiny)), ///
	ysc(reverse) title("Vegetarian PCA") ytitle("") ///
	xscale(range(-0.4 0.6)) xlabel(-0.4 (0.2) 0.6, labsize(small) format(%9.1f)) ///
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
	
graph export ".\PregC_Results\Preg_RSBB_DietPatterns_Vegetarian.pdf", replace

graph close _all


** Also want to save out data as a table in CSV format - Also add in marker to say if 'significant' (p <= 0.05) using bonferroni corrected p-values (as five PCAs, will divide by 5)
gen bon_sig = 0
replace bon_sig = 1 if p <= 0.05 / 5
tab bon_sig, m

* Now save table (plus separate tables for each PCA)
list outcome exposure level model n-p bon_sig in 1/10, clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_fullResults.csv", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Healthy PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_HealthyResults.csv" if outcome == "Healthy PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Traditional PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_TradResults.csv" if outcome == "Traditional PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Processed PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_ProcessedResults.csv" if outcome == "Processed PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Confectionary PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_ConfectResults.csv" if outcome == "Confectionary PCA", comma replace

list outcome exposure level model n-p bon_sig if outcome == "Vegetarian PCA", clean
outsheet outcome exposure level model n-p bon_sig using ".\PregC_Results\PCA_VegResults.csv" if outcome == "Vegetarian PCA", comma replace



*********************************************************************************
**** As seems to be some association between RSBB and diet in the G0 mums in pregnancy, will explore this in more detail by looking at specific nutrient intake. (note that sample size for nutrients is slightly lower than for PCAs - This is because the PCAs and nutrient intakes were calculated separately, with the PCAs based on transformed raw FFQ items (not the nutrient intakes) and allowed for small amounts of missing data)

** Read in the temp dataset which has the cleaned variables
use "RSBB_Diet_CQuest_temp.dta", clear


*** Explore/describe the nutrient data, look at correlations and see if distributions normal-ish
sum c3800-c3836

* Correlation matrix - c3806, c3807 and c3808 (omega-3 intake, DHA and EPA intake from fish) all pretty much perfectly correlated (as all based on the same fish data), so will just use variable c3806. Some other correlations are very high, but as not prefectly correlated will leave as is
corr c3800-c3836

drop c3807 c3808

* Histograms
*foreach var of varlist c3800-c3836 {
*	hist `var', freq
*}

** The following variables have very odd multi-modal distributions. For these variables, will include in ordinary linear regression models along with all other variables for now, but explore them in more depth aferwards to ensure that results not biased due to odd distribution

* c3802 - Daily carotene intake (three peaks at 1000, 2000 and 5000 ug)
hist c3802, freq

* c3806 - Daily omega-3 fatty acid intake (peaks at 0, 0.2 and 0.4, and not even approaching a normal distribution - Because is only based on fish questions, not whole diet)
hist c3806, freq

* c3822 - Daily retinol intake (main peak at ~200, but then another small peak at 2000)
hist c3822, freq


** Will also drop niacin and trypt60, as the sum of these makes niacin equivalent (which will keep)
desc c3814 c3815 c3830
sum c3814 c3815 c3830
corr c3814 c3815 c3830
list c3814 c3815 c3830 in 1/10

drop c3814 c3830


*** Make a marker for each of the exposures to say whether included in CCA or not

* Exposure 1) Belief in God
gen belief_CCA_nut = 1
foreach var of varlist c3800 d810 c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA_nut = 0 if belief_CCA_nut == 1 & `var' >= .
}
tab belief_CCA_nut, m

* Exposure 2) Religious affiliation
gen denom_CCA_nut = 1
foreach var of varlist c3800 d813_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA_nut = 0 if denom_CCA_nut == 1 & `var' >= .
}
tab denom_CCA_nut, m

* Exposure 3) Church attendance
gen attend_CCA_nut = 1
foreach var of varlist c3800 d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace attend_CCA_nut = 0 if attend_CCA_nut == 1 & `var' >= .
}
tab attend_CCA_nut, m

* Exposure 4) Lapsed Christians
gen lapsed_CCA_nut = 1
foreach var of varlist c3800 lapsed_Xian c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA_nut = 0 if lapsed_CCA_nut == 1 & `var' >= .
}
tab lapsed_CCA_nut, m


*** Next, set up a loop to cycle through each nutrient outcome and for each exposure describe the raw data, run an unadjusted analysis (on the CCA sample size), then run adjusted analyses for all nutrients
foreach var of varlist c3800-c3836 {
	
	// Exposure 1) Belief in God (no belief in God as reference category)
	oneway `var' d810 if belief_CCA_nut == 1, tab // Description of diet
	regress `var' ib3.d810 if belief_CCA_nut == 1 // Unadjusted regression model
	regress `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 2) Religious affiliation (none as reference category)
	oneway `var' d813_grp if denom_CCA_nut == 1, tab // Description of diet
	regress `var' i.d813_grp if denom_CCA_nut == 1 // Unadjusted regression model
	regress `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 3) Church attendance (not at all as reference category)
	oneway `var' d816_grp if attend_CCA_nut == 1, tab // Description of diet
	regress `var' ib3.d816_grp if attend_CCA_nut == 1 // Unadjusted regression model
	regress `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
	// Exposure 4) Lapsed Christians (Atheists as reference category)
	oneway `var' lapsed_Xian if lapsed_CCA_nut == 1, tab // Description of diet
	regress `var' i.lapsed_Xian if lapsed_CCA_nut == 1 // Unadjusted regression model
	regress `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp // Adjusted regression model
	
}


** Check the variables with a dodgy distribution by splitting in to deciles and compare results - Overall results are pretty similar, so not likely to be any major bias caused by these oddly-distributed variables

* c3802 - Daily carotene intake (three peaks at 1000, 2000 and 5000 ug)
hist c3802, freq

sum c3802

xtile c3802_dec = c3802, nq(10)
tab c3802_dec

* Belief in God
oneway c3802 d810 if belief_CCA_nut == 1, tab
regress c3802 ib3.d810 if belief_CCA_nut == 1
regress c3802 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3802_dec d810 if belief_CCA_nut == 1, tab
regress c3802_dec ib3.d810 if belief_CCA_nut == 1
regress c3802_dec ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway c3802 d813_grp if belief_CCA_nut == 1, tab
regress c3802 i.d813_grp if belief_CCA_nut == 1
regress c3802 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3802_dec d813_grp if belief_CCA_nut == 1, tab
regress c3802_dec i.d813_grp if belief_CCA_nut == 1
regress c3802_dec i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway c3802 d816_grp if belief_CCA_nut == 1, tab
regress c3802 ib3.d816_grp if belief_CCA_nut == 1
regress c3802 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3802_dec d816_grp if belief_CCA_nut == 1, tab
regress c3802_dec ib3.d816_grp if belief_CCA_nut == 1
regress c3802_dec ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway c3802 lapsed_Xian if belief_CCA_nut == 1, tab
regress c3802 i.lapsed_Xian if belief_CCA_nut == 1
regress c3802 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3802_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress c3802_dec i.lapsed_Xian if belief_CCA_nut == 1
regress c3802_dec i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp


* c3806 - Daily omega-3 fatty acid intake (peaks at 0, 0.2 and 0.4, and not even approaching a normal distribution - Because is only based on fish questions, not whole diet)
hist c3806, freq

sum c3806

xtile c3806_dec = c3806, nq(10)
tab c3806_dec

* Belief in God
oneway c3806 d810 if belief_CCA_nut == 1, tab
regress c3806 ib3.d810 if belief_CCA_nut == 1
regress c3806 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3806_dec d810 if belief_CCA_nut == 1, tab
regress c3806_dec ib3.d810 if belief_CCA_nut == 1
regress c3806_dec ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway c3806 d813_grp if belief_CCA_nut == 1, tab
regress c3806 i.d813_grp if belief_CCA_nut == 1
regress c3806 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3806_dec d813_grp if belief_CCA_nut == 1, tab
regress c3806_dec i.d813_grp if belief_CCA_nut == 1
regress c3806_dec i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway c3806 d816_grp if belief_CCA_nut == 1, tab
regress c3806 ib3.d816_grp if belief_CCA_nut == 1
regress c3806 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3806_dec d816_grp if belief_CCA_nut == 1, tab
regress c3806_dec ib3.d816_grp if belief_CCA_nut == 1
regress c3806_dec ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway c3806 lapsed_Xian if belief_CCA_nut == 1, tab
regress c3806 i.lapsed_Xian if belief_CCA_nut == 1
regress c3806 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3806_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress c3806_dec i.lapsed_Xian if belief_CCA_nut == 1
regress c3806_dec i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp


* c3822 - Daily retinol intake (main peak at ~200, but then another small peak at 2000)
hist c3822, freq

sum c3822

xtile c3822_dec = c3822, nq(10)
tab c3822_dec

* Belief in God
oneway c3822 d810 if belief_CCA_nut == 1, tab
regress c3822 ib3.d810 if belief_CCA_nut == 1
regress c3822 ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3822_dec d810 if belief_CCA_nut == 1, tab
regress c3822_dec ib3.d810 if belief_CCA_nut == 1
regress c3822_dec ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Religion
oneway c3822 d813_grp if belief_CCA_nut == 1, tab
regress c3822 i.d813_grp if belief_CCA_nut == 1
regress c3822 i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3822_dec d813_grp if belief_CCA_nut == 1, tab
regress c3822_dec i.d813_grp if belief_CCA_nut == 1
regress c3822_dec i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Church attendance
oneway c3822 d816_grp if belief_CCA_nut == 1, tab
regress c3822 ib3.d816_grp if belief_CCA_nut == 1
regress c3822 ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3822_dec d816_grp if belief_CCA_nut == 1, tab
regress c3822_dec ib3.d816_grp if belief_CCA_nut == 1
regress c3822_dec ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

* Belief and religion
oneway c3822 lapsed_Xian if belief_CCA_nut == 1, tab
regress c3822 i.lapsed_Xian if belief_CCA_nut == 1
regress c3822 i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp

oneway c3822_dec lapsed_Xian if belief_CCA_nut == 1, tab
regress c3822_dec i.lapsed_Xian if belief_CCA_nut == 1
regress c3822_dec i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp



**********************************************************************************
*** As lots of missing data, will do MI for nutrients. However, as there are such a large number of nutrients I will only impute exposures and covariates, not missing nutrient data, else with over 30 nutrients, plus exposures, covariates and auxiliary variables the imputation would take a very long time (with the MI on dietary patterns, even though there were differences between observed and imputed data, the overall results of the regression models did not materially change, suggesting that it's not necessary to impute the missing nutrient data).

* Drop variables not included in MI to speed up imputation and make file much smaller - Will still include later RSBB data to try and impute any of the missing pregnancy RSBB vars. Ditto for auxiliary SEP variables (partner education and occupational social class)
keep aln c3800-c3836 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

* Drop cases to only included those with non-missing nutrient data
drop if c3800 == .

* Check missing data
misstable sum c3800 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp, all

misstable patterns c3800 d810 d813_grp d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp, freq

** Set up the imputation and register the variables with missing data
mi set mlong
mi register regular c3800-c3836 c994
mi register imputed d810 d813_grp d816_grp c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp k6240 k6243_grp k6247_grp c666a c765_grp mum_smk b371 b040_grp

** Test the imputation with a dry run first, to make sure looks okay
mi impute chained ///
	(pmm, knn(5)) b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp ///
	= c3800-c3836 c994, ///
	add(10) burnin(10) rseed(56789) dryrun
	
	
** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about 2 hours. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010). As convergence okay for more complicated PCA imputation, wont check again here
mi impute chained ///
	(pmm, knn(5)) b371 ///
	(logit) c755_grp b594_grp dur01ind_grp c800_grp c765_grp ///
	(mlogit) d810 d813_grp a006_grp a525_grp k6240 k6243_grp mum_smk ///
	(ologit) d816_grp c645a dimd2010q5 b032_grp k6247_grp c666a b040_grp ///
	= c3800-c3836 c994, ///
	add(50) burnin(10) rseed(56789) dots augment


** Save this imputed dataset, so not have to run whole imputation again to access results
save "imp_nutrients_preg.dta", replace


** Read in the imputed data (if skipping imputation) and make sure is in flong format
use "imp_nutrients_preg.dta", clear
mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d810 c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d813_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
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
foreach var of varlist c3800 lapsed_Xian c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose preg_nut
postfile preg_nut str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "preg_nutrient_results.dta", replace

	
foreach var of varlist c3800-c3836 {
	
	// Save the outcome variable as a macro
	if "`var'" == "c3800" {
		local outcome = "Calcium (mg)"
	}
	else if "`var'" == "c3801" {
		local outcome = "Carbohydrate (g)"
	}
	else if "`var'" == "c3802" {
		local outcome = "Carotene (ug)"
	}
	else if "`var'" == "c3803" {
		local outcome = "Cholesterol (mg)"
	}
	else if "`var'" == "c3804" {
		local outcome = "Energy (kJ)"
	}
	else if "`var'" == "c3805" {
		local outcome = "Fat (g)"
	}
	else if "`var'" == "c3806" {
		local outcome = "Omega-3 (g)"
	}
	else if "`var'" == "c3809" {
		local outcome = "Folate (ug)"
	}
	else if "`var'" == "c3810" {
		local outcome = "Iodine (ug)"
	}
	else if "`var'" == "c3811" {
		local outcome = "Iron (mg)"
	}
	else if "`var'" == "c3812" {
		local outcome = "Magnesium (mg)"
	}
	else if "`var'" == "c3813" {
		local outcome = "Monounsaturated fat (mg)"
	}
	else if "`var'" == "c3815" {
		local outcome = "Niacin equivalent (mg)"
	}
	else if "`var'" == "c3816" {
		local outcome = "Non-milk sugars (g)"
	}
	else if "`var'" == "c3817" {
		local outcome = "Fibre (g)"
	}
	else if "`var'" == "c3818" {
		local outcome = "Phosphorus (mg)"
	}
	else if "`var'" == "c3819" {
		local outcome = "Polyunsaturated fat (g)"
	}
	else if "`var'" == "c3820" {
		local outcome = "Potassium (mg)"
	}
	else if "`var'" == "c3821" {
		local outcome = "Protein (g)"
	}
	else if "`var'" == "c3822" {
		local outcome = "Retinol (ug)"
	}
	else if "`var'" == "c3823" {
		local outcome = "Riboflavin (mg)"
	}
	else if "`var'" == "c3824" {
		local outcome = "Saturated fat (g)"
	}
	else if "`var'" == "c3825" {
		local outcome = "Selenium (ug)"
	}
	else if "`var'" == "c3826" {
		local outcome = "Sodium (mg)"
	}
	else if "`var'" == "c3827" {
		local outcome = "Starch (g)"
	}
	else if "`var'" == "c3828" {
		local outcome = "Sugar (g)"
	}
	else if "`var'" == "c3829" {
		local outcome = "Thiamin (mg)"
	}
	else if "`var'" == "c3831" {
		local outcome = "Vitamin C (mg)"
	}
	else if "`var'" == "c3832" {
		local outcome = "Vitamin B6 (mg)"
	}
	else if "`var'" == "c3833" {
		local outcome = "Vitamin B12 (ug)"
	}
	else if "`var'" == "c3834" {
		local outcome = "Vitamin D (ug)"
	}
	else if "`var'" == "c3835" {
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
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	
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
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: regress `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	regress `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: regress `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
	local model = "MI"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_nut ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
}

postclose preg_nut


********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "preg_nutrient_results.dta", clear
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

label define out_lb 1 "Energy (kJ)" 2 "Carbohydrates (g)" 3 "Sugars (g)" 4 "Non-milk sugars (g)" 5 "Starch (g)" 6 "Fibre (g)" 7 "Fat (g)" 8 "Monounsaturated fat (g)" 9 "Polyunsaturated fat (g)" 10 "Saturated fat (g)" 11 "Omega-3 (g; from fish)" 12 "Cholesterol (mg)" 13 "Protein (g)" 14 "Thiamin (mg)" 15 "Riboflavin (mg)" 16 "Niacin equivalent (mg)" 17 "Vitamin B6 (mg)" 18 "Vitamin B12 (ug)" 19 "Folate (ug)" 20 "Vitamin C (mg)" 21 "Retinol/Vitamin A (ug)" 22 "Carotene (ug)" 23 "Vitamin D (ug)" 24 "Vitamin E (mg)" 25 "Calcium (mg)" 26 "Phosphorus (mg)" 27 "Magnesium (mg)" 28 "Sodium (mg)" 29 "Potassium (mg)" 30 "Iron (mg)" 31 "Zinc (mg)" 32 "Selenium (ug)" 33 "Iodine (ug)"

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
	
graph export ".\PregC_Results\Nutrients_BeliefInGod_pvalues.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/33
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nut_belief_bon.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\PregC_Results\nut_belief_05.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\PregC_Results\nut_belief_full.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


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
	
graph export ".\PregC_Results\Nutrients_Religion_pvalues.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/33
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nut_religion_bon.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\PregC_Results\nut_religion_05.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\PregC_Results\nut_religion_full.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


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
	
graph export ".\PregC_Results\Nutrients_ChurchAttendance_pvalues.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/33
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nut_attend_bon.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\PregC_Results\nut_attend_05.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\PregC_Results\nut_attend_full.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


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
		
graph export ".\PregC_Results\Nutrients_BeliefAndReligion_pvalues.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/33
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nut_belief_relig_bon.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\PregC_Results\nut_belief_relig_05.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\PregC_Results\nut_belief_relig_full.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


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
graph export ".\PregC_Results\Nutrients_combined_pvalues.pdf", replace	

graph close _all


***********************************************************************************
*** But are these differences in nutrient intake meaningful? There may be mean differences, but if everyone is getting enough nutrients, then there might not be any harm/impact on health. Ideally want to split into groups such as 'safe vs harmful' or 'meeting RNIs vs not' and see if results hold.

*** Have put together nutritional requirement intakes based on Scientific Advisory Committee on Nutrition (SACN) 2016 recommendations (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/618167/government_dietary_recommendations.pdf) and 1991 Committee on Medical Aspects of Food Policy (COMA) report (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/743790/Dietary_Reference_Values_-_A_Guide__1991_.pdf). Based on average adult female recommendations (age 19-49), plus extra intake if pregnant

*** Will go through each nutrient and code as whether meeting Recommended Nutrient Intakes (RNI) or Estimated Average Requirements (EAR), or whether consuming less/more than recommended min/max intakes

** Read in the imputed nutrient data and make sure is in flong format
use "imp_nutrients_preg.dta", clear
*mi convert flong


** Create CCA marker variables for each exposure

* 1) Belief in God
tab d810 if _mi_m == 0, m

gen belief_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d810 c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace belief_CCA = 0 if belief_CCA == 1 & `var' >= .
}
tab belief_CCA, m

* 2) Denomination
tab d813_grp if _mi_m == 0, m

gen denom_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d813_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace denom_CCA = 0 if denom_CCA == 1 & `var' >= .
}
tab denom_CCA, m

* 3) Church attendance
tab d816_grp if _mi_m == 0, m

gen attend_CCA = 1 if _mi_m == 0
foreach var of varlist c3800 d816_grp c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
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
foreach var of varlist c3800 lapsed_Xian c994 c645a c755_grp dimd2010q5 a006_grp b594_grp dur01ind_grp c800_grp a525_grp b032_grp {
	replace lapsed_CCA = 0 if lapsed_CCA == 1 & `var' >= .
}
tab lapsed_CCA, m


*** Now go through each nutrient and code as binary variable whether meets recommended daily intakes

** Calcium (mg) - RNI of 700mg for adult women
gen calc_RNI = .
replace calc_RNI = 1 if c3800 < 700
replace calc_RNI = 0 if c3800 >= 700 & c3800 < .
tab calc_RNI if _mi_m == 0, m

label define calc_lb 0 ">= RNI (700mg)" 1 "< RNI (700mg)"
numlabel calc_lb, add
label values calc_RNI calc_lb
tab calc_RNI if _mi_m == 0, m

** Carbohydrate (g) - Minimum 267g for adult women
gen carb_min = .
replace carb_min = 1 if c3801 < 267
replace carb_min = 0 if c3801 >= 267 & c3801 < .
tab carb_min if _mi_m == 0, m

label define carb_lb 0 ">= min (267g)" 1 "< min (267mg)"
numlabel carb_lb, add
label values carb_min carb_lb
tab carb_min if _mi_m == 0, m

** Carotene (ug) - No recommendations for carotene

** Cholesterol (mg) - No recommendations for cholesterol

** Energy (kJ) - Estimated average requirements 2000kJ for adult women (+200 for pregnancy)
gen energy_EAR = .
replace energy_EAR = 1 if c3804 > 2200
replace energy_EAR = 0 if c3804 <= 2200 & c3804 < .
tab energy_EAR if _mi_m == 0, m

label define energy_lb 0 "<= EAR (2200kJ)" 1 "> EAR (2200kJ)"
numlabel energy_lb, add
label values energy_EAR energy_lb
tab energy_EAR if _mi_m == 0, m

* As no-one below the EAR, will drop this variable
drop energy_EAR

** Fat (g) - Maximum 78g for adult women
gen fat_max = .
replace fat_max = 1 if c3805 > 78
replace fat_max = 0 if c3805 <= 78 & c3805 < .
tab fat_max if _mi_m == 0, m

label define fat_lb 0 "<= max (78g)" 1 "> max (78g)"
numlabel fat_lb, add
label values fat_max fat_lb
tab fat_max if _mi_m == 0, m

** Omega-3 fatty acid from fish only (g) - No RNI, but more omega-3 recommended, although do caution against eating oily fish more than twice a week in pregnancy (https://www.nutrition.org.uk/life-stages/pregnancy/healthy-eating-during-pregnancy/nutrition-during-pregnancy/). Because of it's potential importance in pregnancy for birth outcomes, will code into low (<0.2g/day) and high (>0.25g/day) as this is where there is a natural split in the data (but important to bear in mind that this is not based on any formal RNI recommendations, and omega-3 is only counted for fish, not sources from other foods)
sum c3806 if c3806 > 0.2 & c3806 < 0.25

gen omega3 = .
replace omega3 = 1 if c3806 < 0.2
replace omega3 = 0 if c3806 >= 0.25 & c3806 < .
tab omega3 if _mi_m == 0, m

label define omega3_lb 0 "High (>0.25g)" 1 "Low (<0.2g)"
numlabel omega3_lb, add
label values omega3 omega3_lb
tab omega3 if _mi_m == 0, m

** Folate (ug) - RNI of 200ug for adult women (+100 for pregnanct women)
gen folate_RNI = .
replace folate_RNI = 1 if c3809 < 300
replace folate_RNI = 0 if c3809 >= 300 & c3809 < .
tab folate_RNI if _mi_m == 0, m

label define folate_lb 0 ">= RNI (300ug)" 1 "< RNI (300ug)"
numlabel folate_lb, add
label values folate_RNI folate_lb
tab folate_RNI if _mi_m == 0, m

** Iodine (ug) - RNI of 140ug for adult women
gen iodine_RNI = .
replace iodine_RNI = 1 if c3810 < 140
replace iodine_RNI = 0 if c3810 >= 140 & c3810 < .
tab iodine_RNI if _mi_m == 0, m

label define iodine_lb 0 ">= RNI (140ug)" 1 "< RNI (140ug)"
numlabel iodine_lb, add
label values iodine_RNI iodine_lb
tab iodine_RNI if _mi_m == 0, m

** Iron (mg) - RNI of 14.8mg for adult women
gen iron_RNI = .
replace iron_RNI = 1 if c3811 < 14.8
replace iron_RNI = 0 if c3811 >= 14.8 & c3811 < .
tab iron_RNI if _mi_m == 0, m

label define iron_lb 0 ">= RNI (14.8mg)" 1 "< RNI (14.8mg)"
numlabel iron_lb, add
label values iron_RNI iron_lb
tab iron_RNI if _mi_m == 0, m

** Magnesium (mg) - RNI of 270mg for adult women
gen mag_RNI = .
replace mag_RNI = 1 if c3812 < 270
replace mag_RNI = 0 if c3812 >= 270 & c3812 < .
tab mag_RNI if _mi_m == 0, m

label define mag_lb 0 ">= RNI (270mg)" 1 "< RNI (270mg)"
numlabel mag_lb, add
label values mag_RNI mag_lb
tab mag_RNI if _mi_m == 0, m

** Mono-unsaturated fat (g) - RNI of 29g for adult women
gen mono_RNI = .
replace mono_RNI = 1 if c3813 < 29
replace mono_RNI = 0 if c3813 >= 29 & c3813 < .
tab mono_RNI if _mi_m == 0, m

label define mono_lb 0 ">= RNI (29g)" 1 "< RNI (29g)"
numlabel mono_lb, add
label values mono_RNI mono_lb
tab mono_RNI if _mi_m == 0, m

** Niacin equivalent (mg; niacin + tyrpt/60) - RNI of 13.2mg for adult women
gen niacinEq_RNI = .
replace niacinEq_RNI = 1 if c3815 < 13.2
replace niacinEq_RNI = 0 if c3815 >= 13.2 & c3815 < .
tab niacinEq_RNI if _mi_m == 0, m

label define niacinEq_lb 0 ">= RNI (13.2mg)" 1 "< RNI (13.2mg)"
numlabel niacinEq_lb, add
label values niacinEq_RNI niacinEq_lb
tab niacinEq_RNI if _mi_m == 0, m

** Non-milk extrinsic sugars (g; 'free sugars') - Maximum 27g for adult women
gen sugar_max = .
replace sugar_max = 1 if c3816 > 27
replace sugar_max = 0 if c3816 <= 27 & c3816 < .
tab sugar_max if _mi_m == 0, m

label define sugar_lb 0 "<= max (27g)" 1 "> max (27g)"
numlabel sugar_lb, add
label values sugar_max sugar_lb
tab sugar_max if _mi_m == 0, m

** Fibre (g) - RNI of 30g for adult women
gen fibre_RNI = .
replace fibre_RNI = 1 if c3817 < 30
replace fibre_RNI = 0 if c3817 >= 30 & c3817 < .
tab fibre_RNI if _mi_m == 0, m

label define fibre_lb 0 ">= RNI (30g)" 1 "< RNI (30g)"
numlabel fibre_lb, add
label values fibre_RNI fibre_lb
tab fibre_RNI if _mi_m == 0, m

** Phosphorous (mg) - RNI of 550mg for adult women
gen phos_RNI = .
replace phos_RNI = 1 if c3818 < 550
replace phos_RNI = 0 if c3818 >= 550 & c3818 < .
tab phos_RNI if _mi_m == 0, m

label define phos_lb 0 ">= RNI (550mg)" 1 "< RNI (550mg)"
numlabel phos_lb, add
label values phos_RNI phos_lb
tab phos_RNI if _mi_m == 0, m

** Poly-unsaturated fat (g) - RNI of 14g for adult women
gen poly_RNI = .
replace poly_RNI = 1 if c3819 < 14
replace poly_RNI = 0 if c3819 >= 14 & c3819 < .
tab poly_RNI if _mi_m == 0, m

label define poly_lb 0 ">= RNI (14g)" 1 "< RNI (14g)"
numlabel poly_lb, add
label values poly_RNI poly_lb
tab poly_RNI if _mi_m == 0, m

** Potassium (mg) - RNI of 3500mg for adult women
gen pot_RNI = .
replace pot_RNI = 1 if c3820 < 3500
replace pot_RNI = 0 if c3820 >= 3500 & c3820 < .
tab pot_RNI if _mi_m == 0, m

label define pot_lb 0 ">= RNI (3500mg)" 1 "< RNI (3500mg)"
numlabel pot_lb, add
label values pot_RNI pot_lb
tab pot_RNI if _mi_m == 0, m

** Protein (g) - RNI of 45.0g for adult women (+6 for pregnancy)
gen prot_RNI = .
replace prot_RNI = 1 if c3821 < 51
replace prot_RNI = 0 if c3821 >= 51 & c3821 < .
tab prot_RNI if _mi_m == 0, m

label define prot_lb 0 ">= RNI (51g)" 1 "< RNI (51g)"
numlabel prot_lb, add
label values prot_RNI prot_lb
tab prot_RNI if _mi_m == 0, m

** Retinol/Vitamin A (ug) - RNI of 600ug for adult women (+100 for pregnancy)
gen ret_RNI = .
replace ret_RNI = 1 if c3822 < 700
replace ret_RNI = 0 if c3822 >= 700 & c3822 < .
tab ret_RNI if _mi_m == 0, m

label define ret_lb 0 ">= RNI (700ug)" 1 "< RNI (700ug)"
numlabel ret_lb, add
label values ret_RNI ret_lb
tab ret_RNI if _mi_m == 0, m

** Riboflavin (mg) - RNI of 1.1mg for adult women (+0.3 for pregnancy)
gen ribo_RNI = .
replace ribo_RNI = 1 if c3823 < 1.4
replace ribo_RNI = 0 if c3823 >= 1.4 & c3823 < .
tab ribo_RNI if _mi_m == 0, m

label define ribo_lb 0 ">= RNI (1.4mg)" 1 "< RNI (1.4mg)"
numlabel ribo_lb, add
label values ribo_RNI ribo_lb
tab ribo_RNI if _mi_m == 0, m

** Saturated fat (g) - Maximum 24g for adult women
gen sat_max = .
replace sat_max = 1 if c3824 > 24
replace sat_max = 0 if c3824 <= 24 & c3824 < .
tab sat_max if _mi_m == 0, m

label define sat_lb 0 "<= max (24g)" 1 "> max (24g)"
numlabel sat_lb, add
label values sat_max sat_lb
tab sat_max if _mi_m == 0, m

** Selenium (ug) - RNI of 60ug for adult women
gen selen_RNI = .
replace selen_RNI = 1 if c3825 < 60
replace selen_RNI = 0 if c3825 >= 60 & c3825 < .
tab selen_RNI if _mi_m == 0, m

label define selen_lb 0 ">= RNI (60ug)" 1 "< RNI (60ug)"
numlabel selen_lb, add
label values selen_RNI selen_lb
tab selen_RNI if _mi_m == 0, m

** Sodium (mg) - RNI of 1600mg for adult women, but recommended max intake of 2400mg (6g salt)
gen sodium_RNI = .
replace sodium_RNI = 1 if (c3826 < 1600 | c3826 > 2400) & c3826 < .
replace sodium_RNI = 0 if c3826 >= 1600 & c3826 <= 2400
tab sodium_RNI if _mi_m == 0, m

label define sodium_lb 0 ">= RNI (1600mg) & < max (2400mg)" 1 "< RNI (1600mg) OR > max (2400mg)"
numlabel sodium_lb, add
label values sodium_RNI sodium_lb
tab sodium_RNI if _mi_m == 0, m

** Starch (g) - No recommendations for starch

** Total sugar (g) - No recommendations for total sugar

** Thiamin (mg) - RNI of 0.8mg for adult women (+0.1 for pregnancy)
gen thiamin_RNI = .
replace thiamin_RNI = 1 if c3829 < 0.9
replace thiamin_RNI = 0 if c3829 >= 0.9 & c3829 < .
tab thiamin_RNI if _mi_m == 0, m

label define thiamin_lb 0 ">= RNI (0.9mg)" 1 "< RNI (0.9mg)"
numlabel thiamin_lb, add
label values thiamin_RNI thiamin_lb
tab thiamin_RNI if _mi_m == 0, m

** Vitamin C (mg) - RNI of 40mg for adult women (+10 for pregnancy)
gen vitC_RNI = .
replace vitC_RNI = 1 if c3831 < 50
replace vitC_RNI = 0 if c3831 >= 50 & c3831 < .
tab vitC_RNI if _mi_m == 0, m

label define vitC_lb 0 ">= RNI (50mg)" 1 "< RNI (50mg)"
numlabel vitC_lb, add
label values vitC_RNI vitC_lb
tab vitC_RNI if _mi_m == 0, m

** Vitamin B6 (mg) - RNI of 1.2mg for adult women
gen vitB6_RNI = .
replace vitB6_RNI = 1 if c3832 < 1.2
replace vitB6_RNI = 0 if c3832 >= 1.2 & c3832 < .
tab vitB6_RNI if _mi_m == 0, m

label define vitB6_lb 0 ">= RNI (1.2mg)" 1 "< RNI (1.2mg)"
numlabel vitB6_lb, add
label values vitB6_RNI vitB6_lb
tab vitB6_RNI if _mi_m == 0, m

** Vitamin B12 (ug) - RNI of 1.5ug for adult women
gen vitB12_RNI = .
replace vitB12_RNI = 1 if c3833 < 1.5
replace vitB12_RNI = 0 if c3833 >= 1.5 & c3833 < .
tab vitB12_RNI if _mi_m == 0, m

label define vitB12_lb 0 ">= RNI (1.5ug)" 1 "< RNI (1.5ug)"
numlabel vitB12_lb, add
label values vitB12_RNI vitB12_lb
tab vitB12_RNI if _mi_m == 0, m

** Vitamin D (ug) - RNI of 10ug for adult women
gen vitD_RNI = .
replace vitD_RNI = 1 if c3834 < 10
replace vitD_RNI = 0 if c3834 >= 10 & c3834 < .
tab vitD_RNI if _mi_m == 0, m

label define vitD_lb 0 ">= RNI (10ug)" 1 "< RNI (10ug)"
numlabel vitD_lb, add
label values vitD_RNI vitD_lb
tab vitD_RNI if _mi_m == 0, m

** Vitamin E (mg) - Minimum of 3ug for adult women (+0.8 for pregnancy)
gen vitE_min = .
replace vitE_min = 1 if c3835 < 3.8
replace vitE_min = 0 if c3835 >= 3.8 & c3835 < .
tab vitE_min if _mi_m == 0, m

label define vitE_lb 0 ">= min (3.8ug)" 1 "< min (3.8ug)"
numlabel vitE_lb, add
label values vitE_min vitE_lb
tab vitE_min if _mi_m == 0, m

** Zinc (mg) - RNI of 7mg for adult women
gen zinc_RNI = .
replace zinc_RNI = 1 if c3836 < 7
replace zinc_RNI = 0 if c3836 >= 7 & c3836 < .
tab zinc_RNI if _mi_m == 0, m

label define zinc_lb 0 ">= RNI (7mg)" 1 "< RNI (7mg)"
numlabel zinc_lb, add
label values zinc_RNI zinc_lb
tab zinc_RNI if _mi_m == 0, m


*** Check this data, then run through same analyses as above exploring nutrient intake by RSBB
sum calc_RNI-zinc_RNI

* First, save summary stats of both raw nutrient intake and whether meeting RNIs. Store this data so easy to copy into table (install estout package if necessary)
*ssc install estout, replace
estpost summarize c3800-c3836 if _mi_m == 0
esttab . using ".\PregC_Results\PregC_nutrition_SumStats.csv", cells("count mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f))") nomtitle nonumber noobs replace

estpost summarize calc_RNI-zinc_RNI if _mi_m == 0
esttab . using ".\PregC_Results\PregC_nutritionRNI_SumStats.csv", cells("count sum mean(fmt(%9.3f))") nomtitle nonumber noobs replace



*** Now set up a 'postfile' to save results to, loop over each outcome, and save the results of the unadjusted model (CCA), adjusted CCA model, and adjusted MI model

* Create the file to post the data to (the 'capture' just closes the file it it's already open)
capture postclose preg_nut_RNI
postfile preg_nut_RNI str20 outcome str30 exposure str5 model str20 level /// 
	n coef se lci uci p ///
	using "preg_nutrient_RNI_results.dta", replace

	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Not sure"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable - For some reason the 'mi estimate: logistic' command gives the output in log-odds, rather than odds ratios, so need to manually construct these from the log-odds coefficient, the log SE and the critical value
	quietly tab belief_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d810 c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Yes"
	local coef_log = res[1,1]
	local se = res[2,1]
	local crit = res[8,1]
	local p = res[4,1]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
	
	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab denom_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.d813_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Min once a month"
	
	matrix res = r(table)
	local coef = res[1,1]
	local se = res[2,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab attend_CCA
	local n = r(N)
	
	mi estimate: logistic `var' ib3.d816_grp c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Min once a year"
	local coef_log = res[1,2]
	local se = res[2,2]
	local crit = res[8,2]
	local p = res[4,2]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
		
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Multivariable/adjusted model (CCA) - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	logistic `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp if _mi_m == 0
	
	local n = e(N)
	local model = "adj"
	local level = "Christian non-believer"
	
	matrix res = r(table)
	local coef = res[1,2]
	local se = res[2,2]
	local lci = res[5,2]
	local uci = res[6,2]
	local p = res[4,2]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef = res[1,3]
	local se = res[2,3]
	local lci = res[5,3]
	local uci = res[6,3]
	local p = res[4,3]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef = res[1,4]
	local se = res[2,4]
	local lci = res[5,4]
	local uci = res[6,4]
	local p = res[4,4]
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	// Imputed and adjusted model - Run model, then store coefficients as local macros and post results to file - Repeat for each level of variable
	quietly tab lapsed_CCA
	local n = r(N)
	
	mi estimate: logistic `var' i.lapsed_Xian c994 i.c645a i.c755_grp i.dimd2010q5 i.a006_grp i.b594_grp i.dur01ind_grp i.c800_grp i.a525_grp i.b032_grp
	
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
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Christian believer"
	local coef_log = res[1,3]
	local se = res[2,3]
	local crit = res[8,3]
	local p = res[4,3]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
		
	local level = "Other"
	local coef_log = res[1,4]
	local se = res[2,4]
	local crit = res[8,4]
	local p = res[4,4]
	
	local coef = exp(`coef_log')
	local lci = exp(`coef_log' - (`se' * `crit'))
	local uci = exp(`coef_log' + (`se' * `crit'))
	
	post preg_nut_RNI ("`outcome'") ("`exp'") ("`model'") ("`level'") ///
		(`n') (`coef') (`se') (`lci') (`uci') (`p')
}

postclose preg_nut_RNI


********************************************************************************
*** Make some plots based on the nutrient results. As lots of results, will first filter by Bonferonni-corrected p-value to include just those with the strongest association with RSBB
use "preg_nutrient_RNI_results.dta", clear
format %9.3f coef-p

* Transform the p-value by -log10 to get p-values on similar order of magnitude to be readable (0 = p-value of 1)
gen logp = -log10(p)
sum logp

* What's the Bonferroni correction here? Are 28 outcomes assessed, so 0.05 / 28 = 0.0018 (and -log10 of this is 2.75) - As nutrients not independent from one another perhaps the Bonferroni correction is too conservative, but will go with it for now.

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
replace outcome_num = 18 if outcome == "vitD_RNI"
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

label define out_lb 1 "Carbohydrates < min (267g)" 2 "Non-milk sugars > max (27g)" 3 "Fibre < RNI (30g)" 4 "Fat > max (78g)" 5 "Monounsaturated fat < RNI (29g)" 6 "Polyunsaturated fat < RNI (14g)" 7 "Saturated fat > max (24g)" 8 "Omega-3 < 0.2g (from fish)" 9 "Protein < RNI (51g)" 10 "Thiamin < RNI (0.9mg)" 11 "Riboflavin < RNI (1.4mg)" 12 "Niacin equivalent < RNI (13.2mg)" 13 "Vitamin B6 < RNI (1.2mg)" 14 "Vitamin B12 < RNI (1.5ug)" 15 "Folate < RNI (300ug)" 16 "Vitamin C < RNI (50mg)" 17 "Retinol/Vitamin A < RNI (700ug)" 18 "Vitamin D < RNI (10ug)" 19 "Vitamin E < min (3.8ug)" 20 "Calcium < RNI (700mg)" 21 "Phosphorus < RNI (550mg)" 22 "Magnesium < RNI (270mg)" 23 "Sodium < RNI (1.6g) | > max (2.4g)" 24 "Potassium < RNI (3500mg)" 25 "Iron < RNI (14.8mg)" 26 "Zinc < RNI (7mg)" 27 "Selenium < RNI (60ug)" 28 "Iodine < RNI (140ug)"

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
	
graph export ".\PregC_Results\NutrientsRNI_BeliefInGod_pvalues.pdf", replace	


* Now explore how belief in God associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold - Then save these results as CSV files to potentially use as tables
bysort outcome exposure model: egen p_combined = min(p)

sort outcome exposure level

gen belief_bon = 0
replace belief_bon = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05/28
tab belief_bon

list outcome level coef-p if belief_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nutRNI_belief_bon.csv" if belief_bon == 1, comma replace

gen belief_05 = 0
replace belief_05 = 1 if exposure == "Belief (ref = no)" & model == "MI" & p_combined < 0.05
tab belief_05

list outcome level coef-p if belief_05 == 1, clean
outsheet outcome level coef-p belief_bon using ".\PregC_Results\nutRNI_belief_05.csv" if belief_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_05 belief_bon using ".\PregC_Results\nutRNI_belief_full.csv" if exposure == "Belief (ref = no)" & model == "MI", comma replace


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
	
graph export ".\PregC_Results\NutrientsRNI_Religion_pvalues.pdf", replace	

* Now explore how religious affiliation associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen relig_bon = 0
replace relig_bon = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05/28
tab relig_bon

list outcome level coef-p if relig_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nutRNI_religion_bon.csv" if relig_bon == 1, comma replace

gen relig_05 = 0
replace relig_05 = 1 if exposure == "Denomination (ref = none)" & model == "MI" & p_combined < 0.05
tab relig_05

list outcome level coef-p if relig_05 == 1, clean
outsheet outcome level coef-p relig_bon using ".\PregC_Results\nutRNI_religion_05.csv" if relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p relig_05 relig_bon using ".\PregC_Results\nutRNI_religion_full.csv" if exposure == "Denomination (ref = none)" & model == "MI", comma replace


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
	
graph export ".\PregC_Results\NutrientsRNI_ChurchAttendance_pvalues.pdf", replace	

* Now explore how church attendance associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen attend_bon = 0
replace attend_bon = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05/28
tab attend_bon

list outcome level coef-p if attend_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nutRNI_attend_bon.csv" if attend_bon == 1, comma replace

gen attend_05 = 0
replace attend_05 = 1 if exposure == "Church attendance (ref = not a" & model == "MI" & p_combined < 0.05
tab attend_05

list outcome level coef-p if attend_05 == 1, clean
outsheet outcome level coef-p attend_bon using ".\PregC_Results\nutRNI_attend_05.csv" if attend_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p attend_05 attend_bon using ".\PregC_Results\nutRNI_attend_full.csv" if exposure == "Church attendance (ref = not a" & model == "MI", comma replace


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
		
graph export ".\PregC_Results\NutrientsRNI_BeliefAndReligion_pvalues.pdf", replace	

* Now explore how belief and religion associated with nutrient intake both when using Bonferroni threshold and when using ordinary 0.5 threshold
gen belief_relig_bon = 0
replace belief_relig_bon = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05/28
tab belief_relig_bon

list outcome level coef-p if belief_relig_bon == 1, clean
outsheet outcome level coef-p using ".\PregC_Results\nutRNI_belief_relig_bon.csv" if belief_relig_bon == 1, comma replace

gen belief_relig_05 = 0
replace belief_relig_05 = 1 if exposure == "Belief and religion (ref = non" & model == "MI" & p_combined < 0.05
tab belief_relig_05

list outcome level coef-p if belief_relig_05 == 1, clean
outsheet outcome level coef-p belief_relig_bon using ".\PregC_Results\nutRNI_belief_relig_05.csv" if belief_relig_05 == 1, comma replace

* And also save table with full results
outsheet outcome level coef-p belief_relig_05 belief_relig_bon using ".\PregC_Results\nutRNI_belief_relig_full.csv" if exposure == "Belief and religion (ref = non" & model == "MI", comma replace


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
graph export ".\PregC_Results\NutrientsRNI_combined_pvalues.pdf", replace	

graph close _all

log close

clear

