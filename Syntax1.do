*USING WHOLE DATASET*
use "S:\S0276 - MHCYP\IncomingTransferData\In\Jess thesis analysis\MHCYP17dataset.dta", clear

*tell STATA that negative values are missing =.*
replace ghqg2grp =. if ghqg2grp <0
replace ghq12scr =. if ghq12scr<0
replace eda =. if eda <0
replace Ten1 =. if Ten1<0
replace fadsgrp_2 = . if fadsgrp_2 <0
replace eqv3 = . if eqv <0
replace pimpact = . if pimpact <0
replace numsle5 = . if numsle5 <0
replace ethnic4 = . if ethnic4<0
replace fameco1 = . if fameco1<0
replace generalhealthcomb = . if generalhealthcomb<0
replace relac = . if relac<0
replace sescr = . if sescr<0
replace sescrgrp = . if sescrgrp<0
*replace <11s with 0*
replace smokingfreq_4 =0 if smokingfreq_4<0 & ChldAge<11
replace smokingfreq_4=. if smokingfreq_4<0 & ChldAge>=11
replace drinkfreq =0 if drinkfreq<0 & ChldAge<11
replace drinkfreq=. if drinkfreq<0 & ChldAge>=11
replace ParentSN =. if ParentSN<0 

*Compare characteristics of included vs excluded participants on primary variables - Table 3*
generate not_miss=0
replace not_miss=1 if ghqg2grp==. | Ten1==. | relac==. | fameco1==. | ethnic4==. | eda ==. | eqv3==. | fadsgrp_2==. | pimpact==. | numsle5==. | generalhealthcomb==.

*replace 'ghqg2grp' with each categorical variable*
tabulate not_miss ghqg2grp, chi2 row V

*continuous variables cohens d effect sizes*
*child age*
ttest ChldAge, by(not_miss)
esize twosample ChldAge, by(not_miss) cohensd
*parent SDQ impact*
ttest pimpact, by(not_miss)
esize twosample pimpact, by(not_miss) cohensd

*****analysing characteristics of participants with full data n=6933*****
*RQ1 - child MH as outcome*
tabulate dcany ghqg2grp if not_miss==0, chi2 row V
*replace 'ghqg2grp' with each categorical variable*

*continuous variables cohens d effect sizes*
*child age*
ttest ChldAge if not_miss==0, by(dcany)
esize twosample ChldAge, by(dcany) cohensd
*pimpact*
ttest pimpact if not_miss==0, by(dcany)
esize twosample pimpact, by(dcany) cohensd

*RQ2 - parent MH as outcome*
tabulate ghqg2grp dcany if not_miss==0, chi2 row V
*replace 'dcany' with each categorical variable*

*continuous variables cohens d effect sizes*
*child age*
ttest ChldAge if not_miss==0, by(ghqg2grp)
esize twosample ChldAge, by(ghqg2grp) cohensd
*pimpact*
ttest pimpact if not_miss==0, by(ghqg2grp)
esize twosample pimpact, by(ghqg2grp) cohensd

******CREATING DUMMY VARIABLES FOR LOGISTIC REGRESSION so the correct ref group is used to compare*******
*recode general health very good/good=0*
replace generalhealthcomb = 0 if generalhealthcomb==1 | generalhealthcomb==2
replace generalhealthcomb = 1 if generalhealthcomb==3

*recode family type married/cohabiting=0*
replace hrpmar1_3grp = 0 if hrpmar1_3grp==1 | hrpmar1_3grp==2

*recode parent education level=secondary education and above=0*
replace eda = 0 if eda==1 | eda==2 | eda==3 | eda==4
replace eda = 1 if eda==5 | eda==6 | eda==7

*recode ethnicity White(=0) vs Non-white(=1)*
replace ethnic4 = 0 if ethnic4==1
replace ethnic4 = 1 if ethnic4==2 | ethnic4==3 | ethnic4==4

*recode tenure so all types of ownership form ref group (=0)*
replace Ten1 = 0 if Ten1==1 | Ten1==2 | Ten1==3
replace Ten1 = 1 if Ten1==4 | Ten1==5 | Ten1==6

*recode so both and 1 parent working make up ref group (=0)*
replace fameco1 = 0 if fameco1==1 | fameco1==2
*add unemployed parents to economically inactive*
replace fameco1 = 1 if fameco1==2 | fameco1==3 | fameco1==4

*recode SLEs to 0-2 vs 3 or more*
replace numsle5 = 0 if numsle5==1 | numsle5==2
replace numsle5 = 1 if numsle5==3 | numsle5==4 | numsle5==5

*recode eqv3 so medium and highest tertiles make up one reference group*
replace eqv3 = 0 if eqv3==1 | eqv==2
replace eqv3 = 1 if eqv3==3

*UNIVARIATE REGRESSION ON EACH VARIABLE*
*RQ1* 
logistic dcany i.ghqg2grp
*replace 'ghqg2grp' with each variable*
*RQ2* 
replace ghqg2grp = ghqg2grp - 1 /*0=GHQ score 0-3, 1=GHQ score 4+ for input into regression model*/
logistic ghqg2grp i.dcany 
*replace 'dcany' with each variable*

********** RQ1 - child psych disorder = outcome MULTIVARIATE REGRESSION **********
logistic dcany i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*simple moderation analyses*
*child sex and ghq*
logistic dcany i.ChldSx ChldSx#ghqg2grp ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*FULL MODERATION OCCURRING*
*child age and ghq*
logistic dcany i.ChldSx ChldAge c.ChldAge#ghqg2grp i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*general health and ghq*
logistic dcany i.ChldSx ChldAge ghqg2grp#generalhealthcomb i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*single parent and ghq*
logistic dcany i.ChldSx ChldAge ghqg2grp#hrpmar1_3grp i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and pimpact*
logistic dcany i.ChldSx ChldAge ghqg2grp#c.pimpact i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and fadsgrp*
logistic dcany i.ChldSx ChldAge ghqg2grp#fadsgrp_2 i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and eda*
logistic dcany i.ChldSx ChldAge ghqg2grp#eda i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and fameco1*
logistic dcany i.ChldSx ChldAge ghqg2grp#fameco1 i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and eqv3*
logistic dcany i.ChldSx ChldAge ghqg2grp#eqv3 i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and numsle*
logistic dcany i.ChldSx ChldAge ghqg2grp#numsle5 i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*ghq and ethnicity*
logistic dcany i.ChldSx ChldAge ghqg2grp#ethnic4 i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*simple mediation analyses - significant variables only*
*path b and c*
logit dcany i.ChldSx i.ghqg2grp ChldAge pimpact i.fadsgrp_2 i.Ten1 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)

*path a tests*
*recode variables so outcome is dichotomous*
replace fadsgrp_2 = fadsgrp_2 - 1 
*0=healthy family functioning, 1=unhealthy*
replace ChldSx = ChldSx - 1 
*0=male, 1=female*

*perform regression*
regress ChldAge i.ChldSx i.ghqg2grp pimpact i.fadsgrp_2 i.Ten1 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
regress pimpact i.ChldSx i.ghqg2grp ChldAge i.fadsgrp_2 i.Ten1 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit fadsgrp_2 i.ChldSx i.ghqg2grp pimpact ChldAge i.Ten1 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit ChldSx i.ghqg2grp pimpact ChldAge i.fadsgrp_2 i.Ten1 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit Ten1 i.ChldSx i.ghqg2grp pimpact ChldAge i.fadsgrp_2 i.numsle5 i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit numsle5 i.ghqg2grp pimpact ChldAge i.fadsgrp_2 i.Ten1 i.ChldSx i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit ethnic4 i.ghqg2grp pimpact ChldAge i.fadsgrp_2 i.Ten1 i.numsle5 i.ChldSx if relac!=., vce(bootstrap, reps(1000) bca)

********** RQ1 - child psych disorder = outcome SUB-GROUP ANALYSES **********
*SELF ESTEEM*
*box plot of RSES scores*
graph hbox sescr, over(dcany) over(ghqg2grp)

*t test disorder vs no disorder*
ttest sescr, by(dcany) 
*t test ghq score 0-3 vs 4+*
ttest sescr, by(ghqg2grp)

*dummy code: average/high self-esteem=0 vs low self-esteem=1*
replace sescrgrp = 0 if sescrgrp==3 | sescrgrp==2
*univariate regression* 
logistic dcany i.sescrgrp
*multivariate regression*
logistic dcany sescrgrp i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*simple moderation analysis*
logistic dcany sescrgrp#ghqg2grp i.sescrgrp i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*NO*
*stepwise removal*
xi: stepwise, pr(.05): logistic dcany sescr i.ChldSx ChldAge i.ghqg2grp i.hrpmar1_3grp pimpact i.generalhealthcomb i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.numsle5 i.eqv3 i.ethnic4

***SEND***
*dummy code: no SEND=0 vs SEND=1*
replace ParentSN = 0 if ParentSN==2
*univariate regression*
logistic dcany i.ParentSN
*multivariate regression*
logistic dcany i.ParentSN i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*simple moderation analysis*
logistic dcany i.ChldSx ChldAge i.generalhealthcomb i.ParentSN##i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

***SUBSTANCE USE***
*dummy code*
recode never smoked =0*
replace smokingfreq_4 = 0 if smokingfreq_4==1 
*recode ever smoked = 1*
replace smokingfreq_4 = 1 if smokingfreq_4==2 | smokingfreq_4==3 | smokingfreq_4==4

*recode never drank =0*
replace drinkfreq = 0 if drinkfreq==4 
*recode ever drank = 1*
replace drinkfreq = 1 if drinkfreq==1 | drinkfreq==2 | drinkfreq==3

*recode never taken drugs=0*
replace DVDrugIn = 0 if DVDrugIn<0 & ChldAge<11
replace DVDrugIn =. if DVDrugIn<0 & ChldAge>=11
replace DVDrugIn = 0 if DVDrugIn==1

*univariate regression*
logistic dcany i.smokingfreq_4
logistic dcany i.drinkfreq 
logistic dcany i.DVDrugIn
*multivariate regression*
logistic dcany i.smokingfreq_4 i.DVDrugIn i.drinkfreq i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*all non-significant*

**What additional variables affect child psych disorder?**
*final multivariate model with SEND + RSES*
logistic dcany i.ParentSN i.sescrgrp i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*test for mediation*
xi: stepwise, pr(.05): logistic dcany i.ParentSN i.sescrgrp i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*paths b and c*
logit dcany i.ParentSN i.sescrgrp i.ChldSx i.ghqg2grp pimpact i.ethnic4 if relac!=.,  vce(bootstrap, reps(1000) bca)
*path a*
logit ParentSN i.sescrgrp i.ChldSx i.ghqg2grp pimpact i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit sescrgrp i.ParentSN i.ChldSx i.ghqg2grp pimpact i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
regress pimpact i.sescrgrp i.ChldSx i.ghqg2grp i.ParentSN i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit ChldSx i.sescrgrp i.ParentSN i.ghqg2grp pimpact i.ethnic4 if relac!=., vce(bootstrap, reps(1000) bca)
logit ethnic4 i.sescrgrp i.ChldSx i.ghqg2grp pimpact i.ParentSN if relac!=., vce(bootstrap, reps(1000) bca)
*Conclusion: SEND and self esteem are additional mediators*

*** PARENT SDQ IMPACT SCORE ***
*box plot*
graph hbox pimpact, over(ghqg2grp) cw by(dcany)

*********** RQ2 - parent mental health = outcome MULTIVARIATE REGRESSION**********
*COUNT COMORBIDITY*
count if dcany==2
count if dcany==2 & ghqg2grp!=. & relac!=.
***EMOTIONAL DISORDERS***
*total disorders* 
count if dcemot==2 & dcanycd==0 & dcanyhk==0 & dcpdd==0
*no ghq and not parents removed*
count if dcemot==2 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & ghqg2grp!=. & relac!=.
*count single anxiety disorders - total*
count if dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==2 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==2 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==2 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==2 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==2 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==2 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==2 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==2 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==2 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==2
*no ghq and not parents removed*
count if dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==2 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==2 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==2 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==2 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==2 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==2 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==2 & dcspph==0 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==2 & dcagor==0 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==2 & dcbdd==0 & ghqg2grp!=. & relac!=. | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==2 & ghqg2grp!=. & relac!=.
*count single depressive disorders*
count if dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==2 & dcotdep==0 & dcdmdd==0 | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==0 & dcotdep==2 & dcdmdd==0 | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0  & dcanyde==2 & dcmadep==0 & dcotdep==0 & dcdmdd==2
*no ghq and not parents removed*
count if dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==2 & dcotdep==0 & dcdmdd==0 & ghqg2grp!=. & relac!=. | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==0 & dcotdep==2 & dcdmdd==0 & ghqg2grp!=. & relac!=. | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0  & dcanyde==2 & dcmadep==0 & dcotdep==0 & dcdmdd==2 & ghqg2grp!=. & relac!=.

***BEHAVIOURAL DISORDERS***
*count total*
*total disorders* 
count if dcemot==0 & dcanycd==2 & dcanyhk==0 & dcpdd==0
*no ghq and not parents removed*
count if dcemot==0 & dcanycd==2 & dcanyhk==0 & dcpdd==0 & ghqg2grp!=. & relac!=.
*count single behavioural disorders*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==2 & dcodd==0 & dcothcd==0 | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==2 & dcothcd==0 | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==0 & dcothcd==2  
*no ghq and not parents removed*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==2 & dcodd==0 & dcothcd==0 & ghqg2grp!=. & relac!=. | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==2 & dcothcd==0 & ghqg2grp!=. & relac!=. | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==0 & dcothcd==2 & ghqg2grp!=. & relac!=.  
*count conduct disorder*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==2 & dcodd==0 & dcothcd==0 
*no ghq and not parents removed*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==2 & dcodd==0 & dcothcd==0 & ghqg2grp!=. & relac!=.
*count odd*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==2 & dcothcd==0
*no ghq and not parents removed*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==2 & dcothcd==0 & ghqg2grp!=. & relac!=.
*count other conduct disorder*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==0 & dcothcd==2  
*no ghq and not parents removed*
count if dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==0 & dcothcd==2 & ghqg2grp!=. & relac!=.

***NDDs***
*count total NDD diagnoses*
count if dcemot==0 & dcanyhk==2 & dcpdd==0 & dcanycd==0 | dcemot==0 & dcanyhk==0 & dcpdd==2 & dcanycd==0 | dcemot==0 & dcanycd==0 & dcanyhk==2 & dcpdd==2
*no ghq score and not parents removed*
count if dcemot==0 & dcanyhk==2 & dcpdd==0 & dcanycd==0 & ghqg2grp!=. & relac!=. | dcemot==0 & dcanyhk==0 & dcpdd==2 & dcanycd==0 & ghqg2grp!=. & relac!=. | dcemot==0 & dcanycd==0 & dcanyhk==2 & dcpdd==2 & ghqg2grp!=. & relac!=.
*comorbid with each other*
count if dcemot==0 & dcanycd==0 & dcanyhk==2 & dcpdd==2
*no ghq score and not parents removed*
count if dcemot==0 & dcanycd==0 & dcanyhk==2 & dcpdd==2 & ghqg2grp!=. & relac!=.
*ADHD*
count if dcemot==0 & dcanyhk==2 & dcpdd==0 & dcanycd==0 
*no ghq score and not parents removed*
count if dcemot==0 & dcanyhk==2 & dcpdd==0 & dcanycd==0 & ghqg2grp!=. & relac!=.
*autism*
count if dcemot==0 & dcanyhk==0 & dcpdd==2 & dcanycd==0
*no ghq score and not parents removed*
count if dcemot==0 & dcanyhk==0 & dcpdd==2 & dcanycd==0 & ghqg2grp!=. & relac!=.

*children diagnosed with more than 1 disorder (n=407/1204 or 360/1085)*
generate double Comorbid = dcany
*No or only 1 disorder*
replace Comorbid = 0 if dcany==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==2 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==2 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==2 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==2 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==2 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==2 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==2 & dcspph==0 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==2 & dcagor==0 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==2 & dcbdd==0 | dcanyde==0 & dcmania==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyan==2 & dcgena==0 & dcocd==0 & dcotanx==0 & dcpanic==0 & dcptsd==0 & dcsepa==0 & dcsoph==0 & dcspph==0 & dcagor==0 & dcbdd==2 | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==2 & dcotdep==0 & dcdmdd==0 | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0 & dcanyde==2 & dcmadep==0 & dcotdep==2 & dcdmdd==0 | dcmania==0 & dcanyan==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0  & dcanyde==2 & dcmadep==0 & dcotdep==0 & dcdmdd==2 | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==2 & dcodd==0 & dcothcd==0 | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==2 & dcothcd==0 | dcemot==0 & dcanyhk==0 & dcpdd==0 & dcanycd==2 & dccd==0 & dcodd==0 & dcothcd==2 | dcemot==0 & dcanyhk==2 & dcpdd==0 & dcanycd==0 | dcemot==0 & dcanyhk==0 & dcpdd==2 & dcanycd==0
*remove 'other' disorders*
replace Comorbid = 0 if dcany==2 & dcemot==0 & dcanycd==0 & dcanyhk==0 & dcpdd==0
*2= 2 or more diagnoses in the child*
count if Comorbid==2

*MULTIVARIATE REGRESSION*
logistic ghqg2grp i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*remove insignificant variables*
xi: stepwise, pr(.05): logistic ghqg2grp i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*simple moderation analyses*
*disorder + child sex*
logistic ghqg2grp dcany#ChldSx i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*SIGNIFICANT*
*disorder + child age*
logistic ghqg2grp dcany#c.ChldAge i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + gen health*
logistic ghqg2grp dcany#generalhealthcomb i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + single parent*
logistic ghqg2grp dcany#hrpmar1_3grp i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + pimpact*
logistic ghqg2grp dcany#c.pimpact i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + fads*
logistic ghqg2grp dcany#fadsgrp_2 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + eda*
logistic ghqg2grp dcany#eda i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + tenure*
logistic ghqg2grp dcany#Ten1 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + fameco1*
logistic ghqg2grp dcany#fameco1 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + eqv*
logistic ghqg2grp dcany#eqv3 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + numsle*
logistic ghqg2grp dcany#numsle5 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*disorder + ethnicity*
logistic ghqg2grp dcany#ethnic4 i.dcany i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*simple mediation analyses - sig variables only*
*path b and c*
logit ghqg2grp i.dcany pimpact i.fadsgrp_2 i.fameco1 i.eqv3 i.numsle5 if relac!=., vce(bootstrap, reps(1000) bca)
*path a*
regress pimpact i.dcany i.fadsgrp_2 i.fameco1 i.eqv3 i.numsle5 if relac!=., vce(bootstrap, reps(1000) bca)
logit fadsgrp_2 i.dcany pimpact i.fameco1 i.eqv3 i.numsle5 if relac!=., vce(bootstrap, reps(1000) bca)
logit fameco1 i.dcany pimpact i.fadsgrp_2 i.eqv3 i.numsle5 if relac!=., vce(bootstrap, reps(1000) bca)
logit eqv3 i.dcany pimpact i.fadsgrp_2 i.fameco1 i.numsle5 if relac!=., vce(bootstrap, reps(1000) bca)
logit numsle5 i.dcany pimpact i.fadsgrp_2 i.fameco1 i.eqv3 if relac!=., vce(bootstrap, reps(1000) bca)

*********** RQ2 - parent mental health = outcome SUB-GROUP ANALYSES**********
*** SELF ESTEEM ***
*univariate regression*
logistic ghqg2grp i.sescrgrp 
*multivariate regression*
logistic ghqg2grp i.dcany i.Comorbid i.sescrgrp i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*non-significant*

***SEND STATUS***
*univariate regression*
logistic ghqg2grp i.ParentSN
*multivariate regression*
logistic ghqg2grp i.dcany i.Comorbid i.ParentSN i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*non-significant*

***SUBSTANCE USE***
*univariate regression*
logistic ghqg2grp i.smokingfreq_4
logistic ghqg2grp i.drinkfreq
logistic ghqg2grp i.DVDrugIn

*multivariate regression*
logistic ghqg2grp i.dcany i.Comorbid i.smokingfreq_4 i.drinkfreq i.DVDrugIn i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*is child drinking a moderating variable? - no, p=0.533*
logistic ghqg2grp i.dcany##drinkfreq i.Comorbid i.fadsgrp_2 i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

**What additional variables affect parent mental health?**
*child drinking added to final multivariate regression model, becomes insignificant p=0.051*
logistic ghqg2grp i.dcany i.Comorbid i.drinkfreq i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*stepwise removal*
logistic ghqg2grp i.dcany pimpact i.fadsgrp_2 i.fameco1 i.eqv3 i.numsle5 if relac!=.
*no additional variables found to be significant*

**********SUB-GROUP ANALYSIS BY DISORDER TYPE - RQ4*********
*generate disorder type variable - single types only*
generate double Disordertype = dcany
replace Disordertype = 1 if dcemot==2 & dcanycd==0 & dcanyhk==0 & dcpdd==0
replace Disordertype = 3 if dcanycd==2 & dcemot==0 & dcanyhk==0 & dcpdd==0
replace Disordertype = 4 if dcanyhk==2 & dcpdd==2 & dcemot==0 & dcanycd==0 | dcany==2 & dcanyhk==2 & dcemot==0 & dcanycd==0 & dcpdd==0 | dcpdd==2 & dcany==2 & dcanyhk==0 & dcemot==0 & dcanycd==0
replace Disordertype = 5 if dcemot==2 & dcpdd==2 & dcanycd==0 & dcanyhk==0 | dcemot==2 & dcpdd==0 & dcanycd==2 & dcanyhk==0 | dcemot==2 & dcpdd==0 & dcanycd==0 & dcanyhk==2 | dcemot==0 & dcpdd==0 & dcanycd==2 & dcanyhk==2 | dcemot==0 & dcpdd==0 & dcanycd==2 & dcanyhk==2 | dcemot==0 & dcpdd==2 & dcanycd==2 & dcanyhk==0 | dcemot==2 & dcpdd==2 & dcanycd==2 & dcanyhk==0 | dcemot==2 & dcpdd==2 & dcanycd==2 & dcanyhk==2 | dcemot==2 & dcpdd==0 & dcanycd==2 & dcanyhk==2 | dcemot==0 & dcpdd==2 & dcanycd==2 & dcanyhk==2 | dcemot==2 & dcpdd==2 & dcanycd==2 & dcanyhk==0 | dcemot==2 & dcanyhk==2 & dcpdd==2 & dcanycd==0
notes Disordertype: 0 = no disorder, 1 = emotional, 2 = other, 3 = behavioural, 4 = NDD, 5 = morethan1type

*count single type disorders for parent GHQ score 4+*
count if Disordertype==1 & relac!=. & ghqg2grp==1| Disordertype==3 & relac!=. & ghqg2grp==1 | Disordertype==4 & relac!=. & ghqg2grp==1
*count single type parent GHQ score 0-3*
count if Disordertype==1 & relac!=. & ghqg2grp==0| Disordertype==3 & relac!=. & ghqg2grp==0 | Disordertype==4 & relac!=. & ghqg2grp==0
*create bar chart*
graph hbar if Disordertype==1 | Disordertype==3 | Disordertype==4 & relac!=., over(Disordertype) asyvars by(ghqg2grp)
*box plot with parent ghq scores*
graph hbox ghq12scr if Disordertype==1 | Disordertype==3 | Disordertype==4 & relac!=. &ghqg2grp!=., over(Disordertype) cw
*t tests - insignificant differences between parent GHQ scores and disorder type*
ttest ghq12scr if Disordertype==1 | Disordertype==3 & relac!=., by(Disordertype)
ttest ghq12scr if Disordertype==1 | Disordertype==4 & relac!=., by(Disordertype)
ttest ghq12scr if Disordertype==4 | Disordertype==3 & relac!=., by(Disordertype)

***** RQ1 = child psych disorder as outcome* (do the sig variables from final model hold true?) *****
*emotional*
*original multivariate model*
logistic dcemot i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*with the extra variables - self esteem & SEND*
logistic dcemot i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 sescr i.ParentSN if relac!=.
*final model*
logistic dcemot i.ChldSx ChldAge i.ghqg2grp pimpact sescr 

*behavioural*
*original multivariate model*
logistic dcanycd i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*with the extra variables - self esteem + SEND*
logistic dcanycd i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 sescr i.ParentSN if relac!=.
*final model*
logistic dcanycd i.ChldSx ChldAge i.generalhealthcomb pimpact i.numsle5 sescr

*NDDs*
generate ndd = Disordertype==4
*original multivariate model*
logistic ndd i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
*with the extra variables - self esteem + SEND*
logistic ndd i.ChldSx ChldAge i.generalhealthcomb i.ghqg2grp i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 sescr i.ParentSN if relac!=.
*final model*
logistic ndd i.ChldSx ChldAge pimpact i.ParentSN

***** RQ2 = parent mental health as outcome (no additional variables to add) *****
*emotional*
logistic ghqg2grp i.dcemot i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*behavioural*
logistic ghqg2grp i.dcanycd i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.

*NDDs*
logistic ghqg2grp i.ndd i.Comorbid i.ChldSx ChldAge i.generalhealthcomb i.hrpmar1_3grp pimpact i.fadsgrp_2 i.eda i.Ten1 i.fameco1 i.eqv3 i.numsle5 i.ethnic4 if relac!=.
