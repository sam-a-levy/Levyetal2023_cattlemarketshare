
clear
set more off
import delimited "temp/manuscripts/Levyetal2021_PNAS_micro.csv", case(preserve)  // import command, 


*********************
**defining variables ***    
**********************

xtset microcode_ibge year  // declares a panel dataset. if you don't you receive error of data not sorted.

gen lndefor=ln(defor) //define log variables
gen lnn_fines=ln(n_fines+1)
gen lagG4_Cp75dist_nw = l.g4_ms  //define lag variables
gen lagTAC_Cp75dist_nw = l.tac_ms
gen lagpa_perc = l.pa_perc 
gen lagforest = l.forest 
gen laglnn_fines = l.lnn_fines
gen lagpriority_muni = l.priority_muni
gen lagpopulation = l.population 
gen lagcattle_heard = l.cattle_heard
gen lagagriculture = l.agriculture


*********************
**   REGRESSIONS  ***    
*********************

** for main figures ************************** 

**full reg (fe w weights) 
xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.forest l.agriculture cat_price   post_temer   [aw=muni_area] if year>2009, fe r
mat aa_m = r(table)
outreg2 using regmain.doc, replace label ctitle(FD main) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store aa

** two-way fe
xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer   i.year [aw=muni_area] if year>2009, fe r
mat twaa_m = r(table)
outreg2 using regmain.doc, append label ctitle(2-way FE main) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store twaa

**first difference
reg d.(lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  ) [aw=muni_area] if year>2009,  vce(cl microcode_ibge)  nocons
mat fdaa_m = r(table)
outreg2 using regmain.doc, append label ctitle(FD main) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdaa


**same G4 only
xtreg lndefor l.g4_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year>2009, fe r 
mat bb_m = r(table)
outreg2 using regmain.doc, append label ctitle(indiv FE G4) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store bb

xtreg lndefor l.g4_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  i.year [aw=muni_area] if year>2009, fe r 
mat twbb_m = r(table)
outreg2 using regmain.doc, append label ctitle(2-way FE G4) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store twbb

reg d.(lndefor l.g4_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer ) [aw=muni_area] if year>2009,  vce(cl microcode_ibge)  nocons
mat fdbb_m = r(table)
outreg2 using regmain.doc, append label ctitle(FD G4)addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdbb


**same TAC only
xtreg lndefor l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year>2009, fe r 
mat cc_m = r(table)
outreg2 using regmain.doc, append label ctitle(indiv FE TAC) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store cc

xtreg lndefor l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  i.year [aw=muni_area] if year>2009, fe r 
mat twcc_m = r(table)
outreg2 using regmain.doc, append label ctitle(2-way FE TAC) addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store twcc

reg d.(lndefor l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer ) [aw=muni_area] if year>2009,  vce(cl microcode_ibge)  nocons
mat fdcc_m = r(table)
outreg2 using regmain.doc, append label ctitle(FD TAC)addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdcc

estimates table aa twaa fdaa bb twbb fdbb cc twcc fdcc, star(.1 .05 .01) stats(N N_clust r2 r2_a F) // quick way to visualize a table. many more options

*************************************
** for SI 
*************************************

** no area weighting *******

xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  if year>2009, fe r
mat fenowe_m = r(table)
outreg2 using regnoweight.doc, replace label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fenowe
reg d.(lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer)  if year>2009,  vce(cl microcode_ibge)  nocons
mat fdnowe_m = r(table)
outreg2 using regnoweight.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdnowe

estimates table fenowe fdnowe , star(.1 .05 .01) stats(N N_clust r2 r2_a F) // qu

**radii ***************

**radius limits - 40
xtreg lndefor l.g4ms_40 l.tacms_40 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe40_m = r(table)
outreg2 using regradii_fe.doc, replace label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe40
**radius limits - 50
xtreg lndefor l.g4ms_50 l.tacms_50 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe50_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe50
**radius limits - 60
xtreg lndefor l.g4ms_60 l.tacms_60 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe60_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe60
**radius limits - 70
xtreg lndefor l.g4ms_70 l.tacms_70 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe70_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe70
**radius limits - 80
xtreg lndefor l.g4ms_80 l.tacms_80 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe80_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe80
**radius limits - 90
xtreg lndefor l.g4ms_90 l.tacms_90 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fe90_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fe90
**radius limits - fixed
xtreg lndefor l.g4ms_fixed l.tacms_fixed l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer [aw=muni_area] if year>2009, fe r
mat fefix75_m = r(table)
outreg2 using regradii_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fefix75

estimates table fe40 fe50 fe60 fe70 fe80 fe90 fefix75, star(.1 .05 .01) stats(N N_clust r2 r2_a F)

** binary **************

**binary cutoff - 30
xtreg lndefor l.g4_bin30 l.tac_bin30 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer   [aw=muni_area] if year>2009, fe r
outreg2 using regbin_fe.doc, replace label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
mat febin30_m = r(table)
estimates store febin30
**binary cutoff - 40
xtreg lndefor l.G4bin40 l.tac_bin40 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer   [aw=muni_area] if year>2009, fe r
mat febin40_m = r(table)
outreg2 using regbin_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store febin40
**binary cutoff - 50
xtreg lndefor l.g4_bin50 l.tac_bin50 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year>2009, fe r
mat febin50_m = r(table)
outreg2 using regbin_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store febin50
**binary cutoff - 60
xtreg lndefor l.g4_bin60 l.tac_bin60 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year>2009, fe r
mat febin60_m = r(table)
outreg2 using regbin_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store febin60
**binary cutoff - 70
xtreg lndefor l.g4_bin70 l.tac_bin70 l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year>2009, fe r
mat febin70_m = r(table)
outreg2 using regbin_fe.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store febin70


estimates table febin30 febin40 febin50 febin60 febin70, star(.1 .05 .01) stats(N N_clust r2 r2_a F)

** real market share ******

xtreg lndefor l.g4_unmod l.tac_unmod l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if ((year>=2013 & year<=2019) & STATE!="RO") | ((year>=2010 & year<=2018) & STATE=="RO") , fe r
mat fereal_m = r(table)
estimates store fereal
outreg2 using regreal.doc, replace label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
reg d.(lndefor l.g4_unmod l.tac_unmod l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer ) [aw=muni_area] if ((year>=2013 & year<=2019) & STATE!="RO") | ((year>=2010 & year<=2018) & STATE=="RO"),  vce(cl microcode_ibge)  nocons
mat fdreal_m = r(table)
outreg2 using regreal.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdreal
**main but good years only
xtreg lndefor l.g4_gdyr l.tac_gdyr l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if ((year>=2013 & year<=2019) & STATE!="RO") | ((year>=2010 & year<=2018) & STATE=="RO") , fe r
mat fegdyr_m = r(table)
outreg2 using regreal.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fegdyr
reg d.(lndefor l.g4_gdyr l.tac_gdyr l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer) [aw=muni_area] if ((year>=2013 & year<=2019) & STATE!="RO") | ((year>=2010 & year<=2018) & STATE=="RO"),  vce(cl microcode_ibge)  nocons
mat fdgdyr_m = r(table)
outreg2 using regreal.doc, append label addstat(N, e(N), N_clust, e(N_clust), Adjusted R2, e(r2_a),F-test, e(p))
estimates store fdgdyr

estimates table fereal fdreal fegdyr fdgdyr, star(.1 .05 .01) stats(N N_clust r2 r2_a F)

** state-level ******

**RO
xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year> 2009 & STATE=="RO", fe r
outreg2 using regstate.doc, replace label addstat(N, e(N))
estimates store fero
reg d.(lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer) [aw=muni_area] if year> 2009 & STATE=="RO",  vce(cl microcode_ibge)  nocons
outreg2 using regstate.doc, append label addstat(N, e(N))
estimates store fdro
**PA
xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year> 2009 & STATE=="PA", fe r
outreg2 using regstate.doc, append label addstat(N, e(N))
estimates store fepa
reg d.(lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer) [aw=muni_area] if year> 2009 & STATE=="PA",  vce(cl microcode_ibge)  nocons
outreg2 using regstate.doc, append label addstat(N, e(N))
estimates store fdpa
**MT
xtreg lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer  [aw=muni_area] if year> 2009 & STATE=="MT", fe r
outreg2 using regstate.doc, append label addstat(N, e(N))
estimates store femt
reg d.(lndefor l.g4_ms l.tac_ms l.pa_perc l.forest l.lnn_fines l.priority_muni l.population l.cattle_heard l.agriculture cat_price   post_temer) [aw=muni_area] if year> 2009 & STATE=="MT",  vce(cl microcode_ibge)  nocons
outreg2 using regstate.doc, append label addstat(N, e(N))
estimates store fdmt

estimates table fero fdro fepa fdpa femt fdmt, star(.1 .05 .01) stats(N N_clust r2 r2_a F p)


*************************************
** for total effects                * //passed to totaleffect.R
*************************************

sum tac_ms [aw=muni_area] if year>2009 
sum g4_ms [aw=muni_area]  if year>2009 

sum defor if year>2009
disp r(sum)
