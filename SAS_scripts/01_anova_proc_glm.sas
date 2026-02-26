/*==============================================================================
 * SAS ANOVA FOR CASSAVA MULTI-ENVIRONMENT TRIALS
 * 
 * This script performs mixed model ANOVA for cassava yield trials
 * with environment as random effect.
 *
 * Data structure required:
 * - ENV: Environment (location/year)
 * - ENTRY: Genotype entry number
 * - BLK: Block within environment
 * - REP: Replication within block
 * - FYLD: Fresh yield (dependent variable)
 *============================================================================*/

* Clear log and output;
dm 'log;clear;output;clear';

* Set options for output formatting;
options pagesize=500 linesize=132 nocenter pageno=1;
ods html close;
ods html;

* === STEP 1: CREATE INPUT DATASET ===;
data cassava;
    input ENV $ ENTRY BLK REP FYLD;
    datalines;
/* INSERT YOUR DATA HERE */
/* Example format:
IKENNE 1 1 1 25.4
IKENNE 1 1 2 26.8
IKENNE 1 2 1 24.9
MOKWA 1 1 1 32.1
MOKWA 1 1 2 31.5
*/
;

* === STEP 2: DISPLAY RAW DATA ===;
proc print data=cassava (obs=20);
    title1 "Cassava Multi-Environment Trial Data";
    title2 "First 20 Observations";
    var ENV ENTRY BLK REP FYLD;
run;

* === STEP 3: SUMMARY STATISTICS ===;
proc means data=cassava mean std min max n;
    title1 "Summary Statistics by Environment";
    class ENV;
    var FYLD;
run;

* === STEP 4: GLM ANALYSIS WITH RANDOM EFFECTS ===;
proc glm data=cassava;
    title1 "PROC GLM Analysis of Cassava Yield";
    title2 "Environment Treated as Random Effect";
    
    class ENV ENTRY BLK REP;
    model FYLD = ENV BLK(REP*ENV) REP(ENV) ENTRY ENTRY*ENV / ss3;
    random ENV BLK(REP*ENV) REP(ENV) ENTRY*ENV / test;
    lsmeans ENTRY / stderr pdiff;
    means ENTRY / lsd lines;
    output out=glm_diagnostics predicted=predicted residual=residual;
run;
quit;

* === STEP 5: EXPORT RESULTS ===;
proc export data=glm_diagnostics
    outfile="outputs/tables/sas_anova_results.csv"
    dbms=csv
    replace;
run;

* === STEP 6: VARIANCE COMPONENTS USING MIXED ===;
proc mixed data=cassava method=reml;
    class ENV ENTRY BLK REP;
    model FYLD = ENTRY / solution ddfm=kr;
    random ENV BLK(ENV) REP(ENV*BLK) ENTRY*ENV;
    lsmeans ENTRY / diff cl adjust=tukey;
    ods output CovParms=variance_components;
run;

proc export data=variance_components
    outfile="outputs/tables/sas_variance_components.csv"
    dbms=csv
    replace;
run;

* === STEP 7: COMPLETE ===;
title "SAS Analysis Complete";
