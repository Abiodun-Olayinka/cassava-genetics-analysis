🧮 SAS Analysis Script

Uses SAS SAS PROC GLM.

Save as:
  
  SAS_scripts/ANOVA_PROC_GLM.sas


Clean version:
  
  /* ANOVA using PROC GLM */
  
  data cas;
input ENV ENTRY BLK REP FYLD;
datalines;
;
run;

proc glm data=cas;
class ENV ENTRY BLK REP;
model FYLD = ENV BLK(REP*ENV) REP(ENV) ENTRY ENTRY*ENV / ss3;
random ENV BLK(REP*ENV) REP(ENV) ENTRY*ENV / test;
lsmeans ENTRY / stderr;
means ENTRY / lsd;
run;
quit;


