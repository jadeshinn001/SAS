/***************************************************************************************************************************************
* Title: D cohort
code by JS.Shim
****************************************************************************************************************************************/
libname trial 'C:\Users\Administrator\Desktop\Biostatistics work';


*1. import dataset;
proc import datafile = 'C:\Users\Administrator\Desktop\Biostatistics work'
	dbms = xlsx
	out = trial.rawdataset0 replace;
	sheet='dataset';
run;

*2. import coding book;
proc import datafile ='C:\Users\Administrator\Desktop\Biostatistics work'
		dbms = xlsx
		out = trial.label_tot replace;
		sheet = 'coding book';
run;



*create variables ;
proc sql;
create table trial.rawdataset as
select *,
			case when RISC_sum =. then . when RISC_sum < 60 then 1 else 0 end as RISC_bin60,
			case when RISC_sum =. then . when RISC_sum < 65 then 1 else 0 end as RISC_bin_med,

			case when PHQ_sum =. then . when PHQ_sum < 9 then 0 else 1 end as PHQ_bin9,
			case when PHQ_sum =. then . when PHQ_sum < 10 then 0 else 1 end as PHQ_bin10,
			case when PHQ_sum =. then . when PHQ_sum < 3 then 0 else 1 end as PHQ_bin_med,

			case when STAI_sum =. then . when STAI_sum < 40 then 0 else 1 end as STAI_bin40,
			case when STAI_sum =. then . when STAI_sum < 57 then 0 else 1 end as STAI_bin57,
			case when STAI_sum =. then . when STAI_sum < 44 then 0 else 1 end as STAI_bin_med,

			case when AD_onset =. then . 
					when AD_onset = 1 then 0.25 
					when AD_onset = 2 then 0.75 
					when AD_onset = 3 then 1.5 end as AD_onset_mid
from trial.rawdataset0;
quit;

*define macro variables;
%let raw = trial.rawdataset;


%macro renamer(countnum,variable);
	%do i=1 %to &countnum;
		rename x&i = %scan(&variable, &i);
	%end;
%mend renamer;


*3. Check and arrange variables in the dataset;
*1) Import dataset without unnecessary variables;
proc sql;
select variable into: var_use separated by ', ' 
from trial.label_tot
where usage = 1;
quit;


proc sql;
create table trial.dataset as
select &var_use
from &raw;
quit;

%let dsn = trial.dataset;
%put &dsn;



*2) Check variables with missing data and errors;
*(1) generate variable type table ;
proc sql;
create table variabletype as
	select  c.name as Variable, c.type as Raw_type, l.type as Type, 
	case 
		when c.type = 'num' and l.type = 'cate' then 'cont to cate' 
		when c.type = 'char' and l.type = 'cont' then 'cate to cont'
		else ' ' end as Change 	 
	from (select name, type from dictionary.columns where libname = "TRIAL" and memname = "DATASET")  c, trial.label_tot l  
	where c.name = l.variable and l.usage = 1;
	quit;

***check if there are any 'cont to cate' or 'cate to cont';
proc sql;
	select Change, count(Change) as count
		from variabletype
		group by Change;
	quit;


*************************FOR CONT -> CATE********************************************************************
*(2) identify continuous variables to be converted to categorical;
proc sql;
	select Variable into : cont_to_cate separated by ' ' 
	from variabletype 
	where Change = 'cont to cate';
	quit;

%let n_conttocate = %sysfunc(countw(&cont_to_cate));
%put cont_to_cate = &cont_to_cate;
%put n_conttocate = &n_conttocate;

*convert from continuous to categorical;
data &dsn ;
	set &dsn;
	array contx {&n_conttocate} &cont_to_cate;
	array x {&n_conttocate} $ 10.;
	do i=1 to &n_conttocate;
	 x{i} = contx {i};
	 end;
	drop &cont_to_cate i ;
%renamer(&n_conttocate, &cont_to_cate)
run;

*convert "." to " " ;
data &dsn;
	set &dsn;
	array cotoca {&n_conttocate} &cont_to_cate;
	do i=1 to &n_conttocate;
	if trim(strip(cotoca{i})) = '.' then cotoca{i} = " " ;
	end;
	run;



*************************FOR CATE TO CONT*****************************************************************
*(3) identify categorical variables to be converted to continuous ;
proc sql;
	select Variable into : cate_to_cont separated by ' ' 
	from variabletype 
	where Change = 'cate to cont';
quit;

%let n_catetocont = %sysfunc(countw(&cate_to_cont));
%put cate_to_cont = &cate_to_cont;
%put n_catetocont = &n_catetocont;

*identify variables with error values (non-numeric, non-missing values, e.g., "NA", "0?"), x= number of errors in the variable;
data error;
set &dsn end = lastobs;
array charx {*} &cate_to_cont;
array x {&n_catetocont};
	do i=1 to &n_catetocont;
		if input(charx{i},?? best12.) =. and charx{i} ne ' ' then x{i} + 1;
	end;
	if lastobs then do;
		length errlist $ 32767;
		do j=1 to &n_catetocont;
			if x{j} ne . then errlist = catx(' ', errlist, vname(charx{j}));
		end;
	call symputx('errlist',errlist);	
	end;	
	drop &cate_to_cont i  j;
	%renamer(&n_catetocont, &cate_to_cont)
	run;

%put errlist = &errlist;

data out (keep = errlist &cate_to_cont);
	set error end=lastobs;
	if lastobs;
run;

proc print data = out noobs;
title "[errlist = variables containing non-numeric error (cate->cont)] & the number of errors in these variables";
run;
title;

*convert from categorical to continuous (non-numeric error values will be converted to missing value);
data &dsn;
	set &dsn;
	array catex {&n_catetocont} &cate_to_cont;
	array x {&n_catetocont};
	do i=1 to &n_catetocont;
	 x{i} = input(catex{i}, ?? best12.);
	 end;
	drop &cate_to_cont i ;
%renamer(&n_catetocont, &cate_to_cont)
run;




********************************************************************************************************************
*check if all variables were converted right;
proc sql;
create table variabletype_final as
	select  c.name as Variable, c.type as Raw_type, l.type as Type, 
	case 
		when c.type = 'num' and l.type = 'cate' then 'cont to cate' 
		when c.type = 'char' and l.type = 'cont' then 'cate to cont'
		else ' ' end as Change 	 
	from (select name, type from dictionary.columns where libname = "TRIAL" and memname =  "DATASET")  c, trial.label_tot l 
	where c.name = l.variable and l.usage = 1;
	quit;




*4. Run analysis;

proc sql;
select variable into: varlist_cont separated by ' ' 
from trial.label_tot
where usage = 1 and type = 'cont';
quit;

%let normalt = Shapiro-Wilk; 

/**********************************************************************
Descriptive Statistics By Group - Continuous Variables
***********************************************************************/
*1) Continuous;
proc means data = &dsn noprint;
	var &varlist_cont;
	output out = summary (drop=_type_ _freq_) n= nmiss= mean = std=  min= q1=  median= q3 = max= range= qrange= /autoname;	
run;	 

proc transpose data = summary out = vertical;
run; 

proc datasets library = work;
	modify vertical;
		rename col1 = total	;
	run;

data vertical;
	set vertical;
	varname = substr(_name_,1,length(_name_)-(1+length(scan(_name_,-1,'_'))));
	stat = scan(_name_, -1, '_');
	run;

data vertical;
	retain _label_ varname stat total ;
	set vertical;
run;

proc sort data = vertical;
	by varname;
run;

proc transpose data=vertical out=horizontal (drop=_name_);
by varname;
id stat;
var total  ;
run;


%let plusmin = %sysfunc(byte(177));
data horizontal;
	set horizontal;
	Mean_StdDev = strip(round (mean, .01) ||" " ||"&plusmin"|| round(StdDev,.01));
	Median_IQR = strip(round(median,.01)||" " || "["||strip(round(q1,.01))||", "||round(q3,.01)||"]"); 
	Min_Max= strip(round(min,.01)||" ~ "||round(max,.01));
run; 

proc sort data = horizontal;
	by varname;
run;

proc transpose data = horizontal out=prefinal;
var N NMiss Mean_StdDev Median_IQR Min_Max ;
by varname ; 
run;

proc datasets library = work;
	modify prefinal;
		rename _name_ = statistic
					col1 = total	;
run;

data prefinal;
set prefinal;
if statistic = "N" then order2 = 1;
else if statistic = "NMiss" then order2=2;
else if statistic = "Mean_StdDev" then order2=3;
else if statistic = "Median_IQR" then order2=4;
else if statistic = "Min_Max" then order2=5;
run;

*Normality
***(i) total;
proc univariate data = &dsn normal;
	var &varlist_cont;
	ods output testsfornormality=norm_all;
run;

data norm1 (keep = VarName pValue);
	set norm_all ;
	where test = "&normalt";
run;

proc datasets library=work;
	modify norm1;
		rename pValue = total;
	run;

data norm1;
	set norm1;
	if total >0.05 then total_new = "Yes"; else total_new = "No"; *determine y/n based on normality test ;
run;

data norm1 (drop =total);
set norm1;
statistic = "Normality_p";
run;

proc sql;
create table norm2 as 
select varname, statistic, total_new as total
from norm1;
quit;

data prefinal2;
	set prefinal norm2;
run;

data prefinal2;
set prefinal2;
if statistic = "Normality_p" then order2 = 6;
run;


*3) Add labels;

proc sql;
create table cont_final0 as
select a.varname, b.label, a.statistic, a.total, b.order, a.order2
from prefinal2 a
left join trial.label_tot b
on a.varname = b.variable
order by order, order2;
quit;

data cont_final;
	set cont_final0;
		if statistic = "N" then do label = label; varname = varname;
		end;
		else do; label = " "; varname = " " ; 
		end;
run;

proc print data = cont_final noobs;
run;






*categorical vars;
proc sql;
select variable into: varlist_cat separated by ' ' 
from trial.label_tot
where usage = 1 and type = 'cate' and variable ^= 'study_no';
quit;

%put &varlist_cat;



*Descriptive;
**************************************************TOTAL: crosstab_total1;
;

proc freq data=&dsn;
	tables &varlist_cat ;
	 	ods output  OneWayFreqs=onefreq;
run;	

data onefreq ;
	set onefreq;
	if find(table, 'Å×ÀÌºí')^=0 then varname =scan (table, 1, " "); 
	else varname = scan (table, 2, " ");*sas KORver.: scan(table,1," "), ENGver.: scan(table, 2, " ");
run;

data onefreq_total ;
	set onefreq;
	array varcat $ &varlist_cat;
		do i = 1 to dim (varcat);
		if varname = vname(varcat{i}) then category = varcat{i};
		end;
run;


data onefreq_total (keep= varname category freq_prc CumFrequency) ;
	set onefreq_total;
	freq_prc = frequency || " (" ||strip( round(Percent,.01)) || "%"|| ")";
	run;

proc sql;
create table onefreq_total2 as
select varname, category, freq_prc
from onefreq_total;
quit;


**Total;
proc sql;
create table freq_total as
select varname, "Total" as category, cumfrequency as freq_prc
from onefreq
where round(CumPercent,1) = 100;
quit;


proc print data = onefreq;
run;


proc sql;
create table cate_prefinal as
select varname, category, freq_prc
from onefreq_total2 union
(select varname, category, put(freq_prc, 15.)
from freq_total);
quit;


**Missing;

proc freq data=&dsn;
	tables &varlist_cat/missing ;
	 	ods output  OneWayFreqs=missing;
run;	

data missing;
	set missing;
	if find(table, 'Å×ÀÌºí')^=0 then varname =scan (table, 1, " "); 
	else varname = scan (table, 2, " ");*sas KORver.: scan(table,1," "), ENGver.: scan(table, 2, " ");
	array varcat $ &varlist_cat;
		do i = 1 to dim (varcat);
		if varname = vname(varcat{i}) then category = varcat{i};
		end;
run;

proc sql;
create table missing2 as
select varname, "Missing" as category, frequency as freq_prc
from missing
where category=" ";
quit;


proc sql;
create table prefinal_cat as
select varname, category, freq_prc
from cate_prefinal union
(select varname, category, put(freq_prc, 15.)
from missing2);
quit;




*add labels;
proc sql;
create table withlabelcat as 
	select *
	from prefinal_cat c, trial.label_tot l
	where c.varname = l.variable;
	quit;


proc sql;
	select name into : numcode separated by ' ' 
	from (
			select name, type 
			from(select name, type
				from dictionary.columns 
				where libname = "WORK" and memname = "WITHLABELCAT")
			where name contains 'code' and type = 'num');
			quit;
%put numcode = &numcode;
%let n_numcode = %sysfunc(countw(&numcode));
%put n_numcode = &n_numcode;


*convert from continuous to categorical;
data withlabelcat ;
	set withlabelcat;
	array contx {&n_numcode} &numcode;
	array x {&n_numcode} $ &n_numcode ;
	do i=1 to &n_numcode;
	 x{i} = contx {i};
	 end;
	drop &numcode i ;
%renamer(&n_numcode, &numcode)
run;

data withlabelcat;
	set withlabelcat;
	array t category code1--code&n_numcode; 
		do i = 1 to &n_numcode;
	t{i} = strip(t{i});
	end;
	informat category $100. code1--code&n_numcode $30.;
	format category $100. code1--code&n_numcode $30.;
run;

data withlabelcat ;
	set withlabelcat;
		array code {&n_numcode} $ code1--code&n_numcode ;
		array repre {&n_numcode} $ repre1--repre&n_numcode;
		do i =1 to &n_numcode;
			if category = strip(code{i})  then format = repre{i};
		end;
run;

data withlabelcat;
	set withlabelcat;
	if category = "Total" then num = 100;
	else if category = "Missing" then num = 101;
	else num = category;
run;


proc sort data = withlabelcat;
by varname num;
run;

data withlabelcat_prefinal(keep =varname label category format freq_prc order num)  ;
retain varname label category format freq_prc order num;
set withlabelcat;
run;

data withlabelcat_final;
	set withlabelcat_prefinal;
	by varname;
		if first.varname then do label = label; varname = varname;
		end;
		else do; label = " "; varname = " " ;
		end;
	run;


****cont_cate_merge;
proc sql;
create table cont_final2 as
select varname, label, statistic, " " as format, total, order, order2
from cont_final0;
quit;

proc sql;
create table des_all as
select * 
from (select *
from cont_final2
union
select *
from withlabelcat_prefinal)
order by order, order2;
quit; 

proc print data =des_all noobs;
run;

proc sort data = des_all;
by order order2;
run;

data des_all2;
set des_all;
by order;
if first.order=1 then do label = label; varname = varname;
		end;
		else do; label = " "; varname = " " ;
		end;
	run;

proc print data = des_all2 noobs;
run;


*********************************************************************************************************
logistic regression: Asthma,MCT, FEV, V6...
*********************************************************************************************************
*********************************************************************************
[SCREENING] Univariable Analysis: confounding
*********************************************************************************
;

proc sql;
	select variable into: xcov_cont separated by ' ' 
	from trial.label_tot
	where (x_var = 1 or covariate = 1) and type = 'cont';
quit; 

proc sql;
	select variable into: xcov_cate separated by ' ' 
	from trial.label_tot
	where (x_var = 1 or covariate = 1) and type = 'cate';
quit; 

%array (array= xcov_cont, values = &xcov_cont);
%array (array= xcov_cate, values = &xcov_cate);



*run logistic analysis for 
JK
imp_50
call
jk_call
imp_50_call
Asthma
MCT_tot_16
MCT_tot_8
MCT_6yr_16
MCT_6yr_8
tot_FEV1
tot_FEV1_FVC
V6_FEV1
V6_FEV1_FVC

;

%let outcome = JK;

**cont (change response);
%macro lg_uni_cont (factor, response= &outcome);
	proc logistic data = &dsn desc;
	model &response = &factor  ; 
	roc;
	ods output OddsRatios = or_&factor;
	ods output ParameterEstimates = pvalue_&factor;
	ods output ROCAssociation = ROC_&factor;
	run;

data ROC_&factor;
	set ROC_&factor;
	variable = "&factor";
run;

proc sql;
create table AUC_&factor as
	select variable, area as AUC, lowerarea as lowerCI, upperarea as upperCI
	from ROC_&factor
	where ROCmodel = "Model";
	quit;
%mend;

**cate;
%macro lg_uni_cate (factor, response = &outcome);
	proc logistic data = &dsn desc;
		class &factor (ref = first) / param = ref;
		model &response = &factor ;
		roc;
	ods output OddsRatios = or_&factor;
	ods output ParameterEstimates = pvalue_&factor;
	ods output ModelANOVA = anova_&factor;
	ods output ROCAssociation = ROC_&factor;
		run;

data ROC_&factor;
	set ROC_&factor;
	variable = "&factor";
run;

proc sql;
create table AUC_&factor as
	select variable, area as AUC, lowerarea as lowerCI, upperarea as upperCI
	from ROC_&factor
	where ROCmodel = "Model";
	quit;
%mend;

%do_over (xcov_cont, macro = lg_uni_cont);
%do_over (xcov_cate, macro = lg_uni_cate);







**AUC;
data delete0;
	input a $ b c d;
	datalines;
a 1 1 1
;
run;

**cont_OR;
data delete1;
	input a $ b c d ;
	datalines;
a 1 1 1
;
run;

**cont_pval;
data delete2;
	input a$ b c d e f g $;
	datalines;
	a 1 1 1 1 1 a
	;
run;

**cate_or;
data delete3;
	input a $ b c d ;
	datalines;
a 1 1 1
;
run;

**cate_pval;
data delete4;
	input a$ b $ c d e f g h $;
	datalines;
	a a 1 1 1 1 1 a
	;
run;

**type3;
data delete5;
	input a $ b c d ;
	datalines;
a 1 1 1  
;
run;

*************************************************
;
%macro merge(cont,cate);
*merge_AUC;
proc sql;
create table AUC_all as
	select *
	from (
	%do_over(&cont, phrase = select * from AUC_? union)
	%do_over(&cate, phrase = select * from AUC_? union)
	select *
	from delete0) as a
	where variable ^= "a";
	quit;

proc print data = AUC_all noobs;
run;

**merge_cont_OR;
proc sql;
create table or_cont as 
	select *
	from (
	%do_over(&cont, phrase = select * from or_? union)
	select *
	from delete1) as a 
	where a.effect ^= "a";
quit;


**merge_cont_pval;

proc sql;
create table pval_cont as
	select *
	from (
	%do_over(&cont, phrase = select * from pvalue_? union)
	select *
	from delete2) as a 
	where a.variable not in ("a", "Intercept");
quit;

proc sql;
create table lg_cont_final as
	select effect as Variable, cat(round(oddsratioest,.01)," (", round(lowercl,.01), "-", round(uppercl,.01), ")") as OR_CI, probchisq as p_value
		from(
				select a.*, b.probchisq
				from or_cont a, pval_cont b
				where a.effect = b.variable);
quit;




**or_cate;

proc sql;
create table or_cate as 
	select *
	from (
	%do_over(&cate, phrase = select * from or_? union)
	select *
	from delete3) as a 
	where a.effect ^= "a"
	order by effect;
quit;

data or_cate;
	set or_cate;
	varname = scan(strip(effect), 1);
	class = scan(strip(effect),2);
	drop  effect;
	run;


**p_val_Cate;

proc sql;
create table pval_cate as
	select *
	from (
	%do_over(&cate, phrase = select * from pvalue_? union)
	select *
	from delete4) as a 
	where a.variable not in ("a", "Intercept")
	order by variable, classval0;
quit;



**merge or and pval;
proc sql;
create table lg_cate_final as
	select variable, classval0 as class, cat(round(oddsratioest,.01)," (", round(lowercl,.01),"-", round(uppercl,.01),")") as OR_CI, probchisq as p_value
	from (
		select *
		from or_cate a, pval_cate b
		where a.varname = b.variable and a.class = b.classval0);
	quit;


proc sql;
	create table lg_all_final as 
select * 
from lg_cont_final
union
select variable, or_ci, p_value
	from lg_cate_final;
	quit;



%mend;


%merge(xcov_cont, xcov_cate);


proc sql;
	create table lg_all_final as 
	select variable, or_ci, p_value, cat(round(auc,.01)," (",round(lowerci,.01),"-",round(upperci,.01),")") as AUC_CI
	from (
select * 
		from 
		(select * 
		from lg_cont_final
		union
		select variable, or_ci, p_value
			from lg_cate_final) a , auc_all b 
			where a.variable = b.variable);
	quit;


proc sql;
create table uni_pre as
select varname, label, statistic, format, total, or_ci, p_value, AUC_CI, order, order2
from (select* 
from des_all a
	left join lg_all_final b 
	on a.varname = b.variable)
order by order, order2;
quit;


proc sql;
create table uni_final as
select varname0 as varname, label0 as label, statistic, format, total, or_ci0 as OR_CI, p_value0 as p_value, AUC_CI0 as AUC_CI, order, order2
from (select *, 
case when statistic in ("N", "0") then varname else " " end as varname0,
case when statistic in ("N", "0") then label else " " end as label0,
case when statistic in ("N", "1") then or_ci else " " end as or_ci0,
case when statistic in ("N", "1") then p_value else . end as p_value0,
case when statistic in ("N", "1") then AUC_CI else " " end as AUC_CI0
from uni_pre)
order by order, order2;
quit;


proc print data = uni_final noobs;
title "&outcome";
run;
title;



************survival analysis*******************
outcome: ~ever, survival time
AD_onset
AD_onset_mid
yr_Sx_lastfu
Sx_ever
yr_Dx_lastfu
Dx_ever
yr_Tx_lastfu
Tx

***************************************;

*1) endtime -> convert to 3 if less than 3;

data trial.dsn_surv; 
set &dsn;
if yr_Sx_lastfu < 3 then yr_Sx_lastfu = 3; else yr_Sx_lastfu = yr_Sx_lastfu;
if yr_Dx_lastfu < 3 then yr_Dx_lastfu = 3; else yr_Dx_lastfu = yr_Dx_lastfu;
if yr_Tx_lastfu < 3 then yr_Tx_lastfu = 3; else yr_Tx_lastfu = yr_Tx_lastfu;
run;

*2) survival time -> end time - AD onset;

data trial.dsn_surv; 
set trial.dsn_surv;
time_Sx = yr_Sx_lastfu - AD_onset_mid;
time_Dx = yr_Dx_lastfu - AD_onset_mid;
time_Tx = yr_Tx_lastfu - AD_onset_mid;
run;

data trial.dsn_surv;
set trial.dsn_surv;
Sx_ever_con = input(strip(Sx_ever), 5.);
Dx_ever_con = input(strip(Dx_ever), 5.);
Tx_con = input(strip(Tx), 5.);
run;

**************univariable model********************

time_Sx*Sx_ever_con(0) 
time_Dx*Dx_ever_con(0) 
time_Tx*Tx_con(0);

*1) DEFINE MACRO;

%let outcome = time_Tx*Tx_con(0)  ;
**************************************CONTINUOUS*************************************;
%macro unicox_cont (factor, time_and_event= &outcome );*update;
		proc phreg data=trial.dsn_surv zph(global);
			model &time_and_event = &factor / ties=efron rl;
			ods output ParameterEstimates = hr_&factor;
			ods output zphTest=zph_&factor;
			run;

		data zph_&factor;
			retain varname predictor correlation chisquare pvalue tvalue probt;
			set zph_&factor;
			varname = lag(predictor);
			if predictor ^= "_Global_" then varname = predictor;
			drop transform;
		run;
%mend;

%do_over (xcov_cont , macro =  unicox_cont);



**************************************CATEGORICAL*************************************;
%macro unicox_cate (factor, time_and_event = &outcome );*update;
		proc phreg data=trial.dsn_surv zph(global) ;
			class &factor (ref=first) / param=ref;
			model &time_and_event  = &factor / ties=efron rl;
			ods output ParameterEstimates = hr_&factor;
			ods output zphTest=zph_&factor; 
			ods output ModelANOVA = anova_&factor; *Type 3 Tests;
			run;

		data zph_&factor;
			retain varname predictor correlation chisquare pvalue tvalue probt;
			set zph_&factor;
			varname0 = substr(strip(predictor), 1, length(strip(predictor))-1);
			varname = lag(varname0);
			if predictor ^= "_Global_" then varname = varname0;
			drop transform varname0;
		run;

data hr_&factor;
set hr_&factor;
pval2 = ProbChiSq*2;
run;


%mend;

%do_over (xcov_cate , macro = unicox_cate);


*2) MERGE OUTPUT TABLES_HR;
**************************************CONTINUOUS*************************************;
*merge_cont_HR;
data delete1;
	input a $ b c d e f g h i j $ ;
	datalines;
a 1 1 1 1 1 1 1 1 b 
;

proc sql;
create table hr_cont as
	select *
	from (%do_over(xcov_cont, phrase = select *  from hr_? union) 
	select *
	from delete1) as a
	where a.parameter ^= "a";
quit;

proc sort data = hr_cont;
by parameter;
run;

data hr_cont (keep=parameter hr_ci p_value);
	set hr_cont;
	length HR_CI $ 20.;
	HR_CI = strip(round(hazardratio,.01) ||" ("||strip(round(hrlowercl,.01))||"-"||strip(round(hruppercl,.01))||")");
	p_value = probchisq;
	run;


*************************************CATEGORICAL*************************************;
*merge_cate_type3;
data delete3;
	input a $ b c d ;
	datalines;
a 1 1 1  
;

proc sql;
create table type3_cate as
select *
from (%do_over(xcov_cate, phrase = select * from anova_? union)
	select *
	from delete3) as c
	where c.effect ^= "a" and c.df >1;
quit;


*merge_cate_hr;
data delete4;
	input a $ b$ c d e f g h i j k $ ;
	datalines;
a 1 1 1 1 1 1 1 1 1 b 
;

proc sql;
create table hr_cate as 
	select *
	from (%do_over(xcov_cate, phrase = select * from hr_? union)
	select *
	from delete4) as b
	where b.parameter ^= "a";
quit;

proc sort data = hr_cate;
by parameter classval0;
run;

data hr_cate;
	set hr_cate;
	length HR_CI $ 20.;
	hr_CI = strip(round(hazardratio,.01) ||" ("||strip(round(hrlowercl,.01))||"-"||strip(round(hruppercl,.01))||")") ;
	run;


*merge cate and cont;


proc sql;
create table hr_all as
select *
from hr_cont
union (select parameter, hr_ci, ProbChiSq
from hr_cate);
quit;



proc sql;
create table coxuni_pre as
select varname, label, statistic, format, total, hr_ci, p_value, order, order2
from (select* 
from des_all a
	left join hr_all b 
	on a.varname = b.parameter)
order by order, order2;
quit;


proc sql;
create table coxuni_final as
select varname0 as varname, label0 as label, statistic, format, total, hr_ci0 as HR_CI, p_value0 as p_value, order, order2
from (select *, 
case when statistic in ("N", "0") then varname else " " end as varname0,
case when statistic in ("N", "0") then label else " " end as label0,
case when statistic in ("N", "1") then hr_ci else " " end as hr_ci0,
case when statistic in ("N", "1") then p_value else . end as p_value0
from coxuni_pre)
order by order, order2;
quit;


proc print data = coxuni_final noobs;
title "&outcome";
run;
title;




*3) TEST PH ASSUMPTIONS_ZPH;
**************************************CONTINUOUS*************************************;
*merge_cont_zph;
data delete2;
	input a $ b $ c d e f g;
	datalines;
a a 1 1 1 1 1 
;

proc sql;
create table zph_cont as
	select *
	from (
	%do_over(xcov_cont, phrase = select *  from zph_? union) 
	select *
	from delete2) as a
	where a.varname ^= "a";
quit;

**************************************CATEGORICAL*************************************;
*merge_cate_zph;
data delete5;
	input a $ b $ c d e f g;
	datalines;
a a 1 1 1 1 1  
;
proc sql;
create table zph_cate as
select *
from (
%do_over(xcov_cate, phrase = select * from zph_? union)
	select *
	from delete5) as c
	where c.varname ^= "a" ;
quit;

**************************************MERGE CONT, CATE*************************************;
proc sql;
create table zph_all as
select *
	from zph_cont union
		select *
			from zph_cate
order by varname, predictor;
quit;

data zph_all;
	retain varname predictor correlation chisquare pvalue tvalue probt assump;
	length assump $ 20.;
	set zph_all;
	if pvalue >0.05 then assump = "met";
	else if pvalue <= 0.05 then assump = "not met";
run;

proc sql;
create table zph_final as
select b.label, a.*
	from zph_all a
	left join trial.label_tot as b
	on a.varname = b.variable
	order by varname, predictor;
	quit;

proc sql;
create table zph_final_count as
	select b.label, a.*
	from (select varname, count (assump) as unmet_count
		from zph_all
		where assump ^= "met"
		group by varname) as a, trial.label_tot as b
	where a.varname = b.variable;
	quit;

proc print data = zph_final noobs;
title "&outcome";
run;

proc print data = zph_final_count noobs;
title "&outcome";
run;
title;










*****************PH ASSUMPTION NOT MET -> INTERACTION TERM ************************;
**PFS;
proc phreg data = &dsn;
	class PLND (ref=first) / param=ref;
	model PFS*recurrence (0) = PLND PLND_PFS /rl ties = efron;
	PLND_PFS = PLND*PFS;
	run;

***OS;
proc phreg data = &dsn;
	model OS*death (0) = age age_OS /rl ties = efron;
	age_OS = age*OS;
	run;

	
proc phreg data = &dsn;
	model OS*death (0) = total_tmr_size total_tmr_size_OS /rl ties = efron;
	total_tmr_size_OS = total_tmr_size*OS;
	run;


proc phreg data = &dsn alpha = 0.025;
	class initial_tx_2 (ref=first) initial_tx_3 (ref=first) / param=ref;
	model OS*death (0) = initial_tx_2 initial_tx_3 initial_tx_2_OS initial_tx_3_OS /rl ties = efron;
	initial_tx_2_OS = initial_tx_2*OS;
	initial_tx_3_OS = initial_tx_3*OS;
	ods output ParameterEstimates = hr_initial_tx;
	run;


data hr_INITIAL_TX;
set hr_INITIAL_TX;
hr_CI = strip(round(hazardratio,.01) ||" ("||strip(round(hrlowercl,.01))||"-"||strip(round(hruppercl,.01))||")") ;
pval = ProbChiSq*2;
run;


***************************************************
Multivariable Analysis;
***************************************************;
%let dsn = trial.dataset;
%put &dsn;

*list of variables (outcomes related to Asthma/ x variables / outcomes related to AD);
data varinfo;
length Asthma_o mainx AD_o $15 ;
input Asthma_o $ mainx $ AD_o $ ;
datalines;
Asthma RISC_sum JK
MCT_tot_16 RISC_bin60 imp_50
MCT_tot_8 RISC_bin_med call
MCT_6yr_16 PHQ_sum jk_call
MCT_6yr_8 PHQ_bin9 imp_50_call
tot_FEV1 PHQ_bin10 .
tot_FEV1_FVC  PHQ_bin_med .
V6_FEV1 . .   
V6_FEV1_FVC . . 
;
run;

*combinations of outcomes and mainx into a table;
**outcome: Asthma;
proc sql;
create table aa as select a.mainx, b.Asthma_o from varinfo a cross join varinfo b;
quit;

proc sql;
create table bb as select * from aa where not missing(mainx);
quit;

**outcome: AD;
proc sql;
create table cc as select a.mainx, b.AD_o from varinfo a cross join varinfo b;
quit;

proc sql;
create table dd as select * from cc where not missing(AD_o) and not missing(mainx);
quit;

*create table that shows the type of each variable;
proc sql;
select name, type from dictionary.columns where libname = "TRIAL" and memname = "DATASET";
quit;

data _null_;
set trial.vartype;
call symput("t"||name, type);
run;

*******outcome: AD************************************************************;
%macro multi_log (outcome, mainx);
		%if &&t&mainx = char %then %do; *if the type of mainx is a character, then do;
		proc logistic data = &dsn desc;
			class &mainx (ref=first) severe_AD (ref=first) / param = ref;
			model  &outcome = &mainx severe_AD   ; 
			ods output OddsRatios = or_&outcome._&mainx;
			ods output ParameterEstimates = pvalue_&outcome._&mainx;
			ods output ROCAssociation = ROC_&outcome._&mainx;
			roc;
			run;
		%end;

		%else %if &&t&mainx = num %then %do; *if the type of mainx is a number, then do;
		proc logistic data = &dsn desc;
			class severe_AD (ref=first) / param = ref;
			model &outcome = &mainx severe_AD   ; 
			ods output OddsRatios = or_&outcome._&mainx;
			ods output ParameterEstimates = pvalue_&outcome._&mainx;
			ods output ROCAssociation = ROC_&outcome._&mainx;
			roc;
			run;
		%end;

		data or_&outcome._&mainx;
		length effect $30 mainx $25;
		set or_&outcome._&mainx;
		 or_ci = round(OddsRatioEst,.001) || " (" || strip(round(LowerCL, .01)) || " - " || strip(round(UpperCL, .01)) || ")";
		 mainx = "&mainx";
		 variable = scan(effect,1);
		outcome = "&outcome";
		 run;

		proc sql;
		create table orp_&outcome._&mainx as
		select a.outcome, a.mainx, a.variable, a.or_ci, b.ProbChiSq as p_value
		from or_&outcome._&mainx a
		left join pvalue_&outcome._&mainx b
		on a.variable = b.variable;
		quit;

proc datasets nolist library = work;
delete or_&outcome._&mainx  pvalue_&outcome._&mainx;
run;

		data roc_&outcome._&mainx (keep = outcome mainx ROCModel auc_ci);
		length mainx $25;
		set roc_&outcome._&mainx;
		auc_ci = round(area,.001) || " (" || strip(round(LowerArea, .01)) || " - " || strip(round(UpperArea, .01)) || ")";
		mainx = "&mainx";
		outcome = "&outcome";
		run;

%mend;

data _null_;
set dd;
call execute('%multi_log('||strip(AD_o) ||','||strip(mainx)||');');
run;

%array (array= mainx, values = RISC_sum RISC_bin60 RISC_bin_med PHQ_sum PHQ_bin9 PHQ_bin10 PHQ_bin_med);*7;

data delete7;
	input a $ b $ c $ d $ e ;
	datalines;
a a a a 1  
;
run;

data delete8;
input a $ b $  c $ d $;
datalines;
a a a a 
;
run;

proc sql;
create table trial.orp_all_AD as
select *
from ( 
	%do_over(mainx, phrase = select * from orp_JK_? union)
	%do_over(mainx, phrase = select * from orp_imp_50_? union)
	%do_over(mainx, phrase = select * from orp_call_? union)
	%do_over(mainx, phrase = select * from orp_jk_call_? union) 
    %do_over(mainx, phrase = select * from orp_imp_50_call_? union)
	select * 
	from delete7) as a
	where variable ^="a"
	order by outcome, mainx;
	quit;

proc print data = orp_all noobs;
run;

proc sql;
create table auc_all_AD as
select *
from ( 
	%do_over(mainx, phrase = select * from roc_JK_? union)
	%do_over(mainx, phrase = select * from roc_imp_50_? union)
	%do_over(mainx, phrase = select * from roc_call_? union)
	%do_over(mainx, phrase = select * from roc_jk_call_? union) 
    %do_over(mainx, phrase = select * from roc_imp_50_call_? union)
	select * 
	from delete8) as a
	where outcome ^="a"
	order by outcome, mainx;
	quit;

proc sql;
create table trial.auc_all_AD as
select outcome, mainx,ROCModel, auc_ci
from auc_all_AD
where ROCModel = "Model"
order by outcome, mainx;
quit;



*******outcome: Asthma************************************************************;
*PET, Mold, Sensi_inhalant_6yr;
%macro multi_log2 (outcome, mainx);
		%if &&t&mainx = char %then %do;
		proc logistic data = &dsn desc;
			class &mainx (ref=first) PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first)/ param = ref;
			model  &outcome = &mainx PET Mold Sensi_inhalant_6yr  / firth  ; 
			ods output OddsRatios = or_&outcome._&mainx;
			ods output ParameterEstimates = pvalue_&outcome._&mainx;
			ods output ROCAssociation = ROC_&outcome._&mainx;
			roc;
			run;
		%end;

		%else %if &&t&mainx = num %then %do;
		proc logistic data = &dsn desc;
			class PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first) / param = ref;
			model &outcome = &mainx PET Mold Sensi_inhalant_6yr / firth   ; 
			ods output OddsRatios = or_&outcome._&mainx;
			ods output ParameterEstimates = pvalue_&outcome._&mainx;
			ods output ROCAssociation = ROC_&outcome._&mainx;
			roc;
			run;
		%end;

		data or_&outcome._&mainx;
		length effect $30 mainx $25;
		set or_&outcome._&mainx;
		 or_ci = round(OddsRatioEst,.001) || " (" || strip(round(LowerCL, .01)) || " - " || strip(round(UpperCL, .01)) || ")";
		 mainx = "&mainx";
		 variable = scan(effect,1);
		outcome = "&outcome";
		 run;

		proc sql;
		create table orp_&outcome._&mainx as
		select a.outcome, a.mainx, a.variable, a.or_ci, b.ProbChiSq as p_value
		from or_&outcome._&mainx a
		left join pvalue_&outcome._&mainx b
		on a.variable = b.variable;
		quit;

proc datasets nolist library = work;
delete or_&outcome._&mainx  pvalue_&outcome._&mainx;
run;

		data roc_&outcome._&mainx (keep = outcome mainx ROCModel auc_ci);
		length mainx $25;
		set roc_&outcome._&mainx;
		auc_ci = round(area,.001) || " (" || strip(round(LowerArea, .01)) || " - " || strip(round(UpperArea, .01)) || ")";
		mainx = "&mainx";
		outcome = "&outcome";
		run;

%mend;

data _null_;
set bb;
call execute('%multi_log2('||strip(Asthma_o) ||','||strip(mainx)||');');
run;

%multi_log2(Asthma,PHQ_bin10); 
%multi_log2(Asthma,PHQ_bin9);
%multi_log2(Asthma,PHQ_bin_med);
%multi_log2(Asthma,PHQ_sum);
%multi_log2(Asthma,RISC_bin60);

%multi_log2(V6_FEV1,PHQ_bin10); 
%multi_log2(Asthma,PHQ_bin9);
%multi_log2(Asthma,PHQ_bin_med);
%multi_log2(Asthma,PHQ_sum);
%multi_log2(Asthma,RISC_bin60);

PHQ_bin10
PHQ_bin9
PHQ_bin_med
PHQ_sum
RISC_bin60
RISC_bin_med
RISC_sum


%multi_log2(Asthma, mainx)

%array (array= mainx, values = RISC_sum RISC_bin60 RISC_bin_med PHQ_sum PHQ_bin9 PHQ_bin10 PHQ_bin_med);*7;

data delete7;
	input a $ b $ c $ d $ e ;
	datalines;
a a a a 1  
;
run;

data delete8;
input a $ b $  c $ d $;
datalines;
a a a a 
;
run;

proc print data = bb;
run;


proc sql;
create table trial.orp_all_Asthma as
select *
from ( 
	%do_over(mainx, phrase = select * from orp_Asthma_? union)
	%do_over(mainx, phrase = select * from orp_MCT_tot_16_? union)
	%do_over(mainx, phrase = select * from orp_MCT_tot_8_? union)
	%do_over(mainx, phrase = select * from orp_MCT_6yr_16_? union) 
    %do_over(mainx, phrase = select * from orp_MCT_6yr_8_? union)
	%do_over(mainx, phrase = select * from orp_tot_FEV1_? union)
	%do_over(mainx, phrase = select * from orp_tot_FEV1_FVC_? union)
	%do_over(mainx, phrase = select * from orp_V6_FEV1_? union)
	%do_over(mainx, phrase = select * from orp_V6_FEV1_FVC_? union) 
	select * 
	from delete7) as a
	where variable ^="a"
	order by outcome, mainx;
	quit;

proc print data = orp_all_Asthma noobs;
run;

proc sql;
create table auc_all_Asthma as
select *
from ( 
	%do_over(mainx, phrase = select * from roc_Asthma_? union)
	%do_over(mainx, phrase = select * from roc_MCT_tot_16_? union)
	%do_over(mainx, phrase = select * from roc_MCT_tot_8_? union)
	%do_over(mainx, phrase = select * from roc_MCT_6yr_16_? union) 
    %do_over(mainx, phrase = select * from roc_MCT_6yr_8_? union)
	%do_over(mainx, phrase = select * from roc_tot_FEV1_? union)
	%do_over(mainx, phrase = select * from roc_tot_FEV1_FVC_? union)
	%do_over(mainx, phrase = select * from roc_V6_FEV1_? union)
	%do_over(mainx, phrase = select * from roc_V6_FEV1_FVC_? union) 
	select * 
	from delete8) as a
	where outcome ^="a"
	order by outcome, mainx;
	quit;

proc sql;
create table trial.auc_all_Asthma as
select outcome, mainx,ROCModel, auc_ci
from auc_all_Asthma
where ROCModel = "Model"
order by outcome, mainx;
quit;

************************************;
*final output_AD;
proc sql;
select *
from (
		select *, 
		case when outcome = "JK" then 1 
				when outcome = "imp_50" then 2
				when outcome = "call" then 3
				when outcome = "jk_call" then 4
				when outcome = "imp_50_call" then 5 end as order1,
		case when mainx = "RISC_sum" then 1 
				when mainx= "RISC_bin60" then 2 
				when mainx = "RISC_bin_med" then 3
				when mainx = "PHQ_sum" then 4
				when mainx = "PHQ_bin9" then 5
				when mainx = "PHQ_bin10" then 6
				when mainx = "PHQ_bin_med" then 7 end as order2
		from (select a.*, b.auc_ci from (select *  from trial.orp_all_ad where mainx = variable) a
						left join trial.auc_all_ad b
						on a.outcome = b.outcome and a.mainx = b.mainx)
						)
order by order1 asc, order2 asc;
quit;


*final output_Asthma;
proc sql;
select *
from (
		select *, 
		case when outcome = "Asthma" then 1 
				when outcome = "MCT_tot_16" then 2
				when outcome = "MCT_tot_8" then 3 
				when outcome = "MCT_6yr_16" then 4
				when outcome = "MCT_6yr_8" then 5
				when outcome = "tot_FEV1" then 6
				when outcome = "tot_FEV1_FVC" then 7 
				when outcome = "V6_FEV1" then 8
				when outcome = "V6_FEV1_FVC" then 9 end as order1,
		case when mainx = "RISC_sum" then 1 
				when mainx= "RISC_bin60" then 2 
				when mainx = "RISC_bin_med" then 3
				when mainx = "PHQ_sum" then 4
				when mainx = "PHQ_bin9" then 5
				when mainx = "PHQ_bin10" then 6
				when mainx = "PHQ_bin_med" then 7 end as order2
		from (select a.*, b.auc_ci from (select *  from trial.orp_all_Asthma where mainx = variable) a
						left join trial.auc_all_Asthma b
						on a.outcome = b.outcome and a.mainx = b.mainx)
						)
order by order1 asc, order2 asc;
quit;


proc freq data = trial.dataset  ;
tables Asthma * Sensi_inhalant_6yr / nocol norow;
run;


proc freq data = trial.dataset  ;
tables tot_FEV1 * (PET Sensi_inhalant_6yr PHQ_bin10)/ nocol norow;
run;

proc freq data = trial.dataset  ;
tables V6_FEV1 * (PET Sensi_inhalant_6yr PHQ_bin10)/ nocol norow;
run;



proc logistic data = trial.dataset desc;
			class RISC_sum (ref=first) PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first)/ param = ref;
			model  MCT_tot_16 = RISC_sum PET Mold Sensi_inhalant_6yr   ; 
			run;

			proc logistic data = trial.dataset desc;
			class PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first)/ param = ref;
			model  MCT_tot_16 = RISC_sum PET Mold Sensi_inhalant_6yr   ; 
			run;



proc sql;
select MCT_tot_16, MCT_6yr_16, RISC_sum, RISC_bin60, RISC_bin_med, PHQ_sum, PHQ_bin9, PHQ_bin10, PHQ_bin_med, PET, Mold, Sensi_inhalant_6yr
from trial.dataset
where MCT_tot_16 ^= MCT_6yr_16;
quit; 


proc logistic data = trial.dataset desc;
			class  PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first)/ param = ref;
			model  MCT_tot_16 = RISC_sum PET Mold Sensi_inhalant_6yr   ; 
			run;

proc logistic data = trial.dataset desc;
			class  PET (ref=first) Mold (ref=first) Sensi_inhalant_6yr (ref=first)/ param = ref;
			model  MCT_6yr_16 = RISC_sum PET Mold Sensi_inhalant_6yr   ; 
			run;

			proc logistic data = trial.dataset desc;
			class  PET (ref=first) Mold (ref=first) / param = ref;
			model  MCT_tot_16 = RISC_sum PET Mold  ; 
			run;

proc logistic data = trial.dataset desc;
			class  PET (ref=first) Mold (ref=first) / param = ref;
			model  MCT_6yr_16 = RISC_sum PET Mold    ; 
			run;
