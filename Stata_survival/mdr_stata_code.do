stset time, failure(status==2) scale(365.25) id(id)
sts graph, by(tbregister)
sts graph, by(cavity)

/*completion*/
stset time, failure(outcome5==1) scale(365.25) id(id) 
stptime, per(10)

/*cure*/ 
stset time, failure(outcome5==2) scale(365.25) id(id) 
stptime, per(10)

/*death*/
stset time, failure(outcome5==3) scale(365.25) id(id) 
stptime, per(10)

/*default*/
stset time, failure(outcome5==4) scale(365.25) id(id) 
stptime, per(10)

/*failure*/
stset time, failure(outcome5==5) scale(365.25) id(id) 
stptime, per(10)

stptime, by(tbregister) per(10) 
stptime, by(cavity) per(10) 

// generate id
gen id=_n

//replace regimenIS=0 if regimenIS==1
//replace regimenIS=1 if regimenIS==2

//replace status=0 if status==2

//M-H检验交互作用
stmh regimenIS, by(agecat)
//M-H检验交互作用,controlling for sex
stmh regimenIS sex, by(agecat)


// risk ratio,cases:80,30
cs status regimenIS
cs status regimenIS, by(agecat)
cs status regimenIS, by(sex)
cs status regimenIS, by(occupation)
cs status regimenIS, by(household)
cs status regimenIS, by(tbregister)
cs status regimenIS, by(history)
cs status regimenIS, by(hemosputum)
cs status regimenIS, by(cavity)
cs status regimenIS, by(adverse)
cs status regimenIS, by(treatmentmode)
cs status regimenIS, by(city)

//hr by subgroup analysis(effect modification) unadjust
/*overall*/
ipdover, over(agecat) hr nograph: stcox regimenIS 
ipdover, over(sex) hr nooverall nograph: stcox regimenIS 
ipdover, over(occupation) hr nooverall nograph: stcox regimenIS 
ipdover, over(household) hr nooverall nograph: stcox regimenIS
ipdover, over(tbregister) hr nooverall nograph: stcox regimenIS 
ipdover, over(history) hr nooverall nograph: stcox regimenIS 
ipdover, over(hemosputum) hr nooverall nograph: stcox regimenIS 
ipdover, over(cavity) hr nooverall nograph: stcox regimenIS 
ipdover, over(adverse) hr nooverall nograph: stcox regimenIS 
ipdover, over(treatmentmode) hr nooverall nograph: stcox regimenIS 
ipdover, over(city) hr nooverall nograph: stcox regimenIS 

// hr by subgroup adjust demographic characteristics
/*overall*/
ipdover, over(agecat) hr nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(sex) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(occupation) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(household) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(tbregister) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(history) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(hemosputum) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(cavity) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(adverse) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(treatmentmode) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)
ipdover, over(city) hr nooverall nograph: stcox regimenIS agecat sex occupation household,strata(city)


// hr by subgroup adjust all
ipdover, over(agecat) hr nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city)
ipdover, over(sex) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city)
ipdover, over(occupation) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(household) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(tbregister) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(history) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(hemosputum) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(cavity) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(adverse) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(treatmentmode) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 
ipdover, over(city) hr nooverall nograph: stcox regimenIS agecat sex occupation household tbregister history hemosputum cavity adverse treatmentmode,strata(city) 


//OVERALL compute person-years per 10 personyear by subgroup 
stptime, by(agecat) per(10) 
stptime, by(sex) per(10)
stptime, by(occupation) per(10)
stptime, by(household) per(10)
stptime, by(tbregister) per(10)
stptime, by(history) per(10)
stptime, by(hemosputum) per(10)
stptime, by(cavity) per(10)
stptime, by(adverse) per(10)
stptime, by(treatmentmode) per(10)
stptime, by(city) per(10)
//compute IR person-years by subgroup 
stptime if regimenIS==0, by(agecat) per(10) 
stptime if regimenIS==0, by(sex) per(10)
stptime if regimenIS==0, by(occupation) per(10)
stptime if regimenIS==0, by(household) per(10)
stptime if regimenIS==0, by(tbregister) per(10)
stptime if regimenIS==0, by(history) per(10)
stptime if regimenIS==0, by(hemosputum) per(10)
stptime if regimenIS==0, by(cavity) per(10)
stptime if regimenIS==0, by(adverse) per(10)
stptime if regimenIS==0, by(treatmentmode) per(10)
stptime if regimenIS==0, by(city) per(10)
//compute SR person-years by subgroup 
stptime if regimenIS==1, by(agecat) per(10) 
stptime if regimenIS==1, by(sex) per(10)
stptime if regimenIS==1, by(occupation) per(10)
stptime if regimenIS==1, by(household) per(10)
stptime if regimenIS==1, by(tbregister) per(10)
stptime if regimenIS==1, by(history) per(10)
stptime if regimenIS==1, by(hemosputum) per(10)
stptime if regimenIS==1, by(cavity) per(10)
stptime if regimenIS==1, by(adverse) per(10)
stptime if regimenIS==1, by(treatmentmode) per(10)
stptime if regimenIS==1, by(city) per(10)


//查看obs1-2
list in 1/2
//检查所有变量是否有缺失值
misstable summarize _all
//生存分析 first stset the data.
stset time, failure(status==1) id(id) scale(365.25)
/*计算中位生存时间*/
stsum, by(regimenIS) 
/*计算平均生存时间及其可信区间*/
stci, rmean by(regimenIS)

// calculate person-years
// obtain the overall person-time of observation and disease incidence rate
// In epidemiology,incidence rates are often presented per 1,000 person-years.
// Rate per 10 person-years
stptime, title(person-years) per(10)


sts list
sts graph
sts graph, by(regimenIS)
sts graph if regimenIS==1,gwood /*95%CI*/
sts graph if regimenIS==2,gwood

/* 进行Log-Rank 检验*/
sts test regimenIS,logrank 
sts test regimenIS,wilcoxon
sts test regimenIS,trend 
/*检验死亡(生存)率是否随分组变量取值水平的增高而上升或下降*/


/* 进行Log-Rank 检验*/
sts test pattern3,logrank 
//Pr>chi2 = 0.0254, Log-rank test for equality of survivor functions
sts test pattern3,wilcoxon
sts test pattern3,trend 
//Pr>chi2 = 0.0128,Log-rank test for equality of survivor functions,Test for trend of survivor functions
/*检验死亡(生存)率是否随分组变量取值水平的增高而上升或下降*/

stcox
