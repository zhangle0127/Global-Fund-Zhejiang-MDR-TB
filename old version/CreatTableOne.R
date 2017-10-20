library(tableone)
listVars <- names(mdr)
# all describe vars
desVars <-dput(listVars)
# all describe var
desVars <- c("DST缺失情况", "history", "treatmentmodel", "addresstype", 
"agecat", "sex", "tbregister", "cavity", "adverse", 
"household", "symptom", "hemosputum",
"status", "outcome_s_d_f_f", "spectrum4","city","month2",
"addresstype2", "st_treatperiod", 
"time", "timediff_smear_treat", "timediff_smear_culture", "timediff_smear_dst", "SurvObj")
##catigorical var
catVars <- c("DST缺失情况", "history", "treatmentmodel", "addresstype", 
"agecat", "sex", "tbregister", "cavity", "adverse", 
"household", "symptom", "hemosputum",
"status", "outcome_s_d_f_f", "spectrum4","city","month2",
"addresstype2", "st_treatperiod")

#不分层
# tableSD <- CreateTableOne(vars = listVars, data = mdr0830, factorVars = catVars)
# tableSD

# #正态性检验 timediff_smear_treat
# shapiro.test(timediff_smear_treat)
# qqnorm(timediff_smear_treat, main = "Normal Q-Q Plot",
#        xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
#        plot.it = TRUE, datax = FALSE)
# #正态性检验 timediff_smear_treat
# shapiro.test(resistnum)
# qqnorm(resistnum, main = "Normal Q-Q Plot",
#        xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
#        plot.it = TRUE, datax = FALSE)
# #正态性检验 timediff_smear_treat
# shapiro.test(year)
# qqnorm(year, main = "Normal Q-Q Plot",
#        xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
#        plot.it = TRUE, datax = FALSE)
# #正态性检验 timediff_smear_treat
shapiro.test(time)
qqnorm(time, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)

# 对 history 进行卡方检验
# x <- matrix(c(253, 116, 16,69,25,5), ncol = 2)
# chisq.test(x)
# #不准确
# 最小理论频数 ：最小格子数对应的   行和*列和/N


# 对 treatmentmodel 进行卡方检验
# x <- matrix(c(322, 63, 79,20), ncol = 2)
# chisq.test(x)
# # Yates' continuity correction 0.451


# 对 agecat 进行卡方检验
# x <- matrix(c(157,57,36,135,28,10,14,47), ncol = 2)
# chisq.test(x)
# # P=0.02

# 对 sex 进行卡方检验
# x <- matrix(c(112,273,27,72), ncol = 2)
# chisq.test(x)
# # Yates' continuity correction 0.816

# 对 cavity 进行卡方检验
# x <- matrix(c(154,182,49,32,51,16), ncol = 2)
# chisq.test(x)
# # P=0.334

# 对 household 进行卡方检验
# x <- matrix(c(182,203,73,26), ncol = 2)
# chisq.test(x)
# Pearson's Chi-squared test with Yates' continuity correction p=0.0000000004

# 对 status 进行卡方检验
# x <- matrix(c(289,96,66,33), ncol = 2)
# chisq.test(x)
# # Pearson's Chi-squared test with Yates' continuity correction p=0.119

# st_treatperiod 
# x <- matrix(c(80,124,85,96,13,29,30,27), ncol = 2)
# chisq.test(x)
# #  p=0.170

# addresstype 
# x <- matrix(c(289,49,47,73,8,18), ncol = 2)
# chisq.test(x)
# #  p=0.169

# spectrum4 
# x <- matrix(c(83,174,41,15,72,19,32,11,5,32), ncol = 2)
# chisq.test(x)
# #  不准确

# city 
# x <- matrix(c(249,38,2,46,29,21,28,4,6,0,9,52), ncol = 2)
# fisher.test(x)
# #  不准确


# month2 
# x <- matrix(c(233,48,104,57,27,15), ncol = 2)
# chisq.test(x)
# #  p=0.0004


#单因素方差分析（感兴趣的是比较分类因子定义的两个或多个组别中的因变量均值）  
# install.packages("multcomp")  
library(multcomp)  
attach(mdr0830)  
# str(mdr0830)  
  
table(st_treatperiod)  
aggregate(timediff_smear_treat,by=list(st_treatperiod),FUN=mean)  
aggregate(timediff_smear_treat,by=list(st_treatperiod),FUN=sd)  
aggregate(timediff_smear_treat,by=list(st_treatperiod),FUN=median)  
fit<-aov(timediff_smear_treat~st_treatperiod)  
summary(fit)  
library(gplots)  
plotmeans(timediff_smear_treat~st_treatperiod,
  xlab="year",ylab="timediff_smear_treat",main="Mean Plot\n with 95%CI")  


# ######卡方检验各变量是否需要校正
# chisq.test(outcome,regimeSD)

tableSD<- CreateTableOne(desVars,strata = c("regimeSD"), mdr0830, catVars)

tableSD<- print(tableSD, nonnormal=c("timediff_smear_dst","timediff_smear_treat","timediff_smear_treat", "timediff_smear_culture","time"),
    exact = c("history","tbregister","outcome_s_d_f_f","treatmentmodel","household","status","sex","spectrum4","city"),
    #cramVars can be used to show both levels for a 2-level categorical vars.
    #cramVars= c("sex","household","hemosputum","cavity","adverse","regimeSD"),
    showAllLevels = TRUE,
    quote = FALSE)

library(ReporteRsjars)
library(ReporteRs)
library(magrittr)
docx( ) %>% 
  addFlexTable(tableSD %>%
  FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
            header.text.props = textBold( color = "white" ),
            add.rownames = TRUE ) %>%
            setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = "SD.docx")


##### tableoverall ##########################################################################################
tableoverall<-CreateTableOne(vars=desVars,data=mdr0830,factorVars=catVars)
tableoverall<-
	print(tableoverall,
	      nonnormal=c("timediff_smear_dst","timediff_smear_treat",),
           exact = c("history","tbregister","outcome_s_d_f_f","treatmentmodel","household","status","sex","spectrum4","city"),
           showAllLevels = TRUE,quote = FALSE)

library(ReporteRsjars)
library(ReporteRs)
library(magrittr)

docx( ) %>% 
        addFlexTable(tableoverall %>%
                             FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                                       header.text.props = textBold( color = "white" ),
                                       add.rownames = TRUE ) %>%
                             setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
        writeDoc(file = "tableoverall.docx")


