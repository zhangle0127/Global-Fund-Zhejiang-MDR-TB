# TIPS
# Surv：用于创建生存数据对象
# survfit：创建KM生存曲线或是Cox调整生存曲线
# survdiff：用于不同组的统计检验
# coxph：构建COX回归模型
# cox.zph：检验PH假设是否成立
# survreg：构建参数模型

options(scipen=9)
options(digits = 3)
library(epicalc)
library(dplyr)
setwd("G:/work/MDR")
# mdr0830 <- read.csv("mdr0801.csv",header=TRUE)#缺失值是空格
mdr0830 <- read.csv("mdr0830.csv",na="",header=TRUE)#缺失值表示为NA

mdr0830= within(mdr0830,{agecat=NA
agecat[age>65]=">65yrs"
agecat[age>45 & age<=65]="46-65yrs"
agecat[age>25 & age<=45]="26-45yrs"
agecat[age<=25]="<=25yrs"
spectrum5=spectrum
spectrum4=spectrum
spectrum3=spectrum
spectrum2=spectrum
spectrum22=spectrum
resistnum3=resistnum
addresstype2=addresstype
        outcome_s_d_d_f=outcome
        outcome_c_c_d_d_f=outcome
        outcomepoor=outcome
# st_treatperiod=st_treatdate
st_treatperiod=cut(st_treatdate,breaks = c(39994,40359,40724,41090,41468),labels=FALSE)
st_treatperiod=as.factor(st_treatperiod)
levels(st_treatperiod)=c("2009","2010","2011","2012")
regimen=regime
b=gregexpr('\\w+：',regimen)
c=regmatches(x=regimen, b)
d=unlist(c)
regimen=as.factor(d)
Cs_based=grepl("Cs ",regime)
PAS_based=grepl("PAS ",regime)
Am_based=grepl("Am ",regime)

})#"2009/07/01-2010/06/31","2010/07/01-2011/06/31","2011/07/01-2012/06/31","2012/07/01-2013/07/13"


for (i in 1:nrow(mdr0830))
if (mdr0830[i,"regimen"]=="IR：") mdr0830[i,"Cs_based"]="IR"

for (i in 1:nrow(mdr0830))
if (mdr0830[i,"regimen"]=="IR：") mdr0830[i,"PAS_based"]="IR"

for (i in 1:nrow(mdr0830))
if (mdr0830[i,"regimen"]=="IR：") mdr0830[i,"Am_based"]="IR"

####################################################################################################
use(mdr0830)
#重新编码 空洞
recode.is.na(vars=cavity,"unknown")
#重新编码 耐药谱 "MDR only","MDR+1st","MDR+Ofx","MDR+Km","XDR"
#但是 MDR+Km只有3例
recode(vars =spectrum5,
       c("R/H","R/H/E","R/H/E/S","R/H/S",
         "R/H/E/Ofx","R/H/E/S/Ofx","R/H/Ofx","R/H/S/Ofx",
         "R/H/E/S/Km","R/H/Km","R/H/S/Km",
         "R/H/E/Ofx/Km","R/H/E/S/Ofx/Km","R/H/S/Ofx/Km",
         "R/H...","R/H/E...","R/H/E/S...","R/H/S..."
         ),
       c("MDR only","MDR+1st","MDR+1st","MDR+1st",
         "MDR+Ofx","MDR+Ofx","MDR+Ofx","MDR+Ofx",
         "MDR+Km","MDR+Km","MDR+Km",
         "XDR","XDR","XDR",
         "unknown","unknown","unknown","unknown"
         ),
       as.factor.result=TRUE
       )

#### recode resistant spectrum "MDR only","MDR+1st","MDR+Ofx/Km","XDR"
recode(vars =spectrum4,
       c("R/H","R/H/E","R/H/E/S","R/H/S",
         "R/H/E/Ofx","R/H/E/S/Ofx","R/H/Ofx","R/H/S/Ofx",
         "R/H/E/S/Km","R/H/Km","R/H/S/Km",
         "R/H/E/Ofx/Km","R/H/E/S/Ofx/Km","R/H/S/Ofx/Km",
         "R/H...","R/H/E...","R/H/E/S...","R/H/S..."
         ),
       c("MDR only","MDR+1st","MDR+1st","MDR+1st",
         "MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km",
         "MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km",
         "XDR","XDR","XDR",
         "unknown","unknown","unknown","unknown"
         ),
       as.factor.result=TRUE
       )

#### recode resistant spectrum "MDR+1st","MDR+Ofx/Km","XDR"
recode(vars =spectrum3,
       c("R/H","R/H/E","R/H/E/S","R/H/S",
         "R/H/E/Ofx","R/H/E/S/Ofx","R/H/Ofx","R/H/S/Ofx",
         "R/H/E/S/Km","R/H/Km","R/H/S/Km",
         "R/H/E/Ofx/Km","R/H/E/S/Ofx/Km","R/H/S/Ofx/Km",
         "R/H...","R/H/E...","R/H/E/S...","R/H/S..."
         ),
       c("MDR+1st","MDR+1st","MDR+1st","MDR+1st",
         "MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km",
         "MDR+Ofx/Km","MDR+Ofx/Km","MDR+Ofx/Km",
         "XDR","XDR","XDR",
         "unknown","unknown","unknown","unknown"
         ),
       as.factor.result=TRUE
       )

#### recode resistant spectrum "MDR only","MDR+"
recode(vars =spectrum2,
       c("R/H","R/H/E","R/H/E/S","R/H/S",
         "R/H/E/Ofx","R/H/E/S/Ofx","R/H/Ofx","R/H/S/Ofx",
         "R/H/E/S/Km","R/H/Km","R/H/S/Km",
         "R/H/E/Ofx/Km","R/H/E/S/Ofx/Km","R/H/S/Ofx/Km",
         "R/H...","R/H/E...","R/H/E/S...","R/H/S..."
         ),
       c("MDR only","MDR+","MDR+","MDR+",
         "MDR+","MDR+","MDR+","MDR+",
         "MDR+","MDR+","MDR+",
         "MDR+","MDR+","MDR+",
         "unknown","unknown","unknown","unknown"
         ),
       as.factor.result=TRUE
       )


#### recode resistant spectrum "MDR1","MDR2"
recode(vars =spectrum22,
       c("R/H","R/H/E","R/H/E/S","R/H/S",
         "R/H/E/Ofx","R/H/E/S/Ofx","R/H/Ofx","R/H/S/Ofx",
         "R/H/E/S/Km","R/H/Km","R/H/S/Km",
         "R/H/E/Ofx/Km","R/H/E/S/Ofx/Km","R/H/S/Ofx/Km",
         "R/H...","R/H/E...","R/H/E/S...","R/H/S..."
         ),
       c("MDR1","MDR1","MDR1","MDR1",
         "MDR2","MDR2","MDR2","MDR2",
         "MDR2","MDR2","MDR2",
         "MDR2","MDR2","MDR2",
         "unknown","unknown","unknown","unknown"
         ),
       as.factor.result=TRUE
       )

# recode 结果 即status==1 poor ;status=0 删失 :  
#         Frequency Percent Cum. percent
# poor          153    31.4         31.4
# success       335    68.6        100.0
#   Total       488   100.0        100.0

##status 1结局 0删失
recode(vars=status,c("poor","success","default"),c("1","0","0"))

# recode outcome
recode(vars = outcome_s_d_d_f ,
       c("治愈","完成治疗","死亡","失败","失访"),
       c("success","success","death","failure","default")
)

# recode outcome
recode(vars = outcome_c_c_d_d_f ,
       c("治愈","完成治疗","死亡","失败","失访"),
       c("cure","completion","death","failure","default")
)


####recode treatmentmodel
# recode(vars=treatmentmodel,
# 	c(),
# 	c()
# 	)

####recode 现详细地址类型 addresstype ！！tips：其它 not 其他 
recode(vars=addresstype2,c("本省其它地市","本市其它县区","其他省"),
       c("在外地","在外地","在外地"))


# ####recode 等级分类中的“其他”为“返回”
# recode(vars=tbregister,"返回","其他")

####recode  现住址类型 addresstype
recode(vars=addresstype,c("其他省","本省其它地市","本市其它县区"),c("其它地市","其它地市","其它县区"))
             
### recode regimen 治疗方案
recode(vars=regimen,
  c("IR：",  "SR1：", "SR2：", "SR3：", "SR4：", "SR5："),
  c("IR：个体化疗方案","SR1：6Z Am Lfx Cs Pto/18Z Lfx Cs Pto","SR2：6Z Am Lfx PAS Pto/18Z Lfx PAS Pto",
    "SR3：6Z Cm Lfx Cs Pto/18Z Lfx Cs Pto","SR4：6Z Cm Lfx PAS Pto/18Z Lfx Cs Pt","SR5：12Cm Mfx PAS Cs Clr Amx/Clv /24Mfx PAS Cs Clr Amx/Clv")
  )
#### recode Am_based 治疗方案
recode(vars=Am_based,c("FALSE","TRUE"),c("Cm","Am"))


# library(gmodels)
# CrossTable(regimen,outcome_s_d_d_f,prop.t=F,prop.c = F,prop.chisq=F, chisq = T, fisher=T)
# CrossTable(regimeSD,outcome_s_d_d_f,prop.t=F,prop.c = F,prop.chisq=F, chisq = T, fisher=T)
################################################################################################
################################################################################################
mdr0830=.data
mdr0830<- tbl_df(mdr0830) #更简洁
# mdr0830
# 一行5个
mdr0830<-mdr0830 %>%
select(DST缺失情况,name,history,treatmentmodel,addresstype,
       agecat,sex,tbregister,XDR,year,
       cavity,adverse,household,symptom,hemosputum,
       city,regime,regimeSD, outcome,status, 
       spectrum_firstline,spectrum4,spectrum3,spectrum2,spectrum22,
       outcome_s_d_d_f,resistnum,resistnum3,addresstype2,st_treatperiod,
       Cs_based,PAS_based,Am_based,regimen,month2,
       spectrum,outcome_c_c_d_d_f,
       time,timediff_smear_culture,timediff_smear_treat,timediff_smear_dst,timediff_culture_dst,timediff_culture_treat,
       timediff_dst_treat,age
)

var_n=1:44             # 全部变量下标
var_fctr=3:37            # 分类变量下标
var_int= var_n[-c(1:37)] # 数值变量下标
var_char_change=c(1,2,3,4,5) 
# var_char_change 三个变量history,treatmentmodel,addresstype,含有“NA”(字符)的变量，转换为字符型变量，
# 否则删除后，也有NA这一level，效果如下

# mdr0830$addresstype :  
#              Frequency Percent Cum. percent
# NA                   0     0.0          0.0
# 本省其它地市        51    10.5         10.5
# 本市其它县区        65    13.4         24.0
# 本县区             362    74.8         98.8
# 其他省               6     1.2        100.0
#   Total            484   100.0        100.0


##### 删除 NA
# mdr0830[,3]=as.character(history)
# mdr0830[,4]=as.character(treatmentmodel)
# mdr0830[,5]=as.character(addresstype)

# The function [ applied to a data frame returns a data frame (if only one argument is used). 
# If you want to access a single column and return it as a vector, you have to use [[ instead.
# dataframe$var 也行
# since as.factor cannot be used on a data frame.
# table[1] <- factor(table[[1]])

for (i in var_char_change)
mdr0830[,i]=as.character(mdr0830[[i]])

# 数值型和因子型变量设置
for (i in var_fctr)
mdr0830[,i]=as.factor(mdr0830[[i]])
for (i in var_int)
mdr0830[,i]=as.numeric(mdr0830[[i]])


# set reference groups
library(doBy)
mdr0830=within(mdr0830,{
        treatmentmodel=relevel(treatmentmodel,ref="门诊治疗")
        household=relevel(household,ref="本地")
        sex=relevel(sex,ref="女")
        spectrum_firstline=relevel(spectrum_firstline,ref="R/H")
        tbregister = relevel(tbregister, ref="新患者")
        symptom = relevel(symptom,ref="无")
        history= relevel(history,ref="仅使用过一线药")
        spectrum4 = relevel(spectrum4,ref="MDR only")
        agecat= relevel(agecat,ref="26-45yrs")
        regimeSD=relevel(regimeSD,ref="SR")
        Cs_based=relevel(Cs_based,ref="IR")
        PAS_based=relevel(PAS_based,ref="IR")
        Am_based=relevel(Am_based,ref="IR")
        addresstype=relevel(addresstype,ref="本县区")
        addresstype2=relevel(addresstype2,ref="本县区")
        adverse=relevel(adverse,ref="无")
        month2=relevel(month2,ref="Negative")
        resistnum=relevel(resistnum,ref="2")
        # spectrum=relevel(spectrum,ref="XDR")
        resistnum3=recodeVar(resistnum, src=c(2,3:4,5:6), tgt=c("2","3-4","3-4","5-6","5-6"))
        resistnum3=relevel(resistnum3,ref="2")
        #resistnum=recodeVar(resistnum, src=c(2,3,4:6), tgt=c("2","3","4-6","4-6","4-6"))
        #resistnum=recodeVar(resistnum, src=c(2:3,4:6), tgt=c("2-3","2-3","4-6","4-6","4-6"))
        SurvObj=Surv(time, status==1)
})
# # SurvObj <- Surv(timeday,outcomepoor)
# mdr0830$SurvObj <- with(mdr0830, Surv(time, status==1)) 
mdr0830$st_treatperiod=as.numeric(mdr0830$st_treatperiod)
mdr0830$st_treatperiod[mdr0830$st_treatperiod==1]=2009
mdr0830$st_treatperiod[mdr0830$st_treatperiod==2]=2010       
mdr0830$st_treatperiod[mdr0830$st_treatperiod==3]=2011       
mdr0830$st_treatperiod[mdr0830$st_treatperiod==4]=2012


mdr0830$st_treatperiod=as.factor(mdr0830$st_treatperiod)



mdr380<-mdr0830 %>%
filter(spectrum4 != "unknown"&addresstype != "NA")


library(tableone)
library(ReporteRsjars)
library(ReporteRs)
library(magrittr)

desVars="outcome_c_c_d_d_f"
catVars="outcome_c_c_d_d_f"
tablepattern <- CreateTableOne(vars = desVars, strata= c("spectrum4"),data=mdr380, factorVars =catVars)
tablepattern <- print(tablepattern,showAllLevels=TRUE,quote=TRUE)

levels(mdr380$regimen)<-c("IR","SR1","SR2","SR3","SR4","SR5")
tablepattern <- CreateTableOne(vars = desVars, strata= c("regimen"),data=mdr380, factorVars =catVars)
tablepattern <- print(tablepattern,showAllLevels=TRUE,quote=F)
# docx( ) %>% 
#         addFlexTable(tablepattern %>%
#                              FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
#                                        header.text.props = textBold( color = "white" ),
#                                        add.rownames = TRUE ) %>%
#                              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
#         writeDoc(file = "pattern.docx")


# tablespectrum4 <- CreateCatTable(catVars,data=mdr380,strata= c("spectrum4"))
# tablespectrum4 <- print(tablespectrum4,showAllLevels=T,noSpaces=T)	
# docx( ) %>% 
#         addFlexTable(tablepattern %>%
#                              FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
#                                        header.text.props = textBold( color = "white" ),
#                                        add.rownames = TRUE ) %>%
#                              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
#         writeDoc(file = "pattern.docx")

desVars="spectrum"
catVars="spectrum"
tablepattern <- CreateTableOne(vars = desVars, strata= c("outcome_c_c_d_d_f"),data=mdr380, factorVars =catVars)
tablepattern <- print(tablepattern,showAllLevels=TRUE,quote=FALSE)
tablepattern <- CreateTableOne(vars = desVars,data=mdr380, factorVars =catVars)
tablepattern <- print(tablepattern,showAllLevels=TRUE,quote=FALSE)



desVars <- c("DST缺失情况", "history", "treatmentmodel", "addresstype", 
"agecat", "sex", "tbregister", "cavity", "adverse", 
"household", "symptom", "hemosputum",
"status", "outcome_s_d_f_f", "spectrum4","city","month2",
"addresstype2", "st_treatperiod", 
"time", "timediff_smear_treat", "timediff_smear_culture", "timediff_smear_dst", 
"SurvObj")
##catigorical var
catVars <- c("DST缺失情况", "history", "treatmentmodel", "addresstype", 
"agecat", "sex", "tbregister", "cavity", "adverse", 
"household", "symptom", "hemosputum",
"status", "outcome_s_d_f_f", "spectrum4","city","month2",
"addresstype2", "st_treatperiod")
tableSD380 <- CreateTableOne(desVars,strata = c("regimeSD"), mdr380, catVars)
tableSD380 <- print(tableSD380,nonnormal = c("timediff_smear_dst","timediff_smear_treat","timediff_smear_treat", "timediff_smear_culture","time"),
                # exact = c("history","tbregister","outcome_s_d_f_f"),
                exact = c("history","tbregister","outcome_s_d_f_f","treatmentmodel","household","status","sex","spectrum4","city"),
                # cramVars = c("sex","household","hemosputum","cavity","adverse","status","regimeSD"),
                showAllLevels = TRUE,
                quote = FALSE)
library(ReporteRsjars)
library(ReporteRs)
library(magrittr)

docx( ) %>% 
        addFlexTable(tableSD380 %>%
                             FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                                       header.text.props = textBold( color = "white" ),
                                       add.rownames = TRUE ) %>%
                             setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
        writeDoc(file = "SD380.docx")




CrossTable(st_treatperiod,outcome_c_c_d_d_f,digits=3,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,chisq = T,fisher=T,format="SPSS")