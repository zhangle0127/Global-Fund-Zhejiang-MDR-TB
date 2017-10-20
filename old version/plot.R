
# percentage success -------------------------------------------------------
tiff(file="successful_regimen.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
par(pin=c(4,3),mar=c(3,7,4,5)+0.1,mgp=c(3.7,1,0))

se=c(0,0,0,0,0,0)
p=c(0.652,0.683,0.685,0.842,0.643,1.000)
n=c(92,41,219,19,42,2)
for (i in 1:6)
se[i]=sqrt(p[i]*(1-p[i])/n[i])
x=1:6
plot(x,p,
     ylim=c(0.4,1.0),pch=16,cex=1.5,xlab=NA,xaxt="n",bty="n",
     ylab="Treatment success proportion (%)",cex.lab=1.7)
text(1.15,0.652,"IR",cex=1.3)
text(2.18,0.683,"SR1",cex=1.3)
text(3.18,0.685,"SR2",cex=1.3)
text(4.18,0.842,"SR3",cex=1.3)
text(5.18,0.643,"SR4",cex=1.3)
text(5.75,1.000,"SR5",cex=1.3)
     # xaxt="n" # no x axis
     # bty="n"  # no 四周边框
regimenlable=c("IR", "SR1","SR2","SR3","SR4","SR5")
# 做标准差的线
arrows(x,p-se,x,p+se,length=0.05,angle=90,code=3)
legend(0.8,0.53,c("IR：Individualized Regimen", 
	"SR1：6Z Am Lfx Cs Pto/18Z Lfx Cs Pto",
    "SR2：6Z Am Lfx PAS Pto/18Z Lfx PAS Pto",
    "SR3：6Z Cm Lfx Cs Pto/18Z Lfx Cs Pto",
 	"SR4：6Z Cm Lfx PAS Pto/18Z Lfx Cs Pt",
	"SR5：12Cm Mfx PAS Cs Clr Amx/Clv /24Mfx PAS Cs Clr Amx/Clv"),
     bty="n",cex=1.3)
# axis(1,at=seq(1,6,1),label=regimenlable)  
dev.off()

# 总体生存曲线  KM-非参-------------------------------------------------------
# Days -----------------------------------------------------------------------
tiff(file="KM_total_days.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
mdr$SurvObj <- with(mdr, Surv(time, status==1))
fit=survfit(SurvObj~1,data=mdr)
plot(fit,lty=1,ylab="Cumulative survival function",conf.int = F,
     xlab="Survival Time (Days from MDR-TB treatment initiation to poor outcome)",
     mark.time=T,xlim=c(0, 1500), yaxs="r",lwd=1.7,cex.lab=1.6)
dev.off()
# Months -------------------------------------------------------------------
tiff(file="KM_total_months.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
mdr$SurvObj <- with(mdr, Surv(time, status==1))
fit=survfit(SurvObj~1,data=mdr)
plot(fit,lty=1,ylab="Cumulative survival function",conf.int = T,xscale=30.4375,
     xlab="Survival Time (Months from MDR-TB treatment initiation to poor outcome)",
     mark.time=T,xlim=c(0, 50), yaxs="r",lwd=1.7,cex.lab=1.6)
axis(side=1,at=seq(0,1500, 60.875),labels=F)
dev.off()

# regimenIS IR vs SR 生存曲线--------------------------------------------------
# 无差异 Days------------------------------------------------------------------
tiff(file="KM_regimenIS-days.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
mdr$SurvObj <- with(mdr, Surv(time, status==1))
fit=survfit(SurvObj~regimenIS,data=mdr)
plot(fit,col=c("blue3","firebrick3"),lty=1,ylab="Cumulative survival function",
     xlab="Survival Time (Days from MDR-TB treatment initiation to poor outcome)",
     mark.time=T,xlim=c(0, 1500), yaxs="r",lwd=1.7,cex.lab=1.6)
legend("topright",title="Regimen",c("Standardized","Individualized"),
       col=c("blue3","firebrick3"),lty=1,cex=1.5)
dev.off()
# Months --------------------------------------------------------------------
tiff(file="KM_regimenIS-months.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
mdr$SurvObj <- with(mdr, Surv(time, status==1))
fit=survfit(SurvObj~regimenIS,data=mdr)
plot(fit,col=c("blue3","firebrick3"),lty=1,ylab="Cumulative survival function",xscale=30.4375,
     xlab="Survival Time (Months from MDR-TB treatment initiation to poor outcome)",
     mark.time=T,xlim=c(0, 50), yaxs="r",lwd=1.7,cex.lab=1.6)
axis(side=1,at=seq(0,1500, 60.875),labels=F)
legend("topright",title="Regimen",c("Standardized","Individualized"),
       col=c("blue3","firebrick3"),lty=1,cex=1.5)
dev.off()

# 不同生存曲线间是否有差异，可通过survdiff进行比较----------------------------
survdiff(mdr$SurvObj~mdr$regimenIS) # p= 0.426
# fit
fit
# 生存时间比较
quantile(fit, probs=c(0.25, 0.5, 0.75), conf.int=FALSE)

# "MDR only","MDR_FL","MDR_SL","XDR" ----------------------------------------
# p= 0.00167 
tiff(file="KM_pattern4.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
# dataset p4
p4=subset(mdr,queshi=0)
levels(p4$pattern4)
p4$SurvObj <- with(p4, Surv(time, status==1))
fit=survfit(SurvObj~pattern4,data=p4)
plot(fit,col=c("blue3","firebrick3","darkgreen","black"),ylab="Cumulative survival function",xlab="Survival Time (Days from MDR-TB treatment initiation to death)",lwd=1.7,lty=1,mark.time=T,xlim=c(0, 1500),yaxs="r",cex.lab=1.6)
legend("topright",c("MDR only","MDR_FL","MDR_SL","XDR"),
       col=c("blue3","firebrick3","darkgreen","black"),lty=1,cex=1.5)
# 不同生存曲线间是否有差异，可通过survdiff进行比较
survdiff(p4$SurvObj~p4$pattern4) ##有差异
dev.off()

# "MDR_FL","MDR_SL","XDR" ---------------------------------------------------
# p= 0.000567
tiff(file="KM_pattern3_days.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
p3=mdr
p3=subset(mdr,queshi=0)
levels(p3$pattern3)
p3$SurvObj <- with(p3, Surv(time, status==1))
fit=survfit(SurvObj~pattern3,data=p3)
plot(fit,col=c("blue3","firebrick3","darkgreen","black"),lty=1,ylab="Cumulative survival function",xlab="Survival Days (Days from MDR-TB treatment initiation to death)",lwd=1.7,
     mark.time=T,xlim=c(0, 1500), yaxs="r",cex.lab=1.6)
legend("topright",c("MDR_FL","MDR_SL","XDR"),
       col=c("blue3","firebrick3","darkgreen","black"),lty=1,cex=1.5)
dev.off()
survdiff(p3$SurvObj~p3$pattern3) 
# Months ---------------------------------------------------------------------
tiff(file="KM_pattern3_months.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
p3=mdr
p3=subset(mdr,queshi=0)
# SurvObj <- Surv(timeday,outcomepoor)
p3$SurvObj <- with(p3, Surv(time, status==1))
fit=survfit(SurvObj~pattern3,data=p3)
plot(fit,col=c("blue3","firebrick3","darkgreen","black"),ylab="Cumulative survival function",xlab="Survival Months (Months from MDR-TB treatment initiation to death)",lwd=1.7,lty=1,mark.time=T,xlim=c(0, 1300), yaxs="r",cex.lab=1.6)
axis(side=1,at=seq(0,1500, 60.875),labels=F)
legend("topright",c("MDR_FL","MDR_SL","XDR"),
       col=c("blue3","firebrick3","darkgreen","black"),lty=1,cex=1.5)
survdiff(pattern3$SurvObj~pattern3$pattern3) ##略有差异
dev.off()

# "MDR_FL","MDR_SL" -----------------------------------------------------------
# p= 0.00323 
tiff(file="KM_pattern2.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
p2=mdr
p2=subset(mdr,queshi=0)
levels(p2$pattern2)
p2$SurvObj <- with(p2, Surv(time, status==1))
fit=survfit(SurvObj~pattern2,data=p2)
plot(fit,col=c("blue3","firebrick3"),lty=1,ylab="Cumulative survival function",xlab="Survival Days (Days from MDR-TB treatment initiation to death)",lwd=1.7,mark.time=T,xlim=c(0, 1500), yaxs="r",cex.lab=1.6)
legend("topright",c("MDR_FL","MDR_SL"),
       col=c("blue3","firebrick3"),lty=1,cex=1.5)
dev.off()
survdiff(p2$SurvObj~p2$pattern2) 

# LINES------------------------------------------------------------------------
# specify the data
library(gmodels)
CrossTable(mdr$year,mdr$outcome4,digits=3,prop.c=FALSE,prop.r=TRUE,prop.t=FALSE,prop.chisq=FALSE,chisq = T,fisher=T,format="SPSS")
# specify the data 
x <- c(2009,2010,2011,2012,2013)
Failure<-c(0.16667,0.21849,0.23007,0.14679,0.14286)#darkgoldenrod3
Death<-c(0.09524,0.06723,0.06838,0.06422,0.10714)#firebrick3
Success=c(0.73810,0.67227,0.62393,0.71560,0.75000)#blue3
# default=c(0, 0.04202, 0.07692, 0.07339, 0)
# plot--------------------------------------
tiff(file="outcome_year.tiff", width=12, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.4)
par(pin=c(4,3),mar=c(5,6,4,3)+0.1,mgp=c(3.7,1,0))
# par(pin=c(4,3),mar=c(7,7,4,4)+1,mgp=c(4,1.7,0),lwd=2)
# mgp参数共有三个值，第一个控制图跟轴标题之间的行数，
# 第二个控制图跟轴标签的行数，
# 第三个值控制图跟轴线之间的行数
plot(x, Failure,type="b", pch=15, col="black", lty=1, lwd=2,
     ylab="Proportion of cases (%)",
     xlab="",
     xaxt="n",# no x axis
     yaxt="n", # no y axis
     bty="n", # no 四周边框
 	 xlim=c(2009,2013), ylim=c(0,0.9),
 	 xaxs="r", #Set x axis style as internal
     yaxs="r", #Set y axis style as internal 
     cex.lab=2)

lines(x, Death,type="b", pch=16, col="firebrick3",lty=5,lwd=3)
lines(x, Success,type="b", pch=17, col="blue3",lty=4, lwd=3)
points(x, Failure, pch=15, cex=1.8, col="black")
points(x, Death, pch=16, col="firebrick3", cex=1.8)
points(x, Success,pch=17, col="blue3", cex=1.8)
legend("topright", horiz = TRUE,legend=c("Success","Failure","Death"), 
	   pch=c(17,15,16),lty=c(4,1,5), bty = "n", cex=1.7, merge = T,
       col = c("blue3","black","firebrick3"))
       # title="Treatment outcome"
axis(side=1, at=c(2009,2010,2011,2012,2013),las=0,cex.lab=2.5,cex.axis=1.5,
	tcl=-.3,pos=0,col="black",lwd=1.5)
axis(side=2, at=seq(0,0.9,0.1),las=2, cex.lab=2.5,col="black",
	cex.axis=1.5,tcl=-.3,pos=2009,lwd=1.5)
dev.off()

# COX MODEL ----------------------------------------------------------------- 

coxmodel <- coxph(SurvObj~agecat+sex+tbregister+pattern4+cavity+household+
                      hemosputum+history+treatmentmode+regimenIS+
                      adverse+strata(city)+timediff_smear_treat,data=mdr)
summary(coxmodel)
############################################################################################
## COX MODEL 假定 检验 ######################################################################

# # 比例风险假定
# mdr$SurvObj <- with(mdr, Surv(time, status==1))
# plot(survfit(SurvObj~regimenIS,data=mdr),fun="cloglog",conf.int=F,col=c("red3","blue3"))
# # 两条曲线相交不止一次，从图形很难判断是否违反比例风险假定。
# # 可采取如下检验
# czph <- cox.zph(coxph(SurvObj~agecat+sex+tbregister+cavity+household+pattern4+month2+
#                       hemosputum+history+treatmentmode+regimenIS+adverse+strata(city)+
#                       timediff_smear_treat,data=mdr))
# czph 
# plot(czph) # 得到COX调整后生存曲线的 beta图形

##############################################################################################
# 对数线性假设(Linearity of log hazard)
# 对自变量是连续性变量，需检测其线性的假设

# Cumulative survival function
surv_coxmodel<-survfit(coxmodel)
#plot Cumulative survival function
# plot(surv)
# summary
summary(coxmodel)

# stepAIC(coxmodel)

# ######## plot   #####################################################################
# tiff(file="KM_cox.tiff", width=11, height=10, units="in", compression="lzw", res=600)
# par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
# par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))

# # xaxs="i", yaxs="i",则坐标轴从0开始
# plot(surv_coxmodel,lwd=1.7, ylim=c(0, 1), xlim=c(0, 1300), xaxs="r", yaxs="r",lty=1,
#      xlab="Survival Days (Days from MDR-TB treatment initiation to death)",
#      col = c("blue3","firebrick3"),
#      ylab="Survival Distribution Function",
#      conf.int = F,
#      mark.time = T,cex.lab=1.5)#lty=1:2

# legend(1250, 0.93, 
#        legend=c("Standard", "Individualized"), lty=1, 
#        cex=1.5,horiz = F,merge = F,
#        col = c("blue3","firebrick3"),bty = "n",
#        ) # bty = "n",lty=1:2
# text(1570,0.95,"Treatment Regime",cex = 1.5)

# dev.off()



######################################################################################
##### COX MODEL STRATA ###############################################################
######################################################################################
coxmodel_strata <- coxph(SurvObj~agecat+sex+tbregister+cavity+household+pattern4+month2+
                      hemosputum+history+treatmentmode+adverse+regimenIS+strata(city)+
                      timediff_smear_treat,data=mdr)

# Cumulative survival function
surv_coxmodel_strata<-survfit(coxmodel_strata)

#plot Cumulative survival function
# plot(surv)
# summary
summary(coxmodel_strata)

# stepAIC(coxmodel_strata)

# ######## plot   ####################################################################
# tiff(file="KM_cox_strata.tiff", width=11, height=10, units="in", compression="lzw", res=600)
# par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
# par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))

# # xaxs="i", yaxs="i",则坐标轴从0开始
# plot(surv_coxmodel_strata,lwd=1.7, ylim=c(0, 1), xlim=c(0, 1300), xaxs="r", yaxs="r",lty=1,
#      xlab="Survival Days (Days from MDR-TB treatment initiation to death)",
#      col = c("blue3","firebrick3"),
#      ylab="Survival Distribution Function",
#      conf.int = F,
#      mark.time = T,cex.lab=1.5)#lty=1:2

# legend(1250, 0.93, 
#        legend=c("Standard", "Individualized"), lty=1, 
#        cex=1.5,horiz = F,merge = F,
#        col = c("blue3","firebrick3"),bty = "n",
#        ) # bty = "n",lty=1:2
# text(1570,0.95,"Treatment Regime",cex = 1.5)

# dev.off()




 
 # 错误
 # levels(mdr$pattern4)
 # levels(mdr$pattern4)<-c("MDR only","MDR+1st","MDR+Ofx/Km","XDR","unknown")

#  library(gmodels)
# mytable <- table(mdr$regimen,mdr$outcome_s_d_f_f,mdr$pattern4) 
#  ftable(mytable)


CrossTable(mdr$timecat,mdr$outcome_s_d_f_f,prop.t=F,prop.c = F,prop.chisq=F, chisq = T, fisher=T)

CrossTable(mdr$history,mdr$outcome_s_d_f_f,prop.t=F,prop.c = F,prop.chisq=F, chisq = T, fisher=T)

library(gmodels)
CrossTable(mdr$pattern22,mdr$history,prop.t=F,prop.c = F,prop.chisq=F, chisq = T, fisher=T)

mytable <- table(mdr$pattern22,mdr$outcome_s_d_f_f,mdr$history) 
ftable(mytable)
