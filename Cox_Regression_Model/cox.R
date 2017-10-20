# COX MODEL 等比例风险假定 检验 ---------------------------------------------
mdr517$SurvObj <- with(mdr517, Surv(timemonths, status==1))
plot(survfit(SurvObj~city,data=mdr517),fun="cloglog",conf.int=F,col=c("grey3","yellow3","green3","red3","blue3"))
# 两条曲线相交不止一次，从图形很难判断是否违反比例风险假定。
# 可采取如下检验
czph <- cox.zph(coxph(SurvObj~strata(city)+agecat+sex+tbregister+household+occupation+history+cavity+adverse+hemosputum+treatmentmode+pattern3+regimenIS,data=mdr517))
czph
plot(czph) # 得到COX调整后生存曲线的 beta图形

# COX MODEL 517 CPH 满足------------------------------------------------------ 
coxmodel <- coxph(SurvObj~strata(city)+factor(agecat)+factor(sex)+factor(occupation)+factor(household)+factor(tbregister)+factor(history)+factor(treatmentmode)+factor(regimenIS)+factor(pattern3)+factor(hemosputum)+factor(cavity)+factor(adverse),data=mdr517)
summary(coxmodel)
# COX MODEL 517 CPH不满足 ---------------------------------------------------- 
# To convert SURV into the counting process style, we first define a vector of unique event times:
# cut.points <- unique(SURV$time[SURV$death == 1])
# We then call survSplit and save the result to SURV2.
# library("survival")
# SURV2 <- survSplit(data = SURV, cut = cut.points, end = "time",
# + start = "time0", event = "death")
# To make the appearance match SAS, we sort SURV2 by subject then rename and reorder the
# columns.
# SURV2 <- SURV2[order(SURV2$id), ]
# colnames(SURV2) <- c("id", "time1", "death", "age", "female", "time0")
# SURV2 <- SURV2[, c("id", "age", "female", "time0", "time1", "death")]
# SURV2

# cox table ----------------------------------------------------------------
library(stargazer)
# Table with coefficients --------------------------------------------------
# stargazer(coxmodel,digits=1, single.row = TRUE, ci=TRUE, type = "text")
# calculate
OR.vector <- exp(coxmodel$coef)
CI.vector <- exp(confint(coxmodel,level = 0.95))
p.values <- summary(coxmodel)$coefficients[, 5]
# Table with ORs and CIs ---------------------------------------------------
stargazer(coxmodel,digits=1, ci = TRUE, single.row = TRUE, type = "text",
	coef = list(OR.vector), ci.custom = list(CI.vector), p = list(p.values))



# 单因素cox ----------------------------------------------------------------------
coxmodel <- coxph(SurvObj~factor(agecat),data=mdr517)
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(sex),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(occupation),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(household),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(tbregister),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(history),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(treatmentmode),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(regimenIS),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(pattern3),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(hemosputum),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(cavity),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)
coxmodel <- coxph(SurvObj~factor(adverse),data=droplevels(subset(mdr517,regimenshort!="Unknown")))
summary(coxmodel)



# sensitivity test ---------------------------------------------------------
coxmodel <- coxph(SurvObj~strata(city)+factor(agecat)+factor(sex)+factor(occupation)+factor(household)+factor(tbregister)
       +factor(history)+factor(treatmentmode)+factor(regimenIS)+factor(pattern3)+factor(hemosputum)+factor(cavity)+factor(adverse),
       data=droplevels(subset(mdr517,regimenshort!="Unknown")))
# 517(有时间的patients) 中治疗方案已知者415
mdr415=subset(mdr517,regimenshort!="Unknown")
summary(coxmodel)
OR.vector <- exp(coxmodel$coef)
CI.vector <- exp(confint(coxmodel,level = 0.95))
p.values <- summary(coxmodel)$coefficients[, 5]
# Table with ORs and CIs ---------------------------------------------------
stargazer(coxmodel,digits=1, ci = TRUE, single.row = TRUE, type = "text",
       coef = list(OR.vector), ci.custom = list(CI.vector), p = list(p.values))

# 对数线性假设(Linearity of log hazard)检验-----------------------------------
# 对自变量是连续性变量，需检测其线性的假设
# Cumulative survival function
surv_coxmodel<-survfit(coxmodel)
#plot Cumulative survival function
plot(surv_coxmodel)
summary(coxmodel)
# stepAIC(coxmodel)

# cox拟合后生存曲线 cavity ----------------------------------------------------
coxmodel_km <-coxph(SurvObj~strata(cavity)+city+agecat+sex+tbregister+household+occupation+history+cavity+adverse+hemosputum+treatmentmode+EMB+SM+KM+OFX,data=mdr517)
surv_coxmodel_km<-survfit(coxmodel_km)
plot(surv_coxmodel_km)
# tiff cavity -----------------------------------------------------------------
tiff(file="KM_cox.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.3)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
# xaxs="i", yaxs="i",则坐标轴从0开始
plot(surv_coxmodel_km,lwd=1.7, ylim=c(0, 1), xlim=c(0, 1350), xaxs="r", yaxs="r",lty=1,xlab="Survival Days (Days from MDR-TB treatment initiation to poor outcomes)",col = c("blue3","black","firebrick3"),ylab="Survival Distribution Function",conf.int = FALSE,mark.time = T,cex.lab=1.7)#lty=1:2

legend(100, 0.25, 
       legend=c("No", "Unknown","Yes"), lty=1, 
       cex=1.7,horiz = F,merge = F,
       col = c("blue3","black","firebrick3"),bty = "n",
       ) # bty = "n",lty=1:2
text(200,0.27,"Cavity",cex = 1.7)
dev.off()


# cox拟合后生存曲线 cavity ----------------------------------------------------
coxmodel_km <-coxph(SurvObj~strata(cavity)+city+agecat+sex+tbregister+household+occupation+history+adverse+hemosputum+treatmentmode+EMB+SM+KM+OFX,data=mdr517)
surv_coxmodel_km<-survfit(coxmodel_km)
plot(surv_coxmodel_km)
# tiff cavity -----------------------------------------------------------------
tiff(file="KM_cox.tiff", width=11, height=10, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.8, cex.axis=1.7)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
# xaxs="i", yaxs="i",则坐标轴从0开始
plot(surv_coxmodel_km,lwd=1.8, ylim=c(0, 1), xlim=c(0, 1350), yaxs="r",lty=1,xlab="Survival Days (Days from MDR-TB treatment initiation to poor outcomes)",col = c("blue3","black","firebrick3"),ylab="Survival Distribution Function",conf.int = FALSE,mark.time = T,cex.lab=1.8)#lty=1:2

legend("bottomleft", 
       legend=c("No", "Unknown","Yes"), lty=1, 
       cex=1.8,horiz = F,merge = F,
       col = c("blue3","black","firebrick3"),bty = "n",
       ) # bty = "n",lty=1:2
text(100,0.17,"Cavity",cex = 1.8)
dev.off()





# COX MODEL 163------------------------------------------------------------- 
coxmodel163 <- coxph(SurvObj~strata(city)+agecat+sex+tbregister+household+history+cavity+adverse+KM+OFX, data=mdr163)
summary(coxmodel163)
# cox table ----------------------------------------------------------------
OR.vector <- exp(coxmodel163$coef)
CI.vector <- exp(confint(coxmodel163,level = 0.95))
p.values <- summary(coxmodel163)$coefficients[, 5]
# Table with ORs and CIs ---------------------------------------------------
stargazer(coxmodel163,digits=1, ci = TRUE, single.row = TRUE, type = "text",
	coef = list(OR.vector), ci.custom = list(CI.vector), p = list(p.values))