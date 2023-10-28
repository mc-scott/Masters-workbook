rm(list=ls())

library("lme4", lib.loc="C:/Users/Matt/Documents/R/win-library/3.0")
library("nortest", lib.loc="C:/Users/Matt/Documents/R/win-library/3.0")

# ----------------------------Data input-----------------------------------
#     Reading full data

Data<-read.csv("IIV data- working sheet.csv")

names(Data)

ID<-as.factor(Data$ID)
OCC<-(Data$OC)
TRT<-(Data$TRT)
ORD<-(Data$TRT.ORD)
RT<-(Data$RT)
RT.Ln<-(Data$RT.Ln.1)
RT.Lo<-(Data$RT.Log10.1)

#    Reading subsetted data
#        Fed treatment
TRT.F<-subset(Data, TRT=="F")

names(TRT.F)

ID.F<-(TRT.F$ID)
OCC.F<-(TRT.F$OC)
ORD.F<-(TRT.F$TRT.ORD)
RT.F<-(TRT.F$RT)
RT.Ln.F<-(TRT.F$RT.Ln.1)
RT.Lo.F<-(TRT.F$RT.Log10.1)

#        Starved treatment
TRT.S<-subset(Data, TRT=="S")

names(TRT.S)

ID.S<-(TRT.S$ID)
OCC.S<-(TRT.S$OC)
ORD.S<-(TRT.S$TRT.ORD)
RT.S<-(TRT.S$RT)
RT.Ln.S<-(TRT.S$RT.Ln.1)
RT.Lo.S<-(TRT.S$RT.Log10.1)

#--------------------------Normality testing-------------------------

hist(RT, xlab="Reaction Time", main="No trans- Full data")
hist(RT.Ln, xlab="Reaction Time", main="Ln trans- Full data")
hist(RT.Lo, xlab="Reaction Time", main="Log10 trans- Full data")
hist(RT.Ln.F, xlab="Reaction Time", main="Ln trans- Fed")
hist(RT.Ln.S, xlab="Reaction Time", main="Ln trans- Starved")

qqnorm(RT, main="No transformation")
qqnorm(RT.Lo, main="Log10 transformation")
qqnorm(RT.Ln, main="Ln transformation")
qqnorm(RT.Ln.F, main="Ln Fed treatment")
qqnorm(RT.Ln.S, main="Ln Starved treatment")

ad.test(RT)# No transformation, full data
ad.test(RT.Ln)# LN trans, full data
ad.test(RT.Lo)# Log10 trans, full data
ad.test(RT.Ln.F)# LN trans, Fed treatment
ad.test(RT.Lo.F)# Log10 trans, Fed treatment
ad.test(RT.Ln.S)# LN trans, Starved treatment
ad.test(RT.Lo.S)# Log10 trans, Starved treatment

#-------------Visualising Mean RT & Testing for differences---------

boxplot(RT.Ln~TRT, ylab="ln Reaction Time", xlab="Treatment", col="lightgray")
boxplot(RT.Ln~OCC, ylab="ln Reaction time", xlab="Occassion", col="lightgray")
wilcox.test(RT.Ln~TRT, paired=TRUE)#!Need same length data to do a paired test
#71- P<0.05 (0.0019) so reject null hyp that two groups have the same median value.TRT
# is significantly different. (WHEN DON'T USE ASSUMPTION THAT IT IS PAIRED DATA).
boxplot(RT.Ln~ORD, ylab="ln Reaction Time", xlab="Treatment Order", col="lightgray")
wilcox.test(RT.Ln~ORD, paired=TRUE)#!Again, need same sized data set for a paired test

#--------------------Liklihood Ratio tests on models with both treatments-------------------

MF<-lmer(RT.Ln~TRT+OCC+TRT:OCC+(1+TRT|ID)+(1+OCC|ID), REML=FALSE)
summary(MF)

anova(MF, ddf="Kenward-Roger")
rand(MF)

#sTUFF MARK SUGGESTED 24TH jUNE-------------------------------------------------------------------

MF2<-lmer(RT.Ln~TRT+OCC+TRT:OCC+(TRT:OCC|ID), REML=FALSE) #TRY THIS ONE FOR BOTH RANDOM EFFECTS

MF3<-lmer(RT.Ln~TRT+OCC+TRT:OCC+(1+TRT|ID), REML=FALSE)
anova(MF3, ddf="Kenward-Roger")
rand(MF3)

MF4<-lmer(RT.Ln~TRT+OCC+TRT:OCC+(1+OCC|ID), REML=FALSE)
anova(MF4, ddf="Kenward-Roger")
rand(MF4)

#------------------------------------------------------------------------------------------------

NM1<-lmer(RT.Ln~TRT+OCC+(1+TRT|ID)+(1+OCC|ID), REML=FALSE)
summary(NM1)
anova(MF, NM1)
#ANOVA MF vs NM1 which takes out the interaction effect is not significantly different (p=0.2326)
#so the interaction affect between TRT:OCC is not significant.However because the AIC value for 
#the model without the interaction effect is lower, this is the better model (?) so then you compare 
#the best model with the next model (or is it that you compare the full model with all the null 
#models and then compare AIC values?)

NM2<-lmer(RT.Ln~TRT+(1+TRT|ID), REML=FALSE)
summary(NM2)
anova(MF, NM2)
#ANOVA MF vs NM2 which takes out the interaction effect and OCC is significantly different
#(p=0.0004606) and because the interaction effect wasn't significant it means that OCC is a
#significant factor. However MF still has the lowst AIC value.

NM3<-lmer(RT.Ln~OCC+(1+OCC|ID), REML=FALSE)
summary(NM3)
anova(MF, NM3)
#ANOVA MF vs NM3 which takes out the interaction effect and TRT is significantly different
#(p=5.43x10^-8) and because interaction effect isn't significant it means TRT is a significant
#factor. MF has the highest AIC out of all the models.

#After anova on all models, the one with the lowest AIC value is 'lmer(RT.Ln~TRT+OCC+(1+TRT|ID)
#+(1+OCC|ID), REML=FALSE).

#Now liklihood ratio testing random effects models using best fixed effect model:

NM4<-lmer(RT.Ln~TRT+OCC+(1+TRT|ID), REML=FALSE)
summary(NM4)
anova(NM1, NM4)
#Not significantly different however NM4 has the lowest AIC value.

NM5<-lmer(RT.Ln~TRT+OCC+(1+OCC|ID), REML=FALSE)
summary(NM5)
anova(NM1, NM5)
#ANOVA NM1 vs NM5 which takes out the random effect of (1+TRT|ID) is a significant factor
#(p=1.148x10^-6).

NM6<-lmer(RT.Ln~TRT+OCC+(1+TRT|ID)+(1|OCC), REML=FALSE)
summary(NM6)
anova(NM1, NM6)
#ANOVA NM1 vs NM6 not significant, also NM4 still has lowest AIC.

#NM4 has the lowest AIC value out of all models.
#'TRT:OCC', '(1+OCC|ID)' are not significant factors.
#'OCC', 'TRT', and (1+TRT|ID) are all significant predictors of RT.Ln.

#---------------------Residules from best model for both treatments-----------------

#NM4<-lmer(RT.Ln~TRT+OCC+(1+TRT|ID), REML=FALSE)
summary(NM4)
ranef(NM4)
resid(NM4)

#--------------------------Seperate treatment models--------------------------------
#-----Fed treatment
MF.F<-lmer(RT.Ln.F~OCC.F+(1+OCC.F|ID.F), REML=FALSE)
summary(MF.F)
NM1.F<-lmer(RT.Ln.F~1+(1+OCC.F|ID.F), REML=FALSE)
summary(NM1.F)
anova(MF.F, NM1.F)
#ANOVA MF.F vs NM1.F is significant so OCC.F is a significant factor and MF.F has the lowest AIC

NM2.F<-lmer(RT.Ln.F~OCC.F+(1|ID.F)+(1|OCC.F), REML=FALSE)
summary(NM2.F)
anova(MF.F, NM2.F)
#No significant difference between the two but NM2.F has the lowest AIC value

NM3.F<-lmer(RT.Ln.F~OCC.F+(1|ID.F), REML=FALSE)
summary(NM3.F)
anova(NM2.F, NM3.F)
# ANOVA MF.F vs NM3.F, No significant difference but NM3.F has the new lowest AIC value
# ANOVA NM2.F vs NM3.F not significant. AIC NM3.F of 179.64

NM4.F<-lmer(RT.Ln.F~OCC.F+(1|OCC.F), REML=FALSE)
summary(NM4.F)
anova(NM2.F, NM4.F)
# ANOVA NM2.F vs NM4.F is significant so '(1|ID)' is a significant factor. NM4.F has an AIC
# value of 181.64.

#NM3.F best fitting model 'NM3.F<-lmer(RT.Ln.F~OCC.F+(1|ID.F), REML=FALSE)'.

#-----Starved treatment

MF.S<-lmer(RT.Ln.S~OCC.S+(1+OCC.S|ID.S), REML=FALSE)
summary(MF.S)
NM1.S<-lmer(RT.Ln.S~1+(1+OCC.S|ID.S), REML=FALSE)
summary(NM1.S)
anova(MF.S, NM1.S)
#ANOVA MF.S vs NM1.S is significant so OCC.S is a significant factor and MF.S has the lowest AIC

NM2.S<-lmer(RT.Ln.S~OCC.S+(1|ID.S)+(1|OCC.S), REML=FALSE)
summary(NM2.S)
anova(MF.S, NM2.S)
#Close but no significant difference between the two (p=0.082) and MF.S has the lowest AIC value

NM3.S<-lmer(RT.Ln.S~OCC.S+(1|ID.S), REML=FALSE)
summary(NM3.S)
anova(MF.S, NM3.S)
# ANOVA MF.S vs NM3.S, No significant difference but NM3.F has the new lowest AIC value (261.66)

NM4.S<-lmer(RT.Ln.S~OCC.S+(1|OCC.S), REML=FALSE)
summary(NM4.S)
anova(NM2.S, NM4.S)
# ANOVA MF.S vs NM4.F is significant. ANOVA NM2.S vs NM4.S significant so '(1|ID)' is a significant
# factor. NM4.S has an AIC value of 265.97

# NM3.S has lowest AIC value (261.66)- 'NM3.S<-lmer(RT.Ln.S~OCC.S+(1|ID.S), REML=FALSE)'.
# This is the same model as the fed treatment so there is no problem of discrepencies
# between the models.

#----------------------------Residules form seperate treatment models--------------------------

NM3.F<-lmer(RT.Ln.F~OCC.F+(1|ID.F), REML=FALSE)
summary(NM3.F)
ranef(NM3.F)
resid(NM3.F)

NM3.S<-lmer(RT.Ln.S~OCC.S+(1|ID.S), REML=FALSE)
summary(NM3.S)
ranef(NM3.S)
resid(NM3.S)

#----------------------------------F test comparing variances----------------------------------

IIVData<-read.csv("riSD for each treamtnet.csv")
names(IIVData)
IIV.F<-(IIVData$F)
IIV.S<-(IIVData$S)

var.test(IIV.F, IIV.S)
t.test(IIV.F, IIV.S, paired=TRUE)
