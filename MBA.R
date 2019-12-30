#Author: ZhuZheng(Iverson) ZHOU
#z-ing.net


rm(list = ls())
#################Initial setup#########################################
getwd()  #[1] "C:/Users/ZhuZheng ZHOU/Documents"

setwd("C:/Users/Zing/Dropbox/R")
#install.packages("lavaan")
#install.packages("sem")
#install.packages("ellipse")
#install.packages("mi")
#install.packages("VIM")
#install.packages("mice")
library(lavaan)
library(sem)
library(mi)
library(mice)
library(miceadds)
library(VIM)
library(plyr)
library(psych)
library(GPArotation)
library(lavaan)
library(semPlot)
library(psych)



survey <- read.csv("https://dl.dropboxusercontent.com/s/y251ebjal1bnxwl/results-survey974914_Qs.csv", as.is=T, header=T,colClasses="character")


#survey <- read.csv("https://dl.dropboxusercontent.com/s/fvj6ovzjwhwz3i6/results-survey974914_24Jan2015.csv",
                   as.is=T, header=T,colClasses="character")


class(survey)
dim(survey)
names(survey)
summary(survey[,c(63)])  #summary(survey)
survey1=survey[,-c(66:97)]

for(i in seq_along(survey1)[9:65]) {
  survey1[,i][survey1[,i]=="trongly agree"]<-"Strongly agree"
}
names(survey1)

count((survey1)[,10])
count((survey1)[,11])
count((survey1)[c(11,12,13)])
count((survey1)[,11])
count((survey1)[,14])
#######################################################################
#x=survey[,c(65)]
#mylist[vapply(x, Negate(is.null), NA)]
#rm(survey2)

##################Writting a function to recode survey responses into numeric values programatically########################
length(survey1)#check length of the dataset
survey2=survey1 #make a copy off the previous dataset
for(i in seq_along(survey2)[9:65]) {
  #survey2[,i][survey2[,i]==""]<-'NA'
  survey2[,i][survey2[,i]==""]<-NA
  survey2[,i][survey2[,i]=="Strongly disagree"]<-1
  survey2[,i][survey2[,i]=="Disagree"]<-2
  survey2[,i][survey2[,i]=="Neither agree nor disagree"]<-3
  survey2[,i][survey2[,i]=="Agree"]<-4
  survey2[,i][survey2[,i]=="Strongly agree"]<-5
}



#check the recoded data
table(survey1[,c(63)])
table(survey2[,c(63)])
table(survey1[,c(62)])
table(survey2[,c(62)])
table(survey1[,c(9)])
table(survey2[,c(9)])
##############################################################################


##################Remove missing data########################

#check for columns and samples (rows) for the % of the data that is missing (just for the survey variables not system variables)
#survey3=survey2[,c(1,9:51)]
#names(survey3)
DataMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(survey2[,9:51],2,DataMiss)
apply(survey2[,9:51],1,DataMiss)
#apply(survey3[,2:44],1,DataMiss)

#programatically remove observation with more than 70% missing data(survey questions not system variables)
survey3<-survey2[!(apply(survey2[,9:51],1,DataMiss)>70),]

survey4=survey3[,c(1,9:51)]
names(survey4)

dim(survey2)
dim(survey4)


apply(survey4,2,DataMiss)



md.pattern(survey4)
aggr_plot <-aggr(survey4, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                 labels=names(survey4), srt=90, cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))


#marginplot(survey4[, c(26,39)]
#c("Evaluate.the.following.statements..My.team.transforms.individual.knowledge.to.shared.knowledge.","Evaluate.the.following.statements..Members.of.my.team.regularly.share.knowledge.with.other.teams.") ]#
#, col = mdc(1:2), cex = 1.2,cex.lab = 1.2, cex.numbers = 1.3, pch = 19)





##################Imputation########################

#set.seed(1000)
ncol(survey4)
sapply(survey4, mode)
survey4[, 2:44] <- sapply(survey4[, 2:44],as.numeric) #as.factor)


#Model 1: Two-way imputation method of Sijtsma and van der Ark (
set.seed(765)
dat.imp <- tw.imputation(survey4[, 2:44] )
dat.imp[ 103:105,]
survey4[103:105,]
#Model 2: Two-way imputation method using MCMC
dat.imp <- tw.mcmc.imputation(survey4[, 2:44] , iter=5,integer = TRUE)
dat.imp[ 103:105,]

sapply(dat.imp, mode)


#check normaility#
shapiro.test(dat.imp)



##################EFA########################
#corrlation

#parallel analysis
parallel=fa.parallel(dat.imp,fm="ml",fa="fa")
#parallel is five
#eigenvalues(kaiser)
parallel$fa.values
#over 1 =5
#over 0.7=7


#simple structure
fivefactor=fa(dat.imp,nfactors=5,rotate='oblimin',fm='ml')

#check facor loading (0.3 can be negative)
fivefactor

#remove factor load below 0.3 or loaded to two factors
fivefactor2=fa(dat.imp[,-c(2,5,10,15,23,25,34)],nfactors=5,rotate='oblimin',fm='ml')
fivefactor2 #simple structure

#check model adequeacy 
#rmsr 0.05 
#RMSEA .053
#TLI .887 ok
#CFI .9245735

1-((fivefactor2$STATISTIC-fivefactor2$dof)/(fivefactor2$null.chisq-fivefactor2$dof))





#check realiability
#factor4=1,2,3,8,33,34,35,36
#factor1=4,5,6,7
#factor3=26,27,28
#factor2=10,17,29,30,31,32
#factor5=9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,



alpha((dat.imp[, c(4,5,6,7)]))
alpha((dat.imp[, c(10,17,29,30,31,32)]))

alpha((dat.imp[, c(26,27,28)]))
alpha((dat.imp[, c(1,2,3,8,33,34,35,36)]))
alpha((dat.imp[, c(9,11,12,13,14,15,16,18,19,20,21,22,23,24,25)]))



##################Confirmatory Factor Analysis########################
#Knowledge Creation and Sharing(a1.SQ004+a1.SQ005+a1.SQ006+a1.SQ008+a1.SQ009)
#Trust (D01.SQ001,D01.SQ002,D01.SQ003,D01.SQ004)
#Peer-mentoring knowledge and skills (F01.SQ006,F01.SQ009,F01.SQ010,F01.SQ014,F01.SQ016)
#Internalisation(J01.SQ001,J01.SQ002,J01.SQ003,J01.SQ004,J01.SQ005)
#Socialisation(B02.SQ001,B02.SQ002,B02.SQ003,B02.SQ004,B02.SQ005,B02.SQ006)
#Combination(E01.SQ001,E01.SQ002,E01.SQ003,E01.SQ004,E01.SQ005,E01.SQ006)
#Hard Rewards(G01.SQ001,G01.SQ002,G01.SQ003,G01.SQ004)
#Soft rewards(H01.SQ001,H01.SQ002,H01.SQ003,H01.SQ004)
#IT support for Knowledge Management(C01.SQ001],C01.SQ002,C01.SQ003,C01.SQ004)



hs.model='
KC_S =~ a1SQ004+a1SQ005+a1SQ006+a1SQ008+a1SQ009
trust =~ D01SQ001+D01SQ002+D01SQ003+D01SQ004
pmen =~ F01SQ006+F01SQ009+F01SQ010+F01SQ014+F01SQ016
intern =~ J01SQ001+J01SQ002+J01SQ003+J01SQ004+J01SQ005
Soci =~ B02SQ001+B02SQ002+B02SQ003+B02SQ004+B02SQ005+B02SQ006
Comb =~ E01SQ001+E01SQ002+E01SQ003+E01SQ004+E01SQ005+E01SQ006
HRewards =~ G01SQ001+G01SQ002+G01SQ003+G01SQ004
SRewards =~ H01SQ001+H01SQ002+H01SQ003+H01SQ004
IT =~ C01SQ001+C01SQ002+C01SQ003+C01SQ004
'


  
datacov<-cov(dat.imp)


cfa<-specify.model()
KC_S->a1SQ004,KC_S0
KC_S->a1SQ005,KC_S1
  KC_S->a1SQ006,KC_S2
  KC_S->a1SQ008,KC_S3
  KC_S->a1SQ009,KC_S4
trust->D01SQ001,trust0
trust->D01SQ002,trust1
trust->D01SQ003,trust2
trust->D01SQ004,trust3
pmen->F01SQ006,peermentor0
pmen->F01SQ009,peermentor1
pmen->F01SQ010,peermentor2
pmen->F01SQ014,peermentor3
pmen->F01SQ016,peermentor4
intern->J01SQ001,intern0
intern->J01SQ002,intern1
intern->J01SQ003,intern2
intern->J01SQ004,intern3
intern->J01SQ005,intern4
Soci ->B02SQ001,Soci0
Soci ->B02SQ002,Soci1
Soci ->B02SQ003,Soci2
Soci ->B02SQ004,Soci3
Soci ->B02SQ005,Soci4
Soci ->B02SQ006,Soci5
Comb ->E01SQ001,Comb0
Comb ->E01SQ002,Comb1
Comb ->E01SQ003,Comb2
Comb ->E01SQ004,Comb3
Comb ->E01SQ005,Comb4
Comb ->E01SQ006,Comb5
HRewards ->G01SQ001, HRewards0
HRewards ->G01SQ002,HRewards1
HRewards ->G01SQ003,HRewards2
HRewards ->G01SQ004,HRewards3
SRewards ->H01SQ001, SRewards0
SRewards ->H01SQ002,SRewards1
SRewards ->H01SQ003,SRewards2
SRewards ->H01SQ004,SRewards3
IT ->C01SQ001, IT0
IT  ->C01SQ002,IT1
IT ->C01SQ003,IT2
IT  ->C01SQ004,IT3
KC_S <-> KC_S,NA,1
trust <-> trust,NA,1
pmen <-> pmen,NA,1
intern <-> intern,NA,1
Soci <-> Soci,NA,1
Comb <-> Comb,NA,1
HRewards <-> HRewards,NA,1
SRewards <-> SRewards,NA,1
IT <-> IT,NA,1
a1SQ004 <-> a1SQ004,error1
a1SQ005 <-> a1SQ005,error2
a1SQ006 <-> a1SQ006,error3
a1SQ008 <-> a1SQ008,error4
a1SQ009 <-> a1SQ009,error5
D01SQ001 <-> D01SQ001,error5
D01SQ002 <-> D01SQ002,error6
D01SQ003 <-> D01SQ003,error7
D01SQ004 <-> D01SQ004,error8
F01SQ006 <-> F01SQ006,error9
F01SQ009 <-> F01SQ009,error10
F01SQ010 <-> F01SQ010,error11
F01SQ014 <-> F01SQ014,error12
F01SQ016 <-> F01SQ016,error13
J01SQ001 <-> J01SQ001,error14
J01SQ002 <-> J01SQ002,error15
J01SQ003 <-> J01SQ003,error16
J01SQ004 <-> J01SQ004,error17
J01SQ005 <-> J01SQ005,error18
B02SQ001 <-> B02SQ001,error19
B02SQ002 <-> B02SQ002,error20
B02SQ003 <- >B02SQ003,error21
B02SQ004 <-> B02SQ004,error22
B02SQ005 <-> B02SQ005,error23
B02SQ006 <-> B02SQ006,error24
E01SQ001 <-> E01SQ001,error25
E01SQ002 <-> E01SQ002,error26
E01SQ003 <-> E01SQ003,error27
E01SQ004 <-> E01SQ004,error28
E01SQ005 <-> E01SQ005,error29
E01SQ006 <-> E01SQ006,error30
G01SQ001 <-> G01SQ001,error31
G01SQ002 <-> G01SQ002,error32
G01SQ003 <-> G01SQ003,error33
G01SQ004 <-> G01SQ004,error34
H01SQ001 <-> H01SQ001,error35
H01SQ002 <-> H01SQ002,error36
H01SQ003 <-> H01SQ003,error37
H01SQ004 <-> H01SQ004,error38
C01SQ001 <-> C01SQ001,error39
C01SQ002 <-> C01SQ002,error40
C01SQ003 <-> C01SQ003,error41
C01SQ004 <-> C01SQ004,error42
KC_S <-> trust, cov1
KC_S <-> pmen, cov2
KC_S <-> intern, cov3
KC_S <-> Soci, cov4
KC_S <-> Comb, cov5
KC_S <-> HRewards, cov6
KC_S <-> IT, cov7
trust <-> pmen,cov8
trust <-> intern,cov9
trust  <-> Soci,cov10
trust <-> Comb,cov11
trust <-> HRewards,cov12
trust <-> SRewards,cov13
trust <-> IT,cov14
pmen <-> intern,cov15
pmen <-> Soci,cov16
pmen <-> Comb,cov17
pmen <-> HRewards,cov18
pmen <-> SRewards,cov19
pmen <-> IT,cov20
intern <-> Soci,cov21
intern <-> Comb,cov22
intern <-> HRewards,cov23
intern <-> SRewards,cov24
intern <-> IT,cov25
Soci <-> Comb,cov26
Soci <-> HRewards,cov27
Soci <-> SRewards,cov28
Soci <-> IT,cov29
Comb <-> HRewards,cov30
Comb <-> SRewards,cov31
Comb <-> IT,cov32
HRewards <-> SRewards,cov33
HRewards <-> IT,cov34
SRewards <-> IT,cov35


cfa.constraint1 <-sem(cfa,datacov,nrow(dat.imp))
summary( cfa.constraint1,conf.level=.90,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", 
                                       "CFI", "RNI", "IFI", "SRMR"))


semPaths(cfa.constraint1,"std",title=FALSE)








library(corrplot) 
M <- cor(dat.imp[ ,c("a1SQ004","a1SQ005","a1SQ006","a1SQ008","a1SQ009")]) # get correlations
corrplot(M, method = "number",title='Items corrleation of latent variable Knowledge sharing and creation')

M3 <- cor(dat.imp[ ,c(10:14)]) # get correlations
corrplot(M3, method = "number",title='Items corrleation of latent variable peer mentoring')

M4 <- cor(dat.imp[ ,c(15:19)]) # get correlations
corrplot(M4, method = "number",title='Items corrleation of latent variable internalisation')


M5 <- cor(dat.imp[ ,c(20:25)]) # get correlations
corrplot(M5, method = "number",title='Items corrleation of socialisation')

M6 <- cor(dat.imp[ ,c(26:31)]) # get correlations
corrplot(M6, method = "number",title='Items corrleation of combination')

M7 <- cor(dat.imp[ ,c(32:35)]) # get correlations
corrplot(M7, method = "number",title='Items corrleation of hard rewards')

M8 <- cor(dat.imp[ ,c(36:39)]) # get correlations
corrplot(M8, method = "number",title='Items corrleation of soft rewards')

M9 <- cor(dat.imp[ ,c(40:43)]) # get correlations
corrplot(M9, method = "number",title='Items corrleation of IT')


M2 <- cor(dat.imp[ ,c(6:9)]) # get correlations
corrplot(M2, method = "number",title='Items corrleation of latent variable trust')


library(plsdepot)
xx = dat.imp[ ,c("a1SQ004","a1SQ005","a1SQ006","a1SQ008","a1SQ009")]
pls1 = plsreg1(xx, dat.imp[ ,c("a1SQ004")], comps = 3)
plot(pls1)

covariances <-datacov

correlations <- datacov
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa")
fa.diagram(fa.promax, simple=FALSE)


imp<-mice(survey4[, 2:44])
imp$imputations$survey4
summary(imp)



tempData <- mice(survey4,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)


#make impcor matrix
library(ellipse)
ctab <- cor(survey2)
round(ctab, 2)

head(survey2[,c(9:51)],10)

survet3 <-
  lapply(survey2[,c(9:51)],
         function(x) recode(x, recodes = "5:7=NA; 1=4; 2=3; 3=2; 4=1;",
                            as.factor.result = FALSE))
