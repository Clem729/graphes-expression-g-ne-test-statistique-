
###Pour charger la table### 
setwd("....")

data1<-read.table("TABLE_GENE", header = TRUE, sep=";")
summary(data1)

data1


########GRAPHIQUES#######

#les packages nécessaires 
library(ggplot2)
library(dplyr)
data1<-na.omit(data1)

#graph en barre de tous les gènes 
ggplot(data1, aes(x=Condition, y=Expression, fill=Condition)) + 
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.75)) +
  facet_wrap(~Gene, scale="free")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.4))

#découper la table par gène
IL8<- subset(data1, Gene=="IL8")
il1b<- subset(data1, Gene=="IL1b")
tnfa<- subset(data1, Gene=="TNFa")

#boite à moustache par gène 
ggplot(data=IL8, aes(x=Condition, y=Expression, fill=Condition)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.75), dotsize = 0.7) +
  scale_fill_manual(values=c("orangered","orangered4","powderblue"))+
  labs(title="IL8_T8", 
       x="", y = "Relative Expression Level")
theme_bw()

#nuage de point 
ggplot(il1b, aes(Condition, Expression, group=SampleName)) +
  geom_point(aes(shape=SampleName, color=Condition), size=2)+
  scale_shape_manual(values=c(1,2,3,4,5, 6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27))
scale_color_manual(values=c("lightsalmon2","lightskyblue1","mediumpurple"))+
  theme(legend.position="top")+
  scale_x_discrete(limits=c("CTRL","Y250","Y1000"))


##########STATISTIQUES#######

G6PDH<- subset(data1, Gene=="G6PDH")
PFKLA<- subset(data1, Gene=="PFKLA")
PKM<- subset(data1, Gene=="PKM")
HIFAB1<- subset(data1, Gene=="HIFAB1")
HIFAB2<- subset(data1, Gene=="HIFAB2")
GLUT<- subset(data1, Gene=="GLUT1AB")
LDHAA<- subset(data1, Gene=="LDHAA")
LDHAB<- subset(data1, Gene=="LDHAB")
CPT1C<- subset(data1, Gene=="CPT1C")
CPT1D<- subset(data1, Gene=="CPT1D")

###packages pour stat
library(tidyverse)
library(ggpubr)
library(rstatix)
library (car)

###ANOVA####
#anova global type II
anova.test<-aov(Expression ~ Regime*Injection, data=HIFAB2)
summary(anova.test)

#test de normalité (doit être <0.05)
shapiro.test(residuals(anova.test))

#test homogeneité variance
leveneTest(Expression ~ Regime*Injection, data = HIFAB2)



#comparaison 2 à 2
pwc <- HIFAB2 %>% tukey_hsd(Expression ~ Regime*Injection)
pwc

#anova détaillée 
model <- lm(Expression ~ Regime*Injection, data=HIFAB2)
HIFAB2 %>%
  group_by(Regime) %>%
  anova_test(Expression ~ Injection, error = model)


###graph de diagnostic###
graph=lm(Expression ~Regime*Injection, data=HIFAB2)
par(mfrow=c(2,2))
plot(graph)
