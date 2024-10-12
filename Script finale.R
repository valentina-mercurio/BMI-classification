rm(list=ls())
######### Caricamento Data Set######
library(readxl)
DataSet <- read_excel("Dati Analisi Madeo Gerardo e Valentina Mercurio.xlsx")
str(DataSet)
names(DataSet)
#View(DataSet)


######### Imputazione Valori Mancanti #########
# lavoro su una copia del ds
DataSetCopy <- DataSet
colnames(DataSetCopy)<-colnames(DataSet)

# unità n. 82
MeanBMI<-round(mean(DataSetCopy$`BMI (kg/m2)`, na.rm=TRUE) ,2)
DataSetCopy$`BMI (kg/m2)`[82]<-MeanBMI
MeanVES<-round(mean(DataSetCopy$VES, na.rm=TRUE) ,2)
DataSetCopy$VES[82]<-MeanVES

# unità n. 2
DataSetCopy$VES[2]<-MeanVES

# unità n. 48
DataSetCopy$VES[48]<-MeanVES

# unità n. 61
DataSetCopy$VES[61]<-MeanVES

# unità n. 25
DataSetCopy$VES[25]<-MeanVES

# unità n. 43
MeanIns<-round(mean(DataSetCopy$Ins, na.rm=TRUE) ,2)
DataSetCopy$Ins[43]<-MeanIns

# unità n. 20
MeanLDL<-round(mean(DataSetCopy$LDL, na.rm=TRUE) ,2)
MeanTrigliceridi<-round(mean(DataSetCopy$Trigliceridi, na.rm=TRUE) ,2)
MeanCreatin<-round(mean(DataSetCopy$Creatin, na.rm=TRUE) ,2)
MeanGOT<-round(mean(DataSetCopy$GOT, na.rm=TRUE) ,2)
DataSetCopy$LDL[19]<-MeanLDL
DataSetCopy$Trigliceridi[19]<-MeanTrigliceridi
DataSetCopy$Creatin[19]<-MeanCreatin
DataSetCopy$GOT[19]<-MeanGOT
DataSetCopy$Ins[19]<-MeanIns
DataSetCopy$VES[19]<-MeanVES

# variabile sesso
table(DataSet$SESSO)
DataSetCopy$SESSO[9]<-"M"
DataSetCopy$SESSO[102]<-"M"

# eliminazione unità con molti missing
IndiciDaEliminre<-c(11,87,234,236,237,242,244,245,246,247,251,252,256,257,258,259,261,262,263,309,351,352)
DataSetCopy<-DataSetCopy[-IndiciDaEliminre,]
dim(DataSetCopy)

######### Codifica variabile BMI come categoriale #############

# ricodifico BMI in variabile categoriale
# Grave Magrezza
DataSetCopy$BMI[DataSetCopy$`BMI (kg/m2)`<16] = "Grave Magrezza"

# Sottopeso
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`<=18.49 & DataSetCopy$`BMI (kg/m2)`>=16] = "Sottopeso"

# Normopeso
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`<=24.99 & DataSetCopy$`BMI (kg/m2)`>=18.5] = "Normopeso"

# Sovrappeso
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`<=29.9 & DataSetCopy$`BMI (kg/m2)`>24.99] = "Sovrappeso"

# Obesita 1 Grado
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`<=34.99 &  DataSetCopy$`BMI (kg/m2)`>=30] = "Obesità 1 Grado"

# Obesita 2 Grado
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`<=39.99 & DataSetCopy$`BMI (kg/m2)`>=35 ] = "Obesità 2 Grado"

# Obesita 3 Grado
DataSetCopy$`BMI`[DataSetCopy$`BMI (kg/m2)`>=40] = "Obesità 3 Grado"

# trasformo la nuova variabile in un fattore ordinato
DataSetCopy$`BMI` <- factor(DataSetCopy$`BMI`)
DataSetCopy$`BMI`<- ordered(DataSetCopy$`BMI`, 
                            levels=c("Grave Magrezza","Sottopeso", "Normopeso", "Sovrappeso", "Obesità 1 Grado", "Obesità 2 Grado", "Obesità 3 Grado"))
attach(DataSetCopy)


######### Ricodifica Variabile KIDMED ######
# ricodifica KIDMED
# Poca Aderenza
DataSetCopy$AdKIDMED[DataSetCopy$KIDMED <= 3] = "Povera"

# Media Aderenza
DataSetCopy$AdKIDMED[DataSetCopy$KIDMED > 3 & DataSetCopy$KIDMED <8] = "Media"

# Alta Aderenza
DataSetCopy$AdKIDMED[DataSetCopy$KIDMED >= 8 ]  = "Ottimale"


# trasformo la nuova variabile in un fattore ordinato
DataSetCopy$AdKIDMED <- factor(DataSetCopy$AdKIDMED)
DataSetCopy$AdKIDMED<- ordered(DataSetCopy$AdKIDMED, 
                               levels=c("Povera", "Media", "Ottimale"))
######### Modifica set di dati #####

DataSetCopy <- with(DataSetCopy, {
    Sesso<-ifelse(SESSO=="F", 1, 0)
    data.frame(Sesso, Età, DataSetCopy$`CIRCONFERENZA CRANIO (cm)`,
               DataSetCopy$`CIRCONFERENZA BRACCIO (cm)`, DataSetCopy$`CIRCONFERENZA VITA (cm)`,
               DataSetCopy$`CIRCONFERENZA ADDOMINALE (cm)`,DataSetCopy$`CIRCONFERENZA FIANCHI (cm)`, 
               DataSetCopy$`CIRCONFERENZA POLSO (cm)`, DataSetCopy$`RADICE COSCIA (cm)`, 
               DataSetCopy$WHR,DataSetCopy$`Col. Tot.`,DataSetCopy$LDL, 
               DataSetCopy$HDL, DataSetCopy$Trigliceridi, DataSetCopy$Ac.Urico, 
               DataSetCopy$Creatin,DataSetCopy$GOT, DataSetCopy$GPT,DataSetCopy$`Bil Tot.`,
               DataSetCopy$`Bil Dir.`,DataSetCopy$Ca,DataSetCopy$Fe,DataSetCopy$Urea,DataSetCopy$P,
               DataSetCopy$Ferritina, DataSetCopy$Ins, DataSetCopy$VES, DataSetCopy$PCR,
               DataSetCopy$PhA, DataSetCopy$TBW, DataSetCopy$`FFM (%)`, DataSetCopy$`FM (%)`,
               DataSetCopy$`BCM (%)`, DataSetCopy$AdKIDMED, DataSetCopy$BMI)})

colnames(DataSetCopy)<-c("Sesso","Età","Circonferenza_Cranio", "Circonferenza_Braccio",
                         "Circonferenza_Vita","Circonferenza_Addominale","Circonferenza_Fianchi",
                         "Circonferenza_Polso","Radice_Coscia","WHR","Col_Tot","LDL","HDL",
                         "Trigliceridi","Ac_Urico","Creatin","GOT","GPT","Bil_Tot","Bil_Dir","Ca",
                         
                         "Fe","Urea","P","Ferritina","Ins","VES","PCR","PhA","TBW","FFM","FM","BCM",
                         "AdKIDMED","BMI")

#head(DataSetCopy)
#str(DataSetCopy)
attach(DataSetCopy)
#View(DataSetCopy)


######### Analisi Descrittiva ####
library(lessR)
par(mfrow=c(1,1))
Sesso <- factor(Sesso)
cat <- data.frame(Sesso)
cols <-  hcl.colors(length(levels(Sesso)), "Fall")
PieChart(Sesso, data = DataSetCopy, hole = 0,
         fill = cols,
         labels_cex = 0.8,main="Grafico a torta \nVariabile: Sesso")
legend("topright", c("Uomo","Donna"), cex=0.8, fill=cols)
cat1 <- data.frame(BMI)
cols1 <-  hcl.colors(length(levels(BMI)), "Set 2")
PieChart(BMI, data = DataSetCopy, hole = 0,
         fill = cols1,
         labels_cex = 0.8, main="Grafico a torta \nVariabile: BMI")


######### Presenza multicollinearità ############
panel.corrgram <- function(x, y, z, subscripts, at, level = 0.9, label = FALSE, ...) {
    #require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]
    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z)) {
        ell <- ellipse(z[i], level = level, npoints = 50, scale = c(.2, .2)
                       centre = c(x[i], y[i]))
        panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
    }
    if (label) {
        panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
                   col = ifelse(z < 0, "white", "black"))
    }
}
CORDS<-cor(DataSetCopy[,1:33])

#View(DataSetCopy)
library(lattice);library(ellipse)
print(levelplot(CORDS, at = do.breaks(c(-1.01, 1.01), 20), 
                xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
                scales = list(x = list(rot = 90)) , panel = panel.corrgram, 
                label = TRUE ,aspect = "fill",main ="Tabella di correlazione"))

# Calcolo determinante matrice dei predittori

x<-as.matrix(DataSetCopy[,1:33])
xt<-t(x)
matr<-xt %*% x
Deter<-det(matr)
Deter

# Condiction number e min autovalore
X<-as.matrix(cbind(rep(1,nrow(DataSetCopy[,1:33])), DataSetCopy[,1:33]))
autval<-eigen(t(X) %*% X)
MinAut<-min(autval$values)
MaxAut<-max(autval$values)
condition_number<-sqrt(max(autval$values)/min(autval$values))
condition_number


Multicoll<-cbind(Deter, condition_number, min(autval$value))
colnames(Multicoll)<-c("Determinante", "Condition Number","Min Autovalore")
Multicoll

######### ACP #############

library(factoextra)
PCA1 <- princomp(DataSetCopy[,3:10], cor=TRUE)
# Quota di Variabilità spiegata
fviz_eig(PCA1, 
         addlabels = TRUE, ncp=8, 
         ylim = c(0, 70),
         main="Quota Varianza spiegata ")+
    geom_hline(yintercept=12, 
               linetype="dashed", 
               color = "red")

#screeplot(PCA1, type = 'l')
# SCREEPLOT E REGOLA DI KAISER
fviz_eig((PCA1), 
         addlabels = TRUE, 
         choice="eigenvalue", ncp = 8,
         ylim = c(0, 5),
         main="Screeplot") +
    geom_hline(yintercept=1, 
               linetype="dashed", 
               color = "red")

# ?fviz_contrib
fviz_contrib(PCA1, choice = "var", axes = 1)
loadDim1<-as.matrix(PCA1$loadings[,1])
loadDim1
fviz_contrib(PCA1, choice = "var", axes = 2)
loadDim2<-as.matrix(PCA1$loadings[,2])
loadDim2
CompScore<-PCA1$scores[,1:2]

######### Eliminazione variabili da Data Set####

EffComplAllenamento<-(DataSetCopy$BCM*DataSetCopy$PhA) 
names(DataSetCopy)
dataset<-as.data.frame(cbind(DataSetCopy[,1:2], CompScore, DataSetCopy[,11:28], EffComplAllenamento, DataSetCopy[,30:31],DataSetCopy[,34:35]))

#names(dataset)
#View(dataset)
#dim(dataset)

corDATA<-cor(dataset[,1:25])
library(lattice);library(ellipse)
print(levelplot(corDATA, at = do.breaks(c(-1.01, 1.01), 20), 
                xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
                scales = list(x = list(rot = 90)) , panel = panel.corrgram, 
                label = TRUE ,aspect = "fill",main ="Tabella di correlazione"))

######### Costruzione del Modello ########

dataset$Normopeso[BMI=="Normopeso"]=1
dataset$Normopeso[BMI!="Normopeso"]=0
attach(dataset)
modello_completo<-glm(Normopeso ~ Sesso + Età + Comp.1 + Comp.2 + LDL + HDL + Trigliceridi + Ac_Urico + Creatin + GOT + GPT + Bil_Tot + Bil_Dir + Ca + Fe + Urea + P + Ferritina + Ins + VES + PCR + EffComplAllenamento + FFM + AdKIDMED, family=binomial(link=logit))

summary(modello_completo)

#Prove

modello_1<-glm(Normopeso ~ Sesso + Età + Comp.1 + Comp.2 + LDL + HDL + Trigliceridi + Ac_Urico + Creatin + GOT + GPT + Bil_Tot + Bil_Dir + Ca + Fe + Urea + P + Ferritina + Ins + VES + PCR + EffComplAllenamento + FFM + AdKIDMED+Sesso*Età, family=binomial(link=logit))

summary(modello_1)

modello_2<-glm(Normopeso ~ Comp.1+Comp.2, family=binomial(link=logit))

summary(modello_2)

modello_3<-glm(Normopeso ~ Comp.1+Comp.2+Comp.2*Sesso, family=binomial(link=logit))

summary(modello_3)
library(MASS)
modello_selezionato <- stepAIC(modello_completo, direction = "backward")

summary(modello_selezionato)

modello_finale<-glm(Normopeso~LDL+GOT+Comp.1+Comp.2+Ac_Urico+Ins, family=binomial(link=logit))

summary(modello_finale)


#confronto con il modello completo 
#test anova 
anova(modello_finale, modello_completo, test="Chisq")


#si stimano le probabilità
Fitted_modello_finale<-fitted(modello_finale)
Fitted_modello_finale

######### Validazione del modello #######
#tabella di classificazione per valutare l'accuratezza del modello 
tab <- table(Normopeso, modello_finale$fitted >.5)
sum(diag(tab))/sum(tab)
tab
library(caret)
pdata <- predict(modello_finale,  type = "response")
data = as.numeric(pdata>0.5)
confusionMatrix(data = as.factor(data), reference = as.factor(Normopeso))
#ROC per valutare l'accuratezza del modello
pred<-prediction(modello_finale$fitted, Normopeso)
library(glmnet)
perf<-performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE, main="ROC Curve", xlab="1-Specificity", ylab="Sensitivity")
abline(0,1)
performance(pred, "auc")@y.values

######### Interpretazione dei coefficienti ##########

# Intervalli di Confidenza

# Coefficiente LDL

LDL_U <- -0.016362+1.96*0.006876 ; LDL_U   # -0.00288504
LDL_L <- -0.016362-1.96*0.006876 ; LDL_L   # -0.02983896
exp_LDL<-c(exp(LDL_L), exp(LDL_U))
exp_LDL # 0.9706018 0.9971191

# Coefficiente GOT
GOT_U <- -0.037650+1.96*0.018219 ; GOT_U   # -0.00194076
GOT_L <- -0.037650-1.96*0.018219 ; GOT_L   # -0.07335924
exp_GOT<-c(exp(GOT_L), exp(GOT_U))
exp_GOT # 0.9292669 0.9980611

# Coefficiente Comp.1
Comp1_U <- -0.290971+1.96*0.080051 ; Comp1_U #-0.134071
Comp1_L <- -0.290971-1.96*0.2351 ; Comp1_L   #-0.751767
exp_Comp1<-c(exp(Comp1_L), exp(Comp1_U))
exp_Comp1 # 0.4715326 0.8745279


# Coefficiente Comp.2
Comp2_U <- 0.503444+1.96*0.158105 ; Comp2_U  # 0.8133298
Comp2_L <- 0.503444-1.96*0.158105 ; Comp2_L  # 0.1935582
exp_Comp2<-c(exp(Comp2_L), exp(Comp2_U))
exp_Comp2 # 1.213560 2.255406

# Coefficiente Ac. Urico
Ac_Urico_U <- -0.513628+1.96*0.148985 ; Ac_Urico_U  # -0.2216174
Ac_Urico_L <- -0.513628-1.96*0.148985 ; Ac_Urico_L  # -0.8056386
exp_Ac_Urico<-c(exp(Ac_Urico_L), exp(Ac_Urico_U))
exp_Ac_Urico # 0.4468025 0.8012219

# Coefficiente Ins
Ins_U <- -0.124111+1.96*0.027583 ; Ins_U  # -0.07004832
Ins_L <- -0.124111-1.96*0.027583 ;Ins_L   # -0.1781737
exp_Ins<-c(exp(Ins_L), exp(Ins_U))
exp_Ins # 0.8367971 0.9323488
IntConf<-rbind(exp_LDL,exp_GOT,exp_Comp1,exp_Comp2,exp_Ac_Urico, exp_Ins)
rownames(IntConf)<-c("LDL","GOT","Comp1","Comp2","Ac. Urico","Ins")
colnames(IntConf)<-c("Estr. Inf.","Estr. Sup.")
IntConf
Coeff<-exp((modello_finale$coefficients))
Outpu_Exp<-cbind(IntConf, Coeff[2:7])
colnames(Outpu_Exp)<-c("Estr. Inf.","Estr. Sup.", "OR")
Outpu_Exp
