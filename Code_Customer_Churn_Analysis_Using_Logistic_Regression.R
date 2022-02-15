##STRUTTURA DATASET##
##libraries##
library(dplyr) 
library(vcd)
library(ResourceSelection)
library(readr)
##lettura dati##
setwd("C:/Users/Utente/Desktop/UNI/Dati categoriali")
telecomdata <- read_csv("C:/Users/Utente/Desktop/UNI/Dati categoriali/telecomdata.csv")

##data overview##
glimpse(telecomdata) #capiamo la struttura del dataset e la natura delle variabili al suo interno 
#la varaibili contrassegnate da  <chr> dovranno essere convertite in factors

##DATASET PRE-PROCESSING##

#escludiamo preventivamente la variabili "customerID","Multiplelines","Onlinesecurity","Online backup","Device protection" "Seniocitizen", 
#"Contract","Paymentmethod" e "TotalCharges" poiché irrilevanti ai fini della nostra analisi.
telecomdata$customerID<-NULL
telecomdata$MultipleLines<-NULL
telecomdata$OnlineSecurity<-NULL
telecomdata$OnlineBackup<-NULL
telecomdata$DeviceProtection<-NULL
telecomdata$SeniorCitizen<-NULL
telecomdata$Contract<-NULL
telecomdata$PaymentMethod<-NULL
telecomdata$TotalCharges<-NULL

head(telecomdata) 

#verifichiamo la presenza di valori mancanti nel dataset 
sum(is.na(telecomdata))

#procediamo nella conversione delle variabili tramite la funzione as.factor
telecomdata$gender <-as.factor(telecomdata$gender) 
telecomdata$Churn <- as.factor(telecomdata$Churn)
telecomdata$Partner <- as.factor(telecomdata$Partner)
telecomdata$Dependents<- as.factor(telecomdata$Dependents)
telecomdata$PhoneService<- as.factor(telecomdata$PhoneService)
telecomdata$PaperlessBilling <- as.factor(telecomdata$PaperlessBilling)
#accorpiamo le risposte "No" e "No internet service" per le seguenti variabili 
telecomdata$InternetService <- as.factor(telecomdata$InternetService)
telecomdata$TechSupport <- factor( with(telecomdata, replace(TechSupport, TechSupport %in% c( "No", "No internet service"),"No") ) )
telecomdata$StreamingMovies<- factor( with(telecomdata, replace( StreamingMovies, StreamingMovies %in% c( "No", "No internet service"),"No") ) )
telecomdata$StreamingTV <- factor( with(telecomdata, replace( StreamingTV, StreamingTV %in% c( "No", "No internet service"),"No") ) )

#rinominiamo le variabili quantitative
permanenza <- telecomdata$tenure
spesamensile<-telecomdata$MonthlyCharges
spesamensile
str(telecomdata)

#rinominiamo le variabili categoriali, le risposte e riordiniamo le categorie (la prima sarà quella di riferimento)
disdetta <-factor(telecomdata$Churn, levels=c("Yes","No"), labels=c("si", "no"))
sesso <- factor(telecomdata$gender, levels=c("Female","Male"), labels=c("donne", "uomini"))
partner <- factor(telecomdata$Partner, levels=c("Yes","No"), labels=c("con", "single"))
personeacarico<-factor(telecomdata$Dependents, levels=c("Yes","No"), labels=c("con", "senza"))
lineatelefonica<-factor(telecomdata$PhoneService, levels=c("Yes","No"), labels=c("con l.t.", "senza l.t."))
internet<-factor(telecomdata$InternetService, levels=c("Fiber optic","DSL", "No"), labels=c("fibra", "adsl", "no"))
assistenzatecnica<-factor(telecomdata$TechSupport, levels=c("Yes","No"), labels=c("si", "no"))
streamingTV<-factor(telecomdata$StreamingTV, levels=c("Yes","No"), labels=c("si", "no"))
streamingfilm<-factor(telecomdata$StreamingMovies, levels=c("Yes","No"), labels=c("si", "no"))
pagamentoelettronico <- factor(telecomdata$PaperlessBilling, levels=c("Yes","No"), labels=c("si", "no"))

#apportiamo tali modifiche all'interno del dataset di partenza 
new.telecomdata<-data.frame(telecomdata[-1:-12],sesso,partner,personeacarico,permanenza,lineatelefonica,internet,assistenzatecnica,streamingTV,streamingfilm,pagamentoelettronico,spesamensile,disdetta)
str(new.telecomdata)

##ANALISI ESPLORATIVA##

#tabelle a doppia entrata
xtabs(~ personeacarico + disdetta, data=new.telecomdata)
xtabs(~ internet + disdetta, data=new.telecomdata)
xtabs(~ assistenzatecnica+disdetta, data=new.telecomdata)
xtabs(~ streamingfilm+disdetta, data=new.telecomdata)
xtabs(~ streamingTV+disdetta, data=new.telecomdata) #streamingfilm e streamingtv hanno frequenze molto simili rispetto la y
xtabs(~ pagamentoelettronico+disdetta, data=new.telecomdata)
xtabs(~ partner+disdetta, data=new.telecomdata)
xtabs(~ lineatelefonica+disdetta, data=telecomdata)
tabsex<-xtabs(~ sesso + disdetta, data=new.telecomdata)
tabsex#le frequenze sono distribuite in modo pressocché identico tra i due sessi

#indipendenza tra sesso e disdetta
oddsratio(tabsex, log=F) #or prossimo ad 1, sinonimo di indipendenzatra x e y

#dipendenza tra streamingtv e streamignfilm
tabstreaming<-xtabs(~ streamingTV+streamingfilm, data=new.telecomdata)
chisq.test(tabstreaming, correct = F)  #pvalue molto basso

##STIMA DEL MODELLO##

#stimiamo il modello completo di tutte le variabili
logistic1<- glm(disdetta~ sesso+partner+streamingTV+personeacarico+permanenza+lineatelefonica+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+spesamensile, family=binomial(link=logit))
summary(logistic1)#le variabili sesso, partner e linea telefonica risultano essere poco significative

#modello privo delle var sesso, partner e linea telefonica
logistic2<- glm(disdetta~ streamingTV+personeacarico+permanenza+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+spesamensile, family=binomial(link=logit))
summary(logistic2)
#modello con interazioni
logistic3<- glm(disdetta~ partner*lineatelefonica+streamingTV+personeacarico+permanenza*sesso+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+spesamensile, family=binomial(link=logit))
summary(logistic3)
#le variabili partner e linea telefonica risultano ancora prive di significatività rispetto all'obiettivo
#l'interazione sesso*permanenza sembra avere un effetto, in ogni caso, vista la natura del problema e della
#del valore dell'or precedentemente calcolato, rimuoviamo la var sesso.

#modello con le sole variabili categoriali
logistic4<- glm(disdetta~ streamingTV+personeacarico+internet+assistenzatecnica+streamingfilm+pagamentoelettronico, family=binomial(link=logit))
summary(logistic4)

#in virtu della relazione tra streamingtv e streamingfilm verificata in fase di analisi esplorativa consideriamo solo la variabile streaming film
#modello logisti4 senza la variabile streamingtv
logistic5<- update(logistic4, .~. -streamingTV, family=binomial (link=logit))
summary(logistic5)

#testiamo il modello privo dei servizi streaming (rimuoviamo anche streamingfilm)
logistic6<- update(logistic5, .~. -streamingfilm, family=binomial (link=logit))
summary(logistic6)
#testiamo il modello precedente privo del servizio internet
logistic7<- update(logistic6, .~. -internet, family=binomial (link=logit))
summary(logistic7)

anova(logistic7,logistic5, test= "Chisq") #il test anova dimostra che il modello contenente internet e streamingfilm si adatta meglio

#modello solo con variabili quantitative
logistic8<- glm(disdetta~ permanenza+spesamensile, family=binomial(link=logit))
summary(logistic8)

#modello con var categoriali residue e quantitative
logistic9<- glm(disdetta~ personeacarico+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+permanenza+spesamensile, family=binomial(link=logit))
summary(logistic9) #il regressore spesa mensile perde significatività

#modello con interazione personeacarico*spesamensile
logistic10<- glm(disdetta~ personeacarico*spesamensile+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+permanenza, family=binomial(link=logit))
summary(logistic10)  #rimuoviamo spesa mensile

#modello finale
logistic11<-glm(disdetta~ personeacarico+internet+assistenzatecnica+streamingfilm+pagamentoelettronico+permanenza, family=binomial(link=logit))
summary(logistic11)

#VALIDAZIONE MODELLO##

#tabella corretta classificazione
tabcorrclass <- table(disdetta,logistic11$fitted >0.5)
tabcorrclass
sum(diag(tabcorrclass)/sum(tabcorrclass))

##Test HL##
hl<-hoslem.test(logistic11$y, fitted(logistic11), g=10 )
hl                
cbind(hl$expected,hl$observed) 

##UTILIZZO DEL MODELLO##

##STIME PUNTUALI ED INTERVALLARI##
coef<-logistic11$coefficients
coef
exp(coef(logistic11))		#stime puntuali
s.e.<-summary(logistic11)$coefficients[,2];s.e. #standar error 
L<-coef-1.96*s.e.  #estremo inferiore i.c.
U<-coef+1.96*s.e.   #estremo superiore i.c.
CI<-cbind(L,U);CI
expCI<-exp(CI);expCI

##PROBABILITA##

#caso peggiore dopo un anno
prob1<-predict.glm(logistic11, type="response", newdata = data.frame(personeacarico="con",internet="no", assistenzatecnica="si",streamingfilm="no",pagamentoelettronico="no",permanenza=12), se=TRUE)
prob1
L1<-0.9560828-(1.96*0.005934783 );L1
U1<-0.9560828+(1.96*0.005934783 );U1

#caso peggiore dopo 2 anni
prob2<-predict.glm(logistic11, type="response", newdata = data.frame(personeacarico="con",internet="no", assistenzatecnica="si",streamingfilm="no",pagamentoelettronico="no",permanenza=24), se=TRUE)
prob2
L2<-0.9738419 -(1.96*0.003556078  );L2
U2<-0.9738419 +(1.96*0.003556078  );U2
#profilo young fibra dopo un anno (caso migliore)
prob3<-predict.glm(logistic11, type="response", newdata = data.frame(personeacarico="senza",internet="fibra", assistenzatecnica="no",streamingfilm="si",pagamentoelettronico="si",permanenza=12), se=TRUE)
prob3
L3<-0.2731791-(1.96*0.01363701);L3
U3<-0.2731791+(1.96*0.0136370);U3
#profilo young fibra dopo due anni (caso migliore)
prob4<-predict.glm(logistic11, type="response", newdata = data.frame(personeacarico="senza",internet="fibra", assistenzatecnica="no",streamingfilm="si",pagamentoelettronico="si",permanenza=24), se=TRUE)
prob4
L4<-0.3912645-(1.96*0.01465846);L4
U4<-0.3912645+(1.96*0.01465846);U4
#profilo young adsl, con assistenza tecnica e senza pagamento elettronico dopo un anno
prob5<-predict.glm(logistic11, type="response", newdata = data.frame(personeacarico="senza",internet="adsl", assistenzatecnica="si",streamingfilm="si",pagamentoelettronico="no",permanenza=12), se=TRUE)
prob5
L5<-0.7675702-(1.96*0.01897669);L5
U5<-0.7675702+(1.96*0.01897669);U5

##FINE##
