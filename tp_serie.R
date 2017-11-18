library('TSA')
library('tseries')

#################################################
#           TP1 SERIE CHRONO                    #
#################################################

#################################################
#                 QUESTION 1                     #
#################################################


######## importation et traitement primaire du jeu de donnees ###########
#importation du jeu de donnees
taux <- read.csv2("C:/Users/angag426/Desktop/TP_ACT2010/Taux_de_change_US_Euro.csv")
#taux <- read.csv2("C:/Users/Yanic/ulaval/Séries chronologiques/tp/Taux_de_change_US_Euro.csv")
rendement<-taux$US.Euro
anne.mois<-taux$Année.mois
#on transforme en time serie
ttaux<-ts(rendement,start=c(1999,1),end=c(2016,12),frequency = 12)

######### premiere analyse de la serie grace a son graphique et a sa fonction d'autocorrelation ##########
#on trace le graphique
win.graph(height=4,width=6)
plot(ttaux,ylab='taux de change', type='o', xlab='année', main='Taux de change ')
abline(h=1)

#on observe difficilement de stationnarité, 
#forte aurocorrelation, decroissance tres lente avec l'augmentation 
#on ne donc pas une fonction ARMA(p,1,q) ni ARMA(p,2,q)
acf(ttaux, main='autocorrélation')
pacf(ttaux, main='autocorrélation')

######### on verifie si une transformation est appropriee ######

BoxCox.ar(ttaux)
BoxCox.ar(ttaux)$mle
BoxCox.ar(ttaux)$ci
#logiquement et avec nos acquis de gestion du risque financier, on sait qu'il est plus logique d'observer le rendement
#d'un titre financier (ou d'un taux de change). C'est donc sans surprise qu'on trouve un lambda de 0 qui nous suggere d'utiliser
#le logarithme de notre serie chronologique comme transformation. On s'attend egalement a avoir a differencier une seul fois
#pour retrouve le rendement de notre taux de change
#le rendement permet de comparer deux produits fianciers ayant des valeurs differentes entres eux

plot(as.vector(log(ttaux)), type='o', ylab='logarithme du taux', cex=0.5)
#avec une premiere differenciation
plot(as.vector(diff(log(ttaux))), type='o', ylab='différencitation du logarithme')
#la premiere differenciation semble plutot stable
#premiere observation de l'autocorrelation et de l'autocorrelation partielle suite a la transformation
acf(log(ttaux))
#on observe encore assez bien la non stationnarite (forte autocorrelation, decroissance lente)
#ce qui est normal puisque notre transformation n'a pas pour but de stationnarise la serie
#mais bien de la?
pacf(log(ttaux))
#AR de degres 2 fortement suggere ce qu'on retrouve par apres dans le test ADF

######### tests de stationnarite ########
#test ADF pour la stationnarite
#on trouve d'abord la valeur de l'ordre AR(k) suggere par R
ar(log(ttaux))
#R suggere d'utiliser k=2
adf.test(log(ttaux), k=2)
#suggere la non stationnarite avec une p value de 93% (tres pres detre stationnaire)
#on tente une premiere differenciation
ar(diff(log(ttaux)))
#R suggere k=1
adf.test(diff(log(ttaux)),k=1)
#suggere treeees fortement la stationnarite avec une p value inferieur a 1%
#on garde la premiere differenciation

######## on cherche le modele de notre serie maintenant stationnaire ############

#autocorrelation et autocorrelation partielle
acf(diff(log(ttaux)))
#WTF IS A LAG DE 0.1 (echelle du graphique sortie)?
#On semble avoir un modele AR(1) pour la partie autoregressive
#On semble meme pouvoir ignorer la partie MA puisque l'autocorrelation est tres faible apres?
#(aller demander au prof pourquoi la conclusion de MA(0) a la page 88 du chap6 parce qu'on a ici la meme conclusion)
pacf(diff(log(ttaux)))
#On semble avoir un modele MA(1) pour la partie MA, est-ce possible de le retirer?

#qu'est ce que eacf en pense?

eacf(diff(log(ttaux)))
#eacf semble hesitant entre ARMA(0,1) et ARMA(1,1) pour la premiere diff du log
#on serait tenter de choisir un ARMA(0,1) ce qui corroborerait les test d'autocorrelation et d'autocorrelation partiel 
#on peut regarder la valeur de rho 0,2 du tableau (la position matricielle (1,2)) 
#pour voir si on aurait pu la remplacer par un x ou est-ce que la position matricielle (2,2) aurait pu etre un x?
rrr=eacf(diff(log(ttaux)))
rrr$eacf[1,2]


#le modele ARMA(0,1) corrobore la theorie du MFE, le rendement serait le rendment espere plus un bruit blanc
# Wt = W(t-1) + et (ou Wt = log(Yt))-log(Y(t-1) = log(Yt/Y(t-1)))

#Qu'est-ce qui BIC en pense?

plot(armasubsets(y=diff(log(ttaux)), nar=3, nma=3, y.name='test',ar.method='ols'))
#BIC propose donc également d'utiliser ARIMA(0,1,1) mais avec et-3 plutot que et-1? wtf?
#rendement semestriel, d'ou le t-3?

#ON CHOISI DONC UN MODELE ARIMA(0,1,1) pour le log de Yt (plus simple et ressemble a MFE)
#on va quand meme tester ARIMA(1,1,1)

####### estimation des parametres ###########

#pour ARIMA(0,1,1)
#on ne met pas la diff dans la fonction arima car cest specifier order=c(0,1,1)
ttaux011<-arima(log(ttaux), order=c(0,1,1), method='ML')
#intercepte nul a cause du log ou reellement nul?

#pour ARIMA(1,1,1)
ttaux111<-arima(log(ttaux), order=c(1,1,1), method='ML')

#pour le ARIMA(0,1,3) propose par BIC
#on fixe les coefficients avec l'argument fixe
ttaux013<-arima(diff(log(ttaux)), order=c(0,1,3), method='ML', fixe=c(0, 0, NA) )
####### etude des residus ###########

###ARIMA(0,1,1)

##graphique des residus
plot(rstandard(ttaux011), ylab='résidus standardisés', type='o')
abline(h=0)
#distance superieur a 2 frequente ce qui est anormale pour un bruit blanc normal
1-pnorm(2) #prob d'avoir un res superieur ou inferieur a 2

##test de normalite
#histogramme
hist(rstandard(ttaux011), xlab='résidus standardisés')
#graphique des quantiles (qq plot)
qqnorm(rstandard(ttaux011))
qqline(rstandard(ttaux011))
#test de shapiro wilk
shapiro.test(rstandard(ttaux011))
#la p-value ne permet pas de rejetter l'hypothese de normalite

##test d'independance
#run test
runs(residuals(ttaux011))$pvalue
#on ne rejette pas H0 a 5%, les residus sont indep
#autocorrelation des residus
acf(residuals(ttaux011))
#les residus ne semblent pas correle

###ARIMA(1,1,1)

##graphique des residus
plot(rstandard(ttaux111), ylab='résidus standardisés', type='o')
abline(h=0)
hist(rstandard(ttaux111), xlab='résidus standardisés')
qqnorm(rstandard(ttaux111))
qqline(rstandard(ttaux111))
shapiro.test(rstandard(ttaux111))
#la p-value ne permet pas de rejetter l'hypothese de normalite
runs(residuals(ttaux111))$pvalue
#on ne rejette pas H0 5%, les residus sont indep
acf(residuals(ttaux111))
#les residus ne semblent pas correle


###ARIMA(0,1,3)
plot(rstandard(ttaux013), ylab='résidus standardisés', type='o')
abline(h=0)
hist(rstandard(ttaux013), xlab='résidus standardisés')
qqnorm(rstandard(ttaux013))
qqline(rstandard(ttaux013))
shapiro.test(rstandard(ttaux013))
#la p-value ne permet pas de rejetter l'hypothese de stationnarite
runs(residuals(ttaux013))$pvalue
#on rejette pas H0 a 5%, les residus NE SONT PAS INDEP
acf(residuals(ttaux013))
#les residus presentent des anomalies
#ce modele ne semble pas approprie selon les 2 derniers tests


####### justesse du modele et surparametrisation ###########

###ARIMA(0,1,1)
#test de ljung-box pour l'importance relative de residus une fois additionnes
Box.test(residuals(ttaux011), lag=10, type="Ljung-Box", fitdf=1)
#selon cette p value, le modele est approprie
tsdiag(ttaux011, gof=15, omit.initial=F)
#toutes les p values sont superieur a 5% on accepte ce modele qui est plus simple que le prochain


###ARIMA(1,1,1)
Box.test(residuals(ttaux111), lag=10, type="Ljung-Box", fitdf=1)
#selon cette p value, le modele est approprie
tsdiag(ttaux111, gof=15, omit.initial=F)


###ARIMA(0,1,3)
Box.test(residuals(ttaux013), lag=10, type="Ljung-Box", fitdf=1)
#selon cette p value, le modele N'EST PAS APPROPRIE
#inutile de rajouter des graphiques sachant que le modele n'est pas approprie

###surparametrisation
## on test les modeles ARIMA(2,1,1), ARIMA(1,1,2) et ARIMA(0,1,2)
arima(log(ttaux), order=c(2,1,1), method='ML')
#2e coeffient tres pres de 0, on rejette
arima(log(ttaux), order=c(1,1,2), method='ML')
#theta2 significativement different de 0, aller plus loin avec ARIMA(1,1,2)?
arima(log(ttaux), order=c(0,1,2), method='ML')
#theta2 tres pres de 0, on rejette


#################################################
#                 QUESTION 2                     #
#################################################

######## importation et traitement primaire du jeu de donnees ###########
#importation du jeu de donnees
#saaq <- read.csv2("C:/Users/TEMP.ULAVAL/Desktop/nouveau/SAAQ-2015  .csv")
saaq <- read.csv2("C:/Users/Yanic/ulaval/Séries chronologiques/tp/SAAQ-2015.csv")
naadc<-ts(saaq$NAADC,start=saaq$Année[1],end=saaq$Année[length(saaq$Année)])
npa<-ts(saaq$NPA,start=saaq$Année[1],end=saaq$Année[length(saaq$Année)])
ndi<-ts(saaq$NDI,start=saaq$Année[1],end=saaq$Année[length(saaq$Année)])
cti<-ts(saaq$CTI,start=saaq$Année[1],end=saaq$Année[length(saaq$Année)])

#NAADC : Nombre d'accidents avec dommages corporels
#NPA : Nombre de personnes accidentés
#NDI : Nombre de demandes d'indemnités
#CTI : Coût total de l'indemnisation (en millions de dollars, et en dollars constants 2015)

#################################################
#                 NAADC                        #
#################################################

######### premiere analyse de la serie grace a son graphique et a sa fonction d'autocorrelation ##########
#on trace le graphique
win.graph(height=3.875,width=6, pointsize=8)
plot(naadc,ylab="nombres d'accidents")

#on observe difficilement de stationnarité, 
#L'autocorrelation décroisse rapidement signe de stationnarité 
#L'autocorrelation semble osciller tel un processus autorégressif
#Le pacf semble opter pour un processus AR(1)
acf(naadc, main='autocorrélation NAADC')
pacf(naadc, main='autocorrélation NAADC')

######### on verifie si une transformation est appropriee ######

BoxCox.ar(naadc)
BoxCox.ar(naadc)$mle
BoxCox.ar(naadc)$ci
#boxcox nous propose d'utiliser l'inverse du carre... pourquoi logiquement on voudrait
#analyser avec cette transformation dans le cas d'un nombre d'accident?
#pourquoi l'intervalle ne va pas plus pas que -2?

plot(as.vector(naadc^(-2)), type='o')
#dure a dire pour la stationnarite, les valeurs sont minusciles
acf(naadc^(-2))
pacf(naadc^(-2))
#AR de degree 1 fortement suggere

######### tests de stationnarite ########

#test ADF pour la stationnarite
#on trouve d'abord la valeur de l'ordre AR(k) suggere par R
ar(naadc^(-2))
#R suggere d'utiliser k=2
adf.test(naadc^(-2), k=2)
#suggere la stationnarite avec p value de 48%

######## on cherche le modele de notre serie maintenant stationnaire ############

#autocorrelation et autocorrelation partielle
acf(naadc^(-2))
pacf(naadc^(-2))
#resemble enormement a AR(1)

#qu'est ce que eacf en pense?

eacf(naadc)
#eacf ne fonctionne pas... pourquoi, deja stationnaire donc fonctionne pas?

#ON CHOISI DONC UN MODELE AR(1) pour le Yt^(-2)

####### estimation des parametres ###########

#voir la page 4 du chapitre 7 pour les explications
#estimateur de phi1 avec d'autres estimateurs
phi1<-acf(naadc^(-2))$acf[1]
#estimateur de la variance du bruit blanc
lam0<-var(naadc^(-2))
naadc_sige2<-(1-phi1^2)*lam0
naadc_sige2
