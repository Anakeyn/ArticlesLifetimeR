##########################################################################
# Auteur : Pierre Rouarch 2019 - Licence GPL 3
# ArticleLifetimeR
# Duree de vie des articles "Marketing"
# Pour illustrer notre propos nous utiliserons le jeu de données de 
# l'association Networking-Morbihan 
##########################################################################
#Packages et bibliothèques utiles (décommenter au besoin)
##########################################################################
#install.packages("lubridate")  #si vous ne l'avez pas
#install.packages("tseries")
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")  #pour anomalyDetection de Twitter
#install.packages("XML")
#install.packages("stringi")
#install.packages("BSDA")
#install.packages("BBmisc")
#install.packages("stringi")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("rcorr")

#install.packages("lubridate")  #si vous ne l'avez pas
library (lubridate) #pour yday
#library(tseries) #pour ts
library(AnomalyDetection) #pour anomalydetectionVec
#library(XML) # pour xmlParse
#library(stringi) #pour stri_replace_all_fixed(x, " ", "")
library(BSDA)  #pour SIGN.test 
library(BBmisc) #pour which.first
#install.packages("stringi")
library(stringi) #pour stri_detect
#library(ggfortify)  #pour ploter autoplot type ggplot
#install.packages("tidyverse")  #si vous ne l'avez pas #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
#install.packages("forecast") #pour ma
#Chargement des bibliothèques utiles
library(tidyverse) #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
library(forecast)  #pour  arima, ma, tsclean

##########################################################################
# Récupération du Jeu de données nettoyé des pages et des articles
##########################################################################
dfPageViews <- read.csv("dfPageViews.csv", header=TRUE, sep=";") 
#str(dfPageViews) #verif
#transformation de la date en date :-)
dfPageViews$date <- as.Date(dfPageViews$date,format="%Y-%m-%d")
#str(dfPageViews) #verif
str(dfPageViews) #72821 obs
dfPageViews$index <- 1:nrow(dfPageViews)  #création d'un pour retrouver les "articles marketing" 
#ensuite
#pour les articles
myArticles <- read.csv("myArticles.csv", header=TRUE, sep=";") 
#transformation de la date en date :-)
myArticles$date <- as.Date(myArticles$date,format="%Y-%m-%d")
#str(myArticles) #verif


##########################################################################
# Calcul du "trafic de base" ie hors "articles marketing" 
# On va supprimer toutes les pages vues correspondantes aux articles 
# "Marketing" ainsi que toutes les pages vues dont l'entrée s'est faite 
# par un article "Marketing". on comparera  ensuite au traffic 
# Articles Marketing.
##########################################################################
#récupere les chemins des pages pour les comparer dans dfPageViews
myArticles$pagePath <- str_split_fixed(myArticles$link, "https://www.networking-morbihan.com", 2)[,2]
patternArticlesToRemove <- unique(myArticles$pagePath)

#Pour les pages de base on enleve les pagePath de nos articles
indexPagePathToRemove <- -grep(pattern = paste(patternArticlesToRemove, collapse="|"), dfPageViews$pagePath)
dfBasePageViews <- dfPageViews[indexPagePathToRemove,]
#puis on enleve les landingPagePath de nos articles
indexLandingPagePathToRemove <- -grep(pattern = paste(patternArticlesToRemove, collapse="|"), dfBasePageViews$landingPagePath)
dfBasePageViews <- dfBasePageViews[indexLandingPagePathToRemove,]
str(dfBasePageViews) #37614 obs.

#pour la visualisation des pages "de base" (voir plus bas)
dfBaseDatePV <- as.data.frame(dfBasePageViews$date)
colnames(dfBaseDatePV)[1] <- "date"
BaseDaily_data <- dfBaseDatePV  %>%                      
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y")) %>%               #creation de la variable year
  mutate(dayOfYear = yday(date))                  #creation de la variable dayOfYear






###########################################################
# Détermination du trafic des pages articles marketing
###########################################################
#le trafic des pages vues des pages articles est le reste 
dfAMPageViews <- anti_join(x = dfPageViews, y = dfBasePageViews, by = "index")
str(dfAMPageViews) 

#Calcul des pages vues "articles marketing" par jour

dfAMDatePV <- as.data.frame(dfAMPageViews$date)
colnames(dfAMDatePV)[1] <- "date"
AMDaily_data <- dfAMDatePV  %>%                      
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y")) %>%            #creation de la variable year
  mutate(dayOfYear = yday(date))                  #creation de la variable dayOfYear




##########################################################################
# Comparons le trafic "de base" par rapport au trafic "articles marketing"
############################################################################

#Qui est le plus grand ?
nrow(AMDaily_data)  #2478
nrow(BaseDaily_data) #2502
#merge pour être sur la même taille. (le plus grand)
BaseDaily_AMDaily_data <- merge(x = BaseDaily_data, y = AMDaily_data, by = "date", all.x = TRUE)
str(BaseDaily_AMDaily_data)
#Est-ce que l'on a des na ?
sum(is.na(BaseDaily_AMDaily_data$Pageviews.x)) #0 na
sum(is.na(BaseDaily_AMDaily_data$Pageviews.y)) #180 na

#NA en 0
BaseDaily_AMDaily_data[is.na(BaseDaily_AMDaily_data$Pageviews.y),"Pageviews.y"] <- 0
#recalculate cnt_ma30.y  avec les 0
BaseDaily_AMDaily_data$cnt_ma30.y = ma(BaseDaily_AMDaily_data$Pageviews.y, order=30)


#Comparons le traffic de base vs "Articles MArketing" - Pas très lisible sur toute la période
ggplot() + 
  geom_line(data=BaseDaily_AMDaily_data, aes(date, Pageviews.x), color="blue") + 
  geom_line(data = BaseDaily_AMDaily_data, aes(date, Pageviews.y), color="red") + 
  scale_x_date('year')  + 
  ylab("pages vues") +
  ggtitle(label = "PV depuis 2011 - bleu : pages articles marketing - Rouge : pages de base")
#Pas très lisible
ggsave(filename = "PV-s2011-base-vs-mkt.jpg",  dpi="print") #sauvegarde du dernier ggplot.



#Corrélation de Kendall #tau 0.3814596  pas terrible
cor.test(BaseDaily_AMDaily_data$Pageviews.x, BaseDaily_AMDaily_data$Pageviews.y, 
         method="kendall", use = "complete.obs")

#################################################################################
#Et en moyenne mobile sur 30 jours.
ggplot() + 
  geom_line(data=BaseDaily_AMDaily_data, aes(date, cnt_ma30.x), color="blue") + 
  geom_line(data=BaseDaily_AMDaily_data, aes(date, cnt_ma30.y), color="red") + 
  scale_x_date('year')  + 
  ylab("moyenne mobile 30 j - pages vues") +
  ggtitle(label = "Moy.Mob. 30 j. - PV depuis 2011 - bleu : pages articles marketing \n Rouge : pages de base")

ggsave(filename = "PV-s2011-base-vs-mkt-mm30.jpg",  dpi="print") #sauvegarde du dernier ggplot.


#Corrélation de Kendall #tau : 0.5426576  
cor.test(BaseDaily_AMDaily_data$cnt_ma30.x, BaseDaily_AMDaily_data$cnt_ma30.y, 
         method="kendall", use = "complete.obs")


#Sauvegarde éventuelle 
write.csv2(BaseDaily_AMDaily_data, file = "BaseDaily_AMDaily_data.csv",  row.names=FALSE)



############################################################################
# Significativité du trafic « articles marketing » dans les mois suivants 
# la publication.
# il s'agit des pages visitées suite à une entrée sur une page 
# "Marketing"  OU une page "Marketing"  elle même : AM
############################################################################
str(dfPageViews)  #verif
#pour récupéper des NA  plutot que des 0 par la suite.
lastDate <- dfPageViews[nrow(dfPageViews),"date"] 
str(dfBasePageViews) #page de base
str(dfAMPageViews)  #page articles marketing 35207 obs.
str(myArticles) #Verif
rownames(myArticles) <- 1:nrow(myArticles)  #reindexation

#####################################################################
# Visualisation pour le mois n
#
#pour récupéper des NA  plutot que des 0 par la suite.
#lastDate <- dfPageViews[nrow(dfPageViews),"date"]

############################################################################
# Fonction pour récupérer les distributions sur un numéro de période et 
# un nombre de jour 
############################################################################

getMyDistribution <- function(myPageViews, myArticles, myNumPeriode, myNbrOfDays=30, myLastDate, myTestType="AM") {
#'myPageViews = une dataframe de Pages vues à tester avec les variables au minimum
#' date : date YYYY-MM-DD - date de la visite sur la page
#' landinfPagePath : chr path de la page d'entrée sur le site ex "/rentree-2011"
#  PagePath : chr path de la page visitée sur le site site ex "/rentree-2011"
#'myArticles = une dataframe de Pages vues que l'on souhaite investiguer et qui sont 
#'parmi les précédentes avec les variables au minimum  
#' date : date YYYY-MM-DD - date de la visite sur la page
#' PagePath : chr - path de la page visitée sur le site site ex "/rentree-2011"
#'myNumPeriode : integer Numéro de période par exemple 1 si c'est la première période
#'myNbrOfDays : int - nombre de jours pour la période 30 par défaut
#'myLastDate : date YYYY-MM-DD - date limite à investiguer.
#'myTestType="AM" : chr - "AM" test du landingPagePath ou pagePath sinon test du pagePath seul. 
  
 dfThisPeriodPV <- data.frame(ThisPeriodPV=double())  #pour sauvegarder la distribution 
 for (i in (1:nrow(myArticles))) {
    Link <- myArticles[i,"pagePath"] #lien i /
    #cat(paste(myArticles[i,"date"],"\n"))
    Date1 <- myArticles[i,"date"]+((myNumPeriode-1)*myNbrOfDays)
    Date2 <- Date1+myNbrOfDays
    if (myTestType == "AM") {
      myPV <- myPageViews[which( myPageViews$pagePath == Link | myPageViews$landingPagePath == Link ), ]
      } else  {
        myPV <- dfDMPageViews[which( myPageViews$pagePath == Link  ), ]
      }
    #MArketing
    myPVPeriode <- myPV[myPV$date %in% Date1:Date2,]
    myPVPeriode <- nrow(myPVPeriode)
    
    if (Date1 > myLastDate) { 
      myPVPeriode <- NA  #pour éviter d'avoir des 0 nous avions oublié cet aspect là précédemment
    }
    dfThisPeriodPV[i, "ThisPeriodPV"] <- myPVPeriode  
 }
 return(dfThisPeriodPV)
}
#/getMyDistribution

############################################################################
# Pour le mois 1
############################################################################
myMonthNumber <- 1
dfAMThisMonthPV <- getMyDistribution(myPageViews=dfAMPageViews, 
                                       myArticles=myArticles, 
                                       myNumPeriode=myMonthNumber, 
                                       myNbrOfDays=30,
                                       myLastDate=dfAMPageViews[nrow(dfAMPageViews),"date"],
                                       myTestType="AM")

#test de normalité
resST <- shapiro.test(dfAMThisMonthPV$ThisPeriodPV)
resST$p.value #0.0006370982  normalité rejetée

ggplot(data=dfAMThisMonthPV, aes(x=ThisPeriodPV)) + 
  geom_histogram() +
  xlab("Nombre de vues") +
  ylab("Décompte") +
  labs(title = paste("la distribution est très étirée et ne présente pas de normalité.\n p Valeur =", format(round(resST$p.value, 5), scientific = FALSE)," << 0.05"),
  subtitle = "le décompte le plus important se fait pour les pages à 0 vues \n mais il y a aussi des pages avec plus de 600 vues.",
  caption = paste("Distribution du nombre de pages vues Articles Marketing", myMonthNumber, "mois après la parution"))
##################################################################
#  which.max(dfAMThisMonthPV$ThisPeriodPV)  #page la plus vues
ggsave(filename = stri_replace_all_fixed(paste("Dist-PV-AM-Mois-",myMonthNumber,".jpg"), " ", ""),  dpi="print") #sauvegarde du dernier ggplot en fonction du mois


############################################################################
# Pour le mois 2
############################################################################
myMonthNumber <- 2
dfAMThisMonthPV <- getMyDistribution(myPageViews=dfAMPageViews, 
                                     myArticles=myArticles, 
                                     myNumPeriode=myMonthNumber, 
                                     myNbrOfDays=30,
                                     myLastDate=dfAMPageViews[nrow(dfAMPageViews),"date"],
                                     myTestType="AM")

#test de normalité
resST <- shapiro.test(dfAMThisMonthPV$ThisPeriodPV)
resST$p.value

ggplot(data=dfAMThisMonthPV, aes(x=ThisPeriodPV)) + 
  geom_histogram() +
  xlab("Nombre de vues") +
  ylab("Décompte") +
  #labs standard
  labs(title = paste("Le deuxième mois la distribution s'est resserrée.\n p Valeur =", format(round(resST$p.value, 12), scientific = FALSE)," << 0.05"),
       subtitle = "Il n'y a pas de pages au delà de 200 vues.",
       caption = paste("Distribution du nombre de pages vues Articles Marketing", myMonthNumber, "mois après la parution"))
##################################################################
#  which.max(dfAMThisMonthPV$ThisPeriodPV)
ggsave(filename = stri_replace_all_fixed(paste("Dist-PV-AM-Mois-",myMonthNumber,".jpg"), " ", ""),  dpi="print") #sauvegarde du dernier ggplot en fonction du mois


############################################################################
# Pour le mois 10
############################################################################
myMonthNumber <- 10
dfAMThisMonthPV <- getMyDistribution(myPageViews=dfAMPageViews, 
                                     myArticles=myArticles, 
                                     myNumPeriode=myMonthNumber, 
                                     myNbrOfDays=30,
                                     myLastDate=dfAMPageViews[nrow(dfAMPageViews),"date"],
                                     myTestType="AM")

#test de normalité
resST <- shapiro.test(dfAMThisMonthPV$ThisPeriodPV)
resST$p.value

ggplot(data=dfAMThisMonthPV, aes(x=ThisPeriodPV)) + 
  geom_histogram() +
  xlab("Nombre de vues") +
  ylab("Décompte") +
  #labs standard
  labs(title = paste("Dès le dixième mois on s'approche d'un équilibre.\n p Valeur =", format(round(resST$p.value, 16), scientific = FALSE)," << 0.05"),
       subtitle = "Les pages à 0 vues sont pratiquement aussi nombreuses que celles \n à plusieurs vues",
       caption = paste("Distribution du nombre de pages vues Articles Marketing", myMonthNumber, "mois après la parution"))
##################################################################
#  which.max(dfAMThisMonthPV$ThisPeriodPV)
ggsave(filename = stri_replace_all_fixed(paste("Dist-PV-AM-Mois-",myMonthNumber,".jpg"), " ", ""),  dpi="print") #sauvegarde du dernier ggplot en fonction du mois
 


############################################################################
# Pour le mois 40
############################################################################
myMonthNumber <- 40
dfAMThisMonthPV <- getMyDistribution(myPageViews=dfAMPageViews, 
                                     myArticles=myArticles, 
                                     myNumPeriode=myMonthNumber, 
                                     myNbrOfDays=30,
                                     myLastDate=dfAMPageViews[nrow(dfAMPageViews),"date"],
                                     myTestType="AM")

#test de normalité
resST <- shapiro.test(dfAMThisMonthPV$ThisPeriodPV)
resST$p.value

ggplot(data=dfAMThisMonthPV, aes(x=ThisPeriodPV)) + 
  geom_histogram() +
  xlab("Nombre de vues") +
  ylab("Décompte") +
  #labs standard
  labs(title = paste("A 40 mois la distribution est très resserrée.\n p Valeur =", format(round(resST$p.value, 13), scientific = FALSE)," << 0.05."),
       subtitle = "Les pages à 0 vues sont majoritaires.",
       caption = paste("Distribution du nombre de pages vues Articles Marketing", myMonthNumber, "mois après la parution"))
##################################################################
#sauvegarde du dernier ggplot en fonction du mois
ggsave(filename = stri_replace_all_fixed(paste("Dist-PV-AM-Mois-",myMonthNumber,".jpg"), " ", ""),  dpi="print") 




#############################################################################################
# Utilisation du SIGN.test pour tester la significativité des distributions.
#############################################################################################
#initialisation :
#Pour enregistrer les données du SIGN.test pour toutes les distributions, et aussi les données 
#pour le comparatif par rapport à la taille de l'échantillons et pour la méthode via calcul de 
#l'intervalle de confiance de proportion
dfAMPValue <- data.frame(pValue=double(), #pour SIGN.test
                       conf.int.inf = double(),  #facultatif
                       conf.int.sup = double(),  #facultatif
                       statistic = double(),  #pour visualisation stat vs size
                       myNotNas = double(),   #pour méthode SIGN et IC Proportion
                       myNotNull = double(),  #pour méthode IC Proportion
                       myMedian = double())  #pour méthode IC Proportion

dfAMAllMonthsPV <- data.frame(ThisPeriodPV <- double(),
                              numMonth <- integer())
 
myAMMd <- 0.01 #médiane de l'hypothèse nulle : 0 ne marche pas 
#-> à mon avis le test est que la médiane soit inférieure 
#à cette valeur donc < 0 ne donne rien alors que < 0.01
#détecte les 0.
#Rem cela fonctionne pareil avec SAS et Python 
                
myAMCl <- 0.95  #niveau de confiance souhaité
myLastMonth <- 90 #dernier mois à investiguer 7,5 années

for (x in (1:myLastMonth)) {  #on boucle sur tous les mois 
  #Appel à la fonction de récupération d'une ditribution
  dfAMThisMonthPV <- getMyDistribution(myPageViews=dfAMPageViews, 
                                       myArticles=myArticles, 
                                       myNumPeriode=x, 
                                       myNbrOfDays=30,
                                       myLastDate=dfAMPageViews[nrow(dfAMPageViews),"date"],
                                       myTestType="AM")
  
  #pour exporter vers sas on sauvegarde toutes les distributions
  dfAMThisMonthPV$numMonth <- x
  dfAMAllMonthsPV <- rbind(dfAMAllMonthsPV, dfAMThisMonthPV)
  
  res <- SIGN.test(dfAMThisMonthPV$ThisPeriodPV, y=NULL, md=myAMMd, alternative="greater", conf.level = myAMCl)
  #str(res)
  dfAMPValue[x, "pValue"] <- res$p.value
  dfAMPValue[x, "conf.int.inf"] <- res$conf.int[1] #borne inférieure de l'intervalle de confiance
  dfAMPValue[x, "conf.int.sup"] <- res$conf.int[2] #borne supérieure de l'intervalle de confiance
  dfAMPValue[x, "statistic"] <- res$statistic   #nombre de valeurs > 0

  dfAMPValue[x, "myNotNas"] <- sum(!is.na(dfAMThisMonthPV$ThisPeriodPV)) #taille de l'échantillon
  #nombre de valeurs > 0   #calculé à la main hors de SIGN.test pour le calcul de l'IC de proportion
  dfAMPValue[x, "myNotNull"] <- sum(!(dfAMThisMonthPV[!is.na(dfAMThisMonthPV$ThisPeriodPV), "ThisPeriodPV"]==0)) 
  dfAMPValue[x, "myMedian"] <- median(!is.na(dfAMThisMonthPV$ThisPeriodPV)) #médiane 

}

#str(dfAMAllMonthsPV)
#Export des distributions si besoin
write.csv2(dfAMAllMonthsPV, file = "dfAMAllMonthsPV.csv",  row.names=FALSE)


head(dfAMPValue, n=20)
#Visualisation de la p.valeur du SIGN.test
myfirstMonthPValueUpper <- which.first(dfAMPValue$pValue > 0.05)


ggplot(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=pValue)) + 
  geom_line() +  #, size=myNotNas
  geom_vline(xintercept= myfirstMonthPValueUpper, color="green") +
  geom_hline(yintercept = 0.05, color="red") +
  xlab("Nombre de mois") +
  ylab("P-Valeur") +
  labs(title = paste("L'hypothèse nulle est vérifiée dès le mois", myfirstMonthPValueUpper, "(ligne verte)" ), 
       subtitle = "La ligne rouge indique la p Valeur à 0.05.", 
       caption = paste("P.valeur SIGN.test médiane ", myAMMd, " niveau de confiance ", myAMCl))
#sauvegarde du dernier ggplot
ggsave(filename = "AM-SIGN-Test-P-Value.jpg",  dpi="print") 



#comparons la statistique càd ici les pages avec vues > 0
#vs la taille de l'échantillon donnés par myNotNas, et la moitié de ce dernier
ggplot(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=statistic)) + 
  geom_line() +
  geom_line(aes(x=as.numeric(row.names(dfAMPValue)), y=myNotNas), color="red") +
  geom_line(aes(x=as.numeric(row.names(dfAMPValue)), y=statistic), color="blue") +
  geom_line(aes(x=as.numeric(row.names(dfAMPValue)), y=myNotNas/2), color="black") +
  xlab("Nombre de mois") +
  ylab("Nbre de pages vues > 0 (bleu) | taille échantillon (rouge) ") +
  labs(title = "Le nombre de pages avec vues > 0 baisse plus vite \n que la taille de l'échantillon", 
       subtitle = "La courbe bleu s'approche rapidement de la ligne noire \n qui représente la moitié de l'échantillon", 
       caption = paste("Evolution mensuelle du Nbr de pages vues > 0 vs Taille échantillon \n Médiane de test", myAMMd, " niveau de confiance  ", myAMCl))
ggsave(filename = "AM-sup0-SampleSize.jpg",  dpi="print") 



###################################################################################
# vérifions en calculant l'intervalle de confiance à 95% pour une proportion
# avec les données observées.
dfAMPValue$prop <- dfAMPValue$myNotNull/dfAMPValue$myNotNas  #proportion 
#Intervalle de confiance à 95% pour une proportion 
dfAMPValue$confIntProportion <- 1.96 * sqrt((dfAMPValue$prop*(1-dfAMPValue$prop))/dfAMPValue$myNotNas)
#borne inférieure 
dfAMPValue$propCIinf <- dfAMPValue$prop-dfAMPValue$confIntProportion
#borne superieure 
dfAMPValue$propCIsup <- dfAMPValue$prop+dfAMPValue$confIntProportion
#Première valeur de la borne inférieure  sous 0.5
firstpropCIinfUnder <- which.first(dfAMPValue$propCIinf <= 0.5) 

###################################################################################
ggplot(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=prop)) + 
  geom_line() +
  geom_line(aes(x=as.numeric(row.names(dfAMPValue)), y=propCIsup), color="blue") +
  geom_line(aes(x=as.numeric(row.names(dfAMPValue)), y=propCIinf), color="blue") +
  geom_hline(yintercept = 0.5, color="red") +
  geom_vline(xintercept= firstpropCIinfUnder, color="green") +
  xlab("Nombre de mois") +
  ylab("proportion avec intervalle de confiance (bleu) ") +
  labs(title =  paste("L'hypothèse nulle est vérifiée dès le mois ", firstpropCIinfUnder ), 
       subtitle = "La valeur inférieure de l'intervalle de confiance passe sous la barre des 0.5",
       caption = paste("Proportion de pages vues > 0 pour chaque distribution mensuelle \n  niveau de confiance ", myAMCl))

ggsave(filename = "AM-PropPVsup1.jpg",  dpi="print")


###################################################################################
#en Lissage Loess 
#Calcul Valeurs lissées
myAMLoess <- loess((myNotNull/myNotNas)~row.names(dfAMPValue), dfAMPValue )
str(myAMLoess)
myAMLoess$fitted  #valeurs lissées
#Première valeur lissée sous la barre des 0.5 i.e mediane = 0
firstAMLoessUnder <- which.first(myAMLoess$fitted <= 0.5) #le premier
# valeurs lissées inférieures :
myAMLoess$conf.int.inf <- myAMLoess$fitted - (myAMLoess$s/2) 
#Première valeur de la borne inférieure de l'IC lissée sous la barre des 
# 0.5 i.e mediane = 0
firstAMLoessCIFUnder <-  which.first(myAMLoess$conf.int.inf <= 0.5)

#comparatif du rapport entre les Dist > 0 / myNotnas
ggplot(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=myNotNull/myNotNas)) + 
  geom_line() +
  geom_smooth() +
  geom_hline(yintercept = 0.5, color="red") +
  geom_vline(xintercept= firstAMLoessCIFUnder, color="green") +
  xlab("Nombre de mois") +
  ylab("proportion lissée (bleu)") +
  labs(title =  paste("L'hypothèse nulle est vérifiée au mois ", firstAMLoessCIFUnder ), 
     subtitle = "La valeur inférieure de l'intervalle de confiance de la courbe lissée \n passe sous la barre des 0.5",
     caption = paste("Proportion lissée de pages vues > 0 pour chaque distribution mensuelle \n  niveau de confiance ", myAMCl))

  ggsave(filename = "AM-PropPVsup1-Loess.jpg",  dpi="print")
  
  

##########################################################################
# Recherche du point de plus forte pente sur la courbe Loess
# non repris dans le dossier pas d'intérêt ici compte tenu de la forme 
# de la courbe
##########################################################################
#La dérivée seconde indique la variation de la pente 
myLag <- 1
#approximation de la dérivée 
first_diff <- diff(myAMLoess$fitted, lag = myLag) 
plot(first_diff) #point d'inflexion
second_diff <- diff(first_diff, lag = myLag) #approximation de la dérivée 
plot(second_diff) # variation de la pente  
which.max(second_diff) #point de plus forte pente #23
##########################################################################


##########################################################################
# Restreignons l'investigation aux pages "Direct Marketing" visitées uniquement 
# suite à une entrée sur une page "Marketing" (la même ou une autre)
##########################################################################
str(myArticles) #verif

#on garde uniquement les landingPagePath de nos articles  : 
#DM = Direct Marketing
patternArticlesToKeep <- unique(myArticles$pagePath)
indexLandingPagePathToKeep <- grep(pattern = paste(patternArticlesToKeep, collapse="|"), dfPageViews$landingPagePath)
dfDMPageViews <- dfPageViews[indexLandingPagePathToKeep,]
str(dfDMPageViews) #28553 obs.

#derniere date pour ce data.frame
#pour récupéper des NAs  plutot que des 0 par la suite.
lastDMDate <- dfDMPageViews[nrow(dfDMPageViews),"date"] 

#Pour enregistrer les données du test 
dfDMPValue <- data.frame(pValue=double(),   
                       conf.int.inf = double(),
                       conf.int.sup = double(),
                       statistic = double(),
                       myNotNas = double(),
                       myNotNull = double(),
                       myMedian = double())

myDMMd <- 0.01 #médiane de l'hypothèse nulle (0 ne marche pas )
myDMCl <- 0.95  #niveau de confiance souhaité
myLastMonth <- 90 #dernier mois à investiguer 7,5 années


for (x in (1:myLastMonth)) {
  #pour enregistrer le nombre de pages vues de chaque article dans un mois 
  dfDMThisMonthPV <- getMyDistribution(myPageViews=dfDMPageViews, 
                                       myArticles=myArticles, 
                                       myNumPeriode=x, 
                                       myNbrOfDays=30,
                                       myLastDate=lastDMDate,
                                       myTestType="DM")
  
  #SIGN.test
  res <- SIGN.test(dfDMThisMonthPV$ThisPeriodPV, y=NULL, md=myDMMd, alternative="greater", conf.level = myDMCl)
  
  dfDMPValue[x, "pValue"] <- res$p.value
  dfDMPValue[x, "conf.int.inf"] <- res$conf.int[1]
  dfDMPValue[x, "conf.int.sup"] <- res$conf.int[2]
  dfDMPValue[x, "statistic"] <- res$statistic
 
  #taille de l'échantillon :
  dfDMPValue[x, "myNotNas"] <- sum(!is.na(dfDMThisMonthPV$ThisPeriodPV)) 
  #nombre de valeurs > 0   #calculé à la main hors de SIGN.test pour le 
  #calcul IC Proportion :
  dfDMPValue[x, "myNotNull"] <- sum(!(dfDMThisMonthPV[!is.na(dfDMThisMonthPV$ThisPeriodPV), "ThisPeriodPV"]==0)) 
  dfDMPValue[x, "myMedian"] <- median(!is.na(dfDMThisMonthPV$ThisPeriodPV)) #médiane 
}


#Visualisation de la p.valeur du SIGN.test
myfirstMonthPValueUpper <- which.first(dfDMPValue$pValue > 0.05)
ggplot(data=dfDMPValue, aes(x=as.numeric(row.names(dfDMPValue)), y=pValue)) + 
  geom_line() +  #, size=myNotNas
  geom_vline(xintercept= myfirstMonthPValueUpper, color="green") +
  geom_hline(yintercept = 0.05, color="red") +
  xlab("Nombre de mois") +
  ylab("P-Valeur") +
  labs(title = paste("L'hypothèse nulle est vérifiée dès le mois", myfirstMonthPValueUpper, "(ligne verte)" ), 
       subtitle = "La ligne rouge indique la p Valeur à 0.05.", 
       caption = paste("Direct MArketing - P.valeur SIGN.test médiane ", myDMMd, " niveau de confiance ", myDMCl))
#sauvegarde du dernier ggplot
ggsave(filename = "DM-SIGN-Test-P-Value.jpg",  dpi="print") 



#################################################################
##  Comparatifs AM MD .
#Calcul Valeurs lissées pour Direct Marketing
myDMLoess <- loess((myNotNull/myNotNas)~row.names(dfDMPValue), dfDMPValue )
myDMLoess$fitted  #valeurs lissées
#Première valeur lissée sous la barre des 0.5 i.e mediane = 0
firstDMLoessUnder <- which.first(myDMLoess$fitted <= 0.5) #le premier
#borne inférieure # valeurs lissées inférieures
myDMLoess$conf.int.inf <- myDMLoess$fitted - (myDMLoess$s/2) 
#Première valeur de la borne inférieure de l'IC lissée sous la 
#barre des 0.5 i.e mediane = 0
firstDMLoessCIFUnder <-  which.first(myDMLoess$conf.int.inf <= 0.5)

#comparatif du rapport entre les Dist > 0 / myNotnas pour AM 
# et MD
ggplot() + 
  geom_line(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=statistic/myNotNas), col="blue") +
  geom_smooth(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=statistic/myNotNas)) +
  geom_line(data=dfDMPValue, aes(x=as.numeric(row.names(dfDMPValue)), y=statistic/myNotNas), col = "red") +
  geom_smooth(data=dfDMPValue, aes(x=as.numeric(row.names(dfDMPValue)), y=statistic/myNotNas), col = "red") +
  geom_hline(yintercept = 0.5, color="red") +
  geom_vline(xintercept= firstDMLoessCIFUnder, color="green") +
  xlab("Nombre de mois") +
  ylab("proportion lissée - AM: bleu, DM : rouge ") +
  labs(title =  paste("L'hypothèse nulle pour Direct Marketing est vérifiée au mois ", firstDMLoessCIFUnder ), 
       subtitle = "La valeur inférieure de l'intervalle de confiance de la courbe lissée \n passe sous la barre des 0.5",
       caption = paste("Proportion lissée Articles Marketing et Direct Marketing \n de pages vues > 0 pour chaque distribution mensuelle \n  niveau de confiance ", myAMCl))
ggsave(filename = "DM-AM-PropPVsup1-Loess.jpg",  dpi="print")



#################################################################
# Restreignons encore l'investigation aux pages visitées 
# uniquement suite à une entrée sur la même page  "Marketing"  
# UM : Unique Marketing
#################################################################

str(myArticles) #verif


#on garde uniquement les landingPagePath = pagePath
dfUMPageViews <- dfDMPageViews[(as.character(dfDMPageViews$landingPagePath) == as.character(dfDMPageViews$pagePath))  ,]
str(dfUMPageViews) #21214 obs.

#derniere date pour ce data.frame
lastUMDate <- dfUMPageViews[nrow(dfUMPageViews),"date"] #pour récupéper des NA  plutot que des 0 par la suite.



#Pour enregistrer les données du test 
dfUMPValue <- data.frame(pValue=double(),
                         conf.int.inf = double(),
                         conf.int.sup = double(),
                         statistic = double(),
                         myNotNas = double(),
                         myNotNull = double(),
                         myMedian = double())

myUMMd <- 0.01 #médiane de l'hypothèse nulle (0 ne marche pas  )
myUMCl <- 0.95  #niveau de confiance souhaité
myLastMonth <- 90 #dernier mois à investiguer 7,5 années

#################################################################
for (x in (1:myLastMonth)) {
  #pour enregistrer le nombre de pages vues de chaque article 
  #dans un mois 
  dfUMThisMonthPV <- getMyDistribution(myPageViews=dfUMPageViews, 
                                       myArticles=myArticles, 
                                       myNumPeriode=x, 
                                       myNbrOfDays=30,
                                       myLastDate=lastUMDate,
                                       myTestType="AM")
  
  #SIGN.test
  res <- SIGN.test(dfUMThisMonthPV$ThisPeriodPV, y=NULL, md=myUMMd, alternative="greater", conf.level = myUMCl)
  
  dfUMPValue[x, "pValue"] <- res$p.value
  dfUMPValue[x, "conf.int.inf"] <- res$conf.int[1]
  dfUMPValue[x, "conf.int.sup"] <- res$conf.int[2]
  dfUMPValue[x, "statistic"] <- res$statistic

  #taille de l'échantillon :
  dfUMPValue[x, "myNotNas"] <- sum(!is.na(dfUMThisMonthPV$ThisPeriodPV)) 
  #nombre de valeurs > 0  : calculé à la main hors de SIGN.test 
  #pour le calcul avec IC Proportion
  dfUMPValue[x, "myNotNull"] <- sum(!(dfUMThisMonthPV[!is.na(dfUMThisMonthPV$ThisPeriodPV), "ThisPeriodPV"]==0)) 
  dfUMPValue[x, "myMedian"] <- median(!is.na(dfUMThisMonthPV$ThisPeriodPV)) #médiane 
}


#Visualisation de la p.valeur du SIGN.test
myfirstMonthPValueUpper <- which.first(dfUMPValue$pValue > 0.05)
ggplot(data=dfUMPValue, aes(x=as.numeric(row.names(dfUMPValue)), y=pValue)) + 
  geom_line() +  #, size=myNotNas
  geom_vline(xintercept= myfirstMonthPValueUpper, color="green") +
  geom_hline(yintercept = 0.05, color="red") +
  xlab("Nombre de mois") +
  ylab("P-Valeur") +
  labs(title = paste("L'hypothèse nulle est vérifiée dès le mois", myfirstMonthPValueUpper, "(ligne verte)" ), 
       subtitle = "La ligne rouge indique la p Valeur à 0.05.", 
       caption = paste("Unique Marketing - P.valeur SIGN.test médiane ", myUMMd, " niveau de confiance ", myUMCl))
#sauvegarde du dernier ggplot
ggsave(filename = "UM-SIGN-Test-P-Value.jpg",  dpi="print") 


#################################################################
##  Comparatifs AM DM UM 
#################################################################
#Calcul Valeurs lissées pour Direct Marketing
myUMLoess <- loess((myNotNull/myNotNas)~row.names(dfUMPValue), dfUMPValue )
str(myUMLoess)
myUMLoess$fitted  #valeurs lissées
#Première valeur lissée sous la barre des 0.5 i.e mediane = 0
#le premier :
firstUMLoessUnder <- which.first(myUMLoess$fitted <= 0.5) 
#borne inférieure : valeurs lissées inférieures
myUMLoess$conf.int.inf <- myUMLoess$fitted - (myUMLoess$s/2) 
#Première valeur de la borne inférieure de l'IC lissée sous la barre 
#des 0.5 i.e mediane = 0
firstUMLoessCIFUnder <-  which.first(myUMLoess$conf.int.inf <= 0.5)

#comparatif du rapport entre les Dist > 0 / myNotnas pour AM, 
# MD et UM
ggplot() + 
  geom_line(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=statistic/myNotNas), col="blue") +
  geom_smooth(data=dfAMPValue, aes(x=as.numeric(row.names(dfAMPValue)), y=statistic/myNotNas)) +
  geom_line(data=dfDMPValue, aes(x=as.numeric(row.names(dfDMPValue)), y=statistic/myNotNas), col = "red") +
  geom_smooth(data=dfDMPValue, aes(x=as.numeric(row.names(dfDMPValue)), y=statistic/myNotNas), col = "red") +
  geom_line(data=dfUMPValue, aes(x=as.numeric(row.names(dfUMPValue)), y=statistic/myNotNas)) +
  geom_smooth(data=dfUMPValue, aes(x=as.numeric(row.names(dfUMPValue)), y=statistic/myNotNas), col = "black") +
  geom_hline(yintercept = 0.5, color="red") +
  geom_vline(xintercept= firstUMLoessCIFUnder, color="green") +
  xlab("Nombre de mois") +
  ylab("proportion lissée - AM : bleu, DM : rouge, UM : noir ") +
  labs(title =  paste("L'hypothèse nulle pour Unique Marketing est vérifiée au mois ", firstUMLoessCIFUnder ), 
       subtitle = "La valeur inférieure de l'intervalle de confiance de la courbe lissée \n passe sous la barre des 0.5",
       caption = paste("Proportion lissée Articles Marketing, Direct Marketing et Unique Marketing \n de pages vues > 0 pour chaque distribution mensuelle \n  niveau de confiance ", myAMCl))

ggsave(filename = "UM-DM-AM-PropPVsup1-Loess.jpg",  dpi="print")



##########################################################################
# MERCI pour votre attention !
##########################################################################

