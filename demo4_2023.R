# 4.harjoitukset

library(foreign)
dat<-read.spss("demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

# sirontakuvio

plot(oma_tulo,tvsaasto)
abline(lm(tvsaasto~oma_tulo))

#korrelaatiokertoimet

cor.test(oma_tulo, tvsaasto,method="pearson")
cor.test(oma_tulo, tvsaasto,method="spearman")

#yksinkertainen regressiomalli

lm.saasto <- lm(tvsaasto~oma_tulo)
summary(lm.saasto)

# Jäännösdiagnostiikka

plot(fitted(lm.saasto),resid(lm.saasto))
hist(resid(lm.saasto))

# sirontakuvio

plot(evsaasto,tvsaasto)
abline(lm(tvsaasto~evsaasto))

#sirontakuvio

plot(ika,tvsaasto)
abline(lm(tvsaasto~ika))

#korrelaatiokertoimet

cor.test(evsaasto, tvsaasto,method="pearson")
cor.test(evsaasto, tvsaasto,method="spearman")
cor.test(ika, tvsaasto,method="pearson")
cor.test(ika, tvsaasto,method="spearman")

# Kahden selittäjän regressiomalli

lm.saasto2 <- lm(tvsaasto~ika+evsaasto)
summary (lm.saasto2)
# jäännösdiagnostiikka
plot(fitted(lm.saasto2),resid(lm.saasto2))
hist(resid(lm.saasto2))
         
         
# Tutkitaan T-testillä, onko omistusasunnon omistajilla
# ja ei omistajilla eroa säästöissä

t.test(tvsaasto~omistusa)
t.test(evsaasto~omistusa)
         
# sirontakuvio, regressiosuorat osa-aineistoittain

plot(evsaasto,tvsaasto,pch=as.numeric(omistusa))
         
omistusa.ei <- dat[omistusa=="ei",]
omistusa.on <- dat[omistusa=="on",]
lm.ei <- lm(tvsaasto~evsaasto, data=omistusa.ei)
lm.on <- lm(tvsaasto~evsaasto, data=omistusa.on)
abline(lm.ei)
abline(lm.on)

# korrelaatiot osa-aineistoissa

attach(omistusa.ei)
cor.test(evsaasto, tvsaasto,method="pearson")
cor.test(evsaasto, tvsaasto,method="spearman")
detach(omistusa.ei)
attach(omistusa.on)
cor.test(evsaasto, tvsaasto,method="pearson")
cor.test(evsaasto, tvsaasto,method="spearman")
         
detach(omistusa.on)
attach(dat)

# Testataan kulmakertoimien yhtäsuuruusoletusta

summary(lm(tvsaasto~evsaasto*omistusa))

# Kovarianssianalyysi
summary(lm(tvsaasto~evsaasto+omistusa))

# Kovariaattikorjatut keskiarvot

model <- lm(tvsaasto~evsaasto+omistusa)
install.packages ("emmeans")
library (emmeans)
model.emm <- emmeans(model, "omistusa")
pairs(model.emm)
 
         

# 3. harjoituksen osalta jäi tekemättä Mauchlyn testi ja korjatut F-testit,jotka
# tulevat SPSS:ssä automaattisesti. R:ssä vaatii vähän koodausta :)


#Uusi data, jossa kolmen vuoden säästöt matriisimuodossa

library (foreign)
dat2<-read.spss("toistetut mittaukset alekkain 2015r.sav", to.data.frame=TRUE)
attach(dat2)
data_uusi3 <- with(dat2, cbind(euroa[vuosi=="saasto2013"],
                               euroa[vuosi=="saasto2014"], euroa[vuosi=="saasto2015"]))
data_uusi3
model2 <- lm(data_uusi3 ~ 1)
model2

install.packages("car")
library(car)

design<-factor(c("saasto2013", "saasto2014", "saasto2015"))
design
options(contrasts=c("contr.sum", "contr.poly"))
results<-Anova(model2, idata=data.frame(design), idesign=~design, type="III")
summary(results, multivariate=FALSE)

         

