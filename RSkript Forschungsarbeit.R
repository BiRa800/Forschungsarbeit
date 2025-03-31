##01HÄUFIGKEITEN 
data_fm <- read.csv2("data.fm.csv",na="NA")
data_d <- subset(data_fm, Geschlecht == 3)
data_w <- subset(data_fm, Geschlecht == 1)
data_m <- subset(data_fm, Geschlecht == 2)

#absolute Häufigkeit 
table(data_fm$Geschlecht)

#relative Häufigkeit 
prop.table(table(data_fm$Geschlecht))*100

#relative Häufigkeit gerundet 
round(prop.table(table(data_fm$Geschlecht))*100,2)


#02VORAUSSETZUNGEN MIXED ANOVA
#wide format zu long format
data_fm <- read.csv2("data.fm.csv",na="NA")
library(tidyr)
data_long <- gather(data_fm, t, v, Mean1:Mean2)

#Normalverteilung testen 
library(ggpubr)
ggqqplot(data_long, "v") + facet_grid(Gruppe~t)

#deskriptive Statistk Varianz
data_Gruppe1 <- subset(data_fm, Gruppe == 1)
data_Gruppe2 <- subset(data_fm, Gruppe == 2)
var(data_Gruppe1$Mean1)
var(data_Gruppe1$Mean2)
var(data_Gruppe2$Mean1) 
var(data_Gruppe2$Mean2)

#deskriptive Statistik Varianz:einfacher...
library(psych)

describeBy(data_fm$Mean1, data_fm$Gruppe)
describeBy(data_fm$Mean2, data_fm$Gruppe)


#Levene-Test
library(car)
leveneTest(data_fm$Mean1, data_fm$Gruppe)

#Ausreißer (Box-Plot)
boxplot(data_Gruppe1$Mean1, main="Guppe 1, t1", xlab="x-Achse", ylab="y-Achse")
boxplot(data_Gruppe1$Mean2, main="Guppe 1, t2", xlab="x-Achse", ylab="y-Achse")

boxplot(data_Gruppe2$Mean1, main="Guppe 2, t1", xlab="x-Achse", ylab="y-Achse")
boxplot(data_Gruppe2$Mean2, main="Guppe 2, t2", xlab="x-Achse", ylab="y-Achse")


#03MIXED ANOVA 
#Descriptives 
library(dplyr)
data_long %>%
  group_by(t, Gruppe) %>%
  get_summary_stats(v, type = "mean_sd") %>%
  as.data.frame()

#Berechnung mixed ANOVA
data_fm <- read.csv2("data.fm.csv",na="NA")

install.packages("afex")
library(afex)

results_ANOVA <- aov_car(v ~ Gruppe * t + Error(ID/t), data = data_long)
summary(results_ANOVA)

#Post-hoc-Test
install.packages("emmeans")
library(emmeans)

post_hoc_results <- emmeans(results_ANOVA, ~ Gruppe * t)
summary(post_hoc_results)
pairwise_comparisons <- pairs(post_hoc_results)
summary(pairwise_comparisons)


#Effektstärken
## cohens d 
data_long %>%
  cohens_d(v~t, paired = TRUE)

##Eta²
library(rstatix)
fm <- anova_test(data_long, dv = v, wid = ID, between = Gruppe, within = t)
ges <- fm$ges
f <- sqrt(ges/(1-ges))


#04ERGEBNISSE 
#Liniendiagramm
install.packages("Hmisc")
library(Hmisc)

data_Gruppe1 <- subset(data_fm, Gruppe == 1)
data_Gruppe2 <- subset(data_fm, Gruppe == 2)

means1 = c(mean(data_Gruppe1$Mean1), mean(data_Gruppe1$Mean2))
means2 = c(mean(data_Gruppe2$Mean1), mean(data_Gruppe2$Mean2))
sds1 = c(sd(data_Gruppe1$Mean1), sd(data_Gruppe1$Mean2))
sds2 = c(sd(data_Gruppe2$Mean1), sd(data_Gruppe2$Mean2))
xx = c(0,1)

png("04 Liniendiagramm.png", 500, 300)
par(mar=c(5,5,1,4))
plot(xx, means1
     , cex.lab=1.5
     , xlab="Zeitpunkt"
     , ylab="Symptome"
     , t="l"
     , ylim=c(40, 60)
     , xaxt="n"
)
points(xx, means1, pch=1)
lines(xx, means2
      , col="blue"
      , pch=2
      , lty=2
)
points(xx, means2, col="blue", pch=2)
errbar(xx, means1, means1 + sds1, means1 - sds1, add=TRUE)
errbar(xx, means2, means2 + sds2, means2 - sds2, col="blue", add=TRUE)
axis(1, at=xx, labels=c("vor Behandlung ", "nach Behandlung"))
legend("topright"
       , c("Testgruppe", "Kontrollgruppe")
       , lty=c(1, 2)
       , pch=c(1, 2)
       
)
dev.off()
