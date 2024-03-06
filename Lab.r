NS_droplets <- read.csv("1-NS_droplets.csv", header = TRUE)
US_droplets <- read.csv("4-US_droplets.csv", header = TRUE)
HS_droplets <- read.csv("5-HS_droplets.csv", header = TRUE)
salt <- read.csv("salt_calibration.csv", header = TRUE)

# udring af lineaer modeller af intensititerne og salt concentraitonen
I3340 <- lm(salt$mean_intensity_3340 ~ salt$salt_conc)
I3180 <- lm(salt$mean_intensity_3180 ~ salt$salt_conc)

# Opgave 3 ----

# plotning af den førsste kurve
plot(salt$salt_conc, 
     salt$mean_intensity_3340,
     xlab = "koncentration af NaCl [w%]",
     ylab="Intensity",
     col="orange",
     ylim = c(30, 90),
     title("Kalibrerings kurve")
     )

abline(I3340, col = "orange")
par(new = TRUE) # parameter der tillader den anden kurve at blive lagt oven i
plot(salt$salt_conc,
     salt$mean_intensity_3180,
     xlab="",
     ylab="",
     col="blue",
     ylim = c(30,90)
     )

abline(I3180, col = "blue")
legend("bottomright",
       legend = c("Intensity 3340", "Intensity 3180"),
       col = c("orange", "blue"),
       lty = c(1))

# Equation 1 Irelative = I3340/I3180 = (A3340*csalt+B3340)/(A3180*csalt+B3180) -> csalt = (B3180*Irelative-B3340)/(A3340-A3180*Irelative)

# udregning af equation Irelative
IUS <- US_droplets$mean_intensity_3340/US_droplets$mean_intensity_3180
INS <- NS_droplets$mean_intensity_3340/NS_droplets$mean_intensity_3180
IHS <- HS_droplets$mean_intensity_3340/HS_droplets$mean_intensity_3180

# udregning af salt koncentrationerne ud fra equation 1
cUS <- (I3180$coefficients[1]*IUS-I3340$coefficients[1])/(I3340$coefficients[2]-I3180$coefficients[2]*IUS)
cNS <- (I3180$coefficients[1]*INS-I3340$coefficients[1])/(I3340$coefficients[2]-I3180$coefficients[2]*INS)
cHS <- (I3180$coefficients[1]*IHS-I3340$coefficients[1])/(I3340$coefficients[2]-I3180$coefficients[2]*IHS)

# Opgave 3 ----

# parameter for at opsætte kurver
par(mfrow = c(2,3))
# scatterplot over arealet og salt koncentrationen, både logaritmisk og normalt
plot(log(US_droplets$droplet_area_um2), cUS, ylab = "koncentration af NaCl [w%]", xlab = "log af Areal", title("Log US Droplets"))
plot(log(NS_droplets$droplet_area_um2), cNS, ylab = "koncentration af NaCl [w%]", xlab = "log af Areal", title("Log NS Droplets"))
plot(log(HS_droplets$droplet_area_um2), cHS, ylab = "koncentration af NaCl [w%]", xlab = "log af Areal", title("Log HS Droplets"))

plot(US_droplets$droplet_area_um2, cUS, ylab = "koncentration af NaCl [w%]", xlab = "Areal", title("US Droplets"))
plot(NS_droplets$droplet_area_um2, cNS, ylab = "koncentration af NaCl [w%]", xlab = "Areal", title("NS Droplets"))
plot(HS_droplets$droplet_area_um2, cHS, ylab = "koncentration af NaCl [w%]", xlab = "Areal", title("HS Droplets"))

# Opgave 4

# barplos og boxplots over salt koncentrationerne, inklusiv gennemsnit og standardafvigelse
# orange for gennemsnit og blå for sd
par(mfrow = c(2,3))
barplot(cUS, main = "Bar plot US")
abline(h = mean(cUS), col="orange")
abline(h = mean(cUS)+sd(cUS), col="blue")
abline(h = mean(cUS)-sd(cUS), col="blue")

barplot(cNS, main ="Bar plot NS")
abline(h = mean(cNS), col="orange")
abline(h = mean(cNS)+sd(cNS), col ="blue")
abline(h = mean(cNS)-sd(cNS), col ="blue")

barplot(cHS, main="Bar plot HS")
abline(h = mean(cHS), col="orange")
abline(h = mean(cHS)+sd(cHS), col ="blue")
abline(h = mean(cHS)-sd(cHS), col ="blue")

boxplot(cUS, main="box plos US")
abline(h = mean(cUS), col="orange")
abline(h = mean(cUS)+sd(cUS), col ="blue")
abline(h = mean(cUS)-sd(cUS), col ="blue")

boxplot(cNS, main="box plot NS")
abline(h = mean(cNS), col="orange")
abline(h = mean(cNS)+sd(cNS), col ="blue")
abline(h = mean(cNS)-sd(cNS), col ="blue")

boxplot(cHS, main="box plot HS")
abline(h = mean(cHS), col="orange")
abline(h = mean(cHS)+sd(cHS), col ="blue")
abline(h = mean(cHS)-sd(cHS), col ="blue")

# Opgave 5 ----

#kurver over det vægtet gennemsnit for salt koncentrationerne
par(mfrow = c(1,3))
barplot(cUS, main="US weighted mean")
abline(h = weighted.mean(cUS, US_droplets$droplet_area_um2), col="orange")
abline(h = weighted.mean(cUS, US_droplets$droplet_area_um2)-sd(cUS), col="blue")
abline(h = weighted.mean(cUS, US_droplets$droplet_area_um2)+sd(cUS), col="blue")

barplot(cNS, main ="NS weighted mean")
abline(h = weighted.mean(cNS, NS_droplets$droplet_area_um2), col="orange")
abline(h = weighted.mean(cNS, NS_droplets$droplet_area_um2)-sd(cNS), col="blue")
abline(h = weighted.mean(cNS, NS_droplets$droplet_area_um2)+sd(cNS), col="blue")

barplot(cHS, main ="HS weighted mean")
abline(h = weighted.mean(cHS, HS_droplets$droplet_area_um2), col="orange")
abline(h = weighted.mean(cHS, HS_droplets$droplet_area_um2)-sd(cHS), col="blue")
abline(h = weighted.mean(cHS, HS_droplets$droplet_area_um2)+sd(cHS), col="blue")



# Opgave 6 ----

# Histogrammer for saltkoncentrationerne med 10 og 100 breaks
par(mfrow = c(1,2))
histo1 <- hist(cNS, breaks = 10, col = "grey")
histo2 <- hist(cNS, breaks=100, col = "grey")

# normal kurve udregnet ud fra link som er opgivet i opgaven 
xfit <- seq(min(cNS), max(cNS), length = 40)
yfit <- dnorm(xfit, mean = mean(cNS), sd = sd(cNS))
yfit1 <- yfit*diff(histo1$mids[1:2])*length(cNS)
yfit2 <- yfit*diff(histo2$mids[1:2])*length(cNS)


lines(xfit, yfit1, col="blue")
lines(xfit, yfit2, col="blue")

# Opgave 7 ----

par(mfrow = c(1,1))
# udregning af densiteten
d <- density(cNS)
#plotning af densiteten
plot(d, main ="Kernel density of NS")
#polygon for bedre visualisering af densiteten
polygon(d, col="grey", border ="orange")

# Opgave 8 ----

# indlæser biblioteket sm
library(sm)
# laver vektorer af concentration og labels til concentrationen
conc <- c(cUS, cNS, cHS)
conclabels <- c(rep(1, length(cUS)), rep(2, length(cNS)), rep(3, length(cHS)))
# plotter density compare og gemmer det i en variabel for brug til legend
comp <- sm.density.compare(conc, conclabels)
legend("topright", c("US", "NS", "HS"), col = comp$col, lty = comp$lty, lwd = comp$lwd)

# Opgave 9 ----

# plotter arealet af dråberne i et barplot med gennemsnit og sd
par(mfrow =c(1,3))
barplot(US_droplets$droplet_area_um2, main  ="US Droplets", ylab = "Areal")
abline(h = mean(US_droplets$droplet_area_um2), col="orange")
abline(h =mean(US_droplets$droplet_area_um2)-sd(US_droplets$droplet_area_um2), col = "blue")
abline(h =mean(US_droplets$droplet_area_um2)+sd(US_droplets$droplet_area_um2), col = "blue")

barplot(NS_droplets$droplet_area_um2, main="NS Droplets", ylab ="Areal")
abline(h=mean(NS_droplets$droplet_area_um2), col="orange")
abline(h=mean(NS_droplets$droplet_area_um2)-sd(NS_droplets$droplet_area_um2), col ="blue")
abline(h=mean(NS_droplets$droplet_area_um2)+sd(NS_droplets$droplet_area_um2), col ="blue")

barplot(HS_droplets$droplet_area_um2, main ="HS droplets", ylab="Areal")
abline(h = mean(HS_droplets$droplet_area_um2), col ="orange")
abline(h = mean(HS_droplets$droplet_area_um2)-sd(HS_droplets$droplet_area_um2), col = "blue")
abline(h = mean(HS_droplets$droplet_area_um2)+sd(HS_droplets$droplet_area_um2), col = "blue")

# Opgave 10 ----
# laver to nye density compare plots af normal og logaritmen over arealet
par(mfrow = c(1,2))
size <- c(US_droplets$droplet_area_um2, NS_droplets$droplet_area_um2, HS_droplets$droplet_area_um2)
sizeLabels <- c(rep(1, length(US_droplets$droplet_area_um2)), rep(2, length(NS_droplets$droplet_area_um2)), rep(3, length(HS_droplets$droplet_area_um2)))
comp1 <- sm.density.compare(size, sizeLabels, xlim = c(-25, 100))
legend("topright", c("US", "NS", "HS"), col = comp1$col, lty = comp1$lty, lwd = comp1$lwd)

logsize <- log(size)
complog <- sm.density.compare(logsize, sizeLabels)
legend("topright", c("US","NS", "HS"), col = complog$col, lty = complog$lty, lwd = complog$lwd)

# Opgave 11 ----

# Equation c_sample = ((c_drop*p_drop)/p_butter)*x_drop/butter

# udregning af equation 2
c_dropUS <- weighted.mean(cUS, US_droplets$droplet_area_um2)
c_dropNS <- weighted.mean(cNS, NS_droplets$droplet_area_um2)
c_dropHS <- weighted.mean(cHS, HS_droplets$droplet_area_um2)

C_sampleUS <- ((c_dropUS*1)/0.911)*(sum(US_droplets$droplet_area_um2/338514))
C_sampleNS <- ((c_dropNS*1)/0.911)*(sum(NS_droplets$droplet_area_um2/104661))
C_sampleHS <- ((c_dropHS*1)/0.911)*(sum(HS_droplets$droplet_area_um2/41864))
