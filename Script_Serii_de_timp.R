#1. Instalarea pachetelor

install.packages("fpp2")
install.packages("forecast")
install.packages("readxl")
install.packages("tseries")
install.packages("urca")
install.packages("vars")
install.packages("uroot")
install.packages("FinTS")
install.packages("fGarch")
install.packages("lmtest")
install.packages("moments")
install.packages("changepoint")
install.packages("seasonal")
install.packages("dplyr")

# Incarcarea pachetelor

library(fpp2)       # grafice si date pentru serii de timp
library(forecast)   # modele de prognoza: ARIMA, ETS, HW etc.
library(readxl)     # citirea fisierelor Excel
library(tseries)    # teste statistice: ADF, Jarque-Bera etc.
library(urca)       # teste avansate de stationaritate: ADF (ur.df), KPSS, ZA
library(vars)       # modele VAR si VECM
library(uroot)      # teste radacina unitara sezoniera: HEGY, Canova-Hansen
library(FinTS)      # testul ARCH-LM pentru heteroscedasticitate conditionata
library(fGarch)     # modele ARCH si GARCH
library(lmtest)     # testarea semnificatiei coeficientilor (coeftest)
library(moments)    # statistici descriptive: asimetrie, kurtosis
library(changepoint)# detectarea punctelor de schimbare (change points)
library(dplyr)      # manipularea datelor

# Incarcarea datelor

date_innoptari <- read_excel("Date_complete.xlsx", sheet = 2)
date_sosiri <- read_excel("Date_complete.xlsx", sheet = 3)
date_indice <- read_excel("Date_complete.xlsx", sheet = 4)

head(date_innoptari)
head(date_sosiri)
head(date_indice)

# Seria 1: Innoptari
innoptari <- ts(
  date_innoptari[[2]],  
  start = c(2010, 1),        
  frequency = 12             
)

# Seria 2: Sosiri
sosiri <- ts(
  date_sosiri[[2]],
  start = c(2010, 1),
  frequency = 12
)

# Seria 3: Indicele de utilizare a capacitatii
indice <- ts(
  date_indice[[2]],
  start = c(2010, 1),
  frequency = 12
)

# Afisam seriile pentru verificare
cat("=== SERIA 1: INNOPTARI ===\n")
print(innoptari)
cat("\nNumar de observatii:", length(innoptari), "\n")
cat("Perioada:", start(innoptari), "pana la", end(innoptari), "\n")

cat("\n=== SERIA 2: SOSIRI ===\n")
print(sosiri)
cat("\nNumar de observatii:", length(sosiri), "\n")

cat("\n=== SERIA 3: INDICELE DE UTILIZARE ===\n")
print(indice)
cat("\nNumar de observatii:", length(indice), "\n")

# DATE PENTRU TRAINING SI TEST

h <- 24  # orizontul de prognoza si dimensiunea setului de test

# Indicii pentru impartire
n <- length(innoptari)

# Training: toate observatiile mai putin ultimele 24
innoptari_train <- window(innoptari, end = time(innoptari)[n - h])
sosiri_train    <- window(sosiri, end = time(sosiri)[n - h])
indice_train    <- window(indice, end = time(indice)[n - h])

# Test: ultimele 24 de luni
innoptari_test <- window(innoptari, start = time(innoptari)[n - h + 1])
sosiri_test    <- window(sosiri, start = time(sosiri)[n - h + 1])
indice_test    <- window(indice, start = time(indice)[n - h + 1])

# Verificarea dimensiunilor
cat("Dimensiune Training:", length(innoptari_train), "observatii\n")
cat("Dimensiune Test:    ", length(innoptari_test), "observatii\n")

# GRAFICUL DE IMPARTIRE TRAINING / TEST

# Impartirea pentru seria principala (innoptari)
autoplot(innoptari_train, series = "Training") +
  autolayer(innoptari_test, series = "Test") +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari in structuri turistice - Romania (2010-2026)\nImpartirea in Training si Test") +
  guides(colour = guide_legend(title = "Esantion")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cat("Seriile de timp au fost create:\n")
cat("  - innoptari: ", length(innoptari), "obs.\n")
cat("  - sosiri:    ", length(sosiri), "obs.\n")
cat("  - indice:    ", length(indice), "obs.\n")
cat("  - Training:  ", length(innoptari_train), "obs.\n")
cat("  - Test:      ", length(innoptari_test), "obs.\n")

# 2. Analiza descriptiva si vizualizare

# Seria 1: Innoptari 
autoplot(innoptari) +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari in structuri de primire turistica - Romania (2010-2026)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Seria 2: Sosiri
autoplot(sosiri) +
  xlab("An") +
  ylab("Numar sosiri") +
  ggtitle("Sosiri ale turistilor in structuri de primire turistica - Romania (2010-2026)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Seria 3: Indicele de utilizare 
autoplot(indice) +
  xlab("An") +
  ylab("Indice utilizare (%)") +
  ggtitle("Indicele de utilizare neta a capacitatii de cazare - Romania (2010-2026)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# INTERPRETARE:
# - Trend crescator vizibil intre 2010-2019 (boom turistic)
# - Soc extrem in 2020 (pandemia COVID-19): cadere brusca a innoptarilor
# - Redresare rapida in 2021-2022

# Grafic cu cele 3 serii 

cbind("Innoptari" = innoptari,
      "Sosiri"    = sosiri,
      "Indice (%)"= indice) %>%
  autoplot(facets = TRUE) +
  xlab("An") +
  ylab("") +
  ggtitle("Activitatea turistica in Romania (2010-2026)\nCele trei serii analizate") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 2.2. STATISTICI DESCRIPTIVE

# Innoptari
cat("--- SERIA 1: INNOPTARI (numar turisti) ---\n")
cat("Minim:     ", min(innoptari, na.rm = TRUE), "\n")
cat("Maxim:     ", max(innoptari, na.rm = TRUE), "\n")
cat("Medie:     ", round(mean(innoptari, na.rm = TRUE), 0), "\n")
cat("Mediană:   ", round(median(innoptari, na.rm = TRUE), 0), "\n")
cat("Std. dev.: ", round(sd(innoptari, na.rm = TRUE), 0), "\n")
cat("Asimetrie: ", round(skewness(innoptari), 4), "\n")
cat("Kurtosis:  ", round(kurtosis(innoptari), 4), "\n")

# Testul Jarque-Bera pentru normalitate
# H0: distributia este normala
# H1: distributia nu este normala
cat("\nTest Jarque-Bera (normalitate distributie innoptari):\n")
print(jarque.bera.test(as.numeric(innoptari)))

cat("\n--- SERIA 2: SOSIRI (numar turisti) ---\n")
cat("Minim:     ", min(sosiri, na.rm = TRUE), "\n")
cat("Maxim:     ", max(sosiri, na.rm = TRUE), "\n")
cat("Medie:     ", round(mean(sosiri, na.rm = TRUE), 0), "\n")
cat("Mediană:   ", round(median(sosiri, na.rm = TRUE), 0), "\n")
cat("Std. dev.: ", round(sd(sosiri, na.rm = TRUE), 0), "\n")
cat("Asimetrie: ", round(skewness(sosiri), 4), "\n")
cat("Kurtosis:  ", round(kurtosis(sosiri), 4), "\n")

cat("\nTest Jarque-Bera (normalitate distributie sosiri):\n")
print(jarque.bera.test(as.numeric(sosiri)))

cat("\n--- SERIA 3: INDICE UTILIZARE (%) ---\n")
cat("Minim:     ", min(indice, na.rm = TRUE), "\n")
cat("Maxim:     ", max(indice, na.rm = TRUE), "\n")
cat("Medie:     ", round(mean(indice, na.rm = TRUE), 2), "\n")
cat("Mediană:   ", round(median(indice, na.rm = TRUE), 2), "\n")
cat("Std. dev.: ", round(sd(indice, na.rm = TRUE), 4), "\n")
cat("Asimetrie: ", round(skewness(indice), 4), "\n")
cat("Kurtosis:  ", round(kurtosis(indice), 4), "\n")

cat("\nTest Jarque-Bera (normalitate distributie indice):\n")
print(jarque.bera.test(as.numeric(indice)))

# INTERPRETARE:
# - Distributie asimetrica la dreapta 
# - Kurtosis > 3 => distributie leptocurtica 
# - Jarque-Bera: p < 0.05 => respingem normalitatea 

# Histograma distributiei 
ggplot(data.frame(x = as.numeric(innoptari)), aes(x = x)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  ggtitle("Distributia innoptarilor lunare - Romania (2010-2026)") +
  xlab("Numar innoptari") +
  ylab("Frecventa") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 2.3. ANALIZA SEZONALITATII

# 2.3.1. Seasonal plot: innoptari 

ggseasonplot(innoptari_train,
             year.labels = TRUE,
             year.labels.left = TRUE) +
  ylab("Numar innoptari") +
  ggtitle("Grafic sezonier: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Fiecare linie reprezinta un an
# - Pattern sezonier foarte clar: varfuri in iulie-august, minime in ianuarie
# - Nivelul general creste de la an la an (trend crescator)
# - Linia anului 2020 este mult mai jos decat celelalte (COVID-19)
# - Amplitudinea sezonalitatii creste odata cu trecerea anilor

# 2.3.2. Polar seasonal plot: innoptari 
ggseasonplot(innoptari_train, polar = TRUE) +
  ylab("Numar innoptari") +
  ggtitle("Grafic sezonier polar: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Forma alungita in dreptul lunilor iulie-august (varfuri de sezon)
# - Distanta radiala mai mare vara => sezonalitate puternica vara
# - Spirala evidentiaza cresterea nivelului in timp (trend)

# 2.3.3. Subseries plot: innoptari
# In grafic este reprezentata fiecare luna, iar linia albastra este media lunii respective.
ggsubseriesplot(innoptari_train) +
  ylab("Numar innoptari") +
  ggtitle("Grafic subseriilor sezoniere: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Seria Sosiri 
ggseasonplot(sosiri_train,
             year.labels = TRUE,
             year.labels.left = TRUE) +
  ylab("Numar sosiri") +
  ggtitle("Grafic sezonier: Sosiri lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsubseriesplot(sosiri_train) +
  ylab("Numar sosiri") +
  ggtitle("Grafic subseriilor sezoniere: Sosiri lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Seria pentru Indice 
ggseasonplot(indice_train,
             year.labels = TRUE,
             year.labels.left = TRUE) +
  ylab("Indice utilizare (%)") +
  ggtitle("Grafic sezonier: Indice utilizare capacitate cazare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsubseriesplot(indice_train) +
  ylab("Indice utilizare (%)") +
  ggtitle("Grafic subseriilor sezoniere: Indice utilizare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Diferente mari intre mediile lunare (exista sezonalitate)
# - Iulie si august au cele mai mari medii (sezon turistic de vara)
# - Primele 3 luni ale anului au cele mai mici medii (extrasezon)
# - In fiecare luna, valorile cresc in timp (trend crescator)
# - Asa cum reiese si din graficul sezonier, luna aprilie/ mai 2020 apar ca outlieri

# 2.4. AUTOCORELATIA SI DEPENDENTA TEMPORALA (ACF si PACF)

#  ACF pentru seria Innoptari 
ggAcf(innoptari_train, lag.max = 48) +
  ggtitle("ACF - Innoptari lunare (Romania)") +
  theme_bw()

# PACF pentru seria Innoptari
ggPacf(innoptari_train, lag.max = 48) +
  ggtitle("PACF - Innoptari lunare (Romania)") +
  theme_bw()

#  Corelograma pentru Innoptari (serie + ACF + PACF) 
ggtsdisplay(innoptari_train,
            lag.max = 48,
            main = "Corelograma: Innoptari lunare - Romania")

# Corelograma pentru Sosiri
ggtsdisplay(sosiri_train,
            lag.max = 48,
            main = "Corelograma: Sosiri lunare - Romania")

# INTERPRETARE:
# - Varfuri clare la lag 12, 24, 36, 48 => sezonalitate anuala puternica
# - Alternanta de semne intre laguri apropiate => structura sezoniera
# - Graficul PACF confirma necesitatea diferențierii sezoniere 

# Corelograma pentru Indice 
ggtsdisplay(indice_train,
            lag.max = 48,
            main = "Corelograma: Indice utilizare - Romania")

# 2.5. MEDII MOBILE SI TREND

# Medie mobila 7 termeni pe seria innoptari
install.packages("slider")
library(slider)  

innoptari_df <- data.frame(
  timp   = time(innoptari_train),
  valori = as.numeric(innoptari_train)
)

innoptari_df <- innoptari_df %>%
  mutate(
    MA7  = slider::slide_dbl(valori, mean, .before = 3, .after = 3,  .complete = TRUE),
    MA12 = slider::slide_dbl(valori, mean, .before = 6, .after = 5,  .complete = TRUE)
  )

# Grafic: seria originala + MA7 + MA12
ggplot(innoptari_df, aes(x = timp)) +
  geom_line(aes(y = valori, colour = "Seria originala"), linewidth = 0.5) +
  geom_line(aes(y = MA7,    colour = "Medie mobila 7"), linewidth = 0.8, na.rm = TRUE) +
  geom_line(aes(y = MA12,   colour = "Medie mobila 12"), linewidth = 0.8, na.rm = TRUE) +
  scale_colour_manual(values = c("Seria originala" = "gray60",
                                 "Medie mobila 7"  = "blue",
                                 "Medie mobila 12" = "red")) +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari lunare cu medii mobile (7-MA si 12-MA)\nRomania (2010-2024)") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Seria originala (gri): fluctuatii puternice, sezonalitate clara
# - MA7 (albastru): netezire partiala, sezonalitate inca vizibila
# - MA12 (rosu): elimina complet sezonalitatea, evidentiaza trendul pur
#   => trend crescator 2010-2019, cadere abrupta 2020, redresare 2021-2024

# 2.6. DESCOMPUNEREA SERIEI DE TIMP

# Descompunere aditivă 

decomp_aditiv <- decompose(innoptari_train, type = "additive")
autoplot(decomp_aditiv) +
  ggtitle("Descompunere clasica ADITIVĂ: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - data (seria originala): sezonalitate puternica + trend + soc COVID
# - trend: crestere 2010-2019, cadere brusca 2020, redresare incepand cu anul 2021
# - seasonal: pattern lunar repetitiv 
# - reziduu: socul COVID apare ca outlier in 2020

# Descompunere multiplicativa

decomp_multiplicativ <- decompose(innoptari_train, type = "multiplicative")
autoplot(decomp_multiplicativ) +
  ggtitle("Descompunere clasica MULTIPLICATIVĂ: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# DATA: trend crescator 2010-2019, soc COVID extrem in 2020, redresare rapida incepand cu 2021
# TREND: crestere  pana in 2019,prabusire brusca in 2020, revenire in anul 2023
# SEASONAL: 
#           => in iulie/august se inregistreaza de 2x mai multe innoptari fata de medie
#           => pattern sezonier stabil si predictibil pe toata perioada
# REMAINDER: oscileaza in jurul lui 1 in 2010-2019 (model bun)
#            => outlieri in 2020-2021 = socul COVID, impredictibil
# modelul multiplicativ este potrivit deoarece amplitudinea
# sezonalitatii creste proportional cu nivelul trendului

# STL = Seasonal and Trend decomposition using Loess

decomp_stl <- stl(innoptari_train, s.window = "periodic", robust = TRUE)
autoplot(decomp_stl) +
  ggtitle("Descompunere STL: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Data: seria originala cu toate componentele
# - Trend: evolutia de fond - crestere, soc COVID, redresare
# - Seasonal: pattern sezonier lunar, stabil in timp
# - Remainder: ceea ce ramane dupa eliminarea trendului si sezonalitatii
#   => COVID-19 apare ca outlier in 2020

# Seria ajustata sezonier = seria originala MINUS componenta sezoniera
# Aceasta reflecta evolutia "curata" fara efecte de sezon
innoptari_adj <- seasadj(decomp_stl)

autoplot(innoptari_train, series = "Seria originala") +
  autolayer(innoptari_adj, series = "Ajustata sezonier") +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari: seria originala vs. ajustata sezonier") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# - Seria ajustata sezonier (portocalie) este mult mai "neteda"
# - Elimina variatiile sezoniere, pastrand trendul si socurile
# - Socul COVID (2020) ramane vizibil si dupa ajustare sezoniera
#   => confirma ca nu este un efect sezonier, ci un soc structural real

# GRAFICUL COMPONENTEI SEZONIERE (din STL)

# Extragerea si vizualizarea componentei sezoniere
seasonal_comp <- decomp_stl$time.series[, "seasonal"]

autoplot(seasonal_comp) +
  xlab("An") +
  ylab("Componenta sezoniera") +
  ggtitle("Componenta sezoniera STL: Innoptari lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafic subserii sezoniere pe componenta STL
seasonal_ts <- ts(seasonal_comp, start = start(innoptari_train), frequency = 12)
ggsubseriesplot(seasonal_ts) +
  ylab("Efect sezonier") +
  ggtitle("Componenta sezoniera pe luni - STL") +
  theme_bw()

# INTERPRETARE:
# - Lunile iulie si august au efecte sezoniere POZITIVE mari
#   (mult peste media anuala)
# - Lunile ianuarie si februarie au efecte sezoniere NEGATIVE mari
#   (mult sub media anuala)
# - Efectul sezonier este stabil in timp 
#   => sezonalitate determinista, nu stochastica

# STL pentru Sosiri
decomp_stl_sosiri <- stl(sosiri_train, s.window = "periodic", robust = TRUE)
autoplot(decomp_stl_sosiri) +
  ggtitle("Descompunere STL: Sosiri lunare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# STL pentru Indice
decomp_stl_indice <- stl(indice_train, s.window = "periodic", robust = TRUE)
autoplot(decomp_stl_indice) +
  ggtitle("Descompunere STL: Indice utilizare - Romania") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
# INTERPRETARE:
# - Data: seria originala cu toate componentele
# - Trend: evolutia de fond - crestere, soc COVID, redresare
# - Seasonal: pattern sezonier lunar, stabil in timp
# - Remainder: ceea ce ramane dupa eliminarea trendului si sezonalitatii
#   => COVID-19 apare ca outlier in 2020

# 2.7. ANALIZA RELATIEI DINTRE SERII 

# Cream un data frame cu toate 3 seriile
date_all <- data.frame(
  Innoptari = as.numeric(innoptari_train),
  Sosiri    = as.numeric(sosiri_train),
  Indice    = as.numeric(indice_train)
)

# Matricea de scatterplot-uri + corelatii
install.packages("GGally")
library(GGally)
GGally::ggpairs(date_all,
                title = "Relatii dintre seriile turistice - Romania") +
  theme_bw()

# INTERPRETARE ASTEPTATA:
# - Corelatie puternica pozitiva intre Innoptari si Sosiri (0.968)
#   => turistii care sosesc se si cazeaza 
# - Corelatie pozitiva intre Indice si celelalte doua serii(0.969 , 0.930)
#   => cand vin mai multi turisti, gradul de ocupare creste
# - Relatii aproximativ liniare, dar cu dispersie (influenta sezonalitatii)


# 2.8. DETECTAREA PUNCTELOR DE SCHIMBARE (CHANGE POINTS)

# TEORIE (Seminar 2):
# Change point = moment in care proprietatile seriei se schimba (medie/varianta)
# Relevant pentru seria noastra: stim ca COVID-19 (2020) a generat
# un soc masiv. Verificam daca algoritmul il detecteaza automat.

# Change points in medie (BinSeg + penalizare BIC) 
m_binseg <- cpt.mean(as.numeric(innoptari_train),
                     penalty = "BIC",
                     method  = "BinSeg",
                     Q       = 5)

cat("\n=== PUNCTE DE SCHIMBARE IN MEDIE (innoptari) ===\n")
cat("Pozitii detectate (indici):", cpts(m_binseg), "\n")

# Convertim indicii in date calendaristice pentru interpretare
timp_innoptari <- time(innoptari_train)
cat("Datele corespunzatoare:", timp_innoptari[cpts(m_binseg)], "\n")

# Grafic cu punctele de schimbare
autoplot(innoptari_train) +
  geom_vline(xintercept = timp_innoptari[cpts(m_binseg)],
             colour = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari lunare - Puncte de schimbare in medie\nRomania (2010-2024)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# INTERPRETARE:
# Se observa doua zone distincte de ruptura structurala:
#
# ZONA 1 - Mai 2015:
#   Accelerarea cresterii turistice - nivelul mediu al innoptarilor
#   creste semnificativ, vârfurile sezoniere depasind 4 milioane.
#
# ZONA 2 - 2019-2021 (4 puncte grupate):
#   Surprind integral socul pandemic COVID-19:
#   intrarea in criza (2019), prabusirea (2020) si redresarea (2021).
#   Concentrarea punctelor confirma ca pandemia reprezinta
#   cea mai severa ruptura structurala din istoria recenta
#   a turismului romanesc.
#
# Aceste rezultate justifica tratarea anului 2020 ca outlier
# in procesul de modelare si prognoza.

date_innoptari <- read_excel("Date_complete.xlsx", sheet = 2)
date_sosiri    <- read_excel("Date_complete.xlsx", sheet = 3)

innoptari <- ts(date_innoptari[[2]], start = c(2010, 1), frequency = 12)
sosiri    <- ts(date_sosiri[[2]],    start = c(2010, 1), frequency = 12)

h <- 24
n <- length(innoptari)

innoptari_train <- window(innoptari, end = time(innoptari)[n - h])
innoptari_test  <- window(innoptari, start = time(innoptari)[n - h + 1])
sosiri_train    <- window(sosiri,    end = time(sosiri)[n - h])
sosiri_test     <- window(sosiri,    start = time(sosiri)[n - h + 1])

cat("Training:", length(innoptari_train), "obs. | Test:", length(innoptari_test), "obs.\n")
# Training: 170 obs. | Test: 24 obs.

# SECTIUNEA 3: STATIONARITATE
# ============================================================

# 3.1. Testarea stationaritatii - SERIA INNOPTARI
# ============================================================

# ADF - fara termen determinist
cat("\n--- ADF ---\n")
summary(ur.df(innoptari_train, type = "none", selectlags = "AIC"))
#Statistica testului este τ = −3.191, iar valorile critice sunt −2.58 (1%), −1.95 (5%) și −1.62 (10%). 
#Deoarece |−3.191| > |−1.95|, statistica testului depășește în valoare absolută pragul critic la 5%, ceea ce ar sugera respingerea ipotezei nule de rădăcină unitară. 
#Totuși, această specificație este inadecvată pentru seria înnoptărilor, deoarece exclude atât constanta, cât și trendul, în condițiile în care seria prezintă vizibil un nivel mediu nenul și o tendință crescătoare pe termen lung. 
#Prin urmare, concluziile din această specificație nu sunt fiabile și nu vor fi luate în considerare în decizia finală privind ordinul de integrare.

# ADF - cu drift (constanta)
cat("\n--- ADF (drift) ---\n")
summary(ur.df(innoptari_train, type = "drift", selectlags = "AIC"))
#Statistica testului este τ₂ = −8.102, iar valorile critice sunt −3.46 (1%), −2.88 (5%) și −2.57 (10%). 
#Deoarece |−8.102| > |−3.46|, respingem ipoteza nulă de rădăcină unitară la pragul de semnificație de 1% (***).
#Statistica φ₁ = 32.829 depășește cu mult valoarea critică de 6.52 la 1%, confirmând că termenul constant este semnificativ și necesar în specificație.

# ADF - cu trend
cat("\n--- ADF (trend) ---\n")
summary(ur.df(innoptari_train, type = "trend", selectlags = "AIC"))

# KPSS
cat("\n--- KPSS ---\n")
innoptari_train %>% ur.kpss() %>% summary()

# Phillips-Perron
cat("\n--- Phillips-Perron ---\n")
PP.test(innoptari_train)

# ============================================================
# 3.2. Testarea radacinii unitare sezoniere (HEGY)
# ============================================================

cat("\n--- Test HEGY (radacina unitara sezoniera) ---\n")
hegy.test(innoptari_train)

# INTERPRETARE:
# H0: exista radacina unitara sezoniera
# Daca nu respingem H0 => sezonalitate stochastica => D=1
# Daca respingem H0 => sezonalitate determinista => D=0

# Test Canova-Hansen (stabilitatea sezonalitatii)
cat("\n--- Test Canova-Hansen (stabilitate sezonalitate) ---\n")
ch.test(innoptari_train)

# INTERPRETARE:
# H0: sezonalitatea este stabila in timp
# Daca nu respingem H0 => sezonalitate determinista, stabila

# 3.3. Diferentierea seriei innoptari
# ============================================================

# Prima diferenta
cat("\n--- Corelograma dupa diferenta de ordin 1 ---\n")
ggtsdisplay(diff(innoptari_train),
            lag.max = 48,
            main = "Innoptari - dupa diferenta de ordin 1")

# Diferenta sezoniera (lag 12)
cat("\n--- Corelograma dupa diferenta sezoniera (lag 12) ---\n")
ggtsdisplay(diff(innoptari_train, lag = 12),
            lag.max = 48,
            main = "Innoptari - dupa diferenta sezoniera (D=1)")

# Diferenta de ordin 1 + diferenta sezoniera
cat("\n--- Corelograma dupa d=1 si D=1 ---\n")
innoptari_diff <- diff(diff(innoptari_train, lag = 12))
ggtsdisplay(innoptari_diff,
            lag.max = 48,
            main = "Innoptari - dupa d=1 si D=1")

# Retestare stationaritate dupa diferentiere
cat("\n--- ADF pe seria dublu-diferentiata ---\n")
summary(ur.df(innoptari_diff, type = "none", selectlags = "AIC"))
summary(ur.df(innoptari_diff, type = "drift", selectlags = "AIC"))

cat("\n--- KPSS pe seria dublu-diferentiata ---\n")
innoptari_diff %>% ur.kpss() %>% summary()

cat("\n--- PP pe seria dublu-diferentiata ---\n")
PP.test(innoptari_diff)

# Numarul de diferentieri sugerate automat
cat("\nndiffs innoptari:", ndiffs(innoptari_train), "\n")
cat("nsdiffs innoptari:", nsdiffs(innoptari_train), "\n")

# INTERPRETARE:
# dupa d=1 si D=1 seria devine stationara:
# => ADF respinge H0 => fara radacina unitara
# => KPSS nu respinge H0 => stationara
# => Concluzie: d=1, D=1 pentru modelul SARIMA

# 3.4. Testarea stationaritatii - SERIA SOSIRI

cat("3.4. STATIONARITATE - SOSIRI (in nivel)\n")

summary(ur.df(sosiri_train, type = "none", selectlags = "AIC"))
summary(ur.df(sosiri_train, type = "drift", selectlags = "AIC"))
summary(ur.df(sosiri_train, type = "trend", selectlags = "AIC"))
sosiri_train %>% ur.kpss() %>% summary()
PP.test(sosiri_train)

# HEGY si Canova-Hansen pentru sosiri
hegy.test(sosiri_train)
ch.test(sosiri_train)

# Diferentiere sosiri
sosiri_diff <- diff(diff(sosiri_train, lag = 12))
ggtsdisplay(sosiri_diff,
            lag.max = 48,
            main = "Sosiri - dupa d=1 si D=1")

summary(ur.df(sosiri_diff, type = "none", selectlags = "AIC"))
sosiri_diff %>% ur.kpss() %>% summary()
PP.test(sosiri_diff)

cat("\nndiffs sosiri:", ndiffs(sosiri_train), "\n")
cat("nsdiffs sosiri:", nsdiffs(sosiri_train), "\n")

# SECTIUNEA 4: NETEZIRE EXPONENTIALA (ETS)

cat("SECTIUNEA 4: MODELE ETS\n")

# 4.1. Holt-Winters - INNOPTARI

cat("\n--- 4.1. Holt-Winters aditiv si multiplicativ - INNOPTARI ---\n")

fit_hw_add_i <- hw(innoptari_train, seasonal = "additive",     h = h)
fit_hw_mul_i <- hw(innoptari_train, seasonal = "multiplicative", h = h)

# Grafic comparativ HW
autoplot(innoptari_train) +
  autolayer(fit_hw_add_i, series = "HW Aditiv",      PI = FALSE) +
  autolayer(fit_hw_mul_i, series = "HW Multiplicativ", PI = FALSE) +
  autolayer(innoptari_test, series = "Test (real)") +
  xlab("An") +
  ylab("Numar innoptari") +
  ggtitle("Innoptari: Holt-Winters aditiv vs multiplicativ") +
  guides(colour = guide_legend(title = "Model")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Rezumate
summary(fit_hw_add_i)
summary(fit_hw_mul_i)

# Acuratete in-sample
cat("\n--- Acuratete in-sample HW Aditiv (Innoptari) ---\n")
round(accuracy(fit_hw_add_i), 4)

cat("\n--- Acuratete in-sample HW Multiplicativ (Innoptari) ---\n")
round(accuracy(fit_hw_mul_i), 4)

# Acuratete out-of-sample
cat("\n--- Acuratete out-of-sample HW Aditiv (Innoptari) ---\n")
round(accuracy(fit_hw_add_i, innoptari_test), 4)

cat("\n--- Acuratete out-of-sample HW Multiplicativ (Innoptari) ---\n")
round(accuracy(fit_hw_mul_i, innoptari_test), 4)

# INTERPRETARE HW:
# alpha = netezirea NIVELULUI: valoare mare => reactioneaza rapid la date recente
# beta  = netezirea TRENDULUI: valoare mica => trend aproape constant
# gamma = netezirea SEZONALITATII: valoare mare => sezonalitate se actualizeaza rapid
# HW multiplicativ este de obicei preferat cand amplitudinea sezonalitatii
# creste odata cu nivelul seriei (cazul innoptarilor din Romania)

# 4.3. ETS automat - INNOPTARI

cat("\n--- 4.3. Model ETS automat - INNOPTARI ---\n")

fit_ets_i <- ets(innoptari_train)
summary(fit_ets_i)

# INTERPRETARE ETS(E, T, S):
# E = tipul erorii (A=aditiv, M=multiplicativ)
# T = tipul trendului (N=niciun trend, A=aditiv, Ad=amorsat)
# S = tipul sezonalitatii (N=fara, A=aditivă, M=multiplicativă)
# Functia ets() selecteaza automat combinatia optima pe baza AICc

autoplot(fit_ets_i) +
  ggtitle("Componentele modelului ETS - Innoptari") +
  theme_bw()

# Prognoza ETS pe setul de test
fc_ets_i <- forecast(fit_ets_i, h = h)

autoplot(innoptari_train) +
  autolayer(fc_ets_i,       series = "ETS (prognoza)") +
  autolayer(innoptari_test, series = "Test (real)") +
  xlab("An") + ylab("Numar innoptari") +
  ggtitle("Innoptari: Prognoza ETS vs valori reale") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Acuratete ETS
cat("\n--- Acuratete out-of-sample ETS (Innoptari) ---\n")
round(accuracy(fc_ets_i, innoptari_test), 4)

# Diagnostic ETS
checkresiduals(fit_ets_i)

# 4.4. HW si ETS - SOSIRI

cat("\n--- 4.4. Holt-Winters si ETS - SOSIRI ---\n")

fit_hw_add_s <- hw(sosiri_train, seasonal = "additive",      h = h)
fit_hw_mul_s <- hw(sosiri_train, seasonal = "multiplicative", h = h)
fit_ets_s    <- ets(sosiri_train)

summary(fit_hw_add_s)
summary(fit_hw_mul_s)
summary(fit_ets_s)

# Grafic comparativ - Sosiri
fc_ets_s <- forecast(fit_ets_s, h = h)

autoplot(sosiri_train) +
  autolayer(fit_hw_add_s, series = "HW Aditiv",      PI = FALSE) +
  autolayer(fit_hw_mul_s, series = "HW Multiplicativ", PI = FALSE) +
  autolayer(fc_ets_s,     series = "ETS",             PI = FALSE) +
  autolayer(sosiri_test,  series = "Test (real)") +
  xlab("An") + ylab("Numar sosiri") +
  ggtitle("Sosiri: comparatie modele ETS pe setul de test") +
  guides(colour = guide_legend(title = "Model")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Acuratete out-of-sample sosiri
cat("\n--- Acuratete out-of-sample HW Aditiv (Sosiri) ---\n")
round(accuracy(fit_hw_add_s, sosiri_test), 4)

cat("\n--- Acuratete out-of-sample HW Multiplicativ (Sosiri) ---\n")
round(accuracy(fit_hw_mul_s, sosiri_test), 4)

cat("\n--- Acuratete out-of-sample ETS (Sosiri) ---\n")
round(accuracy(fc_ets_s, sosiri_test), 4)

# Diagnostic
checkresiduals(fit_hw_mul_s)
checkresiduals(fit_ets_s)

# SECTIUNEA 5: MODELE SARIMA

cat("SECTIUNEA 5: MODELE SARIMA\n")

# d = 1 (diferentiere ordinara)
# D = 1 (diferentiere sezoniera, lag 12)
# Modelul final va fi SARIMA(p,1,q)(P,1,Q)[12]

# 5.1. Identificarea ordinelor SARIMA - INNOPTARI

cat("\n--- 5.1. Identificare ordine SARIMA - INNOPTARI ---\n")

# Corelograma pe seria dublu-diferentiata pentru identificarea p,q,P,Q
ggtsdisplay(innoptari_diff, lag.max = 48,
            main = "Innoptari dublu-diferentiate: identificare SARIMA")

# INTERPRETARE corelogramei:
# Laguri nesazoniere (1-11):
#   - ACF: spike-uri semnificative la lag 1-3 => MA(q), q=1,2,3 posibil
#   - PACF: spike-uri la lag 1-3 => AR(p), p=1,2,3 posibil
# Laguri sezoniere (12, 24, 36):
#   - ACF la lag 12: spike semnificativ => SMA(Q), Q=1 posibil
#   - PACF la lag 12-24: spike-uri => SAR(P), P=1,2 posibil

# 5.2. Estimarea si compararea modelelor SARIMA - INNOPTARI

cat("\n--- 5.2. Modele SARIMA candidate - INNOPTARI ---\n")

# Modelul 1: SARIMA(1,1,1)(1,1,1)[12]
fit_s1_i <- Arima(innoptari_train,
                  order    = c(1, 1, 1),
                  seasonal = c(1, 1, 1))
cat("\nSARIMA(1,1,1)(1,1,1)[12]:\n")
coeftest(fit_s1_i)


# Modelul 2: SARIMA(0,1,1)(0,1,1)[12] - clasic pentru serii turistice
fit_s2_i <- Arima(innoptari_train,
                  order    = c(0, 1, 1),
                  seasonal = c(0, 1, 1))
cat("\nSARIMA(0,1,1)(0,1,1)[12]:\n")
coeftest(fit_s2_i)

# Modelul 3: SARIMA(1,1,0)(1,1,0)[12]
fit_s3_i <- Arima(innoptari_train,
                  order    = c(1, 1, 0),
                  seasonal = c(1, 1, 0))
cat("\nSARIMA(1,1,0)(1,1,0)[12]:\n")
coeftest(fit_s3_i)

# Modelul 4: SARIMA(2,1,2)(0,1,1)[12]
fit_s4_i <- Arima(innoptari_train,
                  order    = c(2, 1, 2),
                  seasonal = c(0, 1, 1))
cat("\nSARIMA(2,1,2)(0,1,1)[12]:\n")
coeftest(fit_s4_i)

# Modelul 5: SARIMA(1,1,2)(1,1,1)[12]
fit_s5_i <- Arima(innoptari_train,
                  order    = c(1, 1, 2),
                  seasonal = c(1, 1, 1))
cat("\nSARIMA(1,1,2)(1,1,1)[12]:\n")
coeftest(fit_s5_i)

# Auto.ARIMA pentru referinta
cat("\n--- Auto.ARIMA (referinta) ---\n")
fit_auto_i <- auto.arima(innoptari_train,
                         seasonal     = TRUE,
                         stepwise     = FALSE,
                         approximation = FALSE)
summary(fit_auto_i)

# 5.3. Compararea modelelor SARIMA pe baza criteriilor

cat("\n--- 5.3. Comparare criterii informationale SARIMA - INNOPTARI ---\n")

sarima_comp_i <- data.frame(
  Model = c("SARIMA(1,1,1)(1,1,1)",
            "SARIMA(0,1,1)(0,1,1)",
            "SARIMA(1,1,0)(1,1,0)",
            "SARIMA(2,1,2)(0,1,1)",
            "SARIMA(1,1,2)(1,1,1)",
            "auto.arima"),
  AIC   = c(AIC(fit_s1_i), AIC(fit_s2_i), AIC(fit_s3_i),
            AIC(fit_s4_i), AIC(fit_s5_i), AIC(fit_auto_i)),
  BIC   = c(BIC(fit_s1_i), BIC(fit_s2_i), BIC(fit_s3_i),
            BIC(fit_s4_i), BIC(fit_s5_i), BIC(fit_auto_i)),
  RMSE_insample = c(
    accuracy(fit_s1_i)[, "RMSE"],
    accuracy(fit_s2_i)[, "RMSE"],
    accuracy(fit_s3_i)[, "RMSE"],
    accuracy(fit_s4_i)[, "RMSE"],
    accuracy(fit_s5_i)[, "RMSE"],
    accuracy(fit_auto_i)[, "RMSE"]
  )
)

print(data.frame(
  Model = sarima_comp_i$Model,
  round(sarima_comp_i[, c("AIC", "BIC", "RMSE_insample")], 2)
))
# INTERPRETARE: alegem modelul cu AIC/BIC minim SI cu coeficienti semnificativi

# 5.4. Diagnosticul modelului SARIMA ales - INNOPTARI

# Alegem modelul optim pe baza criteriilor de mai sus
# (ajustati fit_sarima_opt_i cu AIC cel mai mic si coef. semnif.)
fit_sarima_opt_i <- fit_s2_i  # SARIMA(0,1,1)(0,1,1) - model clasic

cat("\n--- 5.4. Diagnostic reziduuri SARIMA optim - INNOPTARI ---\n")
cat("\n--- Rezumat model ales ---\n")
summary(fit_sarima_opt_i)

# Testul inversabilitatii/stationaritatii
autoplot(fit_sarima_opt_i) +
  ggtitle("Radacini inverse SARIMA - Innoptari") +
  theme_bw()

# ACF reziduuri
res_sarima_i <- residuals(fit_sarima_opt_i)
ggAcf(res_sarima_i, lag.max = 48) +
  ggtitle("ACF reziduuri SARIMA - Innoptari") +
  theme_bw()

# Ljung-Box
cat("\n--- Test Ljung-Box SARIMA (Innoptari) ---\n")
Box.test(res_sarima_i, lag = 12, type = "Lj")
Box.test(res_sarima_i, lag = 24, type = "Lj")
Box.test(res_sarima_i, lag = 36, type = "Lj")

# Normalitate
cat("\n--- Jarque-Bera SARIMA (Innoptari) ---\n")
jarque.bera.test(res_sarima_i)

# Heteroscedasticitate conditionata
cat("\n--- ARCH-LM SARIMA (Innoptari) ---\n")
ArchTest(res_sarima_i, lags = 12)
ArchTest(res_sarima_i, lags = 24)

# checkresiduals complet
checkresiduals(fit_sarima_opt_i)

# 5.5. Prognoza SARIMA - INNOPTARI

cat("\n--- 5.5. Prognoza SARIMA - INNOPTARI ---\n")

fc_sarima_i <- forecast(fit_sarima_opt_i, h = h)

# Grafic prognoza SARIMA cu intervale de incredere
autoplot(fc_sarima_i) +
  autolayer(innoptari_test, series = "Test (real)") +
  xlab("An") + ylab("Numar innoptari") +
  ggtitle("Innoptari: Prognoza SARIMA cu intervale de incredere 80% si 95%") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Acuratete out-of-sample SARIMA
cat("\n--- Acuratete out-of-sample SARIMA (Innoptari) ---\n")
round(accuracy(fc_sarima_i, innoptari_test), 4)

# 5.6. Modele SARIMA - SOSIRI

cat("\n--- 5.6. Modele SARIMA - SOSIRI ---\n")

# Identificare pe corelograma
ggtsdisplay(sosiri_diff, lag.max = 48,
            main = "Sosiri dublu-diferentiate: identificare SARIMA")

# Modele candidate
fit_s1_s <- Arima(sosiri_train,
                  order    = c(0, 1, 1),
                  seasonal = c(0, 1, 1))
cat("\nSARIMA(0,1,1)(0,1,1)[12] - Sosiri:\n")
coeftest(fit_s1_s)
summary(fit_s1_s)

fit_s2_s <- Arima(sosiri_train,
                  order    = c(1, 1, 1),
                  seasonal = c(1, 1, 1))
cat("\nSARIMA(1,1,1)(1,1,1)[12] - Sosiri:\n")
coeftest(fit_s2_s)
summary(fit_s2_s)

fit_s3_s <- Arima(sosiri_train,
                  order    = c(2, 1, 2),
                  seasonal = c(0, 1, 1))
cat("\nSARIMA(2,1,2)(0,1,1)[12] - Sosiri:\n")
coeftest(fit_s3_s)
summary(fit_s3_s)

fit_auto_s <- auto.arima(sosiri_train,
                         seasonal      = TRUE,
                         stepwise      = FALSE,
                         approximation = FALSE)
cat("\nauto.arima - Sosiri:\n")
summary(fit_auto_s)

# Alegem modelul optim pentru sosiri (modificati dupa rezultate)
fit_sarima_opt_s <- fit_s1_s  # ajustati dupa criteriile informationale

# Diagnostic
checkresiduals(fit_sarima_opt_s)
Box.test(residuals(fit_sarima_opt_s), lag = 12, type = "Lj")
Box.test(residuals(fit_sarima_opt_s), lag = 24, type = "Lj")
jarque.bera.test(residuals(fit_sarima_opt_s))
ArchTest(residuals(fit_sarima_opt_s), lags = 12)

# Prognoza SARIMA Sosiri
fc_sarima_s <- forecast(fit_sarima_opt_s, h = h)

autoplot(fc_sarima_s) +
  autolayer(sosiri_test, series = "Test (real)") +
  xlab("An") + ylab("Numar sosiri") +
  ggtitle("Sosiri: Prognoza SARIMA cu intervale de incredere") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cat("\n--- Acuratete out-of-sample SARIMA (Sosiri) ---\n")
round(accuracy(fc_sarima_s, sosiri_test), 4)

# 5.7. COMPARATIE ETS vs SARIMA
cat("5.7. COMPARATIE ETS vs SARIMA\n")

# Tabel sinteza acuratete - INNOPTARI

cat("\n--- Tabel comparativ acuratete out-of-sample - INNOPTARI ---\n")

acc_hw_add_i <- accuracy(fit_hw_add_i,      innoptari_test)
acc_hw_mul_i <- accuracy(fit_hw_mul_i,      innoptari_test)
acc_ets_i    <- accuracy(fc_ets_i,          innoptari_test)
acc_sarima_i <- accuracy(fc_sarima_i,       innoptari_test)

tabel_acc_i <- rbind(
  "HW Aditiv"      = acc_hw_add_i[2, c("RMSE", "MAE", "MAPE", "MASE")],
  "HW Multiplicativ" = acc_hw_mul_i[2, c("RMSE", "MAE", "MAPE", "MASE")],
  "ETS"            = acc_ets_i[2,    c("RMSE", "MAE", "MAPE", "MASE")],
  "SARIMA"         = acc_sarima_i[2, c("RMSE", "MAE", "MAPE", "MASE")]
)

print(round(tabel_acc_i, 4))

# INTERPRETARE:
# RMSE: eroarea patratica medie - penalizeaza erorile mari
# MAE:  eroarea absoluta medie - mai robusta la outlieri
# MAPE: eroarea procentuala medie - <=10% excelent, 10-20% bun
# MASE: < 1 => model mai bun decat prognoza naiva

# ============================================================
# Tabel sinteza acuratete - SOSIRI
# ============================================================

cat("\n--- Tabel comparativ acuratete out-of-sample - SOSIRI ---\n")

acc_hw_add_s <- accuracy(fit_hw_add_s,  sosiri_test)
acc_hw_mul_s <- accuracy(fit_hw_mul_s,  sosiri_test)
acc_ets_s2   <- accuracy(fc_ets_s,      sosiri_test)
acc_sarima_s <- accuracy(fc_sarima_s,   sosiri_test)

tabel_acc_s <- rbind(
  "HW Aditiv"        = acc_hw_add_s[2, c("RMSE", "MAE", "MAPE", "MASE")],
  "HW Multiplicativ" = acc_hw_mul_s[2, c("RMSE", "MAE", "MAPE", "MASE")],
  "ETS"              = acc_ets_s2[2,   c("RMSE", "MAE", "MAPE", "MASE")],
  "SARIMA"           = acc_sarima_s[2, c("RMSE", "MAE", "MAPE", "MASE")]
)

print(round(tabel_acc_s, 4))

# ============================================================
# Test Diebold-Mariano (ETS vs SARIMA) - INNOPTARI
# ============================================================

cat("\n--- Test Diebold-Mariano: ETS vs SARIMA - INNOPTARI ---\n")

# Erorile de prognoza out-of-sample
err_ets_i    <- as.numeric(innoptari_test) - as.numeric(fc_ets_i$mean)
err_sarima_i <- as.numeric(innoptari_test) - as.numeric(fc_sarima_i$mean)

dm.test(err_ets_i, err_sarima_i, h = 1)

# H0: ETS si SARIMA au aceeasi acuratete de prognoza
# Daca p < 0.05 => diferente semnificative => alegem modelul cu RMSE mai mic

# ============================================================
# Test Diebold-Mariano - SOSIRI
# ============================================================

cat("\n--- Test Diebold-Mariano: ETS vs SARIMA - SOSIRI ---\n")

err_ets_s    <- as.numeric(sosiri_test) - as.numeric(fc_ets_s$mean)
err_sarima_s <- as.numeric(sosiri_test) - as.numeric(fc_sarima_s$mean)

dm.test(err_ets_s, err_sarima_s, h = 1)

# ============================================================
# Grafic comparativ final (modelul ales) - INNOPTARI
# ============================================================

# Folosim modelul cu performanta mai buna (alegeti dupa rezultate)
# Exemplu: daca SARIMA are RMSE mai mic => il afisam

autoplot(innoptari_train) +
  autolayer(fc_ets_i,       series = "ETS",    PI = FALSE) +
  autolayer(fc_sarima_i,    series = "SARIMA", PI = FALSE) +
  autolayer(innoptari_test, series = "Observat") +
  xlab("An") + ylab("Numar innoptari") +
  ggtitle("Innoptari: comparatie prognoza ETS vs SARIMA pe setul de test") +
  guides(colour = guide_legend(title = "Model")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafic comparativ final - SOSIRI
autoplot(sosiri_train) +
  autolayer(fc_ets_s,     series = "ETS",    PI = FALSE) +
  autolayer(fc_sarima_s,  series = "SARIMA", PI = FALSE) +
  autolayer(sosiri_test,  series = "Observat") +
  xlab("An") + ylab("Numar sosiri") +
  ggtitle("Sosiri: comparatie prognoza ETS vs SARIMA pe setul de test") +
  guides(colour = guide_legend(title = "Model")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# SECTIUNEA 6: ANALIZA MULTIVARIATA - VAR / VECM

cat("SECTIUNEA 6: ANALIZA MULTIVARIATA VAR / VECM\n")

# Justificare: innoptarile si sosirile sunt doua variabile strans legate
# economic - turistii care sosesc genereaza innoptari, iar ambele serii
# reflecta aceeasi cerere turistica agregata. Analiza multivariata permite:
# - identificarea relatiei de echilibru pe termen lung
# - testarea directiei cauzale
# - prognoze mai precise prin utilizarea informatiei din ambele serii

# ============================================================
# 6.1. Pregatirea datelor pentru analiza multivariata
# ============================================================

# Folosim seriile complete (training) pentru analiza multivariata
# Log-uri pentru stabilizarea variantei si interpretare ca elasticitati
log_inn_train <- log(innoptari_train)
log_sos_train <- log(sosiri_train)

Y_train <- cbind(Innoptari = log_inn_train, Sosiri = log_sos_train)

# Grafic comparativ log-serii
autoplot(Y_train, facets = TRUE) +
  xlab("An") + ylab("Log(valoare)") +
  ggtitle("Innoptari si Sosiri (log) - Romania (2010-2024)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Corelatia dintre serii
cat("\n--- Corelatia dintre log(Innoptari) si log(Sosiri) ---\n")
cor(as.numeric(log_inn_train), as.numeric(log_sos_train))

# ============================================================
# 6.2. Testarea stationaritatii pe log-serii
# ============================================================

cat("\n--- 6.2. Stationaritate log(Innoptari) ---\n")
summary(ur.df(log_inn_train, type = "drift", selectlags = "AIC"))
log_inn_train %>% ur.kpss() %>% summary()

cat("\n--- 6.2. Stationaritate log(Sosiri) ---\n")
summary(ur.df(log_sos_train, type = "drift", selectlags = "AIC"))
log_sos_train %>% ur.kpss() %>% summary()

cat("\n--- 6.2. Stationaritate diff(log(Innoptari)) ---\n")
summary(ur.df(diff(log_inn_train), type = "none", selectlags = "AIC"))
diff(log_inn_train) %>% ur.kpss() %>% summary()

cat("\n--- 6.2. Stationaritate diff(log(Sosiri)) ---\n")
summary(ur.df(diff(log_sos_train), type = "none", selectlags = "AIC"))
diff(log_sos_train) %>% ur.kpss() %>% summary()

# INTERPRETARE:
# Ambele serii in log sunt I(1): nestationare in nivel, stationare dupa diff
# => Conditie necesara pentru testul de cointegrare Johansen

# ============================================================
# 6.3. Selectia lagului optim pentru VAR
# ============================================================

cat("\n--- 6.3. Selectia lagului optim (VAR) ---\n")

lag_sel <- VARselect(Y_train, lag.max = 12, type = "const")
print(lag_sel)

# Lagul ales (BIC/SC pentru parsimonie, AIC pentru completitudine)
p_bic <- as.numeric(lag_sel$selection["SC(n)"])
p_aic <- as.numeric(lag_sel$selection["AIC(n)"])
cat("Lag BIC:", p_bic, "| Lag AIC:", p_aic, "\n")

# Folosim BIC 
p_opt <- p_bic
cat("Lag ales:", p_opt, "\n")

# ============================================================
# 6.4. Testul de cointegrare Johansen
# ============================================================

cat("\n--- 6.4. Test Johansen (trace) ---\n")
johansen_trace <- ca.jo(Y_train,
                        type  = "trace",
                        ecdet = "const",
                        K     = max(p_opt, 2))  # minim 2 laguri
summary(johansen_trace)

cat("\n--- 6.4. Test Johansen (maxeigen) ---\n")
johansen_max <- ca.jo(Y_train,
                      type  = "eigen",
                      ecdet = "const",
                      K     = max(p_opt, 2))
summary(johansen_max)

# INTERPRETARE:
# Citim tabelul de la r=0 in sus:
# r=0: daca test > valoare critica => respingem H0 => cel putin 1 relatie de cointegrare
# r=1: daca test < valoare critica => nu respingem H0 => exact 1 relatie de cointegrare
#
# Innoptarile si sosirile turistice sunt puternic legate economic:
# ambele masoara cererea turistica agregata, deci ne asteptam la cointegrare.

# Setam rangul de cointegrare pe baza rezultatelor testului
# (modificati r_coint la 0 daca testul nu confirma cointegrarea)
r_coint <- 1   # ajustati dupa rezultate

cat("\nRang de cointegrare ales:", r_coint, "\n")

# ============================================================
# 6.5. Ramura A: VECM (daca seriile SUNT cointegrate, r_coint >= 1)
# ============================================================

if (r_coint >= 1) {
  
  cat("\n======================================================\n")
  cat("6.5. VECM (serii cointegrate)\n")
  cat("======================================================\n")
  
  # Estimarea VECM
  vecm_model <- cajorls(johansen_trace, r = r_coint)
  cat("\n--- Rezultate VECM ---\n")
  print(vecm_model)
  
  cat("\n--- Vectorul de cointegrare (beta) ---\n")
  print(vecm_model$beta)
  
  # INTERPRETARE beta:
  # Relatia de echilibru pe termen lung (normalizata):
  # log(Innoptari) = a + b * log(Sosiri)
  # Coeficientul b reprezinta elasticitatea pe termen lung:
  # o crestere de 1% a sosirilor => b% crestere a innoptarilor
  
  cat("\n--- Coeficientii de ajustare (alpha) ---\n")
  alpha_matrix <- coef(vecm_model$rlm)
  print(alpha_matrix)
  
  # Alpha = viteza de ajustare catre echilibrul pe termen lung
  # Alpha negativ si semnificativ => variabila se ajusteaza activ
  # Alpha nesemnificativ => variabila este "slaba" (exogena pe TL)
  
  # Rezumat detaliat al ecuatiilor VECM
  cat("\n--- Ecuatia VECM pentru Innoptari ---\n")
  summary(vecm_model$rlm)[[1]]
  
  cat("\n--- Ecuatia VECM pentru Sosiri ---\n")
  summary(vecm_model$rlm)[[2]]
  
  # Conversia VECM -> VAR in niveluri (pentru IRF, FEVD, Granger)
  var_din_vecm <- vec2var(johansen_trace, r = r_coint)
  
  # Termenul de corectie al erorii (ECT)
  ect_ts <- ts(vecm_model$rlm$model$ect1,
               start     = time(log_inn_train)[max(p_opt, 2) + 2],
               frequency = 12)
  
  autoplot(ect_ts) +
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
    xlab("An") + ylab("ECT") +
    ggtitle("Termenul de corectie al erorii (ECT)\nInnoptari - Sosiri turistice") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Test stationaritate ECT (confirmare formala a cointegrarii)
  cat("\n--- Stationaritate ECT (confirmare cointegrare) ---\n")
  summary(ur.df(ect_ts, type = "drift", selectlags = "AIC"))
  
  # INTERPRETARE ECT:
  # ECT trebuie sa fie stationar (ADF respinge H0) pentru a confirma cointegrarea
  # ECT oscileaza in jurul valorii 0 => deviatiile de la echilibru sunt tranzitorii
  
  # Diagnosticul modelului VECM
  cat("\n--- Diagnostic VECM: test autocorelare reziduuri (Portmanteau) ---\n")
  serial.test(var_din_vecm, lags.pt = max(p_opt, 2) + 12,
              type = "PT.asymptotic")
  
  cat("\n--- Diagnostic VECM: test ARCH multivariate ---\n")
  arch.test(var_din_vecm, lags.multi = 12)
  
  cat("\n--- Diagnostic VECM: test normalitate reziduuri ---\n")
  normality.test(var_din_vecm)
  
  # ============================================================
  # 6.6. Cauzalitate Granger (in cadrul VECM)
  # ============================================================
  
  cat("\n--- 6.6. Cauzalitate Granger (VECM pe log-uri) ---\n")
  
  # Identificam pozitiile variabilelor in matricea de coeficienti
  coef_mat_v  <- coef(vecm_model$rlm)
  idx_inn_v   <- grep("^Innoptari\\.dl", rownames(coef_mat_v))
  idx_sos_v   <- grep("^Sosiri\\.dl",    rownames(coef_mat_v))
  
  cat("Laguri Innoptari:", rownames(coef_mat_v)[idx_inn_v], "\n")
  cat("Laguri Sosiri:   ", rownames(coef_mat_v)[idx_sos_v], "\n")
  
  # Functie Granger pe obiect mlm
  granger_mlm <- function(model_mlm, idx_regresori, eq_name) {
    b        <- coef(model_mlm)[, eq_name]
    V        <- vcov(model_mlm)
    n_coef   <- length(b)
    eq_names <- colnames(coef(model_mlm))
    eq_pos   <- which(eq_names == eq_name)
    row_idx  <- ((eq_pos - 1) * n_coef + 1):(eq_pos * n_coef)
    V_eq     <- V[row_idx, row_idx]
    b_sub    <- b[idx_regresori]
    V_sub    <- V_eq[idx_regresori, idx_regresori]
    W        <- as.numeric(t(b_sub) %*% solve(V_sub) %*% b_sub)
    df       <- length(idx_regresori)
    pval     <- pchisq(W, df = df, lower.tail = FALSE)
    cat("Chi2 =", round(W, 4), "| df =", df,
        "| p-value =", round(pval, 4),
        ifelse(pval < 0.01, "***",
               ifelse(pval < 0.05, "**",
                      ifelse(pval < 0.10, ".", ""))), "\n")
    invisible(list(statistic = W, df = df, p.value = pval))
  }
  
  if (length(idx_inn_v) > 0 && length(idx_sos_v) > 0) {
    cat("\nH0: log(Sosiri) NU Granger-cauzeaza log(Innoptari) -> ")
    granger_mlm(vecm_model$rlm, idx_sos_v, "Innoptari.d")
    
    cat("H0: log(Innoptari) NU Granger-cauzeaza log(Sosiri) -> ")
    granger_mlm(vecm_model$rlm, idx_inn_v, "Sosiri.d")
  }
  
  # INTERPRETARE Granger:
  # Cauzalitate pe termen scurt = lagurile diferentiate ale X
  # prezic semnificativ Y (dincolo de propria sa inertie)
  # Cauzalitate pe termen lung = prin termenul ECT (coeficient alpha)
  # In contextul turismului, ne asteptam la o relatie bidirectionala:
  # mai multi turisti sositi => mai multe innoptari (si viceversa)
  
  # ============================================================
  # 6.7. Functii de raspuns la impuls (IRF)
  # ============================================================
  
  cat("\n--- 6.7. Functii de raspuns la impuls (IRF) ---\n")
  
  set.seed(42)
  n_ahead_irf <- 24  # 2 ani
  
  # IRF complet (toate combinatiile de soc-raspuns)
  irf_all <- irf(var_din_vecm,
                 n.ahead = n_ahead_irf,
                 boot    = TRUE,
                 ci      = 0.95,
                 ortho   = TRUE,
                 runs    = 300)
  
  plot(irf_all,
       main = "Functii de raspuns la impuls - Innoptari & Sosiri")
  
  # IRF specific: soc in Sosiri -> raspuns in Innoptari
  irf_sos_la_inn <- irf(var_din_vecm,
                        impulse  = "Sosiri",
                        response = "Innoptari",
                        n.ahead  = n_ahead_irf,
                        boot     = TRUE,
                        ci       = 0.95,
                        ortho    = TRUE,
                        runs     = 300)
  
  plot(irf_sos_la_inn,
       main = "IRF: raspunsul log(Innoptari) la un soc in log(Sosiri)")
  
  # IRF: soc in Innoptari -> raspuns in Sosiri
  irf_inn_la_sos <- irf(var_din_vecm,
                        impulse  = "Innoptari",
                        response = "Sosiri",
                        n.ahead  = n_ahead_irf,
                        boot     = TRUE,
                        ci       = 0.95,
                        ortho    = TRUE,
                        runs     = 300)
  
  plot(irf_inn_la_sos,
       main = "IRF: raspunsul log(Sosiri) la un soc in log(Innoptari)")
  
  # INTERPRETARE IRF:
  # Un soc pozitiv de 1% in sosiri genereaza un raspuns pozitiv si persistent
  # in innoptari, reflectand relatia de echilibru pe termen lung.
  # Caracterul persistent (non-zero pe termen lung) confirma cointegrarea:
  # socurile nu se disipeaza complet, ci converg spre un nou nivel de echilibru.
  # Daca banda de incredere exclude zero => efect semnificativ statistic
  
  # ============================================================
  # 6.8. Decompunerea variantei erorii de prognoza (FEVD)
  # ============================================================
  
  cat("\n--- 6.8. FEVD ---\n")
  
  fevd_res <- fevd(var_din_vecm, n.ahead = 24)
  
  cat("\n--- FEVD pentru log(Innoptari) ---\n")
  print(round(as.data.frame(fevd_res$Innoptari), 4))
  
  cat("\n--- FEVD pentru log(Sosiri) ---\n")
  print(round(as.data.frame(fevd_res$Sosiri), 4))
  
  # Grafic FEVD standard
  plot(fevd_res,
       main = "FEVD - Innoptari si Sosiri turistice")
  
  # INTERPRETARE FEVD:
  # Orizont 1: innoptarile sunt explicate 100% de socuri proprii (prin constructia Cholesky)
  # Orizont lung: daca sosirile explica X% din variatia innoptarilor => interdependenta
  # In contextul turismului: ne asteptam ca sosirile sa explice o parte importanta
  # din variatia innoptarilor pe termen mediu si lung
  
  # ============================================================
  # 6.9. Prognoza VECM
  # ============================================================
  
  cat("\n--- 6.9. Prognoza VECM ---\n")
  
  fc_vecm <- predict(var_din_vecm, n.ahead = h, ci = 0.95)
  print(fc_vecm)
  
  # Extragem prognozele in log
  pred_inn_log <- fc_vecm$fcst$Innoptari[, 1]
  pred_sos_log <- fc_vecm$fcst$Sosiri[, 1]
  
  # Benzi de incredere in log
  pred_inn_lower_log <- fc_vecm$fcst$Innoptari[, 2]
  pred_inn_upper_log <- fc_vecm$fcst$Innoptari[, 3]
  pred_sos_lower_log <- fc_vecm$fcst$Sosiri[, 2]
  pred_sos_upper_log <- fc_vecm$fcst$Sosiri[, 3]
  
  # Reconversie din log in nivel (valori originale)
  pred_inn_niv       <- exp(pred_inn_log)
  pred_inn_lower_niv <- exp(pred_inn_lower_log)
  pred_inn_upper_niv <- exp(pred_inn_upper_log)
  
  pred_sos_niv       <- exp(pred_sos_log)
  pred_sos_lower_niv <- exp(pred_sos_lower_log)
  pred_sos_upper_niv <- exp(pred_sos_upper_log)
  
  # Vectorul de timp pentru prognoza
  last_time   <- as.numeric(time(innoptari_train)[length(innoptari_train)])
  future_time <- seq(from = last_time + 1/12, by = 1/12, length.out = h)
  
  # Tabel prognoza
  cat("\n--- Tabel prognoza VECM (innoptari in nivel) ---\n")
  tabel_fc <- data.frame(
    Luna              = format(as.yearmon(future_time), "%b %Y"),
    Innoptari_fc      = round(pred_inn_niv),
    Innoptari_lower95 = round(pred_inn_lower_niv),
    Innoptari_upper95 = round(pred_inn_upper_niv),
    Sosiri_fc         = round(pred_sos_niv),
    Sosiri_lower95    = round(pred_sos_lower_niv),
    Sosiri_upper95    = round(pred_sos_upper_niv)
  )
  print(tabel_fc)
  
  # Grafic prognoza VECM - Innoptari
  df_hist_inn <- data.frame(
    timp    = as.yearmon(time(innoptari_train)),
    valoare = as.numeric(innoptari_train),
    tip     = "Observat"
  )
  df_test_inn <- data.frame(
    timp    = as.yearmon(time(innoptari_test)),
    valoare = as.numeric(innoptari_test),
    tip     = "Test (real)"
  )
  df_fc_inn <- data.frame(
    timp    = as.yearmon(future_time),
    valoare = pred_inn_niv,
    lower   = pred_inn_lower_niv,
    upper   = pred_inn_upper_niv
  )
  
  ggplot() +
    geom_line(data = df_hist_inn,
              aes(x = timp, y = valoare, colour = "Observat"), linewidth = 0.7) +
    geom_line(data = df_test_inn,
              aes(x = timp, y = valoare, colour = "Test (real)"), linewidth = 0.7) +
    geom_ribbon(data = df_fc_inn,
                aes(x = timp, ymin = lower, ymax = upper),
                fill = "lightblue", alpha = 0.4) +
    geom_line(data = df_fc_inn,
              aes(x = timp, y = valoare, colour = "Prognoza VECM"), linewidth = 1.1) +
    geom_vline(xintercept = as.numeric(as.yearmon(min(future_time))),
               linetype = "dashed", colour = "gray50") +
    scale_colour_manual(values = c("Observat"      = "black",
                                   "Test (real)"   = "red",
                                   "Prognoza VECM" = "steelblue")) +
    xlab("An") + ylab("Numar innoptari") +
    ggtitle("Innoptari: Prognoza VECM cu interval de incredere 95%") +
    guides(colour = guide_legend(title = "Serie")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Grafic prognoza VECM - Sosiri
  df_hist_sos <- data.frame(
    timp    = as.yearmon(time(sosiri_train)),
    valoare = as.numeric(sosiri_train)
  )
  df_test_sos <- data.frame(
    timp    = as.yearmon(time(sosiri_test)),
    valoare = as.numeric(sosiri_test)
  )
  df_fc_sos <- data.frame(
    timp    = as.yearmon(future_time),
    valoare = pred_sos_niv,
    lower   = pred_sos_lower_niv,
    upper   = pred_sos_upper_niv
  )
  
  ggplot() +
    geom_line(data = df_hist_sos,
              aes(x = timp, y = valoare, colour = "Observat"), linewidth = 0.7) +
    geom_line(data = df_test_sos,
              aes(x = timp, y = valoare, colour = "Test (real)"), linewidth = 0.7) +
    geom_ribbon(data = df_fc_sos,
                aes(x = timp, ymin = lower, ymax = upper),
                fill = "lightblue", alpha = 0.4) +
    geom_line(data = df_fc_sos,
              aes(x = timp, y = valoare, colour = "Prognoza VECM"), linewidth = 1.1) +
    geom_vline(xintercept = as.numeric(as.yearmon(min(future_time))),
               linetype = "dashed", colour = "gray50") +
    scale_colour_manual(values = c("Observat"      = "black",
                                   "Test (real)"   = "red",
                                   "Prognoza VECM" = "steelblue")) +
    xlab("An") + ylab("Numar sosiri") +
    ggtitle("Sosiri: Prognoza VECM cu interval de incredere 95%") +
    guides(colour = guide_legend(title = "Serie")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Evaluare out-of-sample VECM
  cat("\n--- Acuratete out-of-sample VECM (Innoptari) ---\n")
  
  err_vecm_inn <- as.numeric(innoptari_test) - pred_inn_niv
  err_vecm_sos <- as.numeric(sosiri_test)    - pred_sos_niv
  
  eval_oos <- function(actual, forecast, label) {
    err  <- actual - forecast
    cat(label, "\n")
    cat("  RMSE:", round(sqrt(mean(err^2)), 2), "\n")
    cat("  MAE: ", round(mean(abs(err)), 2), "\n")
    cat("  MAPE:", round(mean(abs(err / actual)) * 100, 2), "%\n")
  }
  
  eval_oos(as.numeric(innoptari_test), pred_inn_niv, "VECM - Innoptari:")
  eval_oos(as.numeric(sosiri_test),    pred_sos_niv, "VECM - Sosiri:")
  
} else {
  
  # ============================================================
  # 6.5b. Ramura B: VAR pe diferente (daca NU exista cointegrare)
  # ============================================================
  
  cat("\n======================================================\n")
  cat("6.5b. VAR pe diferente (serii necointegrate)\n")
  cat("======================================================\n")
  
  # Daca testul Johansen nu confirma cointegrarea, estimam VAR pe diferente
  Y_diff_train <- cbind(
    dInnoptari = diff(log_inn_train),
    dSosiri    = diff(log_sos_train)
  )
  
  var_model <- VAR(Y_diff_train, p = p_opt, type = "const")
  summary(var_model)
  
  # Cauzalitate Granger
  cat("\n--- Cauzalitate Granger (VAR pe diferente) ---\n")
  causality(var_model, cause = "dSosiri")$Granger
  causality(var_model, cause = "dInnoptari")$Granger
  
  # IRF
  irf_var <- irf(var_model, n.ahead = 24, boot = TRUE, runs = 300)
  plot(irf_var)
  
  # FEVD
  fevd_var <- fevd(var_model, n.ahead = 24)
  plot(fevd_var)
  
  # Diagnostic
  serial.test(var_model, lags.pt = p_opt + 12)
  normality.test(var_model)
}

cat("SECTIUNEA 7: VERIFICARE EFECTE ARCH/GARCH\n")

# In literatura, modelele ARCH/GARCH sunt recomandate pentru serii
# financiare cu volatility clustering (randamente bursiere, cursuri valutare).
# Pentru serii de volum turistic (innoptari, sosiri) cu frecventa lunara,
# volatility clustering nu este un fenomen tipic.
# Totusi, verificam formal prezenta efectelor ARCH in reziduurile
# modelelor univariate (SARIMA), pentru a justifica decizia de excludere.

cat("\n--- Test ARCH-LM pe reziduurile SARIMA (Innoptari) ---\n")
cat("H0: nu exista efecte ARCH in reziduuri\n")
ArchTest(residuals(fit_sarima_opt_i), lags = 6)
ArchTest(residuals(fit_sarima_opt_i), lags = 12)
ArchTest(residuals(fit_sarima_opt_i), lags = 24)

cat("\n--- PACF al reziduurilor patratice (Innoptari) ---\n")
ggPacf(residuals(fit_sarima_opt_i)^2, lag.max = 24) +
  ggtitle("PACF reziduuri patratice SARIMA - Innoptari") +
  theme_bw()

cat("\n--- Test ARCH-LM pe reziduurile SARIMA (Sosiri) ---\n")
ArchTest(residuals(fit_sarima_opt_s), lags = 6)
ArchTest(residuals(fit_sarima_opt_s), lags = 12)
ArchTest(residuals(fit_sarima_opt_s), lags = 24)

# INTERPRETARE:
# p-value > 0.05 pentru toate lagurile => NU respingem H0
# => Nu exista efecte ARCH semnificative => GARCH nu este necesar
# Aceasta justifica in mod formal excluderea modelelor ARCH/GARCH din analiza lunară.

# SECTIUNEA 8: SINTEZA SI PROGNOZA FINALA

cat("SECTIUNEA 8: SINTEZA FINALA SI PROGNOZA 2026\n")

# ============================================================
# INNOPTARI: modelul castigator = ETS(A,N,A) (sectiunea 5.7)
# SOSIRI:    modelul castigator = SARIMA(0,1,1)(0,1,1)[12] (sectiunea 5.7)
# ============================================================

cat("\n--- Reestimarea modelelor optime pe intreaga serie (training + test) ---\n")

# INNOPTARI - ETS(A,N,A) pe intreaga serie
fit_final_i <- ets(innoptari, model = "ANA")

# SOSIRI - SARIMA(0,1,1)(0,1,1)[12] pe intreaga serie
fit_final_s <- Arima(sosiri,
                     order    = c(0, 1, 1),
                     seasonal = c(0, 1, 1))

# Prognoza pentru urmatoarele 12 luni
fc_final_i <- forecast(fit_final_i, h = 12)
fc_final_s <- forecast(fit_final_s, h = 12)

# Grafic prognoza finala - Innoptari (ETS)
autoplot(fc_final_i) +
  xlab("An") + ylab("Numar innoptari") +
  ggtitle("Innoptari: Prognoza 2026 (model ETS(A,N,A), seria completa)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafic prognoza finala - Sosiri (SARIMA)
autoplot(fc_final_s) +
  xlab("An") + ylab("Numar sosiri") +
  ggtitle("Sosiri: Prognoza 2026 (model SARIMA(0,1,1)(0,1,1)[12], seria completa)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Tabel prognoza 2026 - Innoptari
cat("\n--- Tabel prognoza 2026 - Innoptari [ETS(A,N,A)] ---\n")
print(data.frame(
  Luna       = format(as.yearmon(time(fc_final_i$mean)), "%b %Y"),
  Prognoza   = round(as.numeric(fc_final_i$mean)),
  Lower_80   = round(as.numeric(fc_final_i$lower[, 1])),
  Upper_80   = round(as.numeric(fc_final_i$upper[, 1])),
  Lower_95   = round(as.numeric(fc_final_i$lower[, 2])),
  Upper_95   = round(as.numeric(fc_final_i$upper[, 2]))
))

# Tabel prognoza 2026 - Sosiri
cat("\n--- Tabel prognoza 2026 - Sosiri [SARIMA(0,1,1)(0,1,1)[12]] ---\n")
print(data.frame(
  Luna       = format(as.yearmon(time(fc_final_s$mean)), "%b %Y"),
  Prognoza   = round(as.numeric(fc_final_s$mean)),
  Lower_80   = round(as.numeric(fc_final_s$lower[, 1])),
  Upper_80   = round(as.numeric(fc_final_s$upper[, 1])),
  Lower_95   = round(as.numeric(fc_final_s$lower[, 2])),
  Upper_95   = round(as.numeric(fc_final_s$upper[, 2]))
))

# ============================================================
# GRAFIC COMPARATIV: valori istorice + prognoza pentru ambele serii
# ============================================================

# Innoptari: istoric + prognoza ETS
autoplot(innoptari, series = "Observat") +
  autolayer(fc_final_i, series = "Prognoza ETS(A,N,A)", PI = TRUE) +
  xlab("An") + ylab("Numar innoptari") +
  ggtitle("Innoptari Romania: Serie istorica si Prognoza 2026\nModel ETS(A,N,A)") +
  guides(colour = guide_legend(title = "Serie"),
         fill   = guide_legend(title = "Interval incredere")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Sosiri: istoric + prognoza SARIMA
autoplot(sosiri, series = "Observat") +
  autolayer(fc_final_s, series = "Prognoza SARIMA(0,1,1)(0,1,1)[12]", PI = TRUE) +
  xlab("An") + ylab("Numar sosiri") +
  ggtitle("Sosiri Romania: Serie istorica si Prognoza 2026\nModel SARIMA(0,1,1)(0,1,1)[12]") +
  guides(colour = guide_legend(title = "Serie"),
         fill   = guide_legend(title = "Interval incredere")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
