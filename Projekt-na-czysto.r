#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#WSTĘPNA ANALIZA POJEDYNCZYCH ZMIENNYCH

library (readxl)
library(tidyverse)

data <- read.csv(paste(getwd(), "/Video_Games_Sales.csv", sep = ""), header = T)
View(data)

#UWAGA! W BAZIE MOGĄ WYSTĘPOWAĆ KILKA RAZY TE SAME TYTUŁY ALE NA INNYCH PLATFORMACH (np. GTA)

class(data)
class(data$Year_of_Release) #jakiego typu obiektem jest zmienna Year_of_Release? - jest to tekst (to niedobrze!)
class(data$User_Score) #jakiego typu obiektem jest zmienna User_Score? - jest to tekst (to niedobrze!)

#przekształcamy zmienne Year_of_Release i User_Score z tekstowych na liczbowwe
data <- data %>%
  mutate (Year_of_Release = as.numeric(Year_of_Release))

data <- data %>%
  mutate (User_Score = as.numeric(User_Score))
#UWAGA! W obu zmiennych mamy wartości N/A

#sprawdźmy jakim obiektem są nowe zmienne
class(data$Year_of_Release)
class(data$User_Score)

#-----------------------------------------------------------------------------------------------------------

#typowa wizualizacja rozkładu zmiennej numerycznej - histogram
summary(data$Year_of_Release)
hist(data$Year_of_Release, breaks=40) 
sort(prop.table(table(data$Year_of_Release))*100, decreasing = TRUE)
#większość gier w bazie jest z okolic 2007, 2008
#Pokrywa się to to z analizą danych z Wikipedii: https://gamrconnect.vgchartz.com/thread.php?id=248467
#ALE dane te są najprawdopodobniej bardzo nieprecyzyjne ze względu na rynek cyfrowy (np. samego Steama): https://www.statista.com/statistics/552623/number-games-released-steam/

library(moments)

summary(data$User_Count)
hist(data$User_Count, breaks=100) #Widać bardzo duży rozsztrzał w ilości oceniających. Mediana to 24 a śrdenia to 162!

summary(data$Critic_Count)
hist(data$Critic_Count, breaks=20) #Zazwyczaj grę ocenia w trochę ponad 20 krytyków

summary(data$User_Score)
hist(data$User_Score, breaks=10) #Najczęstsza ocena użytkowników - 8-9/10

summary(data$Critic_Score)
hist(data$Critic_Score, breaks=10) #Najczęstsza ocena krytyków - 70-80/100 - trochę mniej niż u użytkowników

skewness(data$User_Score, na.rm = TRUE)
skewness(data$Critic_Score, na.rm = TRUE)

kurtosis(data$User_Score, na.rm = TRUE)
kurtosis(data$Critic_Score, na.rm = TRUE)
#Widać że histogram użytkowników jest bardziej lewoskośny a wyniki w nim są bardziej zróżnicowane niż w histogramie krytyków

summary(data$Global_Sales) #widać, że 1 i 3 kwartyl wskazują na przedział 0.06 - 0.47 mln przedanych sztuk gdy max wynosi ponad 82 miliony
hist(data$Global_Sales, breaks=100,prob=T) #Tylko pojedyncze tytuły zarabiają więcej

sort(prop.table(table(data$Platform))*100, decreasing = TRUE) #Jeżeli chodzi o konole to 2 pierwsze pozycje (PS2 i Nintedo DS) zdają się być zgodne z danymi z Wikipedii: https://en.wikipedia.org/wiki/List_of_best-selling_game_consoles

sort(prop.table(table(data$Genre))*100, decreasing = TRUE) #Widać, że najbbardziej popularne są tutaj gry z dużą ilością akcji a najgorzej radzą sobie gry strategiczne i logiczne
#Różni się to od nowszych danych co może wskazywać na zmieniające się trendy w branży: https://rocketbrush.com/blog/most-popular-video-game-genres-in-2024-revenue-statistics-genres-overview

sort(prop.table(table(data$Publisher))*100, decreasing = TRUE)[1:5] #Electronic Arts, Activision, Namco Bandai Games, Ubisoft, Konami Digital Entertainment
sort(prop.table(table(data$Developer))*100, decreasing = TRUE)[1:6] #Ubisoft, EA Sports, EA Canada, Konami, Capcom
#Zarówno w rynku wydawniczym jak i deweloperkim EA jest (łącznie) największym graczem w branży

sort(prop.table(table(data$Rating))*100, decreasing = TRUE) #Zgodnie z systemem ratingu ESRB: https://www.esrb.org/ratings-guide/
#Najwięcej gier dla wszystkich, nastolatków i dojrzałych a najmniej tylko dla dorosłych

#-----------------------------------------------------------------------------------------------------------

# TYM PODPUNKCIE BĘDZIEMY PODDAWAĆ ANALIZIE GŁÓWNIE 3 ATRYBUTY - OCENĘ KRYTYKÓW, OCENĘ UŻYTKOWNIKÓW I SPRZEDAŻ GLOBALNĄ

#W CELU UPROSZCZENIA ANALIZY CZASEM BĘDZIEMY POSŁUGIWAĆ SIĘ PODZIAŁEM NA 3 NAJNOWSZE PLATFORMY UJĘTE W BAZIE - PC, PS4 I XONE

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

#Użytkownicy oceniają gry na PC częściej wyżej niż krytycy

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

#Użytkownicy oceniają gry na PS4 w miarę podobnie jak krytycy

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

#XOne ydaje się być jedyną platformą  tym zestawieniu gdzie to krytycy oceniają zazwyczaj produkcje podobnie/lepiej od użytkowników


data_clear_critic <- data %>% 
  filter(!is.na(data$Critic_Score))

summary(data_clear_critic$Critic_Score)
max(data_clear_critic$Critic_Score) - min(data_clear_critic$Critic_Score) #Odp: 85
IQR(data_clear_critic$Critic_Score) #Odp: 19
var(data_clear_critic$Critic_Score) #Odp: 194.27
sd(data_clear_critic$Critic_Score) #Odp: 13.94
mean(abs(data_clear_critic$Critic_Score-mean(data_clear_critic$Critic_Score))) #Odp: 11.16


data_clear_user <- data %>% 
  filter(!is.na(data$User_Score))

summary(data_clear_user$User_Score)
max(data_clear_user$User_Score) - min(data_clear_user$User_Score) #Odp: 9.7
IQR(data_clear_user$User_Score) #Odp: 1.8
var(data_clear_user$User_Score) #Odp: 2.25
sd(data_clear_user$User_Score) #Odp: 1.5
mean(abs(data_clear_user$User_Score-mean(data_clear_user$User_Score))) #Odp: 1.15

#Możemy spróbować na podstawie powyższych danych wyciągnąć pewne wnioski.
#Oceny użytkowników mają wyższą zarówno medianę jak i średnią. Mają również trochę większe zarówno odchylenie standardowe.
#Krytycy mają mniejszy rozstęp choć trochę większyrozstęp ćwiartkowy
#Na podstaie tego i poprzenich analiz można zaryzykoać stwierdzenie, że użytkownicy są skłonni ystawiać wyższe oceny, ale rónież bardziej różnorodne
#(najnższe wystawiane oceny przez użytkowników są niższe niż te wystawiane przez krytyków)
#-----------------------------------------------------------------------------------------------------------

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()



data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()



data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()


data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()

#Powyższe ykresy nie pokazują jakichś znaczących różnic między ocenami krytyków a użytkowników na danych platformach
#Poszukajmy dalej:

Q1cs <- quantile(data_clear_critic$Critic_Score, .25)
Q3cs <- quantile(data_clear_critic$Critic_Score, .75)
subset(data_clear_critic, data_clear_critic$Critic_Score<(Q1cs - 1.5*IQR(data_clear_critic$Critic_Score)) | data_clear_critic$Critic_Score>(Q3cs + 1.5*IQR(data_clear_critic$Critic_Score))) #Odp: 83

Q1us <- quantile(data_clear_user$User_Score, .25)
Q3us <- quantile(data_clear_user$User_Score, .75)
subset(data_clear_user, data_clear_user$User_Score<(Q1us - 1.5*IQR(data_clear_user$User_Score)) | data_clear_user$User_Score>(Q3us + 1.5*IQR(data_clear_user$User_Score))) #Odp: 305

#Możemy zauażyć, że pomimo mniejszej liczby obserwacji oceny użytkowników mają znacznie więcej przypadków odstających niż krytycy


data %>%
  filter(Platform=="PC") %>%
  subset(Global_Sales<(quantile(Global_Sales, .25) - 1.5*IQR(Global_Sales)) | Global_Sales>(quantile(Global_Sales, .75) + 1.5*IQR(Global_Sales)))#Odp: 146

data %>%
  filter(Platform=="PS4") %>%
  subset(Global_Sales<(quantile(Global_Sales, .25) - 1.5*IQR(Global_Sales)) | Global_Sales>(quantile(Global_Sales, .75) + 1.5*IQR(Global_Sales))) #Odp: 55

data %>%
  filter(Platform=="XOne") %>%
  subset(Global_Sales<(quantile(Global_Sales, .25) - 1.5*IQR(Global_Sales)) | Global_Sales>(quantile(Global_Sales, .75) + 1.5*IQR(Global_Sales))) #Odp: 26

library("dplyr")
data %>% group_by(Platform) %>% summarize(count=n()) %>% arrange(desc(count)) %>%  print(n = 100)
#Wiedząc, że gier na dane platformy mamy: PC - 974, PS4 - 393, XOne - 247 wiać że w kwestii globlanej sprzedaży kopii statystycznie najwięcej przypadków odstających występuje na PC. Trochę mniej na PS4 a najmniej takich przypadków występuje na XOne


#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
# HISTOGRAMY, GGPLOTY I INNE DO PREZENTOWANIA

# Rozkład gier w czasie tylko se zmieniam na prezke
hist(data$Year_of_Release, breaks = 40, col = "#a9dfd0", main = "Game Release Date", xlab = "Year") 

# Tutaj odfiltrowuje wartości NA, bo kod się psuł i wykresy były dziwne
data_yor <- data %>%
  filter(is.na(Year_of_Release) != TRUE)

# Troche głupie, ale niby jeden z trzech ggplot'ów :)
data_yor %>%
  ggplot(aes(x = Year_of_Release)) +
  geom_histogram(binwidth = diff(range(data_yor$Year_of_Release)) / 40, fill = "#a9dfd0", color = "black") +
  labs(title = "Game Release Date", x = "Year", y = "Frequency") +
  theme_minimal()

# User count  histogram
hist(data$User_Count, breaks = 60, col = "#a9dfd0", main = "Number of User Reviews", xlab = "User Count")
# A co jeśli by to odciąć od góry
hist(data$User_Count[data$User_Count <= 500], breaks = 60, col = "#a9dfd0", main = "Number of User Reviews", xlab = "User Count")

# Critic count histogram
hist(data$Critic_Count, breaks = 60, col = "#a9dfd0", main = "Number of Critic Reviews", xlab = "Critic Count")

# Histogram ocen krytyków / użytkowników 
hist(data$User_Score, breaks = 10, col = "#a9dfd0", main = "User Review Scores", xlab = "User Score")
hist(data$Critic_Score, breaks = 10, col = "#a9dfd0", main = "Critic Review Scores", xlab = "Critic Score")

# Histogram sprzedaży globalnych
hist(data$Global_Sales , breaks = 100, prob = T, col = "#a9dfd0", main = "Density of Global Sales", xlab = "Units Sold Globally (in Millions)")

# Histogram sprzedaży globalnych z odcięciem wartości odstających
Global_Sales <- data %>% 
  select("Global_Sales")

IQR <- IQR(Global_Sales$Global_Sales)
low <- quantile(Global_Sales$Global_Sales, 0.25) - 1.5*IQR
up <- quantile(Global_Sales$Global_Sales, 0.75) + 1.5*IQR
outliers <- which(Global_Sales$Global_Sales < low | Global_Sales$Global_Sales > up)
Global_Sales <- Global_Sales[-outliers, ]

hist(Global_Sales, breaks = 100, prob = T, col = "#a9dfd0", main = "Density of Global Sales", xlab = "Units Sold Globally (in Millions)")
summary(Global_Sales)

# Porównania konsol na prezke 
data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2, fill = "#a9dfd0") +
  labs(x = "Critic Score", y = "Frequency")

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2, fill = "#a9dfd0") +
  labs(x = "User Score", y = "Frequency")

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2, fill = "#a9dfd0") +
  labs(x = "Critic Score", y = "Frequency")

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2, fill = "#a9dfd0") +
  labs(x = "User Score", y = "Frequency")

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2, fill = "#a9dfd0") +
  labs(x = "Critic Score", y = "Frequency")

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2, fill = "#a9dfd0") +
  labs(x = "User Score", y = "Frequency")

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#ANALIZA 2 ZMIENNYCH



#przede wszystkim tworzymy obiekt bez braków danych w tych ocenach
data_clear_all <- data %>% 
  drop_na()
#wstępna analiza macierzą korelacji
library(corrplot)
data_clear_all %>% 
  select(Year_of_Release, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales, Critic_Score, Critic_Count, User_Score, User_Count) %>% 
  cor(method = "spearman") %>% 
  corrplot(method = "color")

#zbadajmy jak oceny uzytkowników i krytyków mają się do siebie
#korelacja r persona wynosi 0.58, więc występuje
cor(data_clear_all$Critic_Score, data_clear_all$User_Score)

#ale jak to wygląda na wykresie?
library(ggpointdensity)
library(viridis)
data_clear_all %>% 
  ggplot (aes(x=Critic_Score, y=User_Score))+
  geom_point()+
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm")
# mamy dużo danych i korelacja jest widoczna, ale dane są mocno rozrzucone

#sprawdźmy jak to wygląda na wybranych platformach (8 najpopularniejszych w zbiorze):
data_clear_all %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  ggplot (aes(x=Critic_Score, y=User_Score))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_pointdensity() +
  scale_color_viridis() +
  facet_wrap(~Platform, nrow = 4)
#na oko na pc korelacja jest najmniej widoczna, a jak to wygląda w liczbach?
data_clear_all %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  group_by(Platform) %>%
  summarize(korelacja = cor(User_Score, Critic_Score)) %>% 
  ungroup()
#zgodnie z wykresami najmocniej korelacja występuje w przypadku PS1 i Wii (które są na marginesie najstarszymi z tych platform), a najsłabsza dla PC

#sprawdźmy teraz jak kolejno opinie krytuków i uzytkowników korelują ze sprzedarzą globalną
data_clear_all %>% 
  summarise(
    kKrytyków = cor(Critic_Score, Global_Sales, method = "spearman"),
    kUzytkowników = cor(User_Score, Global_Sales, method = "spearman")
  )
#korelacje wydają się być nieliniowe, gdyż korelacja spearmana daje znacznie wyższy wynik niz persona

data_clear_all %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  group_by(Platform) %>%
  summarise(
    kKrytyków = cor(Critic_Score, Global_Sales, method = "spearman"),
    kUzytkowników = cor(User_Score, Global_Sales, method = "spearman")
  ) %>% 
  ungroup()

#Korelacje występują, w przypadku ocen krytyków są zawsze silniejsze niż w przypadku uzytkowników
#Ciekawy jest natomiast przypadek PC, gdzie korelacja między ocenami uzytkowników, a sprzedażą jest praktycznie zerowa
#Przyjżyjmy się temu bliżej...

data_clear_all %>% 
  filter(Platform == "PC") %>% 
  ggplot (aes(x=User_Score, y=Global_Sales))+
  geom_point()+
  geom_smooth(method = "lm")
#co ciekawe w przypadku każdego stopnia oceny występują dobrze i źle sprzedające się gry
#spróbujmy odfiltrować gry o najlepszej sprzedarzy (>2mln)

data_clear_all %>% 
  filter(Platform == "PC") %>% 
  filter(Global_Sales < 2) %>% 
  ggplot (aes(x=User_Score, y=Global_Sales))+
  geom_point()+
  geom_smooth(method = "lm")

#nic się nie zmienia...

data_clear_all %>% 
  summarise(Ameryka_płn = cor(Year_of_Release, NA_Sales),
            Europa = cor(Year_of_Release, EU_Sales),
            Japonia = cor(JP_Sales, Year_of_Release),
            Inne = cor(Other_Sales, Year_of_Release),
            Globalne = cor(Global_Sales, Year_of_Release))
#co ciekawe rok wydania gry nie ma żadnej korelacji z jej sprzedażą

#A jak wyglądają korelacje sprzedaży w regionach?

data_clear_all %>% 
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>% 
  cor() %>% 
  corrplot(method = "number")

#jak widzimy rynek amerykański i europejski są dość podobne i najbardziej wpływają one na sprzedaż globalną
#rynek japoński jest wyraźnie inny i nie ma on takiego znaczenia


#sprawdźmy ostatnią ciekawą rzecz z macierzy korelacji: ujemną korelację roku wydania gry z ocenami urzytkowników
cor(data_clear_all$User_Score, data_clear_all$Year_of_Release, method = "spearman")

data_clear_all %>% 
  ggplot (aes(x=User_Score, y=Year_of_Release))+
  geom_point()+
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm")
#korelacja jest wyraźnie ujemna, ale zobaczmy jak to wygląda na poszczególnych platformach

data_clear_all %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  group_by(Platform) %>% 
  summarise(korelacja = cor(User_Score, Year_of_Release, method = "spearman")) %>% 
  ungroup()

data_clear_all %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  ggplot (aes(x=User_Score, y=Year_of_Release))+
  geom_point()+
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm", color = "red")+
  facet_wrap(~Platform, nrow = 4)

#Ciekawym wydaje się fakt, że jedynie na PC, korelacja jest zauważalnie wysoka (z lekkim wyjątkiem PS1), co oznacza że na komputerach osobistych gracze stają się z czasem zauważalnie bardziej krytyczni niż na konsolach


#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#ANALIZA SKUPIEŃ

library(tidyverse)
library(factoextra)

#Przykład 1 - metoda K-średnich
#kmeans z pakietu stats
#wymaga jako arguemntu macierzy numerycznej lub ramki ze zmiennymi numerycznymi
#nie lubi przypadków odstających
#sprawdza się w przypadku klastrów o kulistym kształcie

wybrane <- select(data,c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"))
wybrane_stand <- scale(wybrane)

wynik1 <- kmeans(wybrane_stand, 8, nstart = 5) #zalecany argument nstart
#ilość kalstrów metodą prób i błędów - jet to metoda ateoretyczna
print(wynik1)
#pokazuje klastry w stosunku do średnich wyników
#suma wewnętrznych odcyleń sum kwadratów powinna być niewielka

fviz_cluster(wynik1, data = wybrane_stand)

#Na tym etapie na podstawie wykresu i danych przypuszczam, że wymiar 1 (wyjaśniający aż 67,3%) może być związany z udziałem rynku Ameryki Północnej
#Wydaje się to być bardzo ensone patrząc że nie licząc Chin, USSA jest największym rynkiem gier cyfrowych


#UWAGA! Przy tylu obserwacjach poniższe metody obliczeniowe wykonują się bardzo wolno/wcale


#Wybór liczby klastrów metodą gap statistic
install.packages("cluster")
library(cluster)
#set.seed(20)
gap <- clusGap(wybrane_stand, kmeans, K.max = 8, B=500)
fviz_gap_stat(gap) # plik -> analiza_skupien.png
#interseują nas wysokie wyniki w postaci globalnego albo lokalnego maksimum
#metoda niestabilna

#ostateczne rozwiązanie z 2 klastrami
#dla zapewnienia odtwarzalności
set.seed(3)
wynik1 <- kmeans(wybrane_stand, 2, nstart = 3)
print(wynik1)
fviz_cluster(wynik1, data=wybrane_stand)

#co mamy w klastrach
data$klaster <- wynik1$cluster
#table(data$klaster, data$xxxxxx)

#przykład 2 - metoda PAM - klastrowanie wokół medoidów
#lepiej się sprawdza w analizowaniu klastrów nieregularnych
wynik2 <- pam(wybrane_stand, 8)
print(wynik2)
fviz_cluster(wynik2, data = wybrane_stand)

fviz_nbclust(wybrane_stand, pam, method = "wss")

fviz_nbclust(wybrane_stand, pam, method = "silhouette")

gap2 <- clusGap(wybrane_stand, pam, K.max = 8)
fviz_gap_stat(gap2)

#czy 2 klastry to optymalna liczba?
fviz_nbclust(wybrane_stand, pam, method = "wss")
gap2 <- clusGap(wybrane_stand, pam, K.max = 2)
fviz_gap_stat(gap2)

# :(
wybrane2 <- data_clear_all %>%
  filter(Platform == "PC") %>% 
  select("Critic_Score", "User_Score", "Global_Sales")
#przy większych kategoriach pomaga
#wybrane2 <- wybrane2[sample(nrow(wybrane2), 500), ]
IQR <- IQR(wybrane2$Critic_Score)
low <- quantile(wybrane2$Critic_Score, 0.25) - 1.5*IQR
up <- quantile(wybrane2$Critic_Score, 0.75) + 1.5*IQR
outliers <- which(wybrane2$Critic_Score < low | wybrane2$Critic_Score > up)
wybrane2 <- wybrane2[-outliers, ]
IQR <- IQR(wybrane2$User_Score)
low <- quantile(wybrane2$User_Score, 0.25) - 1.5*IQR
up <- quantile(wybrane2$User_Score, 0.75) + 1.5*IQR
outliers <- which(wybrane2$User_Score < low | wybrane2$User_Score > up)
wybrane2 <- wybrane2[-outliers, ]
IQR <- IQR(wybrane2$Global_Sales)
low <- quantile(wybrane2$Global_Sales, 0.25) - 1.5*IQR
up <- quantile(wybrane2$Global_Sales, 0.75) + 1.5*IQR
outliers <- which(wybrane2$Global_Sales < low | wybrane2$Global_Sales > up)
wybrane2 <- wybrane2[-outliers, ]
wybrane2 <- scale(wybrane2)
set.seed(3)
gap <- clusGap(wybrane2, pam, K.max = 8, B=50)
fviz_gap_stat(gap)
wyniki3 <- pam(wybrane2, 3)
fviz_cluster(wyniki3)

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#REGRESJA

library(psych)
library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lmtest)
library(car)
library(ggpointdensity)
library(viridis)
library(ggrepel)

# =====================
# !!! to może się powtarzać z analizą pojedynczych zmiennych na początku !!!

# przygotowanie danych pod regresję
data_clear_critic_user <- data %>% 
  filter(!is.na(data$Critic_Score) & !is.na(data$User_Score))

data_clear_critic_user <- data_clear_critic_user %>% 
  mutate(Critic_Score = Critic_Score / 10) # wyskalowanie ocen krytyków do skali 0-10 tak jak w przypadku użytkowników 

# na tym etapie dane są już czyste, 
# wybieramy zmienne do modelu regresji
regression_data <- data_clear_critic_user %>% 
  select(Critic_Score, User_Score, Global_Sales, Platform) %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC"))


ggplot(data_clear_critic_user, aes(x = Critic_Score)) + 
  geom_boxplot() +
  labs(title = "Boxplot ocen krytyków") +
  theme_minimal()

ggplot(data_clear_critic_user, aes(x = User_Score)) +
  geom_boxplot() +
  labs(title = "Boxplot ocen użytkowników") +
  theme_minimal()

ggplot(data_clear_critic_user, aes(x = Critic_Score)) +
  geom_histogram() +
  labs(title = "Histogram ocen krytyków") +
  theme_minimal()

ggplot(data_clear_critic_user, aes(x = User_Score)) +
  geom_histogram() +
  labs(title = "Histogram ocen użytkowników") +
  theme_minimal()


# pozbycie się outlierów z danych, tak jak w przypadku klastrowania
IQR <- IQR(regression_data$Critic_Score)
low <- quantile(regression_data$Critic_Score, 0.25) - 1.5*IQR
up <- quantile(regression_data$Critic_Score, 0.75) + 1.5*IQR
outliers <- which(regression_data$Critic_Score < low | regression_data$Critic_Score > up)
regression_data <- regression_data[-outliers, ]
IQR <- IQR(regression_data$User_Score)
low <- quantile(regression_data$User_Score, 0.25) - 1.5*IQR
up <- quantile(regression_data$User_Score, 0.75) + 1.5*IQR
outliers <- which(regression_data$User_Score < low | regression_data$User_Score > up)
regression_data <- regression_data[-outliers, ]
IQR <- IQR(regression_data$Global_Sales)
low <- quantile(regression_data$Global_Sales, 0.25) - 1.5*IQR
up <- quantile(regression_data$Global_Sales, 0.75) + 1.5*IQR
outliers <- which(regression_data$Global_Sales < low | regression_data$Global_Sales > up)
regression_data <- regression_data[-outliers, ]

# =====================

# w porównaniu histogramu ocen krytyków i użytkowników widać, że oceny krytyków znacznie lepiej wpisują się w rozkład normalny
# na boxplotach również nie zauważamy dużych wartości odstających, wszystkie mieszczą się w przedziale 0-10, jednak znaczna większość ocen jest w przedziale 6-8 w przypadku zarówno krytyków jak i użytkowników.

# zobaczmy jak wyglądają trendy na poszczególnych systemach gier
regression_data %>% 
  ggplot(aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną na poszczególnych systemach gier", x = "Ocena krytyków", y = "Sprzedaż globalna")+
  facet_wrap(~Platform, nrow = 4)
# najlepszego modelu regresji można się spodziewać w przypadku PS2, PS3 i X360, gdzie zależność wydaje się być liniowa

# i jeszcze dla ocen użytkowników
regression_data %>% 
  ggplot(aes(x = User_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wpływ ocen użytkowników na sprzedaż globalną na poszczególnych systemach gier", x = "Ocena użytkowników", y = "Sprzedaż globalna") +
  facet_wrap(~Platform, nrow = 4)
# najlepsza wydaje się być zależność dla PS3
# dla obu zmiennych zależność wydaje się być liniowa dla PS3 więc dokładniejszą analizę przeprowadzimy na danych dotyczących PS3
regression_data_ps3 <- regression_data %>% 
  filter(Platform == "PS3")

# Spróbujmy najpierw z ocenami użytkowników
model_user_ps3 <- lm(Global_Sales ~ User_Score, data = regression_data_ps3)
summary(model_user_ps3)
# model wyjaśnia zaledwie 7.7% wariancji danych, co jest bardzo niskim wynikiem, przejdźmy więc dalej
regression_data_ps3$pred_linear <- predict(model_user_ps3, newdata = regression_data_ps3)

ggplot(data = regression_data_ps3, aes(x = User_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_line(aes(y = pred_linear), linewidth = 1, color = "red") +
  labs(title = "Wpływ ocen użytkowników na sprzedaż globalną - PS3", x = "Ocena użytkowników", y = "Sprzedaż globalna")

# Spróbujmy z ocenami krytyków
model_critic_ps3 <- lm(Global_Sales ~ Critic_Score, data = regression_data_ps3)
summary(model_critic_ps3)
# ten model wyjaśnia już 27.2% wariancji danych, co jest znacznie lepszym wynikiem, przejdźmy więc dalej z modelem bazującym na opiniach krytyków
regression_data_ps3$pred_linear <- predict(model_critic_ps3, newdata = regression_data_ps3)

ggplot(data = regression_data_ps3, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_line(aes(y = pred_linear), linewidth = 1, color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną - PS3", x = "Ocena krytyków", y = "Sprzedaż globalna")
# model wizualnie nie wygląda na dobrze dopasowany, spróbujmy użyć funkcji eksponencjalnej


model_exp_critic_ps3 <- lm(Global_Sales ~ exp(Critic_Score), data = regression_data_ps3)
summary(model_exp_critic_ps3)
# model eksponencjalny wyjaśnia 27.5% wariancji danych, co jest bardzo zbliżonym wynikiem do modelu liniowego, sprawdźmy więc wizualnie
regression_data_ps3$pred_exp <- predict(model_exp_critic_ps3, newdata = regression_data_ps3)

ggplot(data = regression_data_ps3, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_line(aes(y = pred_exp), linewidth = 1, color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną - PS3", x = "Ocena krytyków", y = "Sprzedaż globalna")
# model wizualnie wygląda na lepiej dopasowany, sprawdźmy jeszcze statystyki

par(mfrow = c(2,2))
par(mar = c(3,3,2,2))

plot(model_exp_critic_ps3)
durbinWatsonTest(model_exp_critic_ps3)
# autokorelacja wysoka - 0.76
shapiro.test(residuals(model_exp_critic_ps3))
# według testu Shapiro-Wilka rozkład reszt znacząco odbiega od rozkładu normalnego

# spróbujmy dodać do modelu oceny użytkowników
model_exp <- lm(Global_Sales ~ exp(Critic_Score) + exp(User_Score), data = regression_data_ps3)
summary(model_exp)
# model nadal wyjaśnia 27.5% wariancji danych
# zmienna User_Score jest nieistotna statystycznie - nie ma rzeczywistego wpływu na sprzedaż globalną
plot(model_exp)
durbinWatsonTest(model_exp)
# autokorelacja jest na poziomie 0.76 - wysoka
shapiro.test(model_exp$residuals)
par(mfrow = c(1, 1))
par(mar = c(5,4,4,2) + 0.1)
# według testu Shapiro-Wilka rozkład reszt znacząco odbiega od rozkładu normalnego
hist(model_exp$residuals, main = "Histogram Reszt", xlab = "Reszty", breaks = 30, col = "#a9dfd0")
# histogram reszt również wskazuje na to, że nie są one zbyt dobrze rozłożone
# możemy wrócić model do jednej zmiennej - oceny krytyków

model_final <- lm(Global_Sales ~ exp(Critic_Score), data = regression_data_ps3)
regression_data_ps3$pred <- predict(model_final, newdata = regression_data_ps3)
summary(model_final)
par(mfrow = c(2,2))
par(mar = c(3,3,2,2))
plot(model_final)
par(mfrow = c(1, 1))

# dodajmy jeszcze przedziały ufności
conf_data <- data.frame(
  Global_Sales = regression_data_ps3$Global_Sales,
  Critic_Score_pred = regression_data_ps3$pred,
  conf_low = predict(model_final, newdata = regression_data_ps3, interval = "confidence")[,"lwr"],
  conf_high = predict(model_final, newdata = regression_data_ps3, interval = "confidence")[,"upr"]
)

ggplot(data = regression_data_ps3, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_ribbon(aes(ymin = conf_data$conf_low, ymax = conf_data$conf_high), fill = "red", alpha = 0.2) +
  geom_line(aes(y = pred), linewidth = 1, color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną - PS3", x = "Ocena krytyków", y = "Sprzedaż globalna")



# przykład predykcji na przykładzie grę, która nie wchodzi w nasz model - Rock Band 2 (2008) (Opinia Krytyków - 91) (https://www.metacritic.com/game/rock-band-2/)
rock_band <- data.frame(Critic_Score = 9.1, Name = "Rock Band 2", Global_Sales = 1.49)
rock_band$pred <- predict(model_final, newdata = rock_band)
predict(model_final, newdata = rock_band, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 1.24 a 1.45 mln kopii z najbardziej prawdopodobną wartością - 1.35 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 1.49 mln kopii - czyli troszkę więcej niż przewidział nasz model na podstawie oceny krytyków

# kolejny przykład - Naruto Shippuden: Ultimate Ninja Storm 3 (2013) (Opinia Krytyków - 77) (https://www.metacritic.com/game/naruto-shippuden-ultimate-ninja-storm-3)
naruto <- data.frame(Critic_Score = 7.7, Name = "Naruto Shippuden: Ultimate Ninja Storm 3", Global_Sales = 0.93)
naruto$pred <- predict(model_final, newdata = naruto)
predict(model_final, newdata = naruto, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 0.57 a 0.63 mln kopii z najbardziej prawdopodobną wartością - 0.60 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 0.93 mln kopii - czyli zdecydowanie więcej niż przewidział nasz model

# kolejny przykład - Bioshock 2 (2010) (Opinia Krytyków - 88) (https://www.metacritic.com/game/bioshock-2)
bioshock <- data.frame(Critic_Score = 8.8, Name = "Bioshock 2", Global_Sales = 1.54)
bioshock$pred <- predict(model_final, newdata = bioshock)
predict(model_final, newdata = bioshock, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 1.02 a 1.16 mln kopii z najbardziej prawdopodobną wartością - 1.09 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 1.54 mln kopii

#kolejny przykład - Mass Effect 2 (2011) (Opinia Krytyków - 94) (https://www.metacritic.com/game/mass-effect-2
mass_effect <- data.frame(Critic_Score = 9.4, Name = "Mass Effect 2", Global_Sales = 1.45)
mass_effect$pred <- predict(model_final, newdata = mass_effect)
predict(model_final, newdata = mass_effect, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 1.55 a 1.84 mln kopii z najbardziej prawdopodobną wartością - 1.69 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 1.45 mln kopii

# kolejny przykład - Warhawk (2007) (Opinia Krytyków - 84) (https://www.metacritic.com/game/warhawk)
warhawk <- data.frame(Critic_Score = 8.4, Name = "Warhawk", Global_Sales = 1.06)
warhawk$pred <- predict(model_final, newdata = warhawk)
predict(model_final, newdata = warhawk, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 0.80 a 0.89 mln kopii z najbardziej prawdopodobną wartością - 0.85 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 1.06 mln kopii

# kolejny przykład - F1 2014 (2014) (Opinia Krytyków - 62) (https://www.metacritic.com/game/f1-2014)
f1 <- data.frame(Critic_Score = 6.2, Name = "F1 2014", Global_Sales = 0.41)
f1$pred <- predict(model_final, newdata = f1)
predict(model_final, newdata = f1, interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 0.37 a 0.45 mln kopii z najbardziej prawdopodobną wartością - 0.41 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 0.41 mln kopii

# kolejny przykład - Iron Man 2 (2010) (Opinia Krytyków - 41) (https://www.metacritic.com/game/iron-man-2)
iron_man <- data.frame(Critic_Score = 4.1, Name = "Iron Man 2", Global_Sales = 0.39)
iron_man$pred <- predict(model_final, newdata = iron_man)
predict(model_final, newdata = data.frame(Critic_Score = 4.1), interval = "confidence")
# według naszego modelu ta gra powinna sprzedać pomiędzy 0.32 a 0.40 mln kopii z najbardziej prawdopodobną wartością - 0.36 mln
# wartość sprzedaży tej gry w rzeczywistości wynosiła 0.40 mln kopii

new_data <- rbind(rock_band, naruto, bioshock, mass_effect, warhawk, f1, iron_man)

ggplot(data = regression_data_ps3, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point(data = new_data, color = "green", size = 3) +
  geom_ribbon(aes(ymin = conf_data$conf_low, ymax = conf_data$conf_high), fill = "red", alpha = 0.2) +
  geom_line(aes(y = pred), linewidth = 1, color = "red") +
  geom_label_repel(data = new_data, aes(label = Name), box.padding = 0.35, point.padding = 0.5) +
  labs(title = "Przykłady gier - PS3", x = "Ocena krytyków", y = "Sprzedaż globalna")

