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
  select(Critic_Score, User_Score, Global_Sales, Platform, Year_of_Release) %>% 
  filter(Platform %in% c("PS2", "DS", "PS3", "Wii", "X360", "PSP", "PS", "PC")) %>% 
  filter(Global_Sales < 20)


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

# =====================

# w porównaniu histogramu ocen krytyków i użytkowników widać, że oceny krytyków znacznie lepiej wpisują się w rozkład normalny
# na boxplotach również nie zauważamy dużych wartości odstających, wszystkie mieszczą się w przedziale 0-10, jednak znaczna większość ocen jest w przedziale 6-8 w przypadku zarówno krytyków jak i użytkowników.

regression_data %>% 
  ggplot(aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną na poszczególnych systemach gier", x = "Ocena krytyków", y = "Sprzedaż globalna")+
  facet_wrap(~Platform, nrow = 4)

regression_data %>% 
  ggplot(aes(x = User_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_smooth(method = "lm") +
  labs(title = "Wpływ ocen użytkowników na sprzedaż globalną na poszczególnych systemach gier", x = "Ocena użytkowników", y = "Sprzedaż globalna") +
  facet_wrap(~Platform, nrow = 4)

regression_data_ps2 <- regression_data %>% 
  filter(Platform == "PS2")

model_exp_critic_ps2 <- lm(Global_Sales ~ exp(Critic_Score), data = regression_data_ps2)
summary(model_exp_critic_ps2)
regression_data_ps2$pred_critic <- predict(model_exp_critic_ps2, newdata = regression_data_ps2)

ggplot(data = regression_data_ps2, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_line(aes(y = pred_critic), linewidth = 1, color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną - PS2", x = "Ocena krytyków", y = "Sprzedaż globalna")


par(mfrow = c(2,2))
par(mar = c(3,3,2,2))

plot(model_exp_critic_ps2)

regression_data_ps2 <- regression_data_ps2 %>% 
  filter(Global_Sales < 10)

model_exp_critic_ps2 <- lm(Global_Sales ~ exp(Critic_Score), data = regression_data_ps2)
summary(model_exp_critic_ps2)

plot(model_exp_critic_ps2)
durbinWatsonTest(model_exp_critic_ps2)
# mocna autokorelacja dodatnia danych

model_exp_user_ps2 <- lm(Global_Sales ~ exp(User_Score), data = regression_data_ps2)
summary(model_exp_user_ps2)
plot(model_exp_user_ps2)
durbinWatsonTest(model_exp_user_ps2)
# jeszcze większa autokorelacja danych

model_exp_critic_user_ps2 <- lm(Global_Sales ~ exp(Critic_Score) + exp(User_Score), data = regression_data_ps2)
summary(model_exp_critic_user_ps2)
plot(model_exp_critic_user_ps2)
durbinWatsonTest(model_exp_critic_user_ps2)
# nadal bardzo silna autokorelacja dodatnia danych, chociaż mniejsza niż w przypadku pojedynczych zmiennych

par(mfrow = c(1, 1))

model_exp <- lm(Global_Sales ~ exp(Critic_Score) + exp(User_Score), data = regression_data_ps2)
summary(model_exp)
# zmienna User_Score jest nieistotna statystycznie - nie ma rzeczywistego wpływu na sprzedaż globalną
durbinWatsonTest(model_exp)
# autokorelacja jest na poziomie 0.7 - bardzo wysoka
shapiro.test(model_exp$residuals)
# według testu Shapiro-Wilka rozkład reszt znacząco odbiega od rozkładu normalnego
hist(model_exp$residuals, main = "Histogram Reszt", xlab = "Reszty", breaks = 30)
# histogram reszt również wskazuje na to, że nie są one zbyt dobrze rozłożone
# możemy wrócić model do jednej zmiennej - oceny krytyków

model_final <- lm(Global_Sales ~ exp(Critic_Score), data = regression_data_ps2)
regression_data_ps2$pred <- predict(model_exp, newdata = regression_data_ps2)
summary(model_final)
par(mfrow = c(2,2))
par(mar = c(3,3,2,2))
plot(model_final)
par(mfrow = c(1, 1))

ggplot(data = regression_data_ps2, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  geom_pointdensity() +
  scale_color_viridis() +
  geom_line(aes(y = pred), size = 1, color = "red") +
  labs(title = "Wpływ ocen krytyków na sprzedaż globalną - PS2", x = "Ocena krytyków", y = "Sprzedaż globalna")

