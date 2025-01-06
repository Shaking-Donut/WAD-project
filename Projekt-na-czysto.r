#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#WSTĘPNA ANALIZA POJEDYNCZYCH ZMIENNYCH

library (readxl)
library(tidyverse)

# WITAM WITAM, barto
# Siema tu będzie nowy commit

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

#sprawdźmy jakim obiektem jest nowa zmienna
class(data$Year_of_Release)
class(data$User_Score)

#typowa wizualizacja rozkładu zmiennej numerycznej - histogram
hist(data$Year_of_Release, breaks=40) #większość gier w bazie jest z okolic 2007, 2008
hist(data$User_Score, breaks=10) #Najczęstsza ocena użytkowników - 8-9/10
hist(data$Critic_Score, breaks=10) #Najczęstsza ocena krytyków - 70-80/100 - trochę mniej niż u użytkoników
hist(data$Global_Sales, breaks=100,prob=T) #Tylko pojedyncze tytuły zarabiają więcej


#do oceny wizualnej rozkładu dodajemy kilka miar
summary(data$Year_of_Release)
summary(data$User_Score)
summary(data$Critic_Score)
summary(data$Global_Sales) #widać, że 1 i 3 kwartyl wskazują na przedział 0.06 - 0.47 mln przedanych sztuk gdy max wynosi ponad 82 miliony

# TYM PODPUNKCIE BĘDZIEMY PODDAWAĆ ANALIZIE GŁÓWNIE 3 ATRYBUTY - OCENĘ KRYTYKÓW, OCENĘ UŻYTKOWNIKÓW I SPRZEDAŻ GLOBALNĄ

#W CELU UPROSZCZENIA ANALIZY CZASEM BĘDZIEMY POŁUGIWAĆ SIĘ PODZIAŁEM NA 3 NAJWIĘKSZE PLATFORMY - PC, PS4 I XONE

data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=Critic_Score))+
  geom_histogram(binwidth = 2)

data_clear_critic <- data %>% 
  filter(!is.na(data$Critic_Score))

max(data_clear_critic$Critic_Score) - min(data_clear_critic$Critic_Score) #Odp: 85
IQR(data_clear_critic$Critic_Score) #Odp: 19
var(data_clear_critic$Critic_Score) #Odp: 194.27
sd(data_clear_critic$Critic_Score) #Odp: 13.94
mean(abs(data_clear_critic$Critic_Score-mean(data_clear_critic$Critic_Score))) #Odp: 11.16



data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=User_Score))+
  geom_histogram(binwidth = 0.2)

#Wszędzie poza XOne użytkownicy oceniają produkcje podobnie/lepiej od krytyków
data_clear_user <- data %>% 
  filter(!is.na(data$User_Score))

max(data_clear_user$User_Score) - min(data_clear_user$User_Score) #Odp: 9.7
IQR(data_clear_user$User_Score) #Odp: 1.8
var(data_clear_user$User_Score) #Odp: 2.25
sd(data_clear_user$User_Score) #Odp: 1.5
mean(abs(data_clear_user$User_Score-mean(data_clear_user$User_Score))) #Odp: 1.15



data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=Critic_Score))+
  geom_boxplot()



data %>%
  filter(Platform=="PC") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="PS4") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()

data %>%
  filter(Platform=="XOne") %>%
  ggplot(aes(x=User_Score))+
  geom_boxplot()


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
data %>% group_by(Platform) %>% summarize(count=n()) %>%  print(n = 100)
#Wiedząc, że gier na dane platformy mamy: PC - 974, PS4 - 393, XOne - 247 wiać że w kwestii globlanej sprzedaży kopii statystycznie najwięcej przypadków odstających występuje na PC. Trochę mniej na PS4 a najmniej takich przypadków ystępuje na XOne


prop.table(table(data$Platform))*100
prop.table(table(data$Year_of_Release))*100
sort(prop.table(table(data$Publisher))*100, decreasing = TRUE)[1:5]
sort(prop.table(table(data$Developer))*100, decreasing = TRUE)[1:6]
sort(prop.table(table(data$Rating))*100, decreasing = TRUE)
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#GGPLOTY I INNE


#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#ANALIZA 2 ZMIENNYCH


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

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#REGRESJA

# przygotowanie danych pod regresję
data_clear_critic_user <- data %>% 
  filter(!is.na(data$Critic_Score) & !is.na(data$User_Score))

data_clear_critic_user <- data_clear_critic_user %>% 
  mutate(Critic_Score = Critic_Score / 10) # wyskalowanie ocen krytyków do skali 0-10 tak jak w przypadku użytkowników 

# na tym etapie dane są już czyste, 

library("ggplot2")
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

# w porównaniu histogramu ocen krytyków i użytkowników widać, że oceny krytyków znacznie lepiej wpisują się w rozkład normalny
# na boxplotach również nie zauważamy dużych wartości odstających, wszystkie mieszczą się w przedziale 0-10, jednak znaczna większość ocen jest w przedziale 6-8 w przypadku zarówno krytyków jak i użytkowników.

# analiza korelacji
corr <- cor(data_clear_critic_user$Critic_Score, data_clear_critic_user$User_Score) # 0.5809
# korelacja jest umiarkowana, co sugeruje, że oceny krytyków i użytkowników są ze sobą powiązane, ale nie są to takie same oceny

# sprawdzenie liniowości zależności pomiędzy zmienną zależną i zmiennymi niezależnymi
ggplot(data_clear_critic_user, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  labs(title = "Zależność ocen krytyków i użytkowników") +
  theme_minimal()


model_regresji <- lm(Global_Sales ~ Critic_Score + User_Score + Year_of_Release + Genre, data = data_clear_critic_user)
summary(model_regresji)
