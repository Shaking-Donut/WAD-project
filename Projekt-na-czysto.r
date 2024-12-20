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
summary(data$Global_Sales) #widać, że 3 kwartyl wskazuje na niecałe 500tys przedanych sztuk gdy max wynosi ponad 82 miliony

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

#Wszędzie poza XOne użytkonicy oceniają produkcje podobnie/lepiej od krytyków

#tu zmieniamy płeć, zmieniłem też geometrię z histogramu na wykres skrzynkowy
data %>% 
  filter(PROGRAM=="Elite Men") %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()

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

wynik1 <- kmeans(wybrane_stand, 6, nstart = 5) #zalecany argument nstart
#ilość kalstrów metodą prób i błędów - jet to metoda ateoretyczna
print(wynik1)
#pokazuje klastry w stosunku do średnich wyników - dużo szybsi, dużo wwolniejsi, trochę wolniejsi i trochę szybsi
#suma wewnętrznych odcyleń sum kwadratów powinna być niewielka

fviz_cluster(wynik1, data = wybrane_stand)

#Wybór liczby klastrów metodą gap statistic
install.packages("cluster")
library(cluster)
#set.seed(20)
gap <- clusGap(wybrane_stand, kmeans, K.max = 8, B=500)
fviz_gap_stat(gap)
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


#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#REGRESJA


