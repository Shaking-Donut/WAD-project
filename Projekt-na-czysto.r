#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#WSTĘPNA ANALIZA POJEDYNCZYCH ZMIENNYCH

library (readxl)
library(tidyverse)


data <- read.csv("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/Projekt/Video_Games_Sales_as_at_22_Dec_2016.csv", header = T)
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

#porównajmy rokłady czasu pływania - osobno kobiety i mężczyźni
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


#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#REGRESJA


