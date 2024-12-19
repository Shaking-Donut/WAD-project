#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#WSTĘPNA ANALIZA POJEDYNCZYCH ZMIENNYCH

library (readxl) #w razie potrzeby: install.packages("readxl")
library(tidyverse)


#sprawdzamy bieżący katalog w R
#jeśli zapisaliśmy pliki z danymi w innym miejscu, to dalej wskazujemy pełną ścieżkę dostępu
getwd()

#plik xls jest uszkodzony, nie każdy R go czyta
data <- read_xls("C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/Paris2024OlympicGamesFull.xls")
#podobnie z csv - zwracamy uwagę na funkcję i rozszerzenie wczytywanego pliku - są różnice
data <- read.csv("C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/Paris2024OlympicGames.xls", header = T) #header - wczytanie nazw kolumn z pierwszego wiersza
#u mnie plik xls wczytał sie poprawnie - zapisuję w formacie R, który powinien wszystkim działać
write.table(data, "C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/dane_triathlon.R")

#to powinno działać u wszystkich!!!
data <- read.table("C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/dane_triathlon.R")
#zaglądamy do bazy danych
View(data)

#chcę analizować zmienną SWIM
class(data) #mój obiekt 'data' to data.frame - ramka danych
class(data$SWIM) #jakiego typu obiektem jest zmienna SWIM?
#jest to tekst (to niedobrze!)

#przekształcamy zmienną SWIM z tekstowej na liczbę sekund
#najpierw funkcja as.difftime
#następnie wynik funkcji as.difftime przekształcamy (as.numeric) w liczbę
#całość dopisujemy do zbioru jako nową zmienną SWIM_s, będącą wynikiem użycia funcji 'mutate' z pakietu dplyr
data <- data %>% # ten 'ptaszek' przesyła elementy ze swojej lewej strony na prawą
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S",units="secs")))

#sprawdźmy jakim obiektem jest nowa zmienna
class(data$SWIM_s)

#typowa wizualizacja rozkładu zmiennej numerycznej - histogram
hist(data$SWIM_s)
#zwiększamy liczbę słupków na histogramie - będzie dokładniejszy
hist(data$SWIM_s, breaks=40)
#histogram na osi Y może mieć liczebności lub gęstość rozkładu prawdopodobieństwa (argument 'prob')
hist(data$SWIM_s,breaks=30,prob=T)

#do oceny wizualnej rozkładu dodajemy kilka miar
summary(data$SWIM_s) #wyświetla wartości skrajne, kwartyle, średnią

#porównajmy rokłady czasu pływania - osobno kobiety i mężczyźni
data %>% #przekazuję argument (skąd dane?) do funkcji filter
  filter(PROGRAM=="Elite Women") %>% # do funkcji wykresu ggplot przekazuje tylko część obserwacji
  ggplot(aes(x=SWIM_s))+
  geom_histogram(binwidth = 5)#typowa, najprostsza składnia ggplot - określenie estetyki (aes) i geometrii (geom)

#tu zmieniamy płeć, zmieniłem też geometrię z histogramu na wykres skrzynkowy
data %>% 
  filter(PROGRAM=="Elite Men") %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()


data %>% 
  filter(PROGRAM=="Elite Women") %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot(fill="#947")#tak, można się bawić kolorami i nie tylko!

#a gdybyśmy chcieli zestawić dwa wykresy obok siebie, by porównać?
data %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM)

#będzie łatwiej porównać, gdy umieścimy je nad sobą, nie obok siebie  
data %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol=1)# tzn. ustaw wartości zmiennej PROGRAM w jednej kolumnie

#MIARY TENDENCJI CENTRALNEJ
min(data$SWIM_s) #wartość minimalna zmiennej
max(data$SWIM_s) #wartość maksymalna
median(data$SWIM_s) # mediana
mean(data$SWIM_s) #średnia; UWAGA! pod warunkiem, że nie ma braków danych!
quantile(data$SWIM_s)#na ustawieniach domyślnych zwraca wartości skrajne i kwartyle
#co z modalną?
#własna lub gotowe funkcje niektórych pakietów np. dprep
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(data$SWIM_s)

#MIARY ROZPROSZENIA
max(data$SWIM_s)-min(data$SWIM_s) #rozstęp
IQR(data$SWIM_s) #rozstęp ćwiartkowy
var(data$SWIM_s) # wariancja
sqrt(var(data$SWIM_s))# pierwiastek kw.z wariancji
sd(data$SWIM_s) #czyli odchylenie standardowe
#a co z odchyleniem przeciętnym?
#trzeba na piechotę
mean(abs(data$SWIM_s-mean(data$SWIM_s)))

#SKOŚNOŚĆ I KURTOZA
library(moments)
skewness(data$SWIM_s)
kurtosis(data$SWIM_s)

#ROZKŁAD ZMIENNYCH JAKOŚCIOWYCH (NOMINALNE + PORZĄDKOWE)
table(data$PROGRAM) #najprostsza funkcja
prop.table(table(data$PROGRAM))#jako proporcja
prop.table(table(data$PROGRAM))*100 #i w procentach

#tabele mogą być wielowymiarowe
#tu chcemy porównać ile kobiet/mężczyzn nie ukończyło zawodów
table(data$POSITION,data$PROGRAM)

###CIEKAWOSTKA
#DLA UWAŻNYCH ;) - WYKORZYSTANIE WYNIKU FUNKCJI TABLE
write.table(table(data$NATIONALITY,data$PROGRAM), "C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/t1.R")
x <- read.table("C:/Users/dszkl/Documents/AGH/Dydaktyka/WAD/Dane/t1.R")
subset(x, Elite.Women>Elite.Men)
filter(x, Elite.Women>Elite.Men)

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#GGPLOTY I INNE
library (readxl)
library(tidyverse)
library(moments)

data <- read.table("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/lab5/dane_triathlon_full.R")

data <- data %>%
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S",units="secs")))

data <- data %>%
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S",units="secs")))

data <- data %>%
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S",units="secs")))

data_clear <- data %>% 
  filter(RUN_s>0)

#Zadanie 1

data_clear %>% 
  ggplot (aes(x=SWIM_s, y=RUN_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")

data_clear %>% 
  ggplot (aes(x=BIKE_s, y=RUN_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")
#_________________________________________________________________________________
#Odp: Dla obu wykresów w przypadku obu płci linie najlepszego dopasowania wskazują na zalezności wprost proporcjonalne a w obrębie danej konkurencji dla obu płci nachylenie linni najlepszego dopasowania jest bardzo podobne
#widać jednak, że wyniki są mocno rozproszone względem linii najlepszego dopasowania
#O ile zależność jest podobna to na obu wykresach widać, że czasy mężczyzn w obu kategoriach większości są niższe od czasów kobiet
#_________________________________________________________________________________
data_clear %>% 
  ggplot (aes(x=SWIM_s, y=RUN_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = ATHLETE.LAST), size = 3)
#Pływanie-bieg kobiet
#Odp: Hannesdottir

#Pływanie-bieg mężczyzn
#Odp: Duchampt

data_clear %>% 
  ggplot (aes(x=BIKE_s, y=RUN_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = ATHLETE.LAST), size = 3)
#Rower-bieg kobiet
#Odp: Hannesdottir, Kasper

#Rower-bieg mężczyzn
#Odp: Duchampt, Kaindl
#================================================================================================
#================================================================================================

#Zadanie 2

data.women <- data_clear %>%
  filter(PROGRAM=="Elite Women")

data.men <- data_clear %>%
  filter(PROGRAM=="Elite Men")

#Siłę i kierunek zależności skazuje nam korelacja. W tym celu używamy 2 różnych metod żeby je ze sobą porównać
data_clear %>%
  group_by(PROGRAM) %>% 
  summarise(corsb = cor(SWIM_s, BIKE_s),
            corrb = cor(RUN_s, BIKE_s),
            corsr = cor(SWIM_s, RUN_s))  %>% 
  ungroup()

data_clear %>%
  group_by(PROGRAM) %>% 
  summarise(corsb_s = cor(SWIM_s, BIKE_s, method = "spearman"),
            corrb_s = cor(RUN_s, BIKE_s, method = "spearman"),
            corsr_s = cor(SWIM_s, RUN_s), method = "spearman")  %>% 
  ungroup()

#Wszystkie korelacje mają wartość dodatnią co oznacza zależność prost proporcjonalną
#Pływanie - rower - silniejsza dla mężczyzn i niższa dla kobiet (brak zbliżonych wyników dla obu metod)
#Bieganie - rower - przeciętna dla kobiet i mężczyzn (ale wyższa dla kobiet)
#Pływanie - Bieganie - przeciętna dla kobiet i słaba dla mężczyzn

#================================================================================================
#================================================================================================

#Zadanie 3

t.test(data_clear$SWIM_s~data_clear$PROGRAM)

t.test(data_clear$RUN_s~data_clear$PROGRAM)

t.test(data_clear$BIKE_s~data_clear$PROGRAM)

#Dla wszyskich konkurencji p-value jest mała. W związku z tym odrzucamy hipotezę zerową. Na tej podstawie przyjmujemy, że różnica jest istotna statystycznie.
#Różnica średnich najmniejsza jest w przypadku biegania

#================================================================================================
#================================================================================================

#Zadanie 4
data <- data %>%
  mutate (AGE = 2024-YOB)

data_clear <- data_clear %>%
  mutate (AGE = 2024-YOB)
#_________________________________________________________________________________
summary(data$AGE)

max(data$AGE - min(data$AGE)) #Odp: 17
IQR(data$AGE) #Odp: 5.75
var(data$AGE) #Odp: 14.57081
sd(data$AGE) #Odp: 3.817173

skewness(data$AGE) #Odp: 0.2294389 - niewielka, prawostronna
kurtosis(data$AGE) #Odp: 2.503681-  zbliżona do przeciętnej (3)

data %>%
  ggplot(aes(x=AGE))+
  geom_histogram(binwidth = 1)

data %>% 
  ggplot(aes(x=AGE))+
  geom_boxplot()

#Nie wydaje się, żeby istniały wyraźnie przypadki odstające
#_________________________________________________________________________________

t.test(data$AGE~data$PROGRAM)
#Kobiety są statystycznie starsze, ale różnica wieku między kobietami a mężczyznami jest niewielka i nie jest istotna statystycznie
#p-value jest wysokie, więc nie mamy podstaw by odrzucić hipotezę zerową

#================================================================================================
#================================================================================================

#Zadanie 5
data_clear %>% 
  ggplot (aes(x=AGE, y=BIKE_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = ATHLETE.LAST), size = 3)

data_clear %>% 
  ggplot (aes(x=AGE, y=RUN_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = ATHLETE.LAST), size = 3)

data_clear %>% 
  ggplot (aes(x=AGE, y=SWIM_s, colour = PROGRAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = ATHLETE.LAST), size = 3)

#wiek w różnym stopniu wpływa na różne dyscypliny w różnych płciach. Jeżeli już wpływ istnieje jest on wprost proporcjonalny
#W przypadku różnych płci wiek wpływa na dyscypliny w różnym stopniu
#Rower - dostrzegalny pływ u obu płci (większe u mężczyzn)
#Bieganie - znikomy wpły u obu płci
#Pływanie - dostrzegalny wpłw u mężczyzn, znikomy u kobiet 
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#ANALIZA 2 ZMIENNYCH
library (readxl)
library(tidyverse)
library(moments)

liderzy <- read.csv("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/lab6/liderzy.csv", header = T)

library(questionr)

freq(liderzy$z1.wiedza.dziedzinowa)
liderzy <- filter(liderzy, Calkowity.czas.wypelniania!="")

ltabs(~z1.wiedza.dziedzinowa+plec, data=liderzy)

#podstawowe table krzyżowe - rozkłady procentowe
prop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
rprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))

chi1 <- chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)
print(chi1$observed)
print(chi1$expected)

#dokładny test niezaleności gdy liczebności oczekiane są niskie
fisher.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)
#alternatynie metoda Monte Carlo
chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec, simulate.p.value = TRUE)
#miara siły związku nr 1 (nominalne-nominalne + mieszane) - V Kramer
cramer.v(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))


#miary siły związku dla zmiennych porządkowych
install.packages("DescTools")
library(DescTools)

freq(liderzy$staz)
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz))

liderzy$staz2 <- factor(liderzy$staz, levels = c("0-2","3-5","6-10","11-15","16-20","powyżej 20 lat"))
freq(liderzy$staz2)

liderzy$z1.wiedza.dziedzinowa <- factor(liderzy$z1.wiedza.dziedzinowa, levels = c("To nie jest dla mnie ważne","Trochę ważne","Ważne (ale nie najważniejsze)","Jest to dla mnie bardzo ważne"))
freq(liderzy$z1.wiedza.dziedzinowa)
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz))
print(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2), total=F), width=120)

library(knitr)
kable(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2)),format = "markdown", digit)

KendallTauB(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
StuartTauC(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
GoodmanKruskalGamma(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
SomersDelta(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),direction = "column",conf.level = 0.95)

#zestawy wielokrotnych odpowiedzi
liderzy %>% 
  select(starts_with("narodowosc")) %>% 
  multi.table()

liderzy %>% 
  select(starts_with("narodowosc")) %>% 
  cross.multi.table(liderzy$plec, freq = T)

#wizualizacja - wykres słupkowy
liderzy %>% 
  ggplot(aes(x=z1.wiedza.dziedzinowa, y=plec))+
  geom_bar() #to nie pójdzie

wykres1 <- as.data.frame(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)))

wykres1 %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  coord_flip()

wykres1 %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  scale_x_discrete(limits=rev)+
  coord_flip()  

wykres1 %>%
  filter(Var1!="Total") %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  scale_x_discrete(limits=rev)+
  coord_flip()

wykres1 %>%
  filter(Var1!="Total") %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="stack")+
  scale_x_discrete(limits=rev)+
  coord_flip()

wykres1 %>%
  filter(Var1!="Total"&Var2!="All") %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="fill")+
  scale_x_discrete(limits=rev)+
  xlab("Ważność")+
  ylab("Procenty")+
  coord_flip()


#Zadanie 1
#dla z2.sposob.pracy i z2.nadmierna.presja

#zmiennna niezależną będzie płeć a wykorzystamy procentowanie cprop
cprop(table(liderzy$z2.sposob.pracy, liderzy$plec))
#jest to ważny aspekt dla obu płci chociaż bardziej dla kobiet (tylko 3.7% kobiet uznało, że nie jest to dla nich ważne)

cprop(table(liderzy$z2.nadmierna.presja, liderzy$plec))
#podobne jak poprzednio jest to ważny aspekt dla obu płci chociaż bardziej dla kobiet (0% kobiet uznało, że nie jest to dla nich ważne i tylko 3.8% mężczyzn)

chisp <- chisq.test(liderzy$z2.sposob.pracy, liderzy$plec)
fisher.test(liderzy$z2.sposob.pracy, liderzy$plec)
# dla obu testów nie ma podstaw by odrzucić hipotezę zerową

chinp <- chisq.test(liderzy$z2.nadmierna.presja, liderzy$plec)
fisher.test(liderzy$z2.nadmierna.presja, liderzy$plec)
# dla obu testów nie ma podstaw by odrzucić hipotezę zerową

KendallTauB(table(liderzy$z2.sposob.pracy, liderzy$plec),conf.level = 0.95)
StuartTauC(table(liderzy$z2.sposob.pracy, liderzy$plec),conf.level = 0.95)
GoodmanKruskalGamma(table(liderzy$z2.sposob.pracy, liderzy$plec),conf.level = 0.95)
SomersDelta(table(liderzy$z2.sposob.pracy, liderzy$plec),direction = "column",conf.level = 0.95)

KendallTauB(table(liderzy$z2.nadmierna.presja, liderzy$plec),conf.level = 0.95)
StuartTauC(table(liderzy$z2.nadmierna.presja, liderzy$plec),conf.level = 0.95)
GoodmanKruskalGamma(table(liderzy$z2.nadmierna.presja, liderzy$plec),conf.level = 0.95)
SomersDelta(table(liderzy$z2.nadmierna.presja, liderzy$plec),direction = "column",conf.level = 0.95)

#Zadanie 2
liderzy$z2.nadmierna.presja <- factor(liderzy$z2.nadmierna.presja, levels = c("To nie jest dla mnie ważne","Trochę ważne","Ważne (ale nie najważniejsze)","Jest to dla mnie bardzo ważne"))

wykres2 <- as.data.frame(cprop(table(liderzy$z2.nadmierna.presja, liderzy$plec)))

wykres2 %>%
  filter(Var1!="Total"&Var2!="All") %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  scale_x_discrete(limits=rev)+
  xlab("Ważność")+
  ylab("Procenty")+
  coord_flip()

wykres2 %>%
  filter(Var1!="Total"&Var2!="All") %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat = "identity", position="fill")+
  scale_x_discrete(limits=rev)+
  xlab("Ważność")+
  ylab("Procenty")+
  coord_flip()

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#ANALIZA SKUPIEŃ

library(tidyverse)
#library(psych)
#library(ggcorrplot)
#library(FactoMineR)
library(factoextra)

baza1 <- read.csv("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/lab9/dane_triathlon_full.csv", header = T)

baza1 <- baza1 %>%
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S",units="secs")))

baza1 <- baza1 %>% 
  filter(RUN_s>0)

#Przykład 1 - metoda K-średnich
#kmeans z pakietu stats
#wymaga jako arguemntu macierzy numerycznej lub ramki ze zmiennymi numerycznymi
#nie lubi przypadków odstających
#sprawdza się w przypadku klastrów o kulistym kształcie

wybrane <- select(baza1,c("SWIM_s", "BIKE_s", "RUN_s"))
wybrane_stand <- scale(wybrane)

wynik1 <- kmeans(wybrane_stand, 4, nstart = 5) #zalecany argument nstart
#ilość kalstrów metodą prób i błędów - jet to metoda ateoretyczna
print(wynik1)
#pokazuje klastry w stosunku do średnich wyników - dużo szybsi, dużo wwolniejsi, trochę wolniejsi i trochę szybsi
#suma wewnętrznych odcyleń sum kwadratów powinna być niewielka

fviz_cluster(wynik1, data = wybrane_stand)

#jak wybrać liczbę klastrów?
#1 metoda graficzna
fviz_nbclust(wybrane_stand, kmeans, method = "wss")

#2 metoda gap statistic
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
baza1$klaster <- wynik1$cluster
table(baza1$klaster, baza1$PROGRAM)

#przykład 2 - metoda PAM - klastrowanie wokół medoidów
#lepiej się sprawdza w analizowaniu klastrów nieregularnych
wynik2 <- pam(wybrane_stand, 4)
print(wynik2)
fviz_cluster(wynik2, data = wybrane_stand)

#czy 4 klastry to optymalna liczba?
fviz_nbclust(wybrane_stand, pam, method = "wss")
gap2 <- clusGap(wybrane_stand, pam, K.max = 8)
fviz_gap_stat(gap2)

#to może 6 klastrów?
wynik2 <- pam(wybrane_stand, 6)
print(wynik2)
fviz_cluster(wynik2, data = wybrane_stand)

#a jak to teraz się przakłada na płeć?
baza1$klasterPAM <- wynik2$clustering
table(baza1$klasterPAM, baza1$PROGRAM)

#przykład 3 - metoda hierarchiczna
#najpierw liczymy macierz odległości
odleglosci <- dist(wybrane_stand) #domyślnie są liczone odl. euklidesowe
wynik3 <- hclust(odleglosci)#domyślnie wybrana jet metoda "complete"
print(wynik3)
plot(wynik3)
plot(wynik3, hang = -0.1, cex=0.7)

#ile klastrów wybrać? Gdzie przyciąć drzewo?
#załóżmy 2, jak przy K-means
klastry <- cutree(wynik3,2)
baza1$klastry_hier <- klastry

#porównajmy rozwiązania
table(baza1$klaster, baza1$klastry_hier)

table(baza1$klastry_hier, baza1$PROGRAM)

#inna polecana metoda - Warda
wynik4 <- hclust(odleglosci, method = "ward.D2")
print(wynik4)
plot(wynik4)
plot(wynik4, hang = -0.1, cex=0.5)

#czy widać różnice w dendrogramach ward v complete?

### Zadanie 1: Proszę wykonać analizę PAM
#osobno dla płci (Panowie dla siebie, Panie dla siebie)
#czy w tej konfiguracji najlepszym roziązaniem będą 3 klastry?
men <- baza1 %>% 
  filter(PROGRAM == "Elite Men")

men_wybrane <- select(men,c("SWIM_s", "BIKE_s", "RUN_s"))
men_wybrane_stand <- scale(men_wybrane)


men_wynik <- pam(men_wybrane_stand, 5)
print(men_wynik)
fviz_cluster(men_wynik, data = men_wybrane_stand)

#czy 4 klastry to optymalna liczba?
fviz_nbclust(men_wybrane_stand, pam, method = "wss")
men_gap <- clusGap(men_wybrane_stand, pam, K.max = 8)
fviz_gap_stat(men_gap)

#to może 6 klastrów?
wynik2 <- pam(men_wybrane_stand, 6)
print(men_wynik)
fviz_cluster(men_wynik, data = wybrane_stand)

#a jak to teraz się przakłada na płeć?
baza1$klasterPAM <- wynik2$clustering
table(baza1$klasterPAM, baza1$PROGRAM)

#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#===========================================================================================================
#REGRESJA

library(tidyverse)
library(car)

baza1 <- read.csv("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/lab11/dane_triathlon_full.csv", header = T)

baza1 <- baza1 %>%
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S",units="secs")))

#nowe zmienne - total time, t1, t2
baza1 <- baza1 %>%
  mutate (TOTAL = as.numeric(as.difftime(TOTAL.TIME, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (T1_s = as.numeric(as.difftime(T1, format="%H:%M:%S",units="secs")))
baza1 <- baza1 %>%
  mutate (T2_s = as.numeric(as.difftime(T2, format="%H:%M:%S",units="secs")))

baza1 <- baza1 %>% 
  filter(RUN_s>0)

bazaK <- filter(baza1, PROGRAM=="Elite Women")
bazaM <- filter(baza1, PROGRAM=="Elite Men")

#regresja pojedynczej zmiennej
#założenia
#1 - niezależnoć obseracji
#2 - normalność rozkładu zm. zależnej (pożądana)
#3 - liniowość związku między zm. niezależną a zależną
#4 - homoskedatyczność / jednorodność ariancji rezt dla wartości
#5 - normalność reszt

#Ad. 2
hist(bazaK$TOTAL, breaks = 40)
hist(bazaM$TOTAL, breaks = 40)

#AD. 3
plot(TOTAL~SWIM_s, data = bazaK) #formuła: zm. zależna ~ zm. niezależna
plot(TOTAL~BIKE_s, data = bazaK)
plot(TOTAL~RUN_s, data = bazaK)

plot(TOTAL~SWIM_s, data = bazaM)

#Ad. 4 i 5 - po dopasowaniu modelu

model_swim <- lm(TOTAL ~ SWIM_s, data = bazaK) #lm - linear model
summary(model_swim)
#Call - budowa modelu,
#Residuals - rozkład reszt (różnic obserwacji od modelu) dobrze jak zbliża się do rozkładu normalnego (rozstęp ćwiartkowy i mediana)
#Coefficients - współczynniki regresji
#Estimate - stała (gdy x=0) i współczynnik regresji (niestandaryzoany)
#Std. Error - 
#t value
#Pr - istotność statystyczna
#Statystyka modelu jako całości  - dopasowanie do danych, F statitic - zerowość współczynnika regresji
#residual tandard error, R-squared - precyzyjność modelu (% wyjasnionej wariancji)

model_bike <- lm(TOTAL ~ BIKE_s, data = bazaK) #lm - linear model
summary(model_bike)

model_run <- lm(TOTAL ~ RUN_s, data = bazaK) #lm - linear model
summary(model_run)

par(mfrow=c(2,2)) #robimy sobie miejsce na domyślne wykresy (2 kolumny i 2 wiersze)
par(mar=c(3,3,3,3))#ustawienie marginesów dla 4 kierunków
plot(model_swim) #interpretacja

#Co najmniej 10-20 oberwacji na jedną zmienną niezależną (predyktor)

#Residuals vs fitted - skupiska reszt a linia referencyjna i krzywa dopasowania
#Q-Q - ykres normalny reszt - na linii powinna się znajdoać większość punktów - wtedy zgodnie z warunkiem reszty układają się normalnie
#Scale-location - pokazuje nieregularności wariancji reszt jeśli chodzi o jej rozszerzalność
#residuls vs leverage - przypadki odstające - siła przyciągania linii regresji przez pojedyncze obserwacje

#i ćwiczenie - proszę zinterpretować model dla bike i run
plot(model_bike)

plot(model_run)

#Rekomendacja: obserwować 49!

#bazaK <- filter(bazaK, ATHLETE.ID!=89532) chwilowo niepotrebne bo punkt może nie być odstający przy wielu wymiarach

model_all <- lm(TOTAL~SWIM_s+BIKE_s+RUN_s, data = bazaK)
vif(model_all) #gdy wartości są mniejsze od 1 to predyktory są ze sobą nieskorelowane, do analizy potrzebujemy wartości mniejsze od 10 (inaczej trzeba robić np. PCA)
summary(model_all)
plot(model_all)

#test Durbina-Watsona
durbinWatsonTest(model_all) #chcemy autokorealcję bliską 0 i D-W statistic bliskie 2
#nie możemy odrzucić że nie są autoskorelowane

#test Shapiro-Wilka
shapiro.test(residuals(model_all)) #nie możemy odrzucić że reszty pochodzą z rozkłądu normalnego

bazaK <- mutate(bazaK, wiek=2024-YOB)

model_wiek <- lm(TOTAL~SWIM_s+BIKE_s+RUN_s+wiek, data = bazaK)

summary(model_wiek) #wiek nie jest istotny statystycznie przy pozostałych zmiennych
plot(model_wiek)
vif(model_wiek)
durbinWatsonTest(model_wiek)
shapiro.test(residuals(model_wiek))


bazaM <- mutate(bazaM, wiek=2024-YOB)

model_wiekm <- lm(TOTAL~SWIM_s+BIKE_s+RUN_s+wiek, data = bazaM)

summary(model_wiekm) #wiek nie jest istotny statystycznie przy pozostałych zmiennych
plot(model_wiekm)
vif(model_wiekm)
durbinWatsonTest(model_wiekm)
shapiro.test(residuals(model_wiekm))
#===================================================================================
#===================================================================================

baza1 <- read.table("C:/Users/jan/Desktop/!PRZEPROWADZKA/!STUDIA/Semestr 3/WAD/lab11/triathlon.last.R")

bazaK <- filter(baza1, PROGRAM=="Elite Women")
bazaM <- filter(baza1, PROGRAM=="Elite Men")

#czy możemy użyć wszystkich zmiennych niezależnych?
#np. tak: no nie bardzo!
#poza tym: case-to-variable ratio (10:1, 20:1)
model_total <- lm(TOTAL ~ SWIM_s+BIKE_s+RUN_s+T1_s+T2_s, data = bazaK)
summary(model_total)
plot(model_total)

#wybór zmiennych ilościowych do stadaryzacji
ilosciowe <- select(bazaK,c("TOTAL", "SWIM_s", "BIKE_s", "RUN_s", "T1_s", "T2_s"))
ilosciowe <- as.data.frame(scale(ilosciowe))
model_ilosciowe <- lm(TOTAL~ SWIM_s+BIKE_s+RUN_s, data = ilosciowe)
summary(model_ilosciowe)

#alternatywa - dodatkowy pakiet
install.packages("lm.beta")
library(lm.beta)
lm.beta(model_ilosciowe)

#czy tylko zmienne ilosciowe? Niekoniecznie. Dummy coding
#model ze zm. ilosciowymi i jakosciowymi
model_mix <- lm(TOTAL~ SWIM_s+BIKE_s+RUN_s+region2, data = bazaK)
summary(model_mix)
plot(model_mix)
lm.beta(model_mix)

#czy powinniśmy uwzględnić interakcje?
macierz <- select(bazaK, c("SWIM_s", "BIKE_s", "RUN_s", "T1_s", "T2_s"))
cor(macierz)

model_int <- lm(TOTAL~ SWIM_s+BIKE_s*RUN_s+T2_s, data = bazaK)
vif(model_int, type= "predictor")
summary(model_int)
plot(model_int)

#predykcja z modelu (dl fikcyjnych, losowych danych)
#zazwyczaj przewidujemy dla rzeczywistego zbioru testowego
nowe_dane <- data.frame(SWIM_s = sample(100:500,5,replace = T),
                        BIKE_s = sample(3000:4000,5,replace = T),
                        RUN_s = sample(500:2000,5,replace = T),
                        region2=c("WE", "WE", "WE", "OTH", "OTH"))

przew_total <- predict(model_mix, newdata = nowe_dane)
print(przew_total)
nowe_dane$total_pred <- przew_total
