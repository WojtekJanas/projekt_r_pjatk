# Zadanie 1: zapisz funkcję, która sprawdza czy liczba jest podzielna przez
# drugą liczbę
podzielnosc <- function(liczba1, liczba2)
  {
  reszta <- liczba1%%liczba2
  
  if (reszta == 0)
  {
    sprintf('Liczba %s jest podzielna przez liczbę %s', liczba1, liczba2)
  }
  else 
  {
    sprintf('Liczba %s nie jest podzielna przez liczbę %s', liczba1, liczba2)
  }
}

podzielnosc(3,3)
podzielnosc(3,4)

# Zadanie 2: funkcja licząca średnia prędkośc pociągu na zadanej trasie i 
# znanych prędkościach dwóch odcinków
# s - droga, t- czas, v- predkosc
# s = v*t, 1/2*s=v1*t1, 1/2*s=v2*t2
# v_sr = s/(t1+t2) -> v_sr = 2v1*v2/(v1+v2)

srednia_predkosc <- function(v1, v2)
  {
  v_sr<-2*v1*v2/(v1+v2)
  return(v_sr)
}

sr_pred<-srednia_predkosc(120,90)
sprintf('Średnia prędkość na trasie Lublin-Warszawa wynosi: %s',sr_pred)

# Zadanie 3: funkcja obliczająca współczynnik korelacji Pearsona dla 2 wektorów
# o tej samej długości

dane = read.csv(file = 'dane.csv', header = TRUE, sep=';')
vector1<-dane$wzrost
vector2<-dane$waga

wspolczynnik_korelacji<-function(vec1,vec2)
  {
  srednia_vec1 = mean(vec1) # srednia wartość wektora 1
  srednia_vec2 = mean(vec2) # srednia wartosc wektora 2
  vec3 = vec1-srednia_vec1  # roznica obserwacji i sredniej wartosci
  vec4 = vec2-srednia_vec2  # roznica obserwacji i sredniej wartosci
  vec34 = vec3*vec4 # iloczyn wartosci wektorow (poszczegolnych wartosci)
  vec3_2 = vec3^2   # kwadrat wektorow (poszczegolnych wartosci)
  vec4_2 = vec4^2   # kwadrat wektorow (poszczegolnych wartosci)
  suma_iloczynu = sum(vec34) # suma wektora, ktory powstal z iloczynu
  suma_kwadratow_vec3 = sum(vec3_2) # suma kwadratow
  suma_kwadratow_vec4 = sum(vec4_2) # suma kwadratow
  wspolczynnik = suma_iloczynu/sqrt(suma_kwadratow_vec3*suma_kwadratow_vec4)
  # podstawienie do wzoru
}

korelacja<-wspolczynnik_korelacji(vector1,vector2)
sprintf('Współczynnik korelacji dla wagi i wzrostu wynosi: %s',korelacja)
sprintf('Tak wysoka wartosc wspolczynnika korelacji dla dwóch wektorów oznacza, 
        że są one silnie dodatnio skorelowane. To znaczy, im człowiek jest 
        wyższy, tym więcej waży')

# Zadanie 4: utwórz dataframe z podanych przez użytkownika wektorów

tworzenie_df<-function(ile=1)
  {
  prompt <- 'Podaj nazwy kolumn oddzielone spacją.'
  lista_kolumn <- strsplit(readline(prompt), " ")[[1]]
  lista_kolumn <- unlist(lista_kolumn)
  liczba_kolumn <- length(lista_kolumn)
  mydf<- data.frame(matrix(NA,nrow=ile, ncol=liczba_kolumn))
  names(mydf)<-lista_kolumn
  for (i in 1:ile)
    {
    if (i==1){
      prompt <- "Podaj pierwszy wiersz ramki danych - wartości oddziel spacją \n"
      dataframe_row <- unlist(strsplit(readline(prompt), " ")[[1]])
      mydf[i,]<-dataframe_row
    }
    else
      {
        prompt <- "Podaj kolejny wiersz ramki danych - wartości oddziel spacją \n"
        dataframe_row <- unlist(strsplit(readline(prompt), " ")[[1]])
        mydf[i,]<-dataframe_row
    }
  }
  return(mydf)
}

tmp<-tworzenie_df(ile=2)


