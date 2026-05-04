
# Biblioteki, które bedziemy wykorzystywać w projekcie 

library(mice)
library(dplyr)
library(ggplot2)
library(corrplot)
library(moments)
library(scales)
library(tidyr)
library(cluster)
library(factoextra)
library(psych)

# Wczytanie danych

dane <- read.csv("C:/STUDIA/5 semestr/SAD/PROJEKT1/dane_proj.csv", 
                 sep = ";", 
                 header = TRUE, 
                 encoding = "UTF-8")


# OBRÓBKA DANYCH I WSTĘPNA ANALIZA

head(dane)
str(dane)

# usunięcie dwóch ostatnich kolumn bo nie bedą przydatne - były tylko pomocnicze

dane <- dane %>% select( -last_col())
dane <- dane %>% select( -last_col())

colnames(dane) <- c(
  "Powiat",
  "Zielen",
  "Bezrobocie_dlugo",
  "Bezrobocie",
  "Regon",
  "Uczniowie",
  "Kultura",
  "Lekarze",
  "Pomoc_spol",
  "Kanalizacja",
  "Mieszkania",
  "Sciezki_row",
  "Zgony"
)


# Sprawdzenie
str(dane)

# Zamiana zmiennych na poprawny typ

num_cols <- setdiff(names(dane), "Powiat")

to_num <- function(x)
{
  if(is.numeric(x)) return(x)
  x <- gsub("\\s+", "", x)  # usuń spacje tysięcy
  x <- gsub(",", ".", x)    # zamień przecinek na kropkę
  suppressWarnings(as.numeric(x))
}

dane[num_cols] <- lapply(dane[num_cols], to_num)

# Sprawdzenie

str(dane)
summary(dane) # już tutaj widzimy, że w przypadku zgonów występuja braki

# Uzupełnienie brakujących wartości medianą 

dane$Zgony[is.na(dane$Zgony)] <- median(dane$Zgony, na.rm = TRUE)

# Sprawdzenie czy na pewno nie występują już braki

md.pattern(dane, rotate.names = T) # wszystko ok

dim(dane)

# Podstawowe statystyki opisowe

podstawowe_statystyki <- function(df) {
  df_num <- df[sapply(df, is.numeric)]
  
  statystyki <- data.frame(
    Min = sapply(df_num, min),
    Q1 = sapply(df_num, function(x) quantile(x, 0.25)),
    Mediana = sapply(df_num, median),
    Srednia = sapply(df_num, mean),
    Q3 = sapply(df_num, function(x) quantile(x, 0.75)),
    Max = sapply(df_num, max),
    Skosnosc = sapply(df_num, skewness),
    Kurtoza = sapply(df_num, kurtosis) # tutaj domyslna wartosc to 3
  )
  
  return(statystyki)
}

statystyki <- podstawowe_statystyki(dane)
statystyki

# Histogramy dla wszystkich zmiennych
dane_long <- dane %>% 
  tidyr::pivot_longer(-Powiat, names_to = "Zmienna", values_to = "Wartosc")

ggplot(dane_long, aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  facet_wrap(~ Zmienna, scales = "free", ncol = 3) +
  theme_minimal()

# wykrywanie wartości skrajnych

ggplot(dane_long, aes(x = Zmienna, y = Wartosc)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red") +
  facet_wrap(~ Zmienna, scales = "free", ncol = 3) +
  labs(title = "Wykresy pudełkowe z wartościami odstającymi",
       x = "", y = "Wartość") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# dokładniejsze zbadanie które powiaty są odstającymi

odstajace <- do.call(rbind, lapply(num_cols, function(col) {
  x <- dane[[col]]
  gorna <- quantile(x, 0.75) + 1.5 * IQR(x)
  dolna <- quantile(x, 0.25) - 1.5 * IQR(x)
  
  odstaje <- (x > gorna | x < dolna)
  
  if (any(odstaje, na.rm = TRUE)) {
    data.frame(
      Powiat = dane$Powiat[odstaje],
      Zmienna = col,
      Wartość = x[odstaje],
      Typ = ifelse(x[odstaje] > gorna, "górna", "dolna"),
      stringsAsFactors = FALSE
    )
  }
}))

# usuwa dziwne indeksy typu 75%1, 75%2 itd.
rownames(odstajace) <- NULL
odstajace

# Sprawdzam wstępnie korelacje
# (używam spearmana bo nie mam gwarancji, że wszystkie zmienne mają rozkład normalny)

shapiro.test(dane$Zielen)
shapiro.test(dane$Bezrobocie)
shapiro.test(dane$Regon)

# macierz korelacji
cor_dane <- dane %>%
  select(-Powiat) %>%
  cor(method = "spearman")

# zwiększenie rozmiaru okna wykresu bo wychodzi on bardzo mały 
windows(width = 10, height = 10)

corrplot(cor_dane, method = "square", type = "upper", 
         title = "Macierz korelacji zmiennych", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)


# sprawdzenie zmienności

srednie <- sapply(dane[num_cols], mean)
odch_sd  <- sapply(dane[num_cols], sd)
wariancja <- sapply(dane[num_cols], var)

v <- round(100 * odch_sd / srednie, 2)

zmiennosc <- data.frame(
  Zmienna = names(v),
  `V [%]` = as.numeric(v)
)

zmiennosc 


# PODZIAŁ NA DESTYMULANTY I STYMULANTY

stymulanty <- c("Zielen", "Regon", "Uczniowie", "Kultura", 
                "Lekarze", "Pomoc_spol", "Kanalizacja", 
                "Mieszkania", "Sciezki_row")

destymulanty <- c("Bezrobocie_dlugo", "Bezrobocie", "Zgony")

#METODA HELLWIGA

dane_hellwig <- dane

#Zamiana destymulant na stymulanty
for(kol in destymulanty)
{
  dane_hellwig[[kol]] <- -1*dane_hellwig[[kol]]
}
head(dane_hellwig)

#Standaryzacja

num_cols <- setdiff(names(dane_hellwig), "Powiat")

for(kol in num_cols)
{
  mean_h <- mean(dane_hellwig[[kol]])
  sd_h <- sd(dane_hellwig[[kol]])
  dane_hellwig[[kol]] <- (dane_hellwig[[kol]] - mean_h) / sd_h
}

#Stworzenie wzorca

wzorzec <- sapply(dane_hellwig[num_cols], max)

#Obliczenie odległości poszczególnych obiektów od wzorca

d1_hellwig <- apply(dane_hellwig[num_cols], 1, function(x)
{
  sqrt(sum((x - wzorzec)^2))
})

dane_hellwig$odleglosc_od_wzorca <- d1_hellwig

#Stworzenie odległości "możliwie dalekiej"

antywzorzec <- sapply(dane_hellwig[num_cols], min)

d0_hellwig <- sqrt(sum((wzorzec - antywzorzec)^2))

dane_hellwig$Hellwig <- 1 - (dane_hellwig$odleglosc_od_wzorca / d0_hellwig)

dane_hellwig <- dane_hellwig %>%
  arrange(desc(Hellwig))

head(dane_hellwig[, c("Powiat", "odleglosc_od_wzorca", "Hellwig")])

# METODA STANDARYZOWANYCH SUM

dane_sum <- dane

# Zamiana destymulant na stymulanty
for(kol in destymulanty) {
  dane_sum[[kol]] <- -1 * dane_sum[[kol]]
}

# Standaryzacja

num_cols <- setdiff(names(dane_sum), "Powiat")

for(kol in num_cols) {
  mean_s <- mean(dane_sum[[kol]])
  sd_s <- sd(dane_sum[[kol]])
  dane_sum[[kol]] <- (dane_sum[[kol]] - mean_s) / sd_s
}

# Wyznaczenie miary syntetycznej (średnia z zestandaryzowanych zmiennych)
dane_sum$s_i <- apply(dane_sum[num_cols], 1, mean)

# Ponowna standaryzacja
min_s <- min(dane_sum$s_i)
max_s <- max(dane_sum$s_i)
dane_sum$s2_i <- (dane_sum$s_i - min_s) / (max_s - min_s)

# Sortowanie
ranking_sum <- dane_sum %>%
  arrange(desc(s2_i)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Powiat, s2_i)

head(ranking_sum, 10)

##  PORZĄDKOWANIE LINIOWE PORÓWNANIE

# Sortowanie danych według nazwy powiatu w obu zbiorach
dane_hellwig_sorted <- dane_hellwig %>%
  arrange(Powiat)

dane_sum_sorted <- dane_sum %>%
  arrange(Powiat)

# Obliczenie korelacji między wynikami metod Hellwiga i standaryzowanych sum po posortowaniu
korelacja_hellwig_sum <- cor(dane_hellwig_sorted$Hellwig, dane_sum_sorted$s2_i, method = "spearman")

# Wyświetlenie wyniku korelacji
korelacja_hellwig_sum

##  grupowanie wg średniej
# Obliczamy średnią dla metod Hellwiga i Standaryzowanych Sum
mean_hellwig <- mean(dane_hellwig$Hellwig)
mean_sum <- mean(dane_sum$s2_i)

# Dodajemy nowe kolumny, które będą zawierały grupy na podstawie średniej
dane_hellwig$Grupa_Hellwig <- ifelse(dane_hellwig$Hellwig >= mean_hellwig, "Wysoka jakość", "Niska jakość")
dane_sum$Grupa_Sum <- ifelse(dane_sum$s2_i >= mean_sum, "Wysoka jakość", "Niska jakość")

# Sprawdzamy nowe kolumny w obu dataframach
head(dane_hellwig[, c("Powiat", "Hellwig", "Grupa_Hellwig")])
head(dane_sum[, c("Powiat", "s2_i", "Grupa_Sum")])

# Możemy teraz połączyć dane w jeden dataframe, żeby zobaczyć grupy w kontekście obu metod
grupowane_dane <- merge(dane_hellwig[, c("Powiat", "Hellwig", "Grupa_Hellwig")],
                        dane_sum[, c("Powiat", "s2_i", "Grupa_Sum")],
                        by = "Powiat")

# Sprawdzamy wynik
head(grupowane_dane)

# Obliczamy liczbę powiatów w każdej grupie
grupy_hellwig <- table(grupowane_dane$Grupa_Hellwig)
grupy_sum <- table(grupowane_dane$Grupa_Sum)

# Wyświetlamy liczbę powiatów w każdej grupie
grupy_hellwig
grupy_sum

# GRUPOWANIE PODZIAŁOWE - METODA K-MEDOID

# standaryzacja 

dane_stand <- dane %>%
  select(-Powiat) %>%
  scale() %>%
  as.data.frame() %>%
  mutate(Powiat = dane$Powiat)

# sprawdzenie czy nadal występują wartości odstające 

num_cols_std <- setdiff(names(dane_stand), "Powiat")


# obserwacje, gdzie |z| > 3 (zasada 3 sigm)
odstajace_std <- do.call(rbind, lapply(num_cols_std, function(col) {
  x <- dane_stand[[col]]
  oddalone <- abs(x) > 3 
  
  if (any(oddalone, na.rm = TRUE)) {
    data.frame(
      Powiat  = dane_stand$Powiat[oddalone],
      Zmienna = col,
      Z = x[oddalone]
    )
  }
}))

odstajace_std

# wybieranie optymalnej liczby grup 

dane_grupy <- dane_stand %>% 
  select(-Powiat) %>%
  as.matrix()

# Metoda łokcia

fviz_nbclust(dane_grupy, pam, method = "wss")

# Metoda profilu 

fviz_nbclust(dane_grupy, pam, method = "silhouette")

# Grupowanie metodą k-medoid

# Ustawienie nazw powiatów jako etykiet w danych
rownames(dane_grupy) <- dane_stand$Powiat

grupy <- pam(dane_grupy, k = 3, metric = "manhattan")

fviz_cluster(grupy,
             data = dane_grupy,
             geom = c("point", "text"), 
             repel = TRUE,
             palette = "Set1",
             ellipse.type = "convex",
             pointsize = 2,              
             labelsize = 9,             
             ggtheme = theme_minimal()) +
  labs(title = "Klastry – metoda k-medoid")


# który powiat należy do którego 
klastry <- dane_stand %>%
  mutate(Klaster = grupy$clustering)

klastry %>%
  select(Klaster, Powiat)


# opis zmiennych w każdej grupie
describeBy(
  klastry %>% dplyr::select(-Powiat, -Klaster),
  group = klastry$Klaster,
  mat   = TRUE,     
  fast  = TRUE      
)


# ŚREDNIE i ODCHYLENIA STANDARDOWE dla każdej zmiennej w każdym klastrze
srednie <- klastry %>%
  group_by(Klaster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "Śr_{.col}"))

odchylenia <- klastry %>%
  group_by(Klaster) %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "SD_{.col}"))

# Połączenie w jedno
profil_klastrów <- left_join(srednie, odchylenia, by = "Klaster")

profil_klastrów

# GRUPOWANIE HIERARCHICZNE

# Wybór zmiennych

library(outliers)

num_cols <- setdiff(names(dane), "Powiat")
dane_standard <- scale(dane[num_cols])

grubbs_results <- lapply(num_cols, function(col) {
  x <- dane_standard[, col]
  test <- grubbs.test(x)
  
  data.frame(
    Zmienna = col,
    Statystyka = round(test$statistic, 3),
    p_value = round(test$p.value, 5),
    Wniosek = ifelse(test$p.value < 0.05, "Odstająca wartość", "Brak odstających")
  )
})

grubbs_results_df <- bind_rows(grubbs_results)

grubbs_results_df

# Macierz odległości

library(stats)

num_cols <- setdiff(names(dane), "Powiat")
X <- as.matrix(scale(dane[, num_cols])) #standaryzacja
rownames(X) <- dane$Powiat              

d_euc <- dist(X, method = "euclidean")

hc_ward <- hclust(d_euc, method = "ward.D2")

#dendogram

plot(hc_ward, cex = 0.7, hang = -1, main = "Dendrogram",
     xlab = "Powiaty", ylab = "Odległość", sub = "")


#Indeks CH

k_grid <- 2:min(10, nrow(X) - 1)
ch_values <- sapply(k_grid, function(k) {
  gr <- cutree(hc_ward, k = k)
  clusterSim::index.G1(X, gr)  
})

# Tabela wyników
idx_tab <- data.frame(k = k_grid, CH = ch_values)

# wykres CH vs k
plot(idx_tab$k, idx_tab$CH, type = "b", xlab = "Liczba klastrów (k)",
     ylab = "Indeks Calińskiego–Harabasza", main = "CH vs k")
abline(v = k_star, lty = 2)

k_star <- idx_tab$k[which.max(idx_tab$CH)]
cat("Optymalna liczba klastrów wg CH:", k_star, "\n")

# Podział na 3 klasy
clusters_3 <- cutree(hc_ward, k = 3)

# Liczba elementów w każdej klasie
table(clusters_3)

# Ramka wynikowa: powiat + przypisana klasa
wynik_klasy_3 <- data.frame(
  Powiat = rownames(X),
  Klasa = clusters_3
)

# Podgląd
print(wynik_klasy_3)

# Wizualizacja na dendrogramie
plot(
  hc_ward,
  cex = 0.7,
  hang = -1,
  main = "Dendrogram - podział na 3 klasy",
  xlab = "Powiaty",
  ylab = "Odległość",
  sub = ""
)
rect.hclust(hc_ward, k = 3, border = 2:4)


# dla 4 klastrów

clusters_4 <- cutree(hc_ward, k = 4)

# Liczba elementów w każdej klasie
table(clusters_4)

# Ramka wynikowa: powiat + przypisana klasa
wynik_klasy_4 <- data.frame(
  Powiat = rownames(X),
  Klasa = clusters_4
)

# Podgląd
print(wynik_klasy_4)

# Wizualizacja na dendrogramie
plot(
  hc_ward,
  cex = 0.7,
  hang = -1,
  main = "Dendrogram - podział na 4 klasy",
  xlab = "Powiaty",
  ylab = "Odległość",
  sub = ""
)
rect.hclust(hc_ward, k = 4, border = 2:4)
