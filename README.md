# Ekonomia Polski: dashboard

Interaktywna aplikacja Shiny z danymi makroekonomicznymi Polski (Eurostat, NBP):
wykresy szeregów czasowych, korelacje na poziomach i różnicach, prognoza ARIMA
oraz test przyczynowości Grangera.

**Aplikacja online:** https://mitek.shinyapps.io/ekonomia-polski/

## Dane

| Seria | Źródło | Zakres |
|---|---|---|
| Inflacja HICP (r/r, %) | Eurostat, `prc_hicp_minr` | 01.1997 – 08.2026 |
| Stopa bezrobocia (SA, % aktywnych) | Eurostat, `une_rt_m` | 01.1997 – 08.2026 |
| EUR/PLN, USD/PLN (średnia miesięczna z kursów średnich) | NBP, tabela A | 01.2002 – 08.2026 |

Tabela końcowa (`data/dane_miesieczne.rds`) obejmuje wspólny okres **01.2002 – 08.2026**
(296 miesięcy, bez braków). Dane pochodzą z dnia 12.10.2026 i są w repozytorium statyczne.

Uwaga o inflacji: od danych za 01.2026 Eurostat przeszedł na klasyfikację ECOICOP v2,
więc używana jest nowa tabela `prc_hicp_minr` (stara `prc_hicp_manr` jest zarchiwizowana).
Na wspólnym okresie obie tabele różnią się o co najwyżej 0,2 p.p., więc traktuję je jako jeden szereg.

## Struktura projektu

| Plik | Co robi |
|---|---|
| `krok1.R` | Pobranie inflacji z Eurostatu, kontrola jakości, porównanie ze starą tabelą |
| `krok2.R` | Wykresy inflacji w ggplot2 (cały okres i od 2010) |
| `krok3.R` | Własna funkcja `fetch_nbp()` pobierająca kursy z API NBP w kawałkach po 365 dni |
| `krok4.R` | Bezrobocie z Eurostatu, agregacja kursów do miesięcy, łączenie w jedną tabelę |
| `krok5.R` | Korelacje na poziomach i różnicach, symulacja regresji pozornej |
| `krok6.R` | Testy stacjonarności, ARIMA (auto.arima), prognoza, test Grangera |
| `app/` | Aplikacja Shiny (`app.R` + własna kopia danych) |
| `data/`, `wykresy/` | Dane pośrednie i wygenerowane wykresy |

Skrypty uruchamia się po kolei (każdy zapisuje dane dla następnego).

## Uruchomienie lokalnie

Wymagane pakiety:

```r
install.packages(c(
  "tidyverse", "eurostat", "httr2",
  "forecast", "tseries", "lmtest",
  "shiny", "bslib"
))
```

Sama aplikacja (bez pobierania danych):

```r
shiny::runApp("app")
```

## Wnioski analityczne

- **Korelacje na poziomach są pozorne.** Inflacja, bezrobocie i kursy mają trend, więc na poziomach
  korelują (np. inflacja ~ EUR/PLN: 0,37), a po zróżnicowaniu związek znika (0,01).
  Symulacja dwóch niezależnych błądzeń losowych daje |r| > 0,5 w ok. 40% przypadków.
- **ARIMA dla inflacji:** auto.arima wybiera ARIMA(1,1,1)(1,0,2)[12]. Na ostatnich 12 miesiącach
  RMSE modelu (0,33) jest tylko nieznacznie lepsze niż naiwnej prognozy „jak dziś” (0,37).
  Pasma prognozy szybko się rozszerzają.
- **Test Grangera:** nie znaleziono wiarygodnej zależności predykcyjnej między inflacją,
  bezrobociem i kursami. Jedyny istotny wynik (EUR/PLN → bezrobocie) nie jest odporny:
  znika po ograniczeniu próby do okresu od 2012 roku i prawdopodobnie wynika z przerwy
  w danych o bezrobociu w styczniu 2010.

## Ograniczenia

- Dane w aplikacji są migawką; nie odświeżają się automatycznie.
- Średnia miesięczna kursu wygładza szereg; do niektórych zastosowań lepszy byłby kurs z końca miesiąca.
- Różnicowanie usuwa pozorne korelacje, ale może ukryć zależności długookresowe (kointegrację), których tu nie badano.
- Test Grangera mierzy użyteczność predykcyjną, nie przyczynowość w sensie ekonomicznym.
