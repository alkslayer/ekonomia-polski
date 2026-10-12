# Ekonomia Polski: dashboard

Interaktywna aplikacja Shiny z danymi makroekonomicznymi Polski (Eurostat, NBP):
wykresy szeregów czasowych, korelacje na poziomach i różnicach, prognoza ARIMA
oraz test przyczynowości Grangera.

**Aplikacja online:** https://mitek.shinyapps.io/ekonomia-polski/

**Kod źródłowy:** https://github.com/alkslayer/ekonomia-polski

## Dane

| Seria | Źródło | Zakres |
|---|---|---|
| Inflacja HICP (r/r, %) | Eurostat, `prc_hicp_minr` | 01.1997 – 08.2026 |
| Stopa bezrobocia (SA, % aktywnych) | Eurostat, `une_rt_m` | 01.1997 – 08.2026 |
| EUR/PLN, USD/PLN (średnia miesięczna z kursów średnich) | NBP, tabela A | 01.2002 – 08.2026 |

Tabela końcowa (`data/dane_miesieczne.rds`) obejmuje wspólny okres **01.2002 – 08.2026**
(296 miesięcy, bez braków; stan na 12.10.2026). Dane są odświeżane automatycznie, zob. niżej.

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
| `krok7.R` | Porównanie ARIMA z sezonowością i bez, ocena na 55 oknach czasowych (rolling origin) vs prognoza naiwna |
| `krok8.R` | Składowe HICP i PPI z Eurostatu; test ARIMAX (regresory z opóźnieniem 3 mies.) na 58 oknach czasowych, test Dieboulda-Mariano |
| `krok9.R` | Porównanie średniej rocznej inflacji 2026 z ARIMA z projekcją NBP z lipca 2026 (`data/projekcje_nbp.csv`) |
| `odswiez_dane.R` | Odświeżanie danych: pobiera Eurostat i NBP, waliduje i zapisuje tabelę dla aplikacji |
| `R/fetch_nbp.R` | Funkcje do API NBP (wspólne dla `krok3.R` i `odswiez_dane.R`) |
| `.github/workflows/odswiez.yml` | GitHub Actions: co miesiąc odświeża dane i wdraża aplikację |
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

## Automatyczne odświeżanie danych

`odswiez_dane.R` pobiera inflację i bezrobocie z Eurostatu oraz kursy z NBP, składa tabelę
miesięczną i **waliduje** ją (brak NA i luk, sensowne zakresy, tabela nie może być krótsza
ani kończyć się wcześniej niż poprzednia). Przy błędzie walidacji skrypt zatrzymuje się
i nic nie jest wdrażane.

Workflow `.github/workflows/odswiez.yml` uruchamia go 20. dnia każdego miesiąca (oraz ręcznie
z zakładki Actions). Jeśli dane się zmieniły, wdraża aplikację na shinyapps.io i commituje nowe
pliki. Wymaga sekretów repozytorium: `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`
(Settings → Secrets and variables → Actions).

Ręcznie: `Rscript odswiez_dane.R` z katalogu projektu.

## Wnioski analityczne

- **Korelacje na poziomach są pozorne.** Inflacja, bezrobocie i kursy mają trend, więc na poziomach
  korelują (np. inflacja ~ EUR/PLN: 0,37), a po zróżnicowaniu związek znika (0,01).
  Symulacja dwóch niezależnych błądzeń losowych daje |r| > 0,5 w ok. 40% przypadków.
- **ARIMA dla inflacji:** auto.arima wybiera ARIMA(1,1,1)(1,0,2)[12]. Na ostatnich 12 miesiącach
  RMSE modelu (0,33) jest tylko nieznacznie lepsze niż naiwnej prognozy „jak dziś” (0,37).
  Pasma prognozy szybko się rozszerzają.
- **Składnik sezonowy jest potrzebny.** ARIMA bez sezonowości ma dużo gorsze AICc (357 vs 238)
  i reszty, które nie są białym szumem (Ljung-Box p ≈ 4·10⁻⁹).
- **Ocena na 55 oknach czasowych (od 2012):** do ok. 3 miesięcy w przód ARIMA z sezonowością
  jest wyraźnie lepsza od naiwnej prognozy (RMSE 0,67 vs 0,94), dalej już nie
  (RMSE dla h = 4–12: 2,96 vs 2,88). Wyniki zawyża szok inflacyjny z lat 2021–2023.
- **Dodatkowe zmienne nie poprawiły prognozy w wiarygodny sposób.** Dodanie PPI, inflacji
  bazowej lub składowych HICP (żywność, energia) do ARIMA obniża RMSE o ok. 10% (0,62 vs 0,69
  dla h = 1–3), ale cała poprawa pochodzi z okresu po 2021 roku (przed 2020: 0,48 vs 0,48),
  a test Dieboulda-Mariano daje p ≈ 0,1–0,2 (jedynie żywność ma p ≈ 0,02–0,04 przy 15 testach).
  Model ze wszystkimi czterema regresorami naraz jest gorszy od bazowego (przeuczenie).
  Wynik pokazuje, że dodatkowe dane nie wystarczają bez oceny poza próbą.
- **Porównanie z projekcją NBP (lipiec 2026, CPI 2,9% w 2026):** średnia roczna 2026 implikowana
  przez ARIMA zmienia się z każdym nowym miesiącem danych (2,68–3,34% dla danych do maja–sierpnia),
  co pokazuje, że dłuższych prognoz modelu nie należy traktować poważnie. Porównanie nie jest
  wprost uczciwe: NBP projektuje CPI, a my mamy HICP, które w 2026 jest o ok. 0,2–0,4 p.p. wyższe,
  a liczby NBP pochodzą z prasy (nbp.pl blokuje automatyczne pobieranie), z pominięciem ścieżki
  miesięcznej i przedziałów.
- **Test Grangera:** nie znaleziono wiarygodnej zależności predykcyjnej między inflacją,
  bezrobociem i kursami. Jedyny istotny wynik (EUR/PLN → bezrobocie) nie jest odporny:
  znika po ograniczeniu próby do okresu od 2012 roku i prawdopodobnie wynika z przerwy
  w danych o bezrobociu w styczniu 2010.

## Ograniczenia

- Dane odświeżają się raz w miesiącu (po publikacji Eurostatu); kursy NBP są dla miesiąca bieżącego pomijane do czasu, aż będzie ich co najmniej 15.
- Analizy z `krok5.R`–`krok8.R` i wnioski w tym README dotyczą danych z 12.10.2026 i nie aktualizują się same.
- Średnia miesięczna kursu wygładza szereg; do niektórych zastosowań lepszy byłby kurs z końca miesiąca.
- Różnicowanie usuwa pozorne korelacje, ale może ukryć zależności długookresowe (kointegrację), których tu nie badano.
- Test Grangera mierzy użyteczność predykcyjną, nie przyczynowość w sensie ekonomicznym.

## Licencja

MIT, zob. plik [LICENSE](LICENSE).
