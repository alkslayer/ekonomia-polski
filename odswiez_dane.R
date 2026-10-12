# Odświeżanie danych do aplikacji: pobiera Eurostat i NBP, składa tabelę miesięczną
# i zapisuje ją w data/ oraz app/data/. Uruchamiany ręcznie albo przez GitHub Actions
# (.github/workflows/odswiez.yml). Zatrzymuje się z błędem, jeśli dane wyglądają źle,
# żeby nie wdrożyć aplikacji z uszkodzonymi danymi.
#
# Użycie (z katalogu projektu):  Rscript odswiez_dane.R

library(tidyverse)
library(eurostat)
library(httr2)

source("R/fetch_nbp.R")

sciezki <- c("data/dane_miesieczne.rds", "app/data/dane_miesieczne.rds")
min_dni_w_miesiacu <- 15      # miesiąc z mniejszą liczbą notowań NBP uznajemy za niepełny

# 1. Pobranie -------------------------------------------------------------------------
message("Pobieranie inflacji (Eurostat)...")
infl <- get_eurostat(
  "prc_hicp_minr",
  filters = list(geo = "PL", coicop18 = "TOTAL", unit = "RCH_A"),
  time_format = "date"
) |>
  select(date = time, inflacja = values) |>
  filter(!is.na(inflacja))

message("Pobieranie bezrobocia (Eurostat)...")
bezr <- get_eurostat(
  "une_rt_m",
  filters = list(geo = "PL", s_adj = "SA", age = "TOTAL", unit = "PC_ACT", sex = "T"),
  time_format = "date"
) |>
  select(date = time, bezrobocie = values) |>
  filter(!is.na(bezrobocie))

message("Pobieranie kursów (NBP)...")
kursy <- map(c("eur", "usd"), \(w) fetch_nbp(w, "2002-01-02")) |>
  list_rbind()

# 2. Kursy dzienne -> miesięczne ------------------------------------------------------
kursy_m <- kursy |>
  mutate(date = floor_date(date, "month")) |>
  group_by(waluta, date) |>
  summarise(kurs = mean(kurs), n_dni = n(), .groups = "drop") |>
  filter(n_dni >= min_dni_w_miesiacu)           # odrzuca niepełny bieżący miesiąc

kursy_w <- kursy_m |>
  select(-n_dni) |>
  pivot_wider(names_from = waluta, values_from = kurs) |>
  rename(eur_pln = EUR, usd_pln = USD)

# 3. Łączenie: tylko miesiące, które mają wszystkie cztery serie -----------------------
dane <- infl |>
  inner_join(bezr,    by = "date") |>
  inner_join(kursy_w, by = "date") |>
  arrange(date)

# 4. Walidacja: błąd zatrzymuje skrypt (i wdrożenie) -----------------------------------
stary <- if (file.exists(sciezki[1])) readRDS(sciezki[1]) else NULL

stopifnot(
  "brak danych po złączeniu"             = nrow(dane) > 0,
  "braki (NA) w tabeli"                  = !anyNA(dane),
  "dane zaczynają się inaczej niż 2002-01" = min(dane$date) == as.Date("2002-01-01"),
  "luki w miesiącach"                    = all(diff(dane$date) %in% 28:31),
  "inflacja poza sensownym zakresem"     = all(dane$inflacja > -5 & dane$inflacja < 40),
  "bezrobocie poza sensownym zakresem"   = all(dane$bezrobocie > 0 & dane$bezrobocie < 30),
  "kursy poza sensownym zakresem"        = all(dane$eur_pln > 2 & dane$eur_pln < 8 &
                                               dane$usd_pln > 1 & dane$usd_pln < 8)
)

if (!is.null(stary)) {
  stopifnot(
    "nowa tabela jest krótsza niż poprzednia" = nrow(dane) >= nrow(stary),
    "nowa tabela kończy się wcześniej niż poprzednia" = max(dane$date) >= max(stary$date)
  )
}

# 5. Raport i zapis ----------------------------------------------------------------------
message(sprintf("Dane: %s do %s, %d miesięcy.", min(dane$date), max(dane$date), nrow(dane)))

if (!is.null(stary)) {
  nowe <- nrow(dane) - nrow(stary)
  message(sprintf("Poprzednio: do %s. Nowych miesięcy: %d.", max(stary$date), nowe))
  # rewizje wstecz (Eurostat poprawia dane) – informacyjnie
  wspolne <- inner_join(stary, dane, by = "date", suffix = c("_stare", "_nowe"))
  rewizje <- sum(abs(wspolne$inflacja_stare - wspolne$inflacja_nowe) > 0.05 |
                 abs(wspolne$bezrobocie_stare - wspolne$bezrobocie_nowe) > 0.05)
  message(sprintf("Zrewidowane wartości inflacji/bezrobocia we wspólnym okresie: %d wierszy.", rewizje))
}

zmienione <- is.null(stary) || !isTRUE(all.equal(stary, dane, tolerance = 1e-8))

if (zmienione) {
  walk(sciezki, \(s) { dir.create(dirname(s), showWarnings = FALSE, recursive = TRUE); saveRDS(dane, s) })
  message("Zapisano: ", paste(sciezki, collapse = ", "))
} else {
  message("Brak zmian w danych, nic nie zapisano.")
}
