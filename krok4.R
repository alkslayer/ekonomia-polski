library(tidyverse)
library(eurostat)

# 1. Bezrobocie z Eurostatu: stopa bezrobocia, dane wyrównane sezonowo (SA),
#    ogółem, obie płcie, % aktywnych zawodowo
bezr_raw <- get_eurostat(
  "une_rt_m",
  filters = list(geo = "PL", s_adj = "SA", age = "TOTAL", unit = "PC_ACT", sex = "T"),
  time_format = "date"
)

glimpse(bezr_raw)

bezr <- bezr_raw |>
  select(date = time, bezrobocie = values) |>
  filter(!is.na(bezrobocie)) |>
  arrange(date)

bezr |>
  summarise(od = min(date), do = max(date), n = n(), braki_na = sum(is.na(bezrobocie)))
saveRDS(bezr, "data/bezr_clean.rds")

# 2. Wczytanie reszty danych z poprzednich kroków
infl  <- readRDS("data/infl_clean.rds")
kursy <- readRDS("data/kursy_nbp.rds")

# 3. Kursy dzienne -> miesięczne (średnia z notowań w miesiącu)
kursy_m <- kursy |>
  mutate(date = floor_date(date, "month")) |>     # każdy dzień -> 1. dzień miesiąca
  group_by(waluta, date) |>
  summarise(kurs = mean(kurs), n_dni = n(), .groups = "drop")

# kontrola: ile notowań ma miesiąc? (normalnie 19-23, niepełny bieżący miesiąc mniej)
kursy_m |>
  summarise(min_dni = min(n_dni), max_dni = max(n_dni))
kursy_m |> filter(n_dni < 15)                     # podejrzanie krótkie miesiące

# z formatu długiego na szeroki: jedna kolumna na walutę
kursy_w <- kursy_m |>
  select(-n_dni) |>
  pivot_wider(names_from = waluta, values_from = kurs) |>
  rename(eur_pln = EUR, usd_pln = USD)

# 4. Łączenie: inner_join zostawia tylko miesiące, które mają WSZYSTKIE 4 serie
#    (kursy NBP są od 2002, inflacja i bezrobocie do 08.2026, więc wypada
#    niepełny październik 2026 i lata 1997-2001)
dane <- infl |>
  inner_join(bezr,    by = "date") |>
  inner_join(kursy_w, by = "date") |>
  arrange(date)

# 5. Kontrola jakości tabeli końcowej
glimpse(dane)

dane |>
  summarise(od = min(date), do = max(date), n = n())

dane |>
  summarise(across(-date, \(x) sum(is.na(x))))    # braki w każdej serii osobno

# ciągłość miesięcy (jak w kroku 1)
dane |>
  mutate(oczekiwana = seq(min(date), by = "month", length.out = n())) |>
  filter(date != oczekiwana)                      # powinno być pusto

summary(dane)                                     # zakresy wszystkich serii

# 6. Zapis
saveRDS(dane, "data/dane_miesieczne.rds")
