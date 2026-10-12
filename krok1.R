library(tidyverse)
library(eurostat)

# 1. Pobranie: nowa tabela (ECOICOP v2), Polska, ogółem, roczna zmiana %
infl <- get_eurostat(
  "prc_hicp_minr",
  filters = list(geo = "PL", coicop18 = "TOTAL", unit = "RCH_A"),
  time_format = "date"
)

# 2. Oględziny
glimpse(infl)
range(infl$time)

# 3. Czyszczenie: data + wartość, od najstarszych
infl_raw <- infl |>
  select(date = time, inflacja = values) |>
  arrange(date)

# 4. Kontrola jakości SUROWYCH danych (przed usunięciem NA)
infl_raw |>
  summarise(
    od       = min(date),
    do       = max(date),
    n        = n(),
    braki_na = sum(is.na(inflacja))
  )   # oczekiwane: 369 miesięcy, 13 NA (12 z 1996 + wrzesień 2026)

# ciągłość miesięcy: czy każdy miesiąc występuje dokładnie raz?
infl_raw |>
  mutate(oczekiwana = seq(min(date), by = "month", length.out = n())) |>
  filter(date != oczekiwana)        # powinno być pusto

# 5. Usunięcie NA: tylko miesiące z opublikowaną wartością
infl_clean <- infl_raw |>
  filter(!is.na(inflacja))

infl_clean |>
  summarise(
    od       = min(date),
    do       = max(date),
    n        = n(),
    braki_na = sum(is.na(inflacja))
  )   # oczekiwane: 1997-01 do 2026-08, n = 356, braki_na = 0

# 6. Porównanie ze starą tabelą na wspólnym okresie (do grudnia 2025)
infl_old <- get_eurostat(
  "prc_hicp_manr",
  filters = list(geo = "PL", coicop = "CP00"),
  time_format = "date"
) |>
  select(date = time, stara = values)

porownanie <- infl_clean |>
  inner_join(infl_old, by = "date") |>
  mutate(roznica = round(inflacja - stara, 1))   # round() przeciw błędom zmiennoprzecinkowym

summary(porownanie$roznica)
porownanie |> count(roznica)                      # rozkład różnic: ile miesięcy ma 0, 0.1, 0.2
porownanie |> filter(abs(roznica) > 0.1)          # tylko różnice większe niż 0.1 pp

# 7. Zapis
dir.create("data", showWarnings = FALSE)
saveRDS(infl_clean, "data/infl_clean.rds")
