library(tidyverse)
library(httr2)

# API NBP: https://api.nbp.pl/api/exchangerates/rates/a/{waluta}/{od}/{do}/
# Limit: max ~367 dni na jedno zapytanie. Dane archiwalne od 2002-01-02.

# 1-2. Funkcje fetch_nbp_chunk() i fetch_nbp() są w R/fetch_nbp.R
#      (wspólne dla krok3.R i odswiez_dane.R)
source("R/fetch_nbp.R")

# 3. Test na małym zakresie, zanim pobierzemy wszystko
fetch_nbp("eur", "2026-09-01", "2026-09-10")

# 4. Pełne pobranie: EUR i USD od początku archiwum
kursy <- map(c("eur", "usd"), \(w) fetch_nbp(w, "2002-01-02")) |>
  list_rbind()

# 5. Kontrola jakości
glimpse(kursy)

kursy |>
  group_by(waluta) |>
  summarise(
    od       = min(date),
    do       = max(date),
    n        = n(),
    braki_na = sum(is.na(kurs)),
    min_kurs = min(kurs),
    max_kurs = max(kurs),
    duplikaty = sum(duplicated(date))          # powinno być 0 (kawałki się nie nakładają)
  )

# największe przerwy między notowaniami (weekendy = 3 dni, święta dłużej)
kursy |>
  group_by(waluta) |>
  mutate(przerwa = as.integer(date - lag(date))) |>
  slice_max(przerwa, n = 3, with_ties = FALSE)

# 6. Zapis
saveRDS(kursy, "data/kursy_nbp.rds")
