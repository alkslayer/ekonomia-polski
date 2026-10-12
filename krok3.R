library(tidyverse)
library(httr2)

# API NBP: https://api.nbp.pl/api/exchangerates/rates/a/{waluta}/{od}/{do}/
# Limit: max ~367 dni na jedno zapytanie. Dane archiwalne od 2002-01-02.

# 1. Funkcja: JEDNO zapytanie (zakres <= 365 dni) -> tibble(date, kurs)
fetch_nbp_chunk <- function(waluta, od, do) {
  resp <- request("https://api.nbp.pl/api/exchangerates/rates/a") |>
    req_url_path_append(waluta, as.character(od), as.character(do)) |>
    req_url_query(format = "json") |>
    req_retry(max_tries = 3) |>
    req_error(is_error = \(r) FALSE) |>       # błędy obsłużymy sami (niżej)
    req_perform()

  # 404 = brak notowań w zakresie (np. same święta); to nie awaria
  if (resp_status(resp) == 404) {
    return(tibble(date = as.Date(character()), kurs = numeric()))
  }
  resp_check_status(resp)                      # inny błąd (np. 400) -> stop z komunikatem

  resp |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("rates") |>
    as_tibble() |>
    transmute(date = as.Date(effectiveDate), kurs = mid)
}

# 2. Funkcja główna: dzieli okres na kawałki po 365 dni i skleja wyniki
fetch_nbp <- function(waluta, od, do = Sys.Date()) {
  od <- as.Date(od)
  do <- as.Date(do)

  starty <- seq(od, do, by = "365 days")
  konce  <- pmin(starty + 364, do)             # pmin: ostatni kawałek nie wychodzi poza `do`

  map2(starty, konce, \(s, k) fetch_nbp_chunk(waluta, s, k)) |>
    list_rbind() |>
    mutate(waluta = toupper(waluta), .before = 1) |>
    arrange(date)
}

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
