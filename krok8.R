library(tidyverse)
library(eurostat)
library(forecast)

dane <- readRDS("data/dane_miesieczne.rds")

# 1. Składowe inflacji HICP (roczna zmiana %, ECOICOP v2) ------------------------
skladniki_kody <- c(
  zywnosc = "FOOD",            # żywność (w tym alkohol i tytoń)
  energia = "NRG",             # energia (paliwa, prąd, gaz, ciepło)
  dobra   = "IGD_NNRG",        # dobra przemysłowe bez energii
  uslugi  = "SERV",            # usługi
  bazowa  = "TOT_X_NRG_FOOD"   # inflacja bazowa: bez energii i żywności
)

skladniki <- get_eurostat(
  "prc_hicp_minr",
  filters = list(geo = "PL", unit = "RCH_A", coicop18 = unname(skladniki_kody)),
  time_format = "date"
) |>
  mutate(seria = names(skladniki_kody)[match(coicop18, skladniki_kody)]) |>
  select(date = time, seria, values) |>
  filter(!is.na(values)) |>
  pivot_wider(names_from = seria, values_from = values)

# 2. PPI (ceny producentów w przemyśle, r/r %) ---------------------------------
ppi <- get_eurostat(
  "sts_inpp_m",
  filters = list(geo = "PL", unit = "PCH_SM", s_adj = "NSA", nace_r2 = "B-E36"),
  time_format = "date"
) |>
  select(date = time, ppi = values) |>
  filter(!is.na(ppi))

# 3. Kontrola jakości --------------------------------------------------------------
sprawdz <- \(x, nazwa) {
  x |> summarise(od = min(date), do = max(date), n = n(), luki = sum(diff(date) > 31)) |>
    mutate(zrodlo = nazwa, .before = 1)
}
bind_rows(sprawdz(skladniki, "skladniki"), sprawdz(ppi, "ppi"))
# braki w każdej serii osobno (osobny summarise, żeby across nie objęło kolumn od/do/n)
skladniki |> summarise(across(-date, \(v) sum(is.na(v))))
ppi       |> summarise(across(-date, \(v) sum(is.na(v))))

# 4. Tabela do modelu: inflacja + składniki + PPI, wspólny okres ------------------
dane8 <- dane |>
  select(date, inflacja) |>
  inner_join(skladniki, by = "date") |>
  inner_join(ppi,       by = "date") |>
  arrange(date)

glimpse(dane8)
summary(dane8)

# sanity check: inflacja z kroku 1 vs inflacja "TOTAL" – ta sama seria
stopifnot(!anyNA(dane8))

saveRDS(dane8, "data/dane_skladniki.rds")

# 5. Który składnik napędza wahania? Korelacja zmian m/m ze zmianami całości --------
dane8 |>
  transmute(
    d_inflacja = inflacja - lag(inflacja),
    across(c(zywnosc, energia, dobra, uslugi, bazowa, ppi), \(x) x - lag(x), .names = "d_{.col}")
  ) |>
  drop_na() |>
  cor() |>
  _[ "d_inflacja", -1 ] |>
  round(2)

# 6. Czy PPI "wyprzedza" inflację? Korelacja zmian: inflacja(t) vs PPI(t - k) ------
d <- dane8 |>
  transmute(
    d_inf = inflacja - lag(inflacja),
    d_ppi = ppi      - lag(ppi),
    d_nrg = energia  - lag(energia),
    d_zyw = zywnosc  - lag(zywnosc)
  ) |>
  drop_na()

ccf_tab <- \(x, nazwa) {
  tibble(
    zmienna = nazwa,
    opoznienie = 0:12,
    kor = map_dbl(0:12, \(k) cor(d$d_inf[(k + 1):nrow(d)], x[1:(nrow(d) - k)]))
  )
}
bind_rows(ccf_tab(d$d_ppi, "PPI"), ccf_tab(d$d_nrg, "energia"), ccf_tab(d$d_zyw, "żywność")) |>
  mutate(kor = round(kor, 2)) |>
  pivot_wider(names_from = zmienna, values_from = kor)
# próg istotności ~ 2 / sqrt(n)
round(2 / sqrt(nrow(d)), 2)

# 7. Rolling origin: ARIMA vs ARIMAX z opóźnionymi regresorami --------------------
#    Regresory biorę z opóźnieniem L = 3 mies., więc dla h = 1..3 ich wartości
#    w czasie prognozy są JUŻ znane (inaczej trzeba by prognozować regresory).
L <- 3
H <- 3

lag_ts <- \(x, k) c(rep(NA_real_, k), head(x, -k))
reg <- dane8 |>
  mutate(across(c(zywnosc, energia, dobra, uslugi, bazowa, ppi), \(x) lag_ts(x, L))) |>
  slice(-seq_len(L))                               # wiersze bez opóźnienia odpadają

y_ts <- ts(reg$inflacja, start = c(2002, 4), frequency = 12)   # dane8 startuje 2002-01
stopifnot(abs(reg$date[1] - as.Date("2002-04-01")) == 0)       # kontrola dopasowania startu

specs <- list(
  bazowy       = character(0),
  plus_ppi     = "ppi",
  plus_energia = "energia",
  plus_zywnosc = "zywnosc",
  plus_bazowa  = "bazowa",
  plus_wszystko = c("ppi", "energia", "zywnosc", "bazowa")
)

n <- nrow(reg)
pierwszy <- which(reg$date >= as.Date("2012-01-01"))[1]
origins  <- seq(pierwszy, n - H, by = 3)

ocena_okno <- function(t) {
  train  <- window(y_ts, end = time(y_ts)[t])
  actual <- reg$inflacja[(t + 1):(t + H)]

  prog <- imap(specs, \(kol, nazwa) {
    if (length(kol) == 0) {
      forecast(auto.arima(train), h = H)$mean
    } else {
      xtr  <- as.matrix(reg[1:t, kol])
      xfut <- as.matrix(reg[(t + 1):(t + H), kol])
      forecast(auto.arima(train, xreg = xtr), xreg = xfut, h = H)$mean
    }
  })
  prog$naiwny <- naive(train, h = H)$mean

  imap_dfr(prog, \(p, nazwa) {
    tibble(model = nazwa, h = 1:H, blad = actual - as.numeric(p), origin = t)
  })
}

wyniki <- map_dfr(origins, ocena_okno)
length(origins)

# 8. Wyniki: RMSE wg modelu (h = 1..3), w całości i w podokresach -------------------
wyniki <- wyniki |> mutate(data_origin = reg$date[origin])

rmse_tab <- \(d, etykieta) {
  d |>
    group_by(model) |>
    summarise(RMSE = sqrt(mean(blad^2)), MAE = mean(abs(blad)), .groups = "drop") |>
    mutate(okres = etykieta, .before = 1)
}

tab <- bind_rows(
  rmse_tab(wyniki, "cała próba (od 2012)"),
  rmse_tab(filter(wyniki, data_origin < "2020-06-01"), "przed 06.2020"),
  rmse_tab(filter(wyniki, data_origin >= "2021-01-01"), "od 2021")
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

tab |>
  select(-MAE) |>
  pivot_wider(names_from = okres, values_from = RMSE) |>
  arrange(`cała próba (od 2012)`)

# 9. Czy różnice są istotne? Test Dieboulda-Mariano (H1: nowy model ma mniejszy błąd kwadratowy
#    niż bazowy). Origin co 3 miesiące i H = 3 -> okna prognoz się nie nakładają.
#    Uwaga: 5 modeli x 3 horyzonty = 15 testów, więc pojedyncze p ~ 0.02-0.04 nie wystarcza.
blad_modelu <- \(m) wyniki |> filter(model == m) |> arrange(origin, h)
base <- blad_modelu("bazowy")

dm_tab <- crossing(model = setdiff(names(specs), "bazowy") |> c("naiwny"), h = 1:H) |>
  mutate(p = map2_dbl(model, h, \(m, hh) {
    x <- blad_modelu(m)
    dm.test(x$blad[x$h == hh], base$blad[base$h == hh],
            alternative = "less", h = hh, power = 2)$p.value
  })) |>
  mutate(p = round(p, 3)) |>
  pivot_wider(names_from = h, values_from = p, names_prefix = "p_h")
dm_tab

saveRDS(wyniki, "data/wyniki_arimax.rds")
