library(tidyverse)
library(forecast)

dane    <- readRDS("data/dane_miesieczne.rds")
infl_ts <- ts(dane$inflacja, start = c(2002, 1), frequency = 12)

# Cel: (1) czy pasma prognozy (80%, 95%) mają deklarowane pokrycie? (2) czy kombinacja
# kilku modeli bije pojedyncze? Wszystko w rolling origin, jak w krok7.R.

h_max   <- 12
n       <- length(infl_ts)
pierwszy <- which(time(infl_ts) >= 2012)[1]
origins <- seq(pierwszy, n - h_max, by = 3)

# 1. Jedno okno: modele, prognozy punktowe i pasma ---------------------------------------
ocena_okno <- function(t) {
  train  <- window(infl_ts, end = time(infl_ts)[t])
  actual <- infl_ts[(t + 1):(t + h_max)]

  prog <- list(
    arima = forecast(auto.arima(train), h = h_max, level = c(80, 95)),
    ets   = forecast(ets(train),        h = h_max, level = c(80, 95)),
    theta = thetaf(train,               h = h_max, level = c(80, 95)),
    naive = naive(train,                h = h_max, level = c(80, 95))
  )

  punktowe <- imap_dfr(prog, \(f, m) {
    tibble(model = m, h = 1:h_max, prognoza = as.numeric(f$mean),
           l80 = f$lower[, 1], u80 = f$upper[, 1],
           l95 = f$lower[, 2], u95 = f$upper[, 2])
  })

  # kombinacja: prosta średnia punktowych prognoz ARIMA, ETS i Theta (bez pasm)
  kombinacja <- punktowe |>
    filter(model %in% c("arima", "ets", "theta")) |>
    group_by(h) |>
    summarise(prognoza = mean(prognoza), .groups = "drop") |>
    mutate(model = "kombinacja", l80 = NA_real_, u80 = NA_real_, l95 = NA_real_, u95 = NA_real_)

  bind_rows(punktowe, kombinacja) |>
    mutate(actual = actual[h], blad = actual - prognoza, origin = t)
}

wyniki <- map_dfr(origins, ocena_okno)
wyniki <- wyniki |> mutate(data_origin = dane$date[origin])
length(origins)

# 2. Dokładność punktowa: RMSE wg grupy horyzontów ------------------------------------------
rmse <- \(x) sqrt(mean(x^2))

wyniki |>
  mutate(horyzont = case_when(h <= 3 ~ "h 1-3", h <= 6 ~ "h 4-6", TRUE ~ "h 7-12")) |>
  group_by(model, horyzont) |>
  summarise(RMSE = rmse(blad), .groups = "drop") |>
  pivot_wider(names_from = horyzont, values_from = RMSE) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  arrange(`h 1-3`)

# w podokresach (wszystkie horyzonty 1-3, najważniejsze)
podokres <- \(d, etykieta) {
  d |> filter(h <= 3) |> group_by(model) |>
    summarise(RMSE = rmse(blad), .groups = "drop") |> mutate(okres = etykieta)
}
bind_rows(
  podokres(wyniki, "cała próba"),
  podokres(filter(wyniki, data_origin < "2020-06-01"), "przed 06.2020"),
  podokres(filter(wyniki, data_origin >= "2021-01-01"), "od 2021")
) |>
  mutate(RMSE = round(RMSE, 2)) |>
  pivot_wider(names_from = okres, values_from = RMSE) |>
  arrange(`cała próba`)

# 3. Czy kombinacja jest istotnie lepsza od ARIMA? Test Dieboulda-Mariano (h = 1..3)
blad_modelu <- \(m) wyniki |> filter(model == m) |> arrange(origin, h)
arima_b <- blad_modelu("arima")
map_dbl(1:3, \(hh) {
  k <- blad_modelu("kombinacja")
  dm.test(k$blad[k$h == hh], arima_b$blad[arima_b$h == hh],
          alternative = "less", h = hh, power = 2)$p.value
}) |> round(3) |> setNames(paste0("p_h", 1:3))

# 4. Kalibracja pasm: jak często rzeczywistość mieści się w paśmie?
#    Deklarowane pokrycie: 80% i 95%. Wartości wyraźnie niższe = model zbyt pewny siebie.
pokrycie <- wyniki |>
  filter(model != "kombinacja") |>
  mutate(w80 = actual >= l80 & actual <= u80,
         w95 = actual >= l95 & actual <= u95,
         horyzont = case_when(h <= 3 ~ "h 1-3", h <= 6 ~ "h 4-6", TRUE ~ "h 7-12")) |>
  group_by(model, horyzont) |>
  summarise(pokrycie_80 = 100 * mean(w80), pokrycie_95 = 100 * mean(w95), .groups = "drop") |>
  mutate(across(starts_with("pokrycie"), \(x) round(x, 0)))

pokrycie |> pivot_wider(names_from = horyzont, values_from = c(pokrycie_80, pokrycie_95))

# szerokość pasm 80% (średnia, p.p.): czy modele różnią się "ostrożnością"?
wyniki |>
  filter(model != "kombinacja", h %in% c(1, 3, 6, 12)) |>
  group_by(model, h) |>
  summarise(szerokosc_80 = round(mean(u80 - l80), 2), .groups = "drop") |>
  pivot_wider(names_from = h, values_from = szerokosc_80, names_prefix = "h")

# 4a. Kalibracja w podokresach: pasma są dobre w spokojnych czasach, za wąskie w szoku 2021-23
wyniki |>
  filter(model == "arima") |>
  mutate(
    okres = case_when(data_origin < "2020-06-01" ~ "przed 06.2020",
                      data_origin >= "2021-01-01" ~ "od 2021", TRUE ~ "2020-2021"),
    horyzont = case_when(h <= 3 ~ "h 1-3", h <= 6 ~ "h 4-6", TRUE ~ "h 7-12")
  ) |>
  group_by(okres, horyzont) |>
  summarise(n_okien = n_distinct(origin),
            pokrycie_80 = round(100 * mean(actual >= l80 & actual <= u80)),
            pokrycie_95 = round(100 * mean(actual >= l95 & actual <= u95)), .groups = "drop")

# 4b. Próba naprawy: pasma empiryczne z historycznych błędów prognoz (tylko przeszłość!).
#     Dla origin t bierzemy błędy z okien, których cel był już znany w chwili t.
#     Wynik: GORSZE pokrycie niż pasma modelu (62% vs 77% dla h 1-3, 80%), bo przed 2021
#     błędy były małe i nie "wiedziały" o szoku. Zostajemy przy pasmach modelu.
arima_w <- filter(wyniki, model == "arima")
pasma_emp <- \(level, min_n = 8) {
  a <- (1 - level / 100) / 2
  map_dfr(sort(unique(arima_w$origin)), \(t) map_dfr(1:h_max, \(hh) {
    past <- filter(arima_w, h == hh, origin + hh <= t)
    if (nrow(past) < min_n) return(NULL)
    cur <- filter(arima_w, h == hh, origin == t)
    q <- quantile(past$blad, c(a, 1 - a), names = FALSE)
    tibble(origin = t, h = hh, szer = q[2] - q[1],
           w_pasmie = cur$actual >= cur$prognoza + q[1] & cur$actual <= cur$prognoza + q[2])
  }))
}
pasma_emp(80) |>
  group_by(horyzont = case_when(h <= 3 ~ "h 1-3", h <= 6 ~ "h 4-6", TRUE ~ "h 7-12")) |>
  summarise(pokrycie_80_empiryczne = round(100 * mean(w_pasmie)), szerokosc = round(mean(szer), 2))

# 5. Wykres: pokrycie pasm 80% ARIMA wg horyzontu
p <- wyniki |>
  filter(model == "arima") |>
  mutate(w80 = actual >= l80 & actual <= u80) |>
  group_by(h) |>
  summarise(pokrycie = 100 * mean(w80)) |>
  ggplot(aes(h, pokrycie)) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "firebrick") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title    = "Pokrycie pasma 80% prognozy ARIMA wg horyzontu",
    subtitle = sprintf("%d okien testowych od 2012; linia przerywana = deklarowane pokrycie", length(origins)),
    x = "Horyzont (miesiące)", y = "Odsetek obserwacji w paśmie (%)"
  ) +
  theme_minimal()

dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/kalibracja_pasm.png", p, width = 9, height = 5, dpi = 150)

saveRDS(wyniki, "data/wyniki_kombinacji.rds")
