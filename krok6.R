library(tidyverse)
library(forecast)
library(tseries)
library(lmtest)

dane   <- readRDS("data/dane_miesieczne.rds")
dane_d <- readRDS("data/dane_roznice.rds")

# 1. Stacjonarność: test ADF (H0: szereg ma pierwiastek jednostkowy = błądzi)
#    Mała p-wartość (< 0.05) -> odrzucamy H0 -> szereg stacjonarny.
#    Uwaga: adf.test ma wbudowany wybór liczby opóźnień; wyniki są orientacyjne.
adf_p <- \(x) suppressWarnings(adf.test(x)$p.value)

seria_poziom  <- c("inflacja", "bezrobocie", "eur_pln", "usd_pln")
seria_roznica <- c("d_inflacja", "d_bezrobocie", "r_eur", "r_usd")   # w tej samej kolejności

tibble(
  seria     = seria_poziom,
  p_poziom  = map_dbl(seria_poziom,  \(s) adf_p(dane[[s]])),
  p_roznica = map_dbl(seria_roznica, \(s) adf_p(dane_d[[s]]))
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# 2. Model ARIMA dla inflacji
#    ts(): obiekt szeregu czasowego, frequency = 12 (dane miesięczne)
infl_ts <- ts(dane$inflacja, start = c(2002, 1), frequency = 12)

fit <- auto.arima(infl_ts)
fit                                          # wybrany rząd (p,d,q)

# 2a. Diagnostyka reszt: czy model "wyłapał" całą strukturę?
#     Ljung-Box: H0 = reszty to biały szum. Duża p-wartość = dobrze.
checkresiduals(fit, plot = FALSE)

# 3. Uczciwa ocena: trenujemy na danych do 08.2025, testujemy na ostatnich 12 mies.
train <- window(infl_ts, end = c(2025, 8))
test  <- window(infl_ts, start = c(2025, 9))

fit_train <- auto.arima(train)
fc_test   <- forecast(fit_train, h = length(test))

accuracy(fc_test, test)                      # RMSE, MAE na zbiorze testowym

# punkt odniesienia: naiwna prognoza "jutro = dziś"
accuracy(naive(train, h = length(test)), test)

# 4. Prognoza właściwa: 12 miesięcy w przód od 09.2026
fc <- forecast(fit, h = 12)
fc

p_fc <- autoplot(fc, include = 60) +         # include: ile ostatnich obserwacji pokazać
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "firebrick") +
  labs(
    title    = "Prognoza inflacji HICP w Polsce (ARIMA)",
    subtitle = "Pasma: 80% i 95%; linia przerywana = cel NBP (2,5%)",
    x = NULL, y = "Inflacja r/r (%)"
  ) +
  theme_minimal()

dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/prognoza_inflacji.png", p_fc, width = 9, height = 5, dpi = 150)

# 5. Test przyczynowości Grangera (na szeregach STACJONARNYCH, czyli różnicach)
#    H0: opóźnienia x NIE pomagają przewidywać y (ponad to, co daje historia y).
#    grangertest(y ~ x, order = k)  ->  "czy x Granger-powoduje y?"
granger_p <- \(y, x, k) {
  grangertest(dane_d[[y]] ~ dane_d[[x]], order = k)$`Pr(>F)`[2]
}

pary <- tribble(
  ~x,             ~y,
  "d_inflacja",   "d_bezrobocie",
  "d_bezrobocie", "d_inflacja",
  "r_eur",        "d_inflacja",
  "d_inflacja",   "r_eur",
  "r_usd",        "d_inflacja",
  "d_inflacja",   "r_usd",
  "r_eur",        "d_bezrobocie",
  "d_bezrobocie", "r_eur",
  "r_usd",        "d_bezrobocie",
  "d_bezrobocie", "r_usd"
)

wyniki_granger <- pary |>
  crossing(opoznienie = c(1, 3, 6, 12)) |>
  mutate(p = pmap_dbl(list(y, x, opoznienie), granger_p)) |>
  mutate(kierunek = paste(x, "->", y), .before = 1) |>
  select(kierunek, opoznienie, p) |>
  pivot_wider(names_from = opoznienie, values_from = p, names_prefix = "lag_") |>
  mutate(across(starts_with("lag_"), \(v) round(v, 3)))

wyniki_granger

# ile testów zrobiliśmy? 10 kierunków x 4 opóźnienia = 40 -> przy 5% poziomie
# spodziewamy się ok. 2 "istotnych" przypadkowo (problem wielokrotnych porównań)
nrow(pary) * 4 * 0.05

saveRDS(fit, "data/model_arima_inflacja.rds")
