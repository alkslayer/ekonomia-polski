library(tidyverse)
library(forecast)

dane    <- readRDS("data/dane_miesieczne.rds")
infl_ts <- ts(dane$inflacja, start = c(2002, 1), frequency = 12)

# 1. Dopasowanie na pełnej próbie: z sezonowością (domyślnie) vs bez
fit_sez  <- auto.arima(infl_ts)
fit_nosz <- auto.arima(infl_ts, seasonal = FALSE)

fit_sez
fit_nosz

tibble(
  model = c("z sezonowością", "bez sezonowości"),
  spec  = c(as.character(fit_sez), as.character(fit_nosz)),
  k     = c(length(coef(fit_sez)), length(coef(fit_nosz))),
  AICc  = c(fit_sez$aicc, fit_nosz$aicc),
  BIC   = c(fit_sez$bic,  fit_nosz$bic)
)

# 1a. Diagnostyka reszt modelu bez sezonowości (Ljung-Box, H0: biały szum)
checkresiduals(fit_nosz, plot = FALSE)

# 2. Ocena na wielu oknach (rolling origin):
#    dla każdej daty startu t uczymy modele na danych do t, prognozujemy 12 mies. w przód
#    i porównujemy z rzeczywistością. Dużo dłuższa i uczciwsza ocena niż jeden rok.
h_max   <- 12
n       <- length(infl_ts)
pierwszy <- which(time(infl_ts) >= 2012)[1]          # pierwszy origin: 01.2012
origins <- seq(pierwszy, n - h_max, by = 3)           # co 3 miesiące, żeby się policzyło szybko
length(origins)

ocena_okno <- function(t) {
  train  <- ts(infl_ts[1:t], start = start(infl_ts), frequency = 12)
  actual <- infl_ts[(t + 1):(t + h_max)]

  prog <- list(
    sezonowy = forecast(auto.arima(train),                   h = h_max)$mean,
    bez_sez  = forecast(auto.arima(train, seasonal = FALSE), h = h_max)$mean,
    naiwny   = naive(train, h = h_max)$mean               # naive() sam zwraca prognozę
  )

  imap_dfr(prog, \(p, nazwa) {
    tibble(model = nazwa, h = 1:h_max, blad = actual - as.numeric(p), origin = t)
  })
}

set.seed(1)
wyniki <- map_dfr(origins, ocena_okno)

# 3. Błędy wg horyzontu
metryki <- wyniki |>
  group_by(model, h) |>
  summarise(RMSE = sqrt(mean(blad^2)), MAE = mean(abs(blad)), .groups = "drop")

metryki |>
  filter(h %in% c(1, 3, 6, 12)) |>
  pivot_wider(names_from = model, values_from = c(RMSE, MAE)) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# średnio po wszystkich horyzontach
wyniki |>
  group_by(model) |>
  summarise(RMSE = sqrt(mean(blad^2)), MAE = mean(abs(blad))) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# 4. Wykres: RMSE w funkcji horyzontu
p <- ggplot(metryki, aes(h, RMSE, color = model)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(
    values = c(sezonowy = "firebrick", bez_sez = "steelblue", naiwny = "grey40"),
    labels = c(sezonowy = "ARIMA z sezonowością", bez_sez = "ARIMA bez sezonowości",
               naiwny = "Naiwny (jak dziś)")
  ) +
  labs(
    title    = "Błąd prognozy inflacji wg horyzontu",
    subtitle = sprintf("%d okien testowych, od 2012", length(origins)),
    x = "Horyzont (miesiące)", y = "RMSE (p.p.)", color = NULL
  ) +
  theme_minimal()

dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/porownanie_modeli.png", p, width = 9, height = 5, dpi = 150)

# 5. Zapis wybranej specyfikacji (bez sezonowości) do aplikacji
saveRDS(fit_nosz, "data/model_arima_inflacja_nosez.rds")
saveRDS(wyniki, "data/wyniki_oceny_modeli.rds")
