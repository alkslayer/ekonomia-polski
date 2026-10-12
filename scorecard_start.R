# Jednorazowe zasilenie scorecardu prognoz historią z backtestu (krok10.R).
# Plik app/data/prognozy_historia.csv zawiera potem dwa rodzaje wierszy:
#   tryb = "backtest"  - prognozy z rolling origin od 2012 (liczone wstecz, wszystkie wiedzą, jak się skończyło),
#   tryb = "na_zywo"   - prognozy zapisane w chwili, gdy dane dopiero się pojawiły
#                        (dopisuje je zapisz_prognoze.R); to jedyne, którym naprawdę można ufać.
#
# Uruchom RAZ (nadpisuje plik, ale zachowuje istniejące wiersze "na_zywo").

library(tidyverse)

sciezka <- "app/data/prognozy_historia.csv"
dane    <- readRDS("data/dane_miesieczne.rds")
wyniki  <- readRDS("data/wyniki_kombinacji.rds")      # z krok10.R

backtest <- wyniki |>
  filter(model %in% c("arima", "naive"), h <= 6) |>
  transmute(
    tryb = "backtest",
    data_prognozy = as.Date(NA),
    dane_do = dane$date[origin],
    cel     = dane$date[origin + h],
    h,
    model = recode(model, naive = "naiwny"),
    prognoza, l80, u80, l95, u95
  )

na_zywo <- if (file.exists(sciezka)) {
  read_csv(sciezka, show_col_types = FALSE) |> filter(tryb == "na_zywo")
} else {
  backtest[0, ]
}

historia <- bind_rows(backtest, na_zywo) |> arrange(tryb, dane_do, model, h)

# kontrola: prognozowany miesiąc ma być po miesiącu danych, o h miesięcy
stopifnot(all(historia$cel > historia$dane_do))

write_csv(historia, sciezka)
historia |> count(tryb, model)
