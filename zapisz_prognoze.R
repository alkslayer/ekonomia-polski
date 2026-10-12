# Zapisuje prognozę "na żywo" w scorecardzie, gdy pojawią się nowe dane.
# Uruchamiany po odswiez_dane.R (także w GitHub Actions). Idempotentny: dla tego samego
# miesiąca danych (dane_do) nie dopisuje drugi raz, więc ponowne uruchomienie niczego nie psuje.
#
# Prognozy na żywo NIE są nigdy nadpisywane ani poprawiane wstecz; o to chodzi w scorecardzie.

library(tidyverse)
library(forecast)

source("R/prognoza.R")

sciezka <- "app/data/prognozy_historia.csv"
dane    <- readRDS("app/data/dane_miesieczne.rds")
ostatni <- max(dane$date)

historia <- read_csv(sciezka, show_col_types = FALSE)

juz_jest <- any(historia$tryb == "na_zywo" & historia$dane_do == ostatni)

if (juz_jest) {
  message("Prognoza na żywo dla danych do ", format(ostatni, "%Y-%m"), " już zapisana, pomijam.")
} else {
  nowa <- prognozuj_inflacje(dane, h = 6) |>
    mutate(tryb = "na_zywo", data_prognozy = Sys.Date(), .before = 1)

  historia |>
    bind_rows(nowa) |>
    arrange(tryb, dane_do, model, h) |>
    write_csv(sciezka)

  message("Dopisano prognozę na żywo (dane do ", format(ostatni, "%Y-%m"), "), ",
          nrow(nowa), " wierszy.")
}
