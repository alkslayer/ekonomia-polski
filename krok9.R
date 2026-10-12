library(tidyverse)
library(forecast)

# Porównanie prognozy ARIMA z projekcją NBP (lipiec 2026) ----------------------------
#
# Ważne zastrzeżenia (wynik nie jest "pojedynek modeli"):
#  1. NBP prognozuje CPI (GUS), a my mamy HICP (Eurostat). W 2026 HICP było o ok. 0,2-0,4 p.p.
#     WYŻSZE niż CPI (np. styczeń: HICP 2,5% vs CPI 2,1%). Poziomy nie są wprost porównywalne.
#  2. Z projekcji NBP mamy tylko średnie roczne (liczby z prasy, nie z raportu NBP:
#     nbp.pl blokuje automatyczne pobieranie). Nie mamy ścieżki miesięcznej ani przedziałów.
#  3. Zero wniosków o "lepszym modelu": jeden rok, jedna projekcja.

dane <- readRDS("data/dane_miesieczne.rds")
nbp  <- read_csv("data/projekcje_nbp.csv", show_col_types = FALSE)

nbp |> filter(rok == 2026)                      # co NBP zakładał dla 2026

nbp_lipiec <- nbp |>
  filter(projekcja == "lipiec 2026", wskaznik == "CPI", rok == 2026) |>
  pull(wartosc)

# 1. Co mówi sama historia 2026? (HICP, dane Eurostatu)
d26 <- dane |> filter(date >= "2026-01-01")
d26 |> summarise(srednia_hicp_sty_sie = round(mean(inflacja), 2), n = n())

# 2. Średnia roczna 2026 implikowana przez ARIMA, zależnie od tego, do kiedy mamy dane
#    średnia = (fakty od stycznia do końca danych + prognoza do grudnia) / 12
y <- ts(dane$inflacja, start = c(2002, 1), frequency = 12)

srednia_2026 <- \(koniec_miesiac) {
  tr   <- window(y, end = c(2026, koniec_miesiac))
  fakt <- as.numeric(window(y, start = c(2026, 1), end = c(2026, koniec_miesiac)))
  prog <- as.numeric(forecast(auto.arima(tr), h = 12 - koniec_miesiac)$mean)
  tibble(
    dane_do          = sprintf("2026-%02d", koniec_miesiac),
    srednia_2026_arima = round(mean(c(fakt, prog)), 2),
    prognoza_grudnia = round(tail(prog, 1), 2)
  )
}

wyniki <- map_dfr(5:8, srednia_2026) |>
  mutate(nbp_lipiec_cpi = nbp_lipiec, roznica = srednia_2026_arima - nbp_lipiec)
wyniki

# Wniosek do przeczytania: ARIMA daje inną "średnią roczną 2026" za każdym razem, gdy dochodzi
# nowy miesiąc (rozrzut ok. 0,6 p.p.), a część różnicy wobec NBP to różnica HICP vs CPI.

# 3. Wykres: stabilność prognozy rocznej ARIMA vs punkt odniesienia NBP
p <- ggplot(wyniki, aes(dane_do, srednia_2026_arima)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_hline(yintercept = nbp_lipiec, linetype = "dashed", color = "firebrick") +
  geom_text(aes(label = srednia_2026_arima), vjust = -0.4) +
  labs(
    title    = "Średnia roczna inflacji 2026: ARIMA a projekcja NBP",
    subtitle = sprintf(paste("Linia przerywana: projekcja NBP z lipca 2026 (CPI), %.1f%%.",
                             "ARIMA liczy HICP, więc poziomy nie są wprost porównywalne"), nbp_lipiec),
    x = "Dane użyte do prognozy (do miesiąca)", y = "Średnia 2026 (%)",
    caption = "Źródła: Eurostat; projekcja NBP z lipca 2026 (liczby z prasy)"
  ) +
  theme_minimal()

dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/porownanie_nbp.png", p, width = 9, height = 5, dpi = 150)
