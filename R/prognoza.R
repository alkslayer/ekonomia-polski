# Prognoza inflacji (ARIMA + benchmark naiwny) w formacie scorecardu.
# Używana w zapisz_prognoze.R. Wymaga: tidyverse, forecast.

prognozuj_inflacje <- function(dane, h = 6, poziomy = c(80, 95)) {
  poczatek <- c(as.integer(format(min(dane$date), "%Y")),
                as.integer(format(min(dane$date), "%m")))
  y <- ts(dane$inflacja, start = poczatek, frequency = 12)

  # miesiące docelowe: h kolejnych miesięcy po ostatnim miesiącu danych
  cele <- seq(max(dane$date), by = "month", length.out = h + 1)[-1]

  do_tabeli <- \(f, nazwa) {
    tibble(
      model = nazwa, h = seq_len(h), cel = cele,
      prognoza = as.numeric(f$mean),
      l80 = f$lower[, 1], u80 = f$upper[, 1],
      l95 = f$lower[, 2], u95 = f$upper[, 2]
    )
  }

  bind_rows(
    do_tabeli(forecast(auto.arima(y), h = h, level = poziomy), "arima"),
    do_tabeli(naive(y, h = h, level = poziomy), "naiwny")
  ) |>
    mutate(dane_do = max(dane$date), .before = 1)
}
