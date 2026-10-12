library(tidyverse)

# 1. Wczytanie danych zapisanych w kroku 1 (bez ponownego pobierania)
infl <- readRDS("data/infl_clean.rds")

# 2. Szybka kontrola: czy to na pewno to, co zapisaliśmy?
glimpse(infl)
range(infl$date)

# 3. Funkcja rysująca wykres inflacji z linią celu NBP.
#    `od` (opcjonalnie) ogranicza dane do okresu od podanej daty; reszta jest wspólna,
#    więc obie wersje wykresu różnią się tylko danymi i tytułem.
wykres_inflacji <- function(dane, tytul, od = NULL, cel = 2.5) {
  if (!is.null(od)) {
    dane <- filter(dane, date >= as.Date(od))
  }

  ggplot(dane, aes(x = date, y = inflacja)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = cel, linetype = "dashed", color = "firebrick") +
    labs(
      title    = tytul,
      subtitle = sprintf("Roczna zmiana %%; linia przerywana = cel NBP (%s%%)",
                         format(cel, decimal.mark = ",")),
      x        = NULL,
      y        = "Inflacja r/r (%)",
      caption  = "Źródło: Eurostat, prc_hicp_minr"
    ) +
    theme_minimal()
}

# 4. Dwie wersje wykresu
p_all  <- wykres_inflacji(infl, "Inflacja HICP w Polsce, 1997-2026")
p_2010 <- wykres_inflacji(infl, "Inflacja HICP w Polsce od 2010 roku", od = "2010-01-01")

p_all
p_2010

# 5. Zapis do plików
dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/inflacja_cala.png", p_all,  width = 9, height = 5, dpi = 150)
ggsave("wykresy/inflacja_2010.png", p_2010, width = 9, height = 5, dpi = 150)
