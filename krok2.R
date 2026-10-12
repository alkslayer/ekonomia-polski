library(tidyverse)

# 1. Wczytanie danych zapisanych w kroku 1 (bez ponownego pobierania)
infl <- readRDS("data/infl_clean.rds")

# 2. Szybka kontrola: czy to na pewno to, co zapisaliśmy?
glimpse(infl)
range(infl$date)

# 3. Wykres bazowy: cała historia
p_all <- ggplot(infl, aes(x = date, y = inflacja)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "firebrick") +
  labs(
    title    = "Inflacja HICP w Polsce, 1997-2026",
    subtitle = "Roczna zmiana %; linia przerywana = cel NBP (2,5%)",
    x        = NULL,
    y        = "Inflacja r/r (%)",
    caption  = "Źródło: Eurostat, prc_hicp_minr"
  ) +
  theme_minimal()

p_all

# 4. Wersja od 2010: filtrujemy dane, reszta kodu ta sama
p_2010 <- infl |>
  filter(date >= as.Date("2010-01-01")) |>
  ggplot(aes(x = date, y = inflacja)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "firebrick") +
  labs(
    title    = "Inflacja HICP w Polsce od 2010 roku",
    subtitle = "Roczna zmiana %; linia przerywana = cel NBP (2,5%)",
    x        = NULL,
    y        = "Inflacja r/r (%)",
    caption  = "Źródło: Eurostat, prc_hicp_minr"
  ) +
  theme_minimal()

p_2010

# 5. Zapis do plików
dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/inflacja_cala.png", p_all,  width = 9, height = 5, dpi = 150)
ggsave("wykresy/inflacja_2010.png", p_2010, width = 9, height = 5, dpi = 150)
