library(tidyverse)

dane <- readRDS("data/dane_miesieczne.rds")
seria <- c("inflacja", "bezrobocie", "eur_pln", "usd_pln")

# 1. Korelacje na POZIOMACH
kor_poziomy <- dane |> select(all_of(seria)) |> cor()
round(kor_poziomy, 2)

# 2. Różnicowanie
#    - inflacja, bezrobocie: pierwsza różnica (zmiana w punktach procentowych)
#    - kursy: zmiana procentowa (100 * różnica logarytmów), bo kurs rośnie
#      multiplikatywnie, a nie o stałą kwotę
dane_d <- dane |>
  transmute(
    date,
    d_inflacja   = inflacja   - lag(inflacja),
    d_bezrobocie = bezrobocie - lag(bezrobocie),
    r_eur        = 100 * (log(eur_pln) - log(lag(eur_pln))),
    r_usd        = 100 * (log(usd_pln) - log(lag(usd_pln)))
  ) |>
  drop_na()                                  # pierwszy wiersz ma NA (brak poprzednika)

nrow(dane_d)                                 # 295 = 296 - 1

kor_roznice <- dane_d |> select(-date) |> cor()
round(kor_roznice, 2)

# 3. Porównanie obok siebie (pary unikalne, bez przekątnej)
pary <- t(combn(seria, 2))                   # 6 par z 4 serii (macierz 6x2: wiersz, kolumna)
pary_d <- t(combn(colnames(kor_roznice), 2))

tibble(
  para     = paste(pary[, 1], pary[, 2], sep = " ~ "),
  poziomy  = round(kor_poziomy[pary], 2),
  roznice  = round(kor_roznice[pary_d], 2)
)

# 4. Dlaczego poziomy kłamią? Autokorelacja rzędu 1 (korelacja szeregu z samym sobą
#    sprzed miesiąca). Bliskie 1 = szereg "pamięta" przeszłość, trend/błądzenie losowe
ac1 <- \(x) cor(x[-1], x[-length(x)])
tibble(
  seria    = seria,
  ac1_poziom  = map_dbl(dane[seria], ac1),
  ac1_roznica = map_dbl(dane_d[c("d_inflacja", "d_bezrobocie", "r_eur", "r_usd")], ac1)
) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# 5. Regresja pozorna: symulacja
#    Dwa NIEZALEŻNE błądzenia losowe (z definicji nic ich nie łączy).
#    Jak często korelacja na poziomach "wykazuje" silny związek?
set.seed(123)
n_obs <- 296                                 # tyle, ile mamy miesięcy
symulacja <- map_dfr(1:2000, \(i) {
  x <- cumsum(rnorm(n_obs))                  # błądzenie losowe
  y <- cumsum(rnorm(n_obs))                  # drugie, niezależne
  tibble(
    kor_poziom  = cor(x, y),
    kor_roznica = cor(diff(x), diff(y))
  )
})

symulacja |>
  summarise(
    `% |kor| > 0.5 na poziomach` = 100 * mean(abs(kor_poziom)  > 0.5),
    `% |kor| > 0.5 na różnicach` = 100 * mean(abs(kor_roznica) > 0.5),
    `% |kor| > 0.2 na różnicach` = 100 * mean(abs(kor_roznica) > 0.2)
  )
# Orientacyjny próg istotności 5% dla n = 295: |r| > ok. 1.96 / sqrt(295) = 0.11.
# Na różnicach działa, na poziomach nie (założenia testu są złamane).

# 6. Test istotności jednej pary na różnicach
cor.test(dane_d$d_inflacja, dane_d$r_eur)

# 7. Wykres: macierze korelacji, poziomy vs różnice
do_long <- function(m, typ) {
  as_tibble(m, rownames = "x") |>
    pivot_longer(-x, names_to = "y", values_to = "kor") |>
    mutate(typ = typ)
}

p_kor <- bind_rows(
  do_long(kor_poziomy, "Poziomy"),
  do_long(kor_roznice, "Różnice")
) |>
  mutate(typ = factor(typ, levels = c("Poziomy", "Różnice"))) |>
  ggplot(aes(x, y, fill = kor)) +
  geom_tile() +
  geom_text(aes(label = round(kor, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       limits = c(-1, 1)) +
  facet_wrap(~ typ, scales = "free") +
  labs(title = "Korelacje: poziomy vs różnice", x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dir.create("wykresy", showWarnings = FALSE)
ggsave("wykresy/korelacje.png", p_kor, width = 10, height = 4.5, dpi = 150)

# 8. Zapis tabeli różnic do kroku 6
saveRDS(dane_d, "data/dane_roznice.rds")
