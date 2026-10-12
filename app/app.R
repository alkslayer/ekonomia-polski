library(shiny)
library(bslib)
library(tidyverse)
library(forecast)
library(lmtest)

# ---- Dane (liczone raz, przy starcie aplikacji) -----------------------------
dane <- readRDS("data/dane_miesieczne.rds")

nazwy <- c(
  inflacja   = "Inflacja HICP r/r (%)",
  bezrobocie = "Stopa bezrobocia (%)",
  eur_pln    = "EUR/PLN",
  usd_pln    = "USD/PLN"
)

# szeregi stacjonarne: zmiana w p.p. (inflacja, bezrobocie) i zmiana % (kursy)
dane_d <- dane |>
  transmute(
    date,
    inflacja   = inflacja   - lag(inflacja),
    bezrobocie = bezrobocie - lag(bezrobocie),
    eur_pln    = 100 * (log(eur_pln) - log(lag(eur_pln))),
    usd_pln    = 100 * (log(usd_pln) - log(lag(usd_pln)))
  ) |>
  drop_na()

historia <- read_csv("data/prognozy_historia.csv", show_col_types = FALSE)

dane_dlugie <- dane |>
  pivot_longer(-date, names_to = "seria", values_to = "wartosc") |>
  mutate(seria = factor(seria, levels = names(nazwy), labels = nazwy))

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "Ekonomia Polski: dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  nav_panel(
    "Szeregi",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "seria", "Szeregi:",
          choices  = setNames(names(nazwy), nazwy),
          selected = c("inflacja", "bezrobocie")
        ),
        dateRangeInput(
          "zakres", "Okres:",
          start = min(dane$date), end = max(dane$date),
          min   = min(dane$date), max = max(dane$date),
          format = "yyyy-mm", startview = "decade", language = "pl"
        ),
        checkboxInput("cel_nbp", "Linia celu NBP (2,5%)", value = TRUE)
      ),
      plotOutput("wykres_szeregi", height = "560px")
    )
  ),

  nav_panel(
    "Korelacje",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "kor_typ", "Policz na:",
          choices = c("Poziomach" = "poziomy", "Różnicach" = "roznice")
        ),
        helpText(
          "Szeregi na poziomach mają trend, więc korelują pozornie.",
          "Różnice (zmiana m/m) pokazują związek współczesny bez tego efektu."
        )
      ),
      plotOutput("wykres_kor", height = "520px")
    )
  ),

  nav_panel(
    "Prognoza ARIMA",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("prog_seria", "Szereg:", choices = setNames(names(nazwy), nazwy)),
        sliderInput("prog_h", "Horyzont (miesiące):", min = 3, max = 24, value = 3),
        helpText("Model dobiera automatycznie auto.arima(). Pasma: 80% i 95%.")
      ),
      plotOutput("wykres_prog", height = "460px"),
      div(
        class = "alert alert-warning",
        strong("Uwaga: "),
        "pasma prognozy rozszerzają się z każdym miesiącem. W testach na 55 oknach czasowych",
        "(od 2012) model jest wyraźnie lepszy od założenia „inflacja zostanie taka jak dziś”",
        "tylko na horyzoncie do ok. 3 miesięcy. Dalej nie jest od niego lepszy, więc dłuższą",
        "prognozę traktuj jako scenariusz bazowy, nie przewidywanie.",
        "Pasma 80% w testach obejmowały 78% obserwacji do 3 miesięcy (84% w spokojnych latach",
        "przed 2020), ale tylko 57% dla horyzontu 7–12 miesięcy. W okresie szoku 2021–2023 pasma",
        "były za wąskie, więc niepewność w turbulentnych czasach jest większa, niż pokazują."
      ),
      verbatimTextOutput("model_prog")
    )
  ),

  nav_panel(
    "Test Grangera",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("gr_x", "Czy ten szereg...", choices = setNames(names(nazwy), nazwy),
                    selected = "eur_pln"),
        selectInput("gr_y", "...pomaga przewidywać ten?", choices = setNames(names(nazwy), nazwy),
                    selected = "inflacja"),
        sliderInput("gr_k", "Liczba opóźnień:", min = 1, max = 12, value = 3, step = 1),
        helpText("Test na różnicach (szeregi stacjonarne).")
      ),
      verbatimTextOutput("granger_wynik"),
      uiOutput("granger_wniosek"),
      card(
        card_header("Jak to czytać"),
        "H0: opóźnienia x nie poprawiają prognozy y ponad to, co daje historia samego y.",
        "To nie jest dowód przyczynowości, tylko test użyteczności predykcyjnej.",
        "Wiele testów naraz zawyża liczbę fałszywych istotności (przy 40 testach i progu 5% ~2 przypadkowe)."
      ),
      card(
        card_header("Znane ograniczenie danych"),
        "Wynik EUR/PLN → bezrobocie wychodzi istotny na całej próbie (2002–2026), ale nie jest odporny:",
        "po ograniczeniu do okresu od 2012 (bez 2020) znika. Prawdopodobnie wynika z przerwy",
        "w danych o bezrobociu (skok +1,2 p.p. w styczniu 2010) i nie świadczy o realnym związku.",
        "Dla pozostałych par nie znaleziono wiarygodnej zależności."
      )
    )
  ),

  nav_panel(
    "Scorecard",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "sc_tryb", "Prognozy:",
          choices = c("Na żywo (zapisane w chwili publikacji danych)" = "na_zywo",
                      "Backtest od 2012 (liczone wstecz)" = "backtest"),
          selected = "na_zywo"
        ),
        selectInput("sc_h", "Horyzont (miesiące):", choices = 1:3, selected = 1),
        helpText("Scorecard porównuje prognozy z tym, co się naprawdę stało.",
                 "Tylko prognozy „na żywo” są uczciwe: backtest powstał, gdy wynik był już znany.")
      ),
      uiOutput("sc_info"),
      plotOutput("sc_wykres", height = "380px"),
      tableOutput("sc_tabela"),
      card(
        card_header("Najnowsza prognoza na żywo"),
        tableOutput("sc_ostatnia")
      )
    )
  ),

  nav_spacer(),
  nav_item(span(class = "navbar-text small",
                paste0("Dane do: ", format(max(dane$date), "%m.%Y"))))
)

# ---- Serwer -----------------------------------------------------------------
server <- function(input, output, session) {

  # 1. Szeregi czasowe
  output$wykres_szeregi <- renderPlot({
    req(input$seria, input$zakres)

    dd <- dane_dlugie |>
      filter(
        seria %in% nazwy[input$seria],
        date >= input$zakres[1], date <= input$zakres[2]
      )

    p <- ggplot(dd, aes(date, wartosc)) +
      geom_line(color = "steelblue", linewidth = 0.8) +
      facet_wrap(~ seria, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL, caption = "Źródło: Eurostat, NBP") +
      theme_minimal(base_size = 14)

    if (input$cel_nbp) {
      p <- p + geom_hline(
        data = tibble(seria = factor(nazwy["inflacja"], levels = levels(dane_dlugie$seria))) |>
          filter(seria %in% unique(dd$seria)),
        aes(yintercept = 2.5), linetype = "dashed", color = "firebrick"
      )
    }
    p
  })

  # 2. Korelacje
  output$wykres_kor <- renderPlot({
    m <- if (input$kor_typ == "poziomy") dane else dane_d
    kor <- m |> select(-date) |> cor()

    as_tibble(kor, rownames = "x") |>
      pivot_longer(-x, names_to = "y", values_to = "kor") |>
      mutate(across(c(x, y), \(v) factor(v, levels = names(nazwy), labels = nazwy))) |>
      ggplot(aes(x, y, fill = kor)) +
      geom_tile() +
      geom_text(aes(label = round(kor, 2)), size = 5) +
      scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                           limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "r") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # 3. Prognoza
  model <- reactive({
    x <- dane[[input$prog_seria]]
    auto.arima(ts(x, start = c(2002, 1), frequency = 12))
  }) |> bindCache(input$prog_seria)

  output$wykres_prog <- renderPlot({
    fc <- forecast(model(), h = input$prog_h)
    p <- autoplot(fc, include = 60) +
      labs(title = nazwy[[input$prog_seria]], x = NULL, y = NULL) +
      theme_minimal(base_size = 14)
    if (input$prog_seria == "inflacja") {
      p <- p + geom_hline(yintercept = 2.5, linetype = "dashed", color = "firebrick")
    }
    p
  })

  output$model_prog <- renderPrint(model())

  # 4. Test Grangera
  granger <- reactive({
    validate(need(input$gr_x != input$gr_y, "Wybierz dwa różne szeregi."))
    grangertest(dane_d[[input$gr_y]] ~ dane_d[[input$gr_x]], order = input$gr_k)
  })

  output$granger_wynik <- renderPrint(granger())

  output$granger_wniosek <- renderUI({
    p <- granger()$`Pr(>F)`[2]
    istotny <- p < 0.05
    div(
      class = paste("alert", if (istotny) "alert-warning" else "alert-secondary"),
      sprintf("p = %.3f. ", p),
      if (istotny) {
        "Odrzucamy H0 na poziomie 5%, ale sprawdź odporność wyniku (inne opóźnienia, podokresy)."
      } else {
        "Brak podstaw do odrzucenia H0: nie widać, by x pomagał przewidywać y."
      }
    )
  })

  # 5. Scorecard prognoz
  sc <- reactive({
    historia |>
      filter(tryb == input$sc_tryb) |>
      left_join(select(dane, cel = date, rzeczywista = inflacja), by = "cel") |>
      mutate(blad = rzeczywista - prognoza)
  })

  output$sc_info <- renderUI({
    d <- sc()
    n_zreal <- sum(!is.na(d$rzeczywista) & d$h <= 3 & d$model == "arima")
    div(class = "alert alert-secondary",
        sprintf("Prognoz ARIMA (h = 1–3) w tym trybie: %d, z czego zrealizowanych: %d.",
                sum(d$h <= 3 & d$model == "arima"), n_zreal),
        if (input$sc_tryb == "na_zywo" && n_zreal == 0) {
          " Pierwsze prognozy na żywo zostaną ocenione po opublikowaniu danych za kolejne miesiące."
        })
  })

  output$sc_wykres <- renderPlot({
    d <- sc() |> filter(h == as.integer(input$sc_h), !is.na(rzeczywista))
    validate(need(nrow(d) > 0, "Brak zrealizowanych prognoz w tym trybie. Pierwsze pojawią się po publikacji kolejnych danych."))

    a <- filter(d, model == "arima")
    ggplot(a, aes(cel)) +
      geom_ribbon(aes(ymin = l80, ymax = u80), fill = "steelblue", alpha = 0.2) +
      geom_line(aes(y = prognoza, color = "Prognoza ARIMA")) +
      geom_line(data = filter(d, model == "naiwny"), aes(y = prognoza, color = "Naiwna (jak dziś)"),
                linetype = "dashed") +
      geom_line(aes(y = rzeczywista, color = "Rzeczywistość"), linewidth = 0.9) +
      scale_color_manual(values = c("Prognoza ARIMA" = "steelblue", "Naiwna (jak dziś)" = "grey40",
                                    "Rzeczywistość" = "black")) +
      labs(x = NULL, y = "Inflacja r/r (%)", color = NULL,
           title = sprintf("Prognoza z horyzontem %s mies. a rzeczywistość", input$sc_h),
           subtitle = "Pasmo: przedział 80%") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  output$sc_tabela <- renderTable({
    d <- sc() |> filter(h <= 3, !is.na(rzeczywista))
    validate(need(nrow(d) > 0, "Brak zrealizowanych prognoz do oceny."))
    d |>
      group_by(Model = model, `Horyzont (mies.)` = as.integer(h)) |>
      summarise(`Liczba prognoz` = n(),
                RMSE = sqrt(mean(blad^2)),
                MAE = mean(abs(blad)),
                `Pokrycie 80%` = paste0(round(100 * mean(rzeczywista >= l80 & rzeczywista <= u80)), "%"),
                .groups = "drop") |>
      mutate(Model = recode(Model, arima = "ARIMA", naiwny = "Naiwny"))
  }, digits = 2)

  output$sc_ostatnia <- renderTable({
    live <- filter(historia, tryb == "na_zywo")
    validate(need(nrow(live) > 0, "Brak prognoz na żywo."))
    ostatnia <- max(live$dane_do)
    live |>
      filter(dane_do == ostatnia, model == "arima", h <= 3) |>
      transmute(`Miesiąc` = format(cel, "%m.%Y"),
                `Prognoza (%)` = prognoza,
                `Pasmo 80%` = sprintf("%.2f – %.2f", l80, u80),
                `Pasmo 95%` = sprintf("%.2f – %.2f", l95, u95))
  }, digits = 2)
}

shinyApp(ui, server)
