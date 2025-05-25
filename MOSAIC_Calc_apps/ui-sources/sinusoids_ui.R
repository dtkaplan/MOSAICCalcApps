sinusoids_ui <- function() {
  page_fillable(
    layout_columns(
      card(card_header("Color"),
           p("Blue", style="color: blue;"),
           p(" "),
           p(" "),
           p("Green", style="color: green;")),
      card(card_header("Amplitude"),
        sliderInput("SI_A1", "Oscillator 1",
                    min=0,  max=2, value  = 1, step  = 0.1),
        sliderInput("SI_A2", "Oscillator 2", min=0,  max=2, value  = 0, step = 0.1)
      ),
      card(card_header("Period"),
           sliderInput("SI_P1", "",
                       min=0.5, max = 5, value = 2, step=0.1),
           sliderInput("SI_P2", "",
                       min=0.5, max = 5, value = 2, step=0.1)
      ),
      card(card_header("Phase"),
           sliderInput("SI_phi1", "",
                       min = 0, max = round(2*pi,2), value  = 0, step = 0.01),
           sliderInput("SI_phi2", "",
                       min = 0, max = round(2*pi,2), value  = 0, step = 0.01)
      ),
      col_widths = c(1,2, 2, 2)
    ),
    sliderInput("SI_baseline", "Baseline", min=-2, max = 2, value=0, step=0.01),
    plotOutput("SI_plot")
  )
}
