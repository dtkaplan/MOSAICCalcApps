flow_1d_ui <- function() {
    list(
      withMathJax(
        textInput("F1_dynamics_eq", "$$\\partial_t x(t) \\equiv f(x(t))$$",
                  placeholder = "f(x) formula goes here")
      ),
      checkboxInput("F1_flow", "Show flow", FALSE),
      checkboxInput("F1_jitterflow", "Jitter flow arrows", FALSE),
      sliderInput("F1_hsize", "h", min = 0.01, max = 1, step = 0.01, value = 0.3),


      plotOutput("F1_DE", click = "F1_state")
  )
}
