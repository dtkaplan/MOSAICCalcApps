cobweb_ui <- function() {
  page_fillable(
    layout_columns(
      withMathJax(
        card(
          numericInput("cob_dynamics_seed", "seed for dynamics f(x)",
                       value = 0, step = 1, min = -2, max = 1000),
          actionButton("cob_take_step", "Take more step(s)"),
          numericInput("cob_n_steps", "Multi-step", min=1, max=10, value=1, step=1),
          sliderInput("cob_xrange", "x-limits", min = -3, max = 3,
                      value = c(0,1), step=0.25),
          tags$p("Double click to set initial condition.")
        )
      ),
      card(
        plotOutput("cob_time_series", height = "180px"),
        plotOutput("cob_dynamics", dblclick="cob_start_x"),
      ),
      col_widths = c(3,8)
    )
  )
}
