log_omega_slider <- sliderInput("log_p1", "angular frequency 𝜔", min=0, max=2, step=0.01, value=1)
log_omega2_slider <- sliderInput("log_p2", "angular frequency 𝜔", min=0.3, max=2, step=0.01, value=1)
log_k_slider <- sliderInput("log_p1", "exponential parameter k", min=-1, max=1.2, step=0.01, value=0.25)
log_n_slider <- sliderInput("log_p1", "Power-law exponent n", min=-2, max=2, step=0.1, value=0.5)
log_center_slider <- sliderInput("log_p1", "Sigmoid center", min = 0, max = 10, step=0.1, value=5)
log_width_slider <- sliderInput("log_p2", "Sigmoid width", min = 0.1, max=8, step=0.1, value=2)

sigmoid <- function(t, center, width) {
  pnorm(t, mean=center, sd = width)
}

# set up the list of candidate ansatze
log_ansatz_candidates <- tibble::tribble(
  ~ fun, ~ control1, ~ control2,
  function(t, 𝜔, p2) sin(𝜔 * t), log_omega_slider, NA, # replace p1 and p2 with the actual names
  function(t, k, p2) exp(k * t), log_k_slider, NA,
  function(t, k, 𝜔) exp(k * t) * sin(𝜔 * t), log_k_slider, log_omega2_slider,
  function(t, center, width) sigmoid(t, center, width), log_center_slider, log_width_slider,
  function(t, n, p2) (t^n), log_n_slider, NA,
  function(t, n, 𝜔) (t^n) * sin(𝜔 * t), log_n_slider, log_omega2_slider,
) %>% mutate(display = unlist(sapply(fun, function(x) deparse(body(x)))),
             n = row_number())
log_ansatz_choices <- as.list(ansatz_candidates$n)
names(log_ansatz_choices) <- log_ansatz_candidates$display

log_success <- reactiveVal(FALSE)
output$log_param1 <- renderUI({
  req(input$log_ansatz)
  control <- log_ansatz_candidates$control1[[as.numeric(input$log_ansatz)]]

  if (!inherits(control, "shiny.tag")) NULL
  control
})
output$log_param2 <- renderUI({
  req(input$log_ansatz)
  control <- log_ansatz_candidates$control2[[as.numeric(input$log_ansatz)]]

  if (!inherits(control, "shiny.tag")) NULL
  else control
})

log_get_ansatz <- reactive({
  log_ansatz_candidates$fun[[as.numeric(input$log_ansatz)]]
})
log_parameterized_ansatz <- reactive({
  fun <- log_get_ansatz()
  function(t) fun(t, log_p1(), log_p2())
})
observe({
  f <- log_get_ansatz()
})
## parameters
log_p1 <- reactive({
  req(input$log_p1)
  input$log_p1
})
log_p2 <- reactive({
  req(input$log_p2)
  input$log_p2
})


log_points_to_plot <- reactive({
  req(input$log_ansatz)
  req(input$log_p1)
  fun <- log_parameterized_ansatz()
  dfun <- D(fun(t) ~ t)
  Res <- tibble::tibble(
    t = seq(0, 10, length=200),
    x = fun(t),
    x_scaled = input$log_alpha_val * x * (1- x),
    dx = dfun(t)
  )
  # are the functions balanced
  log_success((max(abs(Res$dx - Res$x_scaled)) < (abs(input$log_alpha_val)*0.02)))
  Res
})
output$log_fun_plot <- renderPlot({
  Pts <- log_points_to_plot()
  gf_path(x_scaled ~ t, data = Pts,
          color="green", size=2, alpha = 0.5) %>%
    gf_path(dx ~ t, color = "tomato") %>%
    gf_labs(y = "Red function & green function")
})
output$log_ansatz_plot <- renderPlot({
  Pts <- log_points_to_plot()
  gf_path(x ~ t, data = Pts, color="dodgerblue")
})
output$log_status <- renderText({
  message <-
    if (log_success())
      "<span style='color: green; font-size: 30px;'>Good enough!</span>"
  else
    "<span style='color: red; font-size: 20px;'>Functions are not balanced.</span>"

})
