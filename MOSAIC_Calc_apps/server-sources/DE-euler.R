dee_nsteps <- reactiveVal(0)
dee_the_fun <- reactive({
  dee_function_set[[as.numeric(input$dee_dynamics_fun), "fun"]][[1]]
})
dee_minx <- reactive({
  dee_function_set[[as.numeric(input$dee_dynamics_fun), "min"]]
})
dee_maxx <- reactive({
  dee_function_set[[as.numeric(input$dee_dynamics_fun), "max"]]
})
observeEvent(input$dee_take_step, {
  dee_nsteps(dee_nsteps() + 1)
})
observeEvent(input$dee_undo, {
  dee_nsteps(dee_nsteps() - 1)
})
observeEvent(c(input$dee_clear, input$dee_dynamics_fun), {
  dee_nsteps(0)
})
dee_the_steps <- reactive({
  Res <- tibble::tibble(
    t = (0:dee_nsteps()) * input$dee_hstep,
    dt_x = 0*t,
    x = 0*t,
    step = 0*t)
  Res$x[1] <- input$dee_x0
  Res$dt_x[1] <- dee_the_fun()(Res$x[1])
  Res$step[1] <- Res$dt_x[1] * input$dee_hstep
  if (dee_nsteps() > 0) {
    for (k in 1:dee_nsteps()) {
      Res$step[k+1] = Res$dt_x[k] * input$dee_hstep
      Res$x[k+1] = Res$x[k] + Res$step[k+1]
      Res$dt_x[k+1] = dee_the_fun()(Res$x[k+1])
    }
  }

  Res$step[1] <-  NA

  Res
})
output$dee_steps <- renderTable({
  dee_the_steps() %>% tail(10)
})
output$dee_dynamics <- renderPlot({
  Traj <- dee_the_steps()
  # dom <- range(Traj$x) + c(-1, 1)
  Dyn <- tibble::tibble(
    x = seq(dee_minx(), dee_maxx(), length = 300),
    dt_x = dee_the_fun()(x)
  )
  Points <- Traj["x"] %>%
    mutate(dx = dee_the_fun()(x),
           n = row_number(),
           alpha = .3 + n/length(n))
  current_slope = Points$dx[nrow(Points)]
  gf_path(dt_x ~ x, data = Dyn) %>%
    gf_hline(yintercept = 0, color="orange3") %>%
    gf_point(dx ~ x, data = Points, alpha = ~ alpha) %>%
    gf_hline(yintercept = current_slope, color="dodgerblue")

})
output$dee_solution <- renderPlot({
  Traj <- dee_the_steps()
  max_time <- 10
  Slope <- tibble::tibble(
    t = seq(Traj$t[nrow(Traj)], max_time),
    x = Traj$x[nrow(Traj)] +
      Traj$dt_x[nrow(Traj)]*(t - Traj$t[nrow(Traj)])
  )
  gf_point(x ~ t, data = Traj) %>%
    gf_path(x ~ t) %>%
    gf_path(x ~ t, data = Slope, color="dodgerblue") %>%
    gf_hline(yintercept = 0, color="orange3") %>%
    gf_lims(y = c(dee_minx(), dee_maxx()), x = c(0, max_time))
})

