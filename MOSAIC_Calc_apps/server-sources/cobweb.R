# stores the time series
cob_traj_x <- reactiveVal(numeric(0))

observe({
  # reset trajectory when dynamics change
  cob_dynamics()
  cob_traj_x(numeric(0))
})
cob_dynamics <- reactive({
  if (input$cob_dynamics_seed == 0) {
    return(mosaic::makeFun(2*x*(1-x) ~ x))
  } else if (input$cob_dynamics_seed == -1) {
    return(mosaic::makeFun(3.6*x*(1-x) ~ x))
  } else if (input$cob_dynamics_seed == -2) {
    return(mosaic::makeFun(4.0*x*(1-x) ~ x))
  } else { #return some random dynamics
    fraw <- mosaic::rfun(~ x, seed=as.numeric(input$cob_dynamics_seed))
    vals <- range(fraw(seq(-5, 5, by=.01)))
    f <- function(x) {
      (fraw(6*(x-.5)) - vals[1])/diff(vals)
    }
    return(f)
  }

  isolate(cob_traj_x(numeric(0)))
  form <- as.formula(paste(input$cob_dynamics_eq, "~ yyzzyy"))
  vars <- all.vars(form)
  if (length(vars) > 2) stop("Use only 1 dynamical variable")
  if (length(vars) <= 1) stop("Must use a dynamical variable")
  dyvar <- setdiff(vars, "yyzzyy")
  form <- as.formula(paste(input$cob_dynamics_eq, "~", dyvar))
  mosaic::makeFun(form)

})
observeEvent(input$cob_take_step, {
  req(length(cob_traj_x()) > 0)

  x <- numeric(input$cob_n_steps + 1)
  x[1] <- isolate(cob_traj_x()[length(cob_traj_x())])
  for (k in 1:input$cob_n_steps) {
    x[k+1] = cob_dynamics()(x[k])
  }

  if (length(cob_traj_x()) == 0 ) isolate(cob_traj_x(x))
  isolate(cob_traj_x(c(cob_traj_x(), x[-1] )))

})
output$cob_dynamics <- renderPlot({
  dynamics_plot()
})

# Trigger the trajectory by double clicking
observeEvent(input$cob_start_x, {
  if (! is.null(input$cob_start_x$x)) {
    x <- numeric(length = input$cob_n_steps)
    x[1] <- input$cob_start_x$x
    for (k in 1:input$cob_n_steps) x[k+1] <- cob_dynamics()(x[k])
    isolate(cob_traj_x(x))
  }
})

dynamics_plot <- reactive({
  Points <- tibble::tibble(
    domain =seq(input$cob_xrange[1], input$cob_xrange[2], length = 100),
    y = cob_dynamics()(domain)
  )
  suppressWarnings({
    P <- gf_line(y ~ domain, data = Points) %>%
      gf_abline(intercept = 0, slope = 1, color="blue") %>%
      gf_labs(x = expression(x[t]), y = expression(x[t+1]),
              title = "Dynamics") %>%
      gf_refine(coord_fixed())
  })
  Cobweb <- cobweb()
  if (!is.null(Cobweb) && nrow(Cobweb) > 1) P <- P %>%
    gf_point(y ~ x, data = Cobweb, inherit=FALSE) %>%
    gf_path(y ~ x, data = Cobweb, alpha = 0.3, color = "red") %>%
    gf_text(y ~ x, label = ~ label, data = Cobweb, size = 4,
            hjust=1, vjust=0)

  P
})

cobweb <- reactive({
  if (length(cob_traj_x()) < 2) return(NULL)

  Points <- tibble(
    x = cob_traj_x()[-length(cob_traj_x())],
    y = cob_traj_x()[-1],
    step = 0:(length(y)-1) * 2
  )
  Diags <- tibble(
    x = cob_traj_x(),
    y = x,
    step = (1:length(y))*2 - 3
  )

  bind_rows(Points, Diags) %>% arrange(step) %>%
    mutate(label = ifelse((step %% 2) == 1 , (step+1)/2, ""))
})

output$cob_time_series <- renderPlot({
  tmax <- pmax(10, length(cob_traj_x()))
  if (length(cob_traj_x())==0) return(NULL)
  Points <- tibble(
    t = 0:(length(cob_traj_x()) -1),
    y = cob_traj_x()
  )
  gf_point(y ~ t, data = Points) %>%
    gf_lims(x = c(0, tmax)) %>%
    gf_labs(y = expression(x[t]), x = "t") %>%
    gf_line(y ~ t, data = Points, alpha = 0.3, color="red")
})
