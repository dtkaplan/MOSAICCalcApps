F1_dynamics <- reactive({
  if (input$F1_dynamics_eq == "")
    return(mosaic::makeFun(2*x*(1-x) ~ x))

  # if it's a number, return some random dynamics
  if (grepl("^[0-9]*$", input$F1_dynamics_eq)) {
    fraw <- rfun(~ x, seed=as.numeric(input$F1_dynamics_eq))
    vals <- range(fraw(seq(-5, 5, by=.01)))
    f <- function(x) {
      (fraw(6*(x-.5)) - mean(vals))/diff(vals)
    }
    return(f)
  }

  # isolate(traj_x(numeric(0))) # What for?
  form <- as.formula(paste(input$F1_dynamics_eq, "~ yyzzyy"))
  vars <- all.vars(form)
  if (length(vars) > 2) stop("Use only 1 dynamical variable")
  if (length(vars) <= 1) stop("Must use a dynamical variable")
  dyvar <- setdiff(vars, "yyzzyy")
  form <- as.formula(paste(input$F1_dynamics_eq, "~", dyvar))
  mosaic::makeFun(form)

})


F1_where <- reactiveVal(.25)

observe({
  if (!is.null(input$F1_state$x))
    F1_where(input$F1_state$x)
})

output$F1_DE <- renderPlot({
  suppressWarnings(F1_make_plot())
})

F1_make_plot <- reactive({
  state <- F1_where()
  f <- F1_dynamics()
  startarrow <- state - f(state)*input$F1_hsize
  Flow <- tibble::tibble(
    x = seq(-0.2, 1.2, by=0.05),
    speed = f(x),
    xstart = x - input$F1_hsize*speed,
    asize = abs(speed),
    arrow_size = 3*asize/max(asize),
    y=0,
    yj = seq(-0.3, 0, length=length(x))) #sort(runif(length(x), min = -0.3, max = 0)))


  redarrowsize = 3*abs(f(state))/max(abs(Flow$speed))
  P <- slice_plot(f(x) ~ x, domain(x = c(-0.25, 1.25))) %>%
    gf_hline(yintercept = 0, color = "blue") %>%
    gf_segment(0 + 0 ~ startarrow + state, inherit = FALSE, color = "red",
               size=3, alpha = 0.5, arrow=grid::arrow()) %>%
    gf_point(0 ~ state, color="blue", inherit=FALSE, size=3, fill=NULL) %>%
    gf_labs(x = "State x", y = "Change of state dx/dt",
            title="\"Logistic\" Dynamics. State shown in blue.")

  if (input$F1_flow && input$F1_jitterflow) {
    P <- P |>
      gf_segment(yj + yj ~ xstart + x, data = Flow, inherit=FALSE,
                 color="red", # size = ~arrow_size,
                 alpha = 0.3,
                 arrow = arrow(length = unit(Flow$arrow_size, "mm")))
  } else if (input$F1_flow) {
    P <- P |>
      gf_segment(y + y ~ xstart + x, data = Flow, inherit=FALSE,
                 color="red", # size = ~arrow_size,
                 alpha = 0.3,
                 arrow = arrow(length = unit(Flow$arrow_size, "mm")))
  }
  P |> gf_refine(scale_size_identity())

})
