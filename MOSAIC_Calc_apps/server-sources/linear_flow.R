LF_clickx <- reactiveVal()
LF_clicky <- reactiveVal()
LF_clickx(1)
LF_clicky(0)

LF_dx <- reactive({
  mosaic::makeFun(a*x + b*y ~ x + y, a=input$LF_aparam, b = input$LF_bparam)
})
LF_dy <- reactive({
  mosaic::makeFun(c*x + d*y ~ x + y, c=input$LF_cparam, d = input$LF_dparam)
})
observeEvent(input$LF_where, {
  isolate(LF_clickx(input$LF_where$x))
  isolate(LF_clicky(input$LF_where$y))
})
output$LF_flowplot <- renderPlot({
  P <- LF_make_flowplot()

  P %>%
    gf_path(y ~ x, data = LF_traj(), inherit = FALSE, color="red")

})
LF_make_flowplot <- eventReactive(list(input$LF_nsteps, LF_the_matrix()),{
  E <- LF_eigen_stuff()
  if (!is.complex(E$values)) {
    # Construct the eigenvectors
    one <- E$vectors[,1]
    two <- E$vectors[,2]

    # make them unit length x 3
    one <- 3*one / sqrt(sum(one^2))
    two <- 3*two / sqrt(sum(two^2))
    Vecs <- tibble::tibble(
      xend = c(one[1], two[1]),
      yend = c(one[2], two[2]),
      x = -xend, y = -yend
    )
  } else {
    Vecs <- tibble(x=c(0,0), y=0, xend=0, yend=0)
  }
  LF_dyn_x <<- LF_dx()
  LF_dyn_y <<- LF_dy()
  mosaicCalc::streamlines(dx ~ LF_dyn_x(x,y), dy ~ LF_dyn_y(x,y), nsteps = input$LF_nsteps,
                          domain(x = c(-3, 3), y = c(-3, 3)),
                          npts = 15) %>%
    gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
               color = c("green", "blue"), alpha = 0.4, size = 2) %>%
    gf_refine(coord_fixed())


})
LF_the_matrix <- reactive({
  req(input$LF_aparam)
  req(input$LF_bparam)
  req(input$LF_cparam)
  req(input$LF_dparam)
  matrix(c(input$LF_aparam, input$LF_bparam,
           input$LF_cparam, input$LF_dparam),
         nrow = 2, byrow = TRUE)
})
LF_eigen_stuff <- reactive({
  eigen(LF_the_matrix())
})
output$LF_eigenvalues <- renderText({
  L1 <- round(LF_eigen_stuff()$values[1], 2)
  L2 <- round(LF_eigen_stuff()$values[2], 2)

  glue::glue("Eigenvalues: 𝜆₁ = {L1}, 𝜆₂ = {L2}")
})

LF_traj <- reactive({
  req(LF_the_matrix())
  x <- y <- numeric(input$LF_traj_steps)
  x[1] <- LF_clickx()
  y[1] <- LF_clicky()
  for (k in 2:length(x)) {
    dx = 0.05*LF_dx()(x = x[k-1], y[k-1])
    dy = 0.05*LF_dy()(x = x[k-1], y[k-1])
    x[k] <- x[k-1] + dx
    y[k] <- y[k-1] + dy
  }

  tibble(x=x, y=y)

})
