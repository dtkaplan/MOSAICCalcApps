eig_the_matrix <- abcdServer("eig")
eig_clickx <- reactiveVal()
eig_clicky <- reactiveVal()
eig_clickx(1)
eig_clicky(0)
observeEvent(input$eig_where, {
  # If the clicked point is near the eigenvector put it exactly on
  # the eigenvector
  # Get the eigenvalues, vector will be (lambda, 1)
  isolate(eig_clicky(input$eig_where$y))
  eigvalues <- eig_eigen_stuff()$values
  ratio <- input$eig_where$x / input$eig_where$y
  if (abs(ratio - eigvalues[1]) < 0.03) {
    isolate(eig_clickx(eigvalues[1] * input$eig_where$y))
  } else if (abs(ratio - eigvalues[2]) < 0.03) {
    isolate(eig_clickx(eigvalues[2] * input$eig_where$y))
  } else {
    isolate(eig_clickx(input$eig_where$x))
  }
})


eig_eigen_stuff <- reactive({
  eigen(eig_the_matrix())
})
eig_traj <- reactive({
  req(eig_the_matrix())
  x <- y <- numeric(input$eig_traj_steps)
  x[1] <- eig_clickx()
  y[1] <- eig_clicky()
  for (k in 2:length(x)) {
    dx = 0.05*eig_dx()(x = x[k-1], y[k-1])
    dy = 0.05*eig_dy()(x = x[k-1], y[k-1])
    x[k] <- x[k-1] + dx
    y[k] <- y[k-1] + dy
  }

  tibble(x=x, y=y,
         thickness = (1:length(x)/(0.2*length(x))),
         alpha = 0.2 + thickness, size = 0.6*thickness,
         lineend = "round"
  ) %>%
    mutate(size = ifelse(size < 1, 1, size))

})

eig_dx <- reactive({
  mosaic::makeFun(a*x + b*y ~ x + y, a=eig_the_matrix()[1,1], b = eig_the_matrix()[1,2])
})
eig_dy <- reactive({
  mosaic::makeFun(c*x + d*y ~ x + y, c=eig_the_matrix()[2,1], d = eig_the_matrix()[2,2])
})

output$eig_orientation <- renderUI({
  ang <- eig_traj_ang_start()
  vec_string <- paste0("$$\\left(\\begin{array}{c}",
                       signif(1/tan(ang),4), "\\\\",
                       1, "\\end{array}\\right)$$")
  withMathJax(
    HTML(
      glue::glue("<div style='color: green; width: 100%; text-align: center;'>
                   Vector along green line: {vec_string}Orientation
                   {round(180*ang/pi, 1)}°</div>")

    ))
})
output$eig_flowplot <- renderPlot({
  P <- eig_make_flowplot()

  P %>%
    gf_path(y ~ x, data = eig_traj(), inherit = FALSE, color="tomato",
            alpha = ~ alpha, linewidth = ~ size) %>%
    gf_abline(intercept=0,
              slope = tan(eig_traj_ang_start()),
              color="green") %>%
    gf_point(0 ~ 0, color="green") %>%
    gf_refine(coord_fixed(xlim=c(-2,2), ylim=c(-2,2), expand=FALSE, clip="off")) %>%
    gf_theme(theme_void())

})
eig_traj_ang_start <- reactive({
  atan2(eig_clicky(), eig_clickx())
})
eig_make_flowplot <- eventReactive(list(input$eig_nsteps, eig_the_matrix()),{
  E <- eig_eigen_stuff()
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
  funx <<- eig_dx()
  funy <<- eig_dy()

  mosaicCalc::streamlines(dx ~ funx(x,y), dy ~ funy(x,y), nsteps = 10,
                        domain(x = c(-2.5, 2.5), y = c(-2.5, 2.5)),
                        npts = 10) %>%
    # gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
    #            color = c("green", "dodgerblue"), alpha = 0.4, size = 2) %>%
    gf_refine(coord_fixed())


})
