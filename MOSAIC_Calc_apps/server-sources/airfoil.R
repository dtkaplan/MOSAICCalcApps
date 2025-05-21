airfoil_camber_front <- makeFun(
  m * (2*p*x - x^2)/(p^2) ~ x + m + p)
airfoil_dcamber <- makeFun(
  ifelse(x < p,
         2*m*(p-x)/(p^2),
         2*m*(p-x)/((1-p)^2)) ~ x + m + p)

airfoil_camber_back <- makeFun(
  m * (1 - 2*p + 2*p*x - x^2)/((1-p)^2) ~ x + m + p)
airfoil_thickness <- makeFun(
  5 * h * (0.2969*sqrt(x) - 0.126*x - 0.3516*x^2 + 0.2843*x^3 +
             -0.1036 * x^4) ~ x + h)
airfoil_camber <-
  makeFun(ifelse(x < p,
                 airfoil_camber_front(x, m=m, p=p),
                 airfoil_camber_back(x, m=m, p=p)) ~ x + m + p)

output$airfoil_camber_plot <- renderPlot({
  m <- input$airfoil_m
  p <- input$airfoil_p
  h <- input$airfoil_h
  airfoil_camber_fun <- makeFun(airfoil_camber(x, m = m, p = p) ~ x)

  x <- (1 - cos(seq(0, pi, length=200)))/ 2
  yc <- airfoil_camber(x, m = m, p = p)
  dyc <- airfoil_dcamber(x, m = m, p = p)
  yt <- airfoil_thickness(x, h = h)
  theta <- atan(dyc)
  Airfoil <- data.frame(
    xpos = x,
    xtop = x + yt * sin(theta),
    ytop = yc + yt * cos(theta),
    xbottom = x - yt * sin(theta),
    ybottom = yc - yt * cos(theta)
  )

  slice_plot(airfoil_camber_fun(x) ~ x, domain(x = c(0, 1)),
             npts = 300, color = "orange") %>%
    gf_line(ytop ~ xtop, data = Airfoil) %>%
    gf_line(ybottom ~ xbottom, data = Airfoil) %>%
    gf_lims(y = c(-.5, .5))
})
