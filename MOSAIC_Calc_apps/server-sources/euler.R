euler_draw_accum <- function(fun, domain=list(x=c(-3,3)), h=0.1,
                       n=11, alpha = 1, hshift=0,
                       accumgraph = TRUE, Fgraph=TRUE, C=0) {

  if (inherits(fun, "formula")) fun <- makeFun(fun)
  Fun <- mosaicCalc::antiD(fun(x) ~ x)
  xrange <- domain[[1]]
  xwidth <- h
  f_data <- tibble::tibble(
    xpts = seq(xrange[1], xrange[2], by=h) +
      hshift * xwidth,
    xstart = xpts,
    xend = xpts + h,
    slope = fun(xpts),
    scaled_slope = (slope - min(slope)) / diff(range(slope)), # on [0,1] scale
    ystart = 0,
    yend = slope*xwidth,
    move_vertical = c(0,cumsum(yend[-length(yend)])),
    hue = hsv(.1 + 4*scaled_slope/5, 1, 1),
    ystart2 = alpha*move_vertical + C,
    yend2 = alpha*move_vertical + xwidth*slope + C,
    midpoint = ystart2
  )

  dom <- mosaicCalc::domain(x = as.numeric(xrange))

  if (accumgraph) {
    P <- NULL
    P <- slice_plot(Fun(x) ~ x, dom,
                    color=ifelse(Fgraph, "gray", NA),
                    alpha=Fgraph)
    P %>%
      gf_point(data = f_data, midpoint ~ xpts, color = ~ hue) %>%
      gf_segment(data = f_data, ystart2 + yend2 ~ xstart + xend,
                 color = ~ hue, size=2) %>%
      gf_refine(scale_colour_identity()) %>%
      gf_labs(y = "F(x) and accumulated slopes", x="x")
  } else {
    slice_plot(fun(x) ~ x, dom) %>%
      gf_point(data = f_data, slope ~ xpts, color = ~ hue, size=2) %>%
      gf_refine(scale_colour_identity()) %>%
      gf_labs(y="f(x) -- the slope of F(x)")
  }
}

euler_fun <- reactive({
  ff <- as.formula(input$euler_tilde)
  makeFun(ff)
})
output$euler_fx <- renderPlot({
  euler_draw_accum(euler_fun(), h = as.numeric(input$euler_h),
             domain = list(x=input$euler_domain),
             accumgraph=FALSE, Fgraph = TRUE, alpha = 0)
})
output$euler_segments <- renderPlot({
  euler_draw_accum(euler_fun(), h = as.numeric(input$euler_h),
             domain = list(x=input$euler_domain),
             accumgraph=TRUE, Fgraph = TRUE,
             alpha = input$euler_alpha,
             C = input$euler_C)
})
