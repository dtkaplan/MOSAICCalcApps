zzfind_Quadratic_approx <- function(g, x0, y0) {
  dx <- D(g(x,y) ~ x)
  dy <- D(g(x,y) ~ y)
  dxy <- D(g(x,y) ~ x + y)
  dxx <- D(g(x,y) ~ x + x)
  dyy <- D(g(x,y) ~ y + y)

  function(x,y) {
    g(x0, y0) +
      dx(x=x0,y=y0)*(x-x0) +
      dy(x=x0, y=y0)*(y-y0) +
      dxy(x=x0, y=y0)*(x-x0)*(y-y0) +
      dxx(x=x0, y=y0)*(x-x0)^2 +
      dyy(x=x0, y=y0)*(y-y0)^2
  }
}

zzfind_our_fun <- reactiveVal(rfun(~ x + y))
zzfind_guesses_x <- reactiveVal(numeric(0))
zzfind_guesses_y <- reactiveVal(numeric(0))
zzfind_dx_fun  <- reactiveVal(NA)
zzfind_dy_fun <- reactiveVal(NA)
zzfind_quad <- reactiveVal(NA)


observeEvent(input$zzfind_xstar, {
  zzfind_guesses_x(c(zzfind_guesses_x(), input$zzfind_xstar$x))
  zzfind_guesses_y(c(zzfind_guesses_y(), input$zzfind_xstar$y))
})

observeEvent(
  input$zzfind_restart,
  {
    fun <- rfun(~ x + y)
    zzfind_our_fun(fun)
    zzfind_dx_fun(D(zzfind_our_fun()(x,y) ~ x))
    zzfind_dy_fun(D(zzfind_our_fun(x,y) ~ y))
    zzfind_quad <- zzfind_Quadratic_approx(zzfind_our_fun())
    zzfind_guesses_x(numeric(0))
    zzfind_guesses_y(numeric(0))
  },
  ignoreNULL = FALSE,
  ignoreInit = FALSE)

output$zzfind_graphics <- renderPlot({
  Guesses <- data.frame(x = zzfind_guesses_x(), y = zzfind_guesses_y())
  Guesses$val <- with(Guesses, zzfind_our_fun()(x=x, y=y))

  P <- ggplot(data = Guesses, mapping = aes(x=x, y=y))

  if (input$zzfind_show_fun) {
    P <- contour_plot(zzfind_our_fun()(x=x, y=y) ~ x + y,
                      domain(x=c(-5,5), y=c(-5,5)),
                      filled = TRUE)
  } else {
    P <- P + geom_blank()
  }


  if (nrow(Guesses) > 0) {
    P <- P + geom_text(data = Guesses, aes(x=x,y=y,label=as.character(round(z,2)))) +
      geom_point(aes(x=x, y=y), data = Guesses)
  }

  if (length(zzfind_guesses_x()) > 0) {
    xx <- zzfind_guesses_x()[length(zzfind_guesses_x())]
    yy <- zzfind_guesses_x()[length(zzfind_guesses_x())]
    angle <- atan2(zzfind_dy_fun()(x=xx,y=yy), zzfind_dx_fun()(x=xx, y=yy))

    if (input$hint_level == 2) {
      P <- P + geom_segment(aes(x=xx, y=yy, xend = xx+cos(angle), yend=yy+sin(angle)), arrow = arrow())
    }
  }

  P
})

