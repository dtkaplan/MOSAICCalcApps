zfind_our_fun <- reactiveVal(rfun(~ x))
zfind_guesses <- reactiveVal(numeric(0))
zfind_dx_fun  <- reactiveVal(D(zfind_our_fun()(x) ~ x))
zfind_dxx_fun <- reactiveVal(D(zfind_our_fun()(x) ~ x + x))
zfind_fun_vals <- reactiveValues(x = 0, y = 0)
zfind_ylimits <- reactiveVal(0)

observeEvent(input$zfind_xstar, {
  zfind_guesses(c(zfind_guesses(), input$zfind_xstar$x))
})

observeEvent(
  input$zfind_restart,
  {
    fun <- rfun(~ x)
    zfind_our_fun(fun)
    zfind_dx_fun(D(fun(x) ~ x))
    zfind_dxx_fun(D(fun(x) ~ x + x))
    zfind_fun_vals$x <- x <-  seq(-7.5, 7.5, length = 500)
    zfind_fun_vals$y <- fun(x)
    zfind_ylimits(extendrange(zfind_fun_vals$y))
    zfind_guesses(numeric(0))
  },
  ignoreNULL = FALSE,
  ignoreInit = FALSE)

output$zfind_graphics <- renderPlot({
  Dat <- data.frame(x = zfind_fun_vals$x, y = zfind_fun_vals$y)
  P <- ggplot(data = Dat, mapping = aes(x=x, y=y))

  if (input$zfind_show_fun) {
    P <- P + geom_line()
  } else {
    P <- P + geom_blank()
  }


  if (length(zfind_guesses()) > 0) {
    Info <- data.frame(x = zfind_guesses(), y = zfind_our_fun()(zfind_guesses()))
    Info$n <- as.character(1:nrow(Info))
    P <- P + geom_point(data = Info) #+
    #geom_text(mapping = aes(x=x, y=y, label =  n), vjust = 1, hjust = 1)
  }

  if (length(zfind_guesses()) > 0) {
    xx <- zfind_guesses()[length(zfind_guesses())]
    yy <- zfind_our_fun()(xx)
    xx_end <- xx + sign(zfind_dx_fun()(xx)) + 0.2
    if (input$zfind_hint_level == 2)
      P <- P + geom_segment(x=xx, xend=xx_end, y=yy, yend=yy, arrow = arrow())
    fslope <- zfind_dx_fun()(xx)
    ffslope <- zfind_dxx_fun()(xx)
    y_intercept <- yy - xx*fslope
    where_zero <- xx - yy/fslope
    if (input$zfind_hint_level == 3) {
      P <- P +
        geom_abline(slope = fslope, intercept = y_intercept, color="blue") +
        geom_point(x = where_zero, y = 0, shape = "x", color="blue", size=10)
    }
    parab <- data.frame(x = seq(-7.5, 7.5, length=100))

    parab$y <- yy + fslope*(parab$x-xx) + ffslope*(parab$x-xx)^2 / 2

    if (input$zfind_hint_level == 4) {
      where <- xx - fslope/ffslope
      P <- P + geom_line(data = parab, aes(x=x, y=y), color="blue") +
        geom_vline(xintercept = where, color="blue")
    }
  }

  P + lims(y = zfind_ylimits(), x = c(-7.5, 7.5))
})
