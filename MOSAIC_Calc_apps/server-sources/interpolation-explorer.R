ie_jitterList <- reactiveValues(jitters=list())
ie_orig_funs <- reactiveValues()
ie_npoints <- reactive({
  as.numeric(input$ie_npoints)
})

ie_xrange <- reactive({
  x <- ie_knot_locations()$x
  toss <- as.numeric(input$ie_exclude)
  if (toss < 1) return(range(x))

  toss <- pmin(floor((length(x)/2))-1, toss)
  keepers <- (toss+1):(length(x) -toss)
  range(x[keepers])
})

ie_knot_locations <- reactive({
  input$ie_newpoints
  diffs <- pmax(0.5, abs(rexp(ie_npoints(), rate=3)))
  tibble::tibble(
    x = cumsum(diffs),
    y = runif(length(diffs))
  )
})

observeEvent(input$ie_newpoints, {
  # Eliminate the old jittered plots
  ie_jitterList$jitters <- list()
})
ie_new_jitter <- eventReactive(input$ie_jitter, {
  Res <- ie_knot_locations()
  set.seed(NULL)
  Res$y <- Res$y + rnorm(nrow(Res), 0, .05)
  Res
})
ie_make_funs <- reactive({
  K <- ie_knot_locations()
  ie_orig_funs$linear <- mosaic::connector(y ~ x, data = K)
  ie_orig_funs$cubic <- mosaic::spliner(y ~ x, data = K)
  ie_orig_funs$global <-
    makeFun(lm(y ~ poly(x, ie_npoints() - 1), data = K))
  ie_orig_funs
})
ie_make_jitter_funs <- observeEvent(input$ie_jitter, {
  req(input$ie_jitter > 0)
  J <- isolate(ie_new_jitter())
  Previous <- reactiveValuesToList(isolate(ie_jitterList))
  n <- length(Previous$jitters)
  res <- list()
  res$linear <- mosaic::connector(y ~ x, data = J)
  res$cubic <- mosaic::spliner(y ~ x, data = J)
  res$global <-
    makeFun(lm(y ~ poly(x, ie_npoints() - 1), data = J))
  isolate(ie_jitterList$jitters[[n+1]] <- res)
})
ie_plot_points <- reactive({
  funs <- ie_make_funs()
  K <- ie_knot_locations()
  dom <- domain(x=extendrange(ie_xrange(), f=0.005))
  P <- gf_point(y ~ x, data = K, size=4, alpha=0.75, color="black")
  if (input$ie_linear) {
    P <- P %>%
      mosaicCalc::slice_plot(funs$linear(x) ~ x,
                             domain = dom,
                             npts = 500,
                             linewidth = 2, alpha = 0.5,
                             color="darkgray")
  }
  if (input$ie_cubic) {
    P <- P %>%
      mosaicCalc::slice_plot(funs$cubic(x) ~ x,
                             domain = dom,
                             npts = 500,
                             linewidth = 2, alpha = 0.5,
                             color="dodgerblue")
  }
  if (input$ie_global) {
    P <- P %>%
      mosaicCalc::slice_plot(funs$global(x) ~ x,
                             domain = dom,
                             npts = 500,
                             linewidth = 2, alpha = 0.5,
                             color="tomato")
  }

  P %>% gf_lims(x = ie_xrange())
})
output$ie_fplot <- renderPlot({
  P <- ie_plot_points()
  Jitters <- ie_jitterList$jitters
  if (length(Jitters) == 0) return(P)
  dom <- domain(x=extendrange(ie_xrange(), f=0.02))
  for (k in 1:length(Jitters)) {
    if (input$ie_linear) {
      P <- P %>%
        mosaicCalc::slice_plot(Jitters[[k]]$linear(x) ~ x,
                               domain = dom,
                               npts=500, inherit=FALSE,
                               color="darkgray", alpha=.5)
    }
    if (input$ie_cubic) {
      P <- P %>%
        mosaicCalc::slice_plot(Jitters[[k]]$cubic(x) ~ x,
                               domain = dom,
                               npts=500,
                               color="dodgerblue", alpha=.5)
    }
    if (input$ie_global) {
      P <- P %>%
        mosaicCalc::slice_plot(Jitters[[k]]$global(x) ~ x,
                               domain = dom,
                               npts=500,
                               color="tomato", alpha=.5)
    }
  }
  P %>% gf_lims(x = ie_xrange()) %>%
    gf_vline(xintercept=~x, data = ie_knot_locations(), alpha=0.4, color="magenta", linewidth=1)

})

output$ie_dfplot <- renderPlot({
  funs <- ie_make_funs()
  K <- ie_knot_locations()
  dom <- domain(x=extendrange(ie_xrange(), f=0.02))
  dfuns <- list()
  dfuns$linear <- D(funs$linear(x) ~ x)
  dfuns$cubic <-  D(funs$cubic(x)  ~ x)
  dfuns$global <- D(funs$global(x) ~ x)
  P <- NULL
  if (input$ie_linear) {
    P <- P %>%
      mosaicCalc::slice_plot(dfuns$linear(x) ~ x,
                             domain = dom,
                             linewidth = 2, alpha = 0.5,
                             color="darkgray", npts=500)
  }
  if (input$ie_cubic) {
    P <- P %>%
      mosaicCalc::slice_plot(dfuns$cubic(x) ~ x,
                             domain = dom,
                             linewidth = 2, alpha = 0.5,
                             color="dodgerblue", npts=500)
  }
  if (input$ie_global) {
    P <- P %>%
      mosaicCalc::slice_plot(dfuns$global(x) ~ x,
                             domain = dom,
                             linewidth = 2, alpha = 0.5,
                             color="tomato", npts=500)
  }
  P %>%
    gf_lims(x = ie_xrange()) %>%
    gf_vline(xintercept=~x, data = K,
             alpha=0.4, linewidth=1, color="magenta") %>%
    gf_labs(y="1st deriv. w.r.t. x")
})

output$ie_ddfplot <- renderPlot({
  if (!(input$ie_cubic || input$ie_global)) return(NULL)
  funs <- ie_make_funs()
  K <- ie_knot_locations()
  dom <- domain(x=extendrange(ie_xrange(), f=0.02))
  ddfuns <- list()
  ddfuns$linear <- D(funs$linear(x) ~ x & x)
  ddfuns$cubic <-  D(funs$cubic(x)  ~ x & x)
  ddfuns$global <- D(funs$global(x) ~ x & x)
  P <- NULL
  if (input$ie_linear) {
      P <- P %>%
          mosaicCalc::slice_plot(ddfuns$linear(x) ~ x,
                                 domain = dom,
                                 color="gray", npts=500)
  }
  if (input$ie_cubic) {
    P <- P %>%
      mosaicCalc::slice_plot(ddfuns$cubic(x) ~ x,
                             domain = dom,
                             linewidth = 2, alpha = 0.5,
                             color="dodgerblue", npts=500)
  }
  if (input$ie_global) {
    P <- P %>%
      mosaicCalc::slice_plot(ddfuns$global(x) ~ x,
                             domain = dom,
                             linewidth = 2, alpha = 0.5,
                             color="tomato", npts=500)
  }
  P %>%
    gf_lims(x = ie_xrange()) %>%
    gf_vline(xintercept=~x, data = K,
             alpha=0.4, linewidth=1, color="magenta") %>%
    gf_labs(y="2nd deriv. w.r.t. x")

})
