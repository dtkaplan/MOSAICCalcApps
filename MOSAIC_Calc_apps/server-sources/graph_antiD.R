gad_f_seed <- reactive({input$gad_f_seed})
gad_f_function <- reactive({
  mosaic::rfun( ~ t, seed = gad_f_seed())
})

output$gad_boxstats <- renderText({
  if (is.null(input$gad_Fbrush)) return("Drag mouse on graph to measure")
  width  <- input$gad_Fbrush$xmax - input$gad_Fbrush$xmin
  f <- gad_f_function()
  F <- antiD(f(t) ~ t)
  yvals <- F(c(input$gad_Fbrush$xmin, input$gad_Fbrush$xmax))
  deltaF <- diff(yvals)
  glue::glue(
    "<p>Left: {signif(input$gad_Fbrush$xmin,3)}      Right: {signif(input$gad_Fbrush$xmax, 3)}      Width: {signif(width,3)}</p>
    <p>F(left): {signif(yvals[1], 3)}       F(right): {signif(yvals[2], 3)}       ∆F: {signif(deltaF,3)}</p>
    <p>Slope of F(): {signif(deltaF/width, 3)} </p>"
  ) %>% HTML()
})

output[["gad_littlef"]] <- renderPlot({
  f <- gad_f_function()

  suppressWarnings({
    P <- slice_plot(f(t) ~ t, domain(t=c(-5, 5))) %>%
      gf_hline(yintercept = 0, color="dodgerblue") %>%
      gf_labs(y = paste("f(t) for seed", input$gad_seed),
              title = "f(t), the derivative of F(t)") %>%
      gf_refine(scale_x_continuous(breaks = (-5):5))
  })
  if (! is.null(input$gad_Fbrush)) {
    Area <- tibble(
      t = seq(input$gad_Fbrush$xmin, input$gad_Fbrush$xmax, length=100),
      y = f(t)
    )
    Neg_area <- Area %>% filter(y < 0)
    P <- P %>% gf_ribbon(0 + y ~ t, data = Area, inherit=FALSE,
                         fill = "dodgerblue", color=NA, alpha = 0.3)
    if (nrow(Neg_area) > 0)
      P <- P %>%
      gf_ribbon(0 + y ~ t, data=Neg_area %>% filter(y < 0), inherit = FALSE,
                fill = "orange3", color = NA, alpha = 0.3)
  }
  P
})
