cow_base_consumption <- makeFun(beta*v^2/(v_0^2 + v^2) ~ v, beta=0.1, v_0=3)
cow_grass_growth <- makeFun(r*v*(1-v/k) ~ v, k=25, r=1/3)
cow_consumption <- reactive({
  function(v) {input$cow_ncows * cow_base_consumption(v)}
})
cow_net_growth <- reactive({
  function(v) {cow_grass_growth(v) - cow_consumption()(v)}
})
output$cow_grassPlot <- renderPlot({
  P <- gf_hline(yintercept= ~ 0, color="orange3")
  dom <- domain(v=c(0, 25))
  if (input$cow_growth) P <- P %>% slice_plot(cow_grass_growth(v) ~ v, dom,
                                          color="green", size=2, alpha=0.4)
  if (input$cow_consumption) P <- P %>% slice_plot(cow_consumption()(v) ~ v, dom,
                                               color="brown", size=2, alpha=0.4)
  if (input$cow_net) P <- P %>% slice_plot(cow_net_growth()(v) ~ v, dom,
                                       color="dodgerblue")

  P %>% gf_labs(x = "Biomass of grass", y = "Biomass growth/day") %>%
    gf_theme(theme_minimal())
})
