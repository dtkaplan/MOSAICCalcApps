az_success <- reactiveVal(FALSE)
output$az_param1 <- renderUI({
  req(input$az_ansatz)
  control <- ansatz_candidates$control1[[as.numeric(input$az_ansatz)]]

  if (!inherits(control, "shiny.tag")) NULL
  control
})
output$az_param2 <- renderUI({
  req(input$az_ansatz)
  control <- ansatz_candidates$control2[[as.numeric(input$az_ansatz)]]

  if (!inherits(control, "shiny.tag")) NULL
  else control
})

az_get_ansatz <- reactive({
  ansatz_candidates$fun[[as.numeric(input$az_ansatz)]]
})
az_parameterized_ansatz <- reactive({
  fun <- az_get_ansatz()
  function(t) fun(t, az_p1(), az_p2())
})
observe({
  az_f <- az_get_ansatz()
})
## parameters
az_p1 <- reactive({
  req(input$az_p1)
  input$az_p1
})
az_p2 <- reactive({
  req(input$az_p2)
  input$az_p2
})


az_points_to_plot <- reactive({
  #browser()
  req(input$az_ansatz)
  # req(input$az_p1)
  fun <- az_parameterized_ansatz()
  dfun <- D(fun(t) ~ t)
  Res <- tibble::tibble(
    t = seq(0, 10, length=200),
    x = fun(t),
    x_scaled = input$az_alpha_val * x,
    dx = dfun(t)
  )
  # are the functions balanced
  az_success((max(abs(Res$dx - Res$x_scaled)) < 0.01))
  Res
})
output$az_fun_plot <- renderPlot({
  Pts <- az_points_to_plot()
  #browser()
  gf_path(x_scaled ~ t, data = Pts,
          color="green", size=2, alpha = 0.5) %>%
    gf_path(dx ~ t, color = "red") %>%
    gf_labs(y = "Red function & green function")
})
output$az_ansatz_plot <- renderPlot({
  #browser()
  Pts <- az_points_to_plot()
  gf_path(x ~ t, data = Pts, color="blue")
})
output$az_status <- renderText({
  message <-
    if (az_success())
      "<span style='color: green; font-size: 30px;'>Solution found!</span>"
  else
    "<span style='color: red; font-size: 20px;'>Functions are not balanced.</span>"

})
