SI_timebase <- reactive({
  seq(0,20, length = 500)
})

SI_waveforms <- reactive({
  res <- data.frame(
    time = SI_timebase() ,
    first  = input$SI_A1*sin(2*pi*SI_timebase()/input$SI_P1 + input$SI_phi1),
    second = input$SI_A2*sin(2*pi*SI_timebase()/input$SI_P2 + input$SI_phi2)
  )

  res$sum <-  res$first + res$second + input$SI_baseline

  res
})


output$SI_plot <- renderPlot({
  ggplot(data = SI_waveforms(), aes(x = time, y = sum)) +
    geom_line(color = "black", size = 3, alpha = 0.25)  +
    geom_line(aes(y = first), color = "blue") +
    geom_line(aes(y  = second), color = "green")  +
    ylim(-4,  4)
})
