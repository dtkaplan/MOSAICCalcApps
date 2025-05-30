output$hist_distPlot <- renderPlot({

  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = input$hist_bins + 1)

  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white',
       xlab = 'Waiting time to next eruption (in mins)',
       main = 'Histogram of waiting times')

})
