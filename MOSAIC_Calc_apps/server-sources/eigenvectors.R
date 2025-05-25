eigen_clickx <- reactiveVal()
eigen_clicky <- reactiveVal()
eigen_clickx(1)
eigen_clicky(0)

eigen_spread_angle <- reactive({
  pmax(0.02, input$eigen_spread *pi / 180)
})

eigen_xout <- reactive({
  mosaic::makeFun(a*x + b*y ~ x + y, a=input$eigen_a, b = input$eigen_b)
})
eigen_yout <- reactive({
  mosaic::makeFun(c*x + d*y ~ x + y, c=input$eigen_c, d = input$eigen_d)
})
observeEvent(input$eigen_where, {
  isolate(eigen_clickx(input$eigen_where$x))
  isolate(eigen_clicky(input$eigen_where$y))
})
output$eigen_plot <- renderPlot({
  P <- make_eigenplot()
  center_angle <- atan2(eigen_clicky(), eigen_clickx())
  angles <- center_angle + seq(-eigen_spread_angle(), eigen_spread_angle(), length=50)
  Dots <- tibble::tibble(
    x = cos(angles),
    y = sin(angles),
    x2 = eigen_xout()(x, y),
    y2 = eigen_yout()(x, y),
    color = hcl.colors(length(x), palette = "Dark2")
  )
  # typical_dist <- mean(sqrt(Dots$x2^2 + Dots$y2^2))
  # Dots$x3 <- Dots$x2 / typical_dist
  # Dots$y3 <- Dots$y2 / typical_dist



  P <- gf_point(y ~ x, data = Dots, alpha = .6,
                color = ~color, shape="o", size=5, inherit=FALSE) %>%
    gf_point(y2 ~ x2, data = Dots, alpha = .6, color = ~color, inherit=FALSE) %>%
    gf_segment(y + y2 ~ x + x2, data = Dots, alpha = 0.25, color=~color) %>%
    # gf_point(y3 ~ x3, data = Dots, color = ~color, inherit=FALSE) %>%
    gf_refine(coord_fixed(), scale_color_identity())

  E <- eigen_stuff()
  if (!is.complex(E$values)) {
    # Construct the eigenvectors
    one <- E$vectors[,1]
    two <- E$vectors[,2]
    slope_one <- one[2] / one[1]
    slope_two <- two[2] / two[1]
    #cat("Slopes are", slope_one, "and", slope_two, "\n")
    if (abs(slope_one) != Inf) {
      P <- P %>%
        gf_abline(intercept = 0, slope = slope_one,
                  alpha = 0.2, size=2)
    } else {
      P <- P %>%
        gf_vline(xintercept =0,alpha = 0.2, size=2 )
    }
    if (abs(slope_two) != Inf) {
      P <- P %>%
        gf_abline(intercept = 0, slope = slope_two, alpha = 0.2, size=2)
    } else {
      P <- P %>%
        gf_vline(xintercept =0,alpha = 0.2, size=2 )
    }
  }
  Circle <- tibble::tibble(
    angle = seq(0, 2*pi, length = 360),
    x = cos(angle),
    y = sin(angle)
  )

  if (input$eigen_zoom) {
    P %>%
      gf_theme(theme_minimal())
  } else {
    P %>%
      gf_path(y ~ x, data = Circle, alpha = 0.3, inherit = FALSE) %>%
      gf_theme(theme_minimal())
  }
})

make_eigenplot <- eventReactive(eigen_the_matrix(),{
  E <- eigen_stuff()
  if (!is.complex(E$values)) {
    # Construct the eigenvectors
    one <- E$vectors[,1]
    two <- E$vectors[,2]

    # make them unit length
    one <- one / sqrt(sum(one^2))
    two <- two / sqrt(sum(two^2))
    Vecs <- tibble::tibble(
      xend = c(one[1], two[1]),
      yend = c(one[2], two[2]),
      x = -xend, y = -yend
    )
  } else {
    Vecs <- tibble(x=c(0,0), y=0, xend=0, yend=0)
  }
  gf_segment(y + yend ~ x + xend, data = Vecs, inherit=FALSE,
             color = c("green", "blue"), alpha = 0.2, size = 2)


})
eigen_the_matrix <- reactive({
  req(input$eigen_a)
  req(input$eigen_b)
  req(input$eigen_c)
  req(input$eigen_d)
  matrix(c(input$eigen_a, input$eigen_b,
           input$eigen_c, input$eigen_d),
         nrow = 2, byrow = TRUE)
})
eigen_stuff <- reactive({
  eigen(eigen_the_matrix())
})
output$eigen_vals <- renderText({
 paste("𝞴", c("₁" , "₂"), "=", round(eigen_stuff()$values, 2), collapse = "     ")
})
