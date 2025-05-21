am_make_fun_from_tilde <- function(tilde) {
  ## This is copied from the chain of derivatives function,
  ## except for this next line
  default_fun <- makeFun(0*x ~ x)

  # Is <tilde> even a valid expression
  check_tilde <- try(parse(text = tilde),  silent = TRUE)
  if (inherits(check_tilde, "try-error")) return(default_fun)
  fun_formula <- try(as.formula(tilde))
  if (inherits(fun_formula,  "try_error"))
    return(default_fun)
  if (length(fun_formula) != 3)
    return(default_fun)
  if (length(setdiff(all.vars(fun_formula), "pi")) != 1)
    return(default_fun)
  if (length(all.vars(fun_formula[[3]])) !=  1)
    return(default_fun)
  if (length(all.vars(fun_formula[[2]])) ==  0) {
    # it's a constant
    fun_formula = as.formula(paste(paste(fun_formula[[2]], "+0*x"),  "~ x"))
  }

  # test that the function works
  vals <- -3:3
  f <- try(makeFun(fun_formula))
  if (inherits(f, "try-error")) return(default_fun)


  y <- try(f(vals))
  if (inherits(f, "try-error")) return(default_fun)
  if (!is.numeric(y)) return(default_fun)
  if (length(y) != length(vals) ) return(default_fun)

  f
}

am_standard_domain <- domain(x = c(0, 10))


am_xmin <- reactiveVal(5)
am_xmax <- reactiveVal(5.1)

am_get_function <- reactive({
    am_make_fun_from_tilde(input$am_tilde)
  })
am_get_derivative <- reactive({
  D(am_get_function()(x) ~ x)
  })

  # update endpoints
  observe( {
    if (!is.null(input$am_mark)) {
      am_xmin(input$am_mark$xmin)
      am_xmax(input$am_mark$xmax)
    }
  })

  am_average_slope <- reactive({
    (am_get_function()(am_xmin()) - am_get_function()(0))/
      am_xmin()
  })
  am_secant_slope <- reactive({
    (am_get_function()(am_xmax()) - am_get_function()(am_xmin())) /
      (am_xmax() - am_xmin())
  })
  am_derivative_slope <- reactive({
    am_get_derivative()(am_xmin())
  })

  output$am_Function <- renderPlot({
    Pt <- tibble(x = am_xmin(), xmax = am_xmax())
    half_length <- 2
    if (!is.null(Pt)) {
      Pt$y = am_get_function()(Pt$x)
      Pt$ymax = am_get_function()(Pt$xmax)
      Pt$xleft = pmax(0, Pt$x - half_length)
      Pt$xright = pmin(10, Pt$x + 1.5*half_length)
      slope = am_get_derivative()(Pt$x)
      Pt$yleft = Pt$y + slope*(Pt$xleft - Pt$x)
      Pt$yright = Pt$y + slope*(Pt$xright - Pt$x)

      tangent_slope <- (Pt$ymax - Pt$y) / (Pt$xmax - Pt$x)
      Pt$tan_yleft = Pt$y + tangent_slope*(Pt$xleft - Pt$x)
      Pt$tan_yright = Pt$y + tangent_slope*(Pt$xright - Pt$x)
    }
    P <- slice_plot(am_get_function()(x) ~ x, am_standard_domain)

    if (!is.null(Pt)){
      P <- P %>%
        gf_segment(tan_yleft + tan_yright ~ xleft + xright,
                   data = Pt, color = "red") %>%
        gf_segment(y + ymax ~ x + xmax, data = Pt, color = "red",
                   linetype = "dotted") %>%
        gf_segment(0 + y ~ 0 + x, data = Pt, color = "green") %>%
        gf_segment(yleft + yright ~ xleft + xright, data = Pt, color = "blue")

    }

    P
  })

  output$am_ave_slope <- renderText({
    glue::glue(
      "Average slope:, {signif(am_average_slope(), 3)}")
  })

  output$am_secant_slope <- renderText({
    paste("Finite diff. slope:", signif(am_secant_slope(), 3))
  })

  output$am_tangent_slope <- renderText({
    paste("Infinitesimal slope:", signif(am_derivative_slope(), 3))
  })
