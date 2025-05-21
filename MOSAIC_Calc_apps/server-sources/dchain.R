chain_fixed_domain <- domain(x = -5:5)
chain_kill_x <- theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())


chain_f <- reactiveVal()
chain_F <- reactiveVal()
chain_df <- reactiveVal()
chain_ddf <- reactiveVal()
chain_g <- reactiveVal()
chain_G <- reactiveVal()
chain_dg <- reactiveVal()
chain_ddg <- reactiveVal()

chain_is_function_valid  <- function(tilde, default_val = 1) {

  default_tilde <- as.formula(paste(default_val, "+0*x  ~  x"))
  default_fun <- makeFun(default_tilde)

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

chain_get_function_as_tilde  <- function(f, degree = 1) {
  b <- body(f)
  var <- setdiff(all.vars(b), "pi")
  if (length(var) == 0 ) var <- "x"
  f <- 1 ~ 1
  f[[3]] <- as.name(var)
  if (degree == 2) f[[3]]  <- parse(text=paste(var, "+",  var))
  f[[2]] <- b

  f
}


chain_get_f_function <- reactive({
  chain_is_function_valid(input$chain_tilde_f)
})
chain_get_g_function <- reactive({
  chain_is_function_valid(input$chain_tilde_g, default_val = 0)
})

observe({ # assign functions to f, F,  df, ddf
  chain_f(chain_get_f_function())
  chain_tilde <- chain_get_function_as_tilde(chain_f())
  chain_F(antiD(chain_tilde))
  chain_df(D(chain_tilde))
  chain_ddf(D(chain_get_function_as_tilde(chain_f(), 2)))
})

observe({ # assign functions to g, G,  dg, ddg
  chain_g(chain_get_g_function())
  chain_tilde <- chain_get_function_as_tilde(chain_g())
  chain_G(antiD(chain_tilde))
  chain_dg(D(chain_tilde))
  chain_ddg(D(chain_get_function_as_tilde(chain_g(), 2)))
})

output$chain_Accum <- renderPlot({
  slice_plot(chain_F()(x) ~ x, chain_fixed_domain, linewidth=1.5, npts=500) %>%
    slice_plot(chain_G()(x) ~ x,  color = "tomato",  linewidth=1.2, npts=500) %>%
    gf_labs(y  = "") +
    chain_kill_x
})
output$chain_Fun <- renderPlot({
  slice_plot(chain_f()(x) ~ x, chain_fixed_domain, linewidth=1.5, npts=500) %>%
    slice_plot(chain_g()(x) ~ x, color = "tomato",  linewidth=1.2, npts=500) %>%
    gf_labs(y  = "")  +
    chain_kill_x
})
output$chain_D1 <- renderPlot({
  slice_plot(chain_df()(x) ~ x, chain_fixed_domain, linewidth=1.5, npts=500) %>%
    slice_plot(chain_dg()(x) ~  x, color = "tomato",  linewidth=1.2, npts=500) %>%
    gf_labs(y  = "")  +
    chain_kill_x
})
output$chain_D2 <- renderPlot({
  slice_plot(chain_ddf()(x) ~ x, chain_fixed_domain, linewidth=1.5, npts=500)%>%
    slice_plot(chain_ddg()(x)  ~ x,   color  = "tomato",  linewidth=1.2, npts=500) %>%
    gf_labs(y  = "")
})

