it_current_start <- reactiveVal(c(0,0))

it_step_n <- reactive({
  if (is.null(input$it_nsteps) || is.na(input$it_nsteps)) 5
  else input$it_nsteps
})

observe({
  req(input$it_initial)
  where <- c(input$it_initial$x, input$it_initial$y)
  it_current_start(where)
})

observe({
  it_A() # for the dependency
  it_current_start(c(NA, NA))
  it_trajectory_store$L = list()
})

it_A <- reactive({
  res <- matrix(c(input$it_avalue, input$it_bvalue,
                  input$it_cvalue, input$it_dvalue), nrow=2, byrow=TRUE)
  res[is.na(res)] <- 0
  res
})

it_colors <- c("black", "blue", "green", "red", "orange",
            topo.colors(10), heat.colors(10), terrain.colors(10), rainbow(10))
it_current_trajectory <- reactive({
  it_current_start()
  trajectory <- matrix(rep(0,2*(it_step_n()+1)), nrow=2)
  trajectory[, 1] <- it_current_start()
  for(k in 1:it_step_n()){
    trajectory[, k+1] <- it_A() %*% trajectory[, k]
  }

  Res <- data.frame(t(trajectory)) |>
    rename(x = "X1", y = "X2") |>
    mutate(n=row_number()-1) |>
    na.omit()
  Res
})
it_trajectory_store <- reactiveValues(L = list())
observeEvent(input$it_clear, {
  it_trajectory_store$L <- list()
})
observeEvent(input$it_clear_last, {
  n <- length(it_trajectory_store$L)
  if (n > 0) it_trajectory_store$L[n] <- NULL
})
observeEvent(it_current_trajectory(), { # update the store
  req(it_current_trajectory())
  n <- length(it_trajectory_store$L) + 1
  shade <- it_colors[n]
  it_trajectory_store$L[[n]] <-
    it_current_trajectory() |>
    dplyr::mutate(path = rnorm(1), hue = shade)
})

it_time_series_plot_y <- reactive({
  if (length(it_trajectory_store$L) == 0) {
    Dat <- data.frame(n=1:20, y=-2:2)
    return(ggplot(Dat, aes(x=n, y=y)) + geom_blank())
  }
  suppressWarnings(
    gf_point(y ~ n, data=bind_rows(it_trajectory_store$L),
             color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) |>
      gf_path(y ~ n, group=~path) |>
      gf_refine(scale_color_identity()) |>
      gf_theme(theme(legend.position = "none")) |>
      gf_labs(title = "y versus n")
  )
})

it_time_series_plot_x <- reactive({
  if (length(it_trajectory_store$L) == 0) {
    Dat <- data.frame(n=1:20, y=-2:2)
    suppressWarnings(P <- ggplot(Dat, aes(x=n, y=x)) + geom_blank())
    return(P)
  }
  suppressWarnings(
    gf_point(x ~ n, data=bind_rows(it_trajectory_store$L),
             color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) |>
      gf_path(x ~ n, group=~path) |>
      gf_refine(scale_color_identity()) |>
      gf_theme(theme(legend.position = "none")) |>
      gf_labs(title = "x versus n")
  )
})

it_trajectory_plot <- reactive({
  if (length(it_trajectory_store$L) == 0) {
    Dat <- data.frame(x=-2:2, y=-2:2)
    suppressWarnings(P <- ggplot(Dat, aes(x=x, y=y)) + geom_blank() + coord_fixed())
    return(P)
  }

  suppressWarnings(
    gf_point(y ~ x, data=bind_rows(it_trajectory_store$L),
             color= ~ hue, group=~path, alpha = ~ 1.1 - sqrt(1/n)) |>
      #gf_text(y ~ x, label=~n) |>
      gf_path(y ~ x, group=~path) |>
      gf_refine(scale_color_identity(),
                coord_fixed(xlim=c(-2, 2), ylim=c(-2, 2), clip="on")) |>
      gf_theme(theme(legend.position = "none"))
  )
})

output$it_space <- renderPlot({
  it_trajectory_plot()
})

output$it_timey <- renderPlot({
  it_time_series_plot_y()
})

output$it_timex <- renderPlot({
  it_time_series_plot_x()
})

output$it_eigenvalues <- renderText({
  paste(paste(c("𝝺₁ =","𝝺₂ ="),
              round(eigen(it_A())$values, 2)
  ), collapse=",     ")
})
