interpolation_explorer_ui <- function() {
  list(
    sidebarPanel(
      actionButton("ie_newpoints", "Start again"),
      tags$hr(),
      selectInput("ie_npoints", "# of knot points",
                  c(3, 5, 10, 15, 20), selected=5),
      tags$hr(),
      tags$p("Interpolating function type"),
      checkboxInput("ie_linear", "linear interpolator", TRUE),
      checkboxInput("ie_cubic", "cubic spline", FALSE),
      checkboxInput("ie_global", "global polynomial", FALSE),
      actionButton("ie_jitter", "Jitter"),
      radioButtons("ie_exclude", "Exclude endpoints",
                   choices=0:6, inline=TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ie_fplot", height = "200px"),
      plotOutput("ie_dfplot", height = "200px"),
      plotOutput("ie_ddfplot", height = "200px" )
    )
  )
}
