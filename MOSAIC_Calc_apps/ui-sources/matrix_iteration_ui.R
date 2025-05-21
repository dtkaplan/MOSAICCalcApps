matrix_iteration_ui <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        tags$table(
          tags$th("The Matrix"),
          tags$tr(
            tags$td(numericInput("it_avalue", label="", value=0.90,min=-2, max=2, step=0.01, width="85px")),
            tags$td(numericInput("it_bvalue", label="", value=0.05,min=-2, max=2, step=0.01, width="85px")),
          ),
          tags$tr(
            tags$td(numericInput("it_cvalue", label="", value=0.05,min=-2, max=2, step=0.01, width="85px")),
            tags$td(numericInput("it_dvalue", label="", value=1.01,min=-2, max=2, step=0.01, width="85px")),
          )
        ),
        textOutput("it_eigenvalues"),
        tags$hr(),
        splitLayout(actionButton("it_clear", "Clear graph"),
                    actionButton("it_clear_last", "Undo")),
        p("  "),
        numericInput("it_nsteps", label="number of iterations:", value=30, min=2, max=100),
        p("Click in the top graph to start a trajectory from that (x,y) point. The trajectory
            will be plotted in that graph, while y versus n will be in the bottom graph. The colors
              mean nothing in terms of dynamics. They are there to help you associate each trajectory segment to it's
              y vs. n solution.")
      ),
      mainPanel(
        plotOutput("it_space", click="it_initial"),
        plotOutput("it_timey", height="200px"),
        plotOutput("it_timex", height="200px")
      )
    )
  )
}
