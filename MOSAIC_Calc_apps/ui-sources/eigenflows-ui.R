eigenflows_ui <- function() {
  list(
    h2("Flows, eigenvalues, and eigenvectors"),
    p("Click in the left graph to set a & b in the [ab10] matrix.
  The eigenvalues are shown below the graph."),
    p("Click in the right graph to set the initial conditions for the trajectory."),
    p("Each [ab10] value corresponds to one shape of flow, shown in the
    right graph. The black glyphs  follow streamlines of the flow over
    the same time span. The red glyph is also a streamline, but you can click
    to start it wherever you want and you can adjust the time span."),
    p("The thin green line runs from (0,0) to the starting point of the red
    streamline. When the green line aligns exactly with the red streamline,
    you have found an eigenvector."),
    splitLayout(
      cellWidths = c("45%", "55%"),
      div(
        abcdUI("eig")
      ),
      div(
        plotOutput("eig_flowplot", click="eig_where"),
        uiOutput("eig_orientation"),
        span(
          radioButtons("eig_traj_steps", "time steps", choices = c(5, 10, 25, 100, 250, 1000), selected = 10, inline = TRUE,
                       width = NULL),
          style="color: red;")
      )
    )
  )
}
