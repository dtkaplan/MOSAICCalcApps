DE_euler_ui <- function() {

  list(
  tags$p("Select one of the basic differential equation models provided.
    Pick an initial condition x_0.
    Then press 'step' to take a Euler step with dt=h.
    With many Euler steps, you have constructed the solution x(t)."),
  tags$p("The table shows the initial condition in the first row followed by the value of
           x at each successive time step."),
  tags$p("The top graph shows the function dynamics(x) versus x. The lower graph
           shows x(t) versus t."),
  tags$p("The blue line in the top graph shows the value of the instantaneous rate of change
           of x at the last step in the table. Read the value off the vertical axis."),
  tags$p("The corresponding blue line in the bottom graph shows that same rate of change.
           But since that graph is x(t) versus t, the rate of change corresponds to the slope
           of the blue line. The next Euler step will fall on that blue line. If the next step
           brings us to a different value of x, blue line will change in accordance with the
           dynamical function."),
  sidebarLayout(
    sidebarPanel(
      selectInput("dee_dynamics_fun", "Dynamics",
                  choices = dee_fun_choices
      ),
      sliderInput("dee_hstep",
                  "Size of h:",
                  min = 0.1,
                  max = 2,
                  step = 0.1,
                  value = 0.5),
      sliderInput("dee_x0",
                  "Initial condition on x:",
                  min = -10, max=10, step=0.1, value=1),
      splitLayout(actionButton("dee_take_step", "Step"),
                  actionButton("dee_undo", "Undo"),
                  actionButton("dee_clear", "Clear")),
      tableOutput("dee_steps")
    ),


    mainPanel(
      plotOutput("dee_dynamics"),
      plotOutput("dee_solution")
    )
  )
)
}
