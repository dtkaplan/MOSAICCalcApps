about_ui <- function() {
  list(h2("The MOSAIC Calculus Apps"),
      p("This is a collection of interactive 'apps' that are intended to develop intuition for various mathematical concepts. They are arranged by broad topic categories. Use the menu items above to find the individual apps."),
      p("Enjoy!")
  )
}

solar_ui <- function() {
  list(
    plotOutput("solar_fplot", height = "200px"),
    plotOutput("solar_eplot", height = "50px",
               brush = brushOpts("interval", direction = "x", opacity = 0.5)),
    plotOutput("solar_Fplot", height = "200px")
  )
}

cows_ui <- function() {
  list(
    sliderInput("cow_ncows",
                "Number of cows:",
                min = 0,
                max = 50,
                step = 1,
                value = 0),
    checkboxInput("cow_growth",
                  HTML("<span style='color: green;'>Show grass growth dynamics</span>"),
                  value=FALSE),
    checkboxInput("cow_consumption",
                  HTML("<span style='color: brown;'>Show grass consumption</span>"),
                  value=FALSE),
    checkboxInput("cow_net",
                  HTML("<span style='color: blue;'>Show net grass dynamics</span>"),
                  value=TRUE),
    plotOutput("cow_grassPlot")
  )
}

euler_ui <- function() {
  list(
    textInput("euler_tilde", "Tilde expression for f(x)",
              value = "exp(1.5*x) ~ x", width="30%"),
    sliderInput("euler_domain", "Domain for plot", min=-10, max=10,
                value=c(0,3), step = 0.5, width="200px"),
    selectInput("euler_h", "h", choices=c(0.01,0.05,0.1, 0.25, 0.5),
                selected=0.25, width="20%"),
    sliderInput("euler_alpha", "Move toward 1 to connect the segments together:",
                min=0, max=1, value = 0, width="200px"),
    sliderInput("euler_C", "Constant of integration, C",
                min=-10, max=10, value=0, width="200px"),
    plotOutput("euler_segments"),
    plotOutput("euler_fx")

  )
}

airfoil_ui <- function() {
  page_fillable(
    layout_columns(
      card(
        sliderInput('airfoil_p', 'Parameter p', min = 0, max = 1,   value = 0.4 ),
        sliderInput('airfoil_m', "Parameter m", min = 0, max = 0.2, value = 0.05),
        sliderInput('airfoil_h', "Parameter h", min = 0, max = 0.5, value = .2)
      ),
      plotOutput("airfoil_camber_plot"),
      col_widths = c(3,7)
    )
  )
}

am_ui <- function() {
  list(
    sidebarLayout(
      sidebarPanel(
        textInput("am_tilde", "Tilde expression for function:", "x/exp(x) ~ x"),
        wellPanel(textOutput("am_ave_slope"),  style="color: green;"),
        wellPanel(textOutput("am_secant_slope"), style="color: red;"),
        wellPanel(textOutput("am_tangent_slope"), style="color: blue;")
      ),
      mainPanel(
        plotOutput("am_Function", brush = brushOpts("am_mark", direction="x"))
      )
    )
  )
}




ansatze_ui <- function() {
  list(
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          "Solve",
          p(HTML("<span style='font-size: 30px;'><span style='color: red'>ẋ</span> = <span style='color: green'>a x</span></span>")),
          p("where"),
          splitLayout(
            numericInput("az_alpha_val", "a = ",
                         min=-1, max=1, step=0.25, value=-0.25,
                         width="85px"),
            numericInput("az_b_val", "b = ",
                         min=-1, max=1, step=0.25, value=-0.25,
                         width="85px")
          ),
          tags$hr(),
          selectInput("az_ansatz", "Ansatz form", ansatz_choices),
          htmlOutput("az_param1"),
          htmlOutput("az_param2")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          p(HTML("<span style='color: blue; font-size: 30px;'>Ansatz function: x(t)</span>")),
          plotOutput("az_ansatz_plot", height="200px"),
          p(HTML("Functions to balance: <span style='color: red; font-size: 30px;'>ẋ</span> and <span style='color: green; font-size: 30px;'>𝛼x(t)</span>")),
          plotOutput("az_fun_plot", height="200px"),
          htmlOutput("az_status")
        )
      )
    )
}

#------------------------------
sailing_ui <- function() {
  list(
    plotOutput("sailing_boat_velocity"),
    selectInput("sailing_dt", "Select the length dt",
                choices = c(0.005, 0.01, 0.02, 0.05),
                selected = 0.05),

    checkboxInput("sailing_show_area",
                  "Show the multiplication as an area.",
                  value=FALSE)
  )
}
#--------------------
logistic_balance_ui <- function() {

  list(
    sidebarPanel(
      "Solve",
      p(HTML("<span style='font-size: 30px;'><span style='color: red'>ẋ</span> = <span style='color: green'>r x (1-x)</span></span>")),
      p("where"),
      splitLayout(
        numericInput("log_alpha_val", "r = ",
                     min=-1, max=1, step=0.25, value=-0.25,
                     width="85px") #,
        # numericInput("log_b_val", "b = ",
        #              min=-1, max=1, step=0.25, value=-0.25,
        #              width="85px")
      ),
      tags$hr(),
      selectInput("log_ansatz", "Ansatz form", ansatz_choices),
      htmlOutput("log_param1"),
      htmlOutput("log_param2")
    ),
    mainPanel(
      plotOutput("log_ansatz_plot", height="200px"),
      p(HTML("<span style='color: blue; font-size: 30px;'>Ansatz function: x(t)</span>")),
      # plotOutput("log_ansatz_plot", height="200px"),
      p(HTML("Functions to balance: <span style='color: red; font-size: 30px;'>ẋ</span> and <span style='color: green; font-size: 30px;'>r x(t) (1-x(t))</span>")),
      plotOutput("log_fun_plot", height="200px"),
      htmlOutput("log_status")
    )
  )

}
