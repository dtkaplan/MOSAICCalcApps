dchain_ui <- function() {
  list(
    h5("Antiderivatives of f( ) and g( )" ),
    plotOutput("chain_Accum", height="160px"),
    hr(),
    h5("The functions"),
    textInput("chain_tilde_f", "Tilde expression for f( )",
              value = "sin(2*x) ~ x"),
    textInput("chain_tilde_g", "Tilde expression for g( )",
              value = "0 ~ x"),
    plotOutput("chain_Fun", height = "160px"),
    hr(),
    h5("Derivatives of f( ) and g( )"),
    plotOutput("chain_D1", height = "160px"),
    hr(),
    h5("2nd derivatives of f( ) and g( )"),
    plotOutput("chain_D2", height = "160px")
  )
}
