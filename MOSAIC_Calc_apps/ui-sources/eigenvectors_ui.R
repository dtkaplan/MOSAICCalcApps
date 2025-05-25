eigenvectors_ui <- function() {
  page_fillable(

    layout_columns(
      card(card_header("Action of matrix"),
           plotOutput("eigen_plot", click = "eigen_where")
           ),
      card(
        layout_columns(
          card(card_header(mat_val("eigen_a", "A")),  mat_val("eigen_c", "C")),
          card(card_header(mat_val("eigen_b", "B")),  mat_val("eigen_d", "D"))
        ),
        card(card_header("Eigenvalues:"),
               textOutput("eigen_vals")
        ),
        card(card_header(checkboxInput("eigen_zoom", "Zoom in", value=FALSE)),
             sliderInput("eigen_spread", "Angle spread",
                         min = 0, max = 180, step = 5, value = 180)
        )
      ),
      col_widths = c(7,5)
    ),
    card(card_header("Card 4"))

  )
}

mat_val <- function(id, label="A") {
  sliderInput(id, label, min = -1.5, max = 1.5, value = 0, step=0.01)
}


#     fluidRow(
#     plotOutput("eigenplot", click = "where")
#     )
# )
# }
#
