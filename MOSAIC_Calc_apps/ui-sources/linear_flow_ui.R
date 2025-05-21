linear_flow_ui <- function() {
  page_fillable(
    layout_columns(
      card(card_header("Flow field"),
           plotOutput("LF_flowplot", click = "LF_where")),
      card(
        layout_columns(
          card(card_header(mat_val("LF_aparam", "A")),  mat_val("LF_cparam", "C")),
          card(card_header(mat_val("LF_bparam", "B")),  mat_val("LF_dparam", "D"))
        ),
        textOutput("LF_eigenvalues"),
        sliderInput("LF_nsteps",
                    "# of Euler steps for flowfield",
                    min = 5, max=50, step=1, value = 10),
        sliderInput("LF_traj_steps", "Duration of trajectory", min=5, max=100,step=5, value=10)
      )
    )
  )
}
