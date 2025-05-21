search_2d_ui <- function() {
  list(
    actionButton("zzfind_restart", "Start again with a new function"),
    checkboxInput("zzfind_show_fun", "Use training wheels."),
    radioButtons("zzfind_hint_level", "Method:",
                 choices = c(
                   "1) Function value" = 1,
                   "2) Follow gradient"  = 2,
                   "3) Zeros of gradient" = 3,
                   "4) Local quadratic" = 4
                 )
    ),
    plotOutput("zzfind_graphics", click=clickOpts("zzfind_xstar"))
  )
}
