search_1d_ui <- function() {
  list(
    p("This App is designed to help you develop an intuition about numerical methods for finding zeros of functions and for finding extreme values (maxes and mins)."),
    p("The graph shows the domain of a function. The function itself **is not plotted** because doing so would solve the problem by the *method of exhaustion*. Instead, you can click wherever you like in the domain and the function value at that input will be displayed by a black dot."),
    p("**Method 1** can be used for finding zeros or for finding maxima and minima. Make an initial guess about x and then another, nearby guess. The pattern may suggest how to make further guesses that move toward a zero of the function or to an extremum."),
    p("**Method 2** evaluates not just the value of the function at each guess, but also the sign of the derivative, which tells you whether the function has a positive or negative slope at your guess. This can help you move your guess toward a value that produces a zero, or toward a value that produces an extreme value."),
    p("**Method 3** uses the function value and the value of the derivative at your guessed x to create an order-one polynomial--a straight-line function---that matches your function at the point x. Effectively, this tells you not just the sign of the derivative (as in Method 2) but also its magnitude. This is particularly useful for finding zeros, since you can easily trace the linear approximation to where it crosses zero. Since the algebra of finding the zero of a linear function is trivial, we've marked the point for you with an x."),
    p("You can also use Method 3 to find extreme points. The idea is that when the derivative is very small, you may well be very near an extreme value."),
    p("**Method 4** finds the order-two polynomial that approximates the function at your guess x. The algebra of finding the extreme point of a quadratic is simple, so we do it for you and plot a vertical line at the extremum. Thus, this method gives a hint not only whether to move left or right, but how far to move."),
    p("The function you work with is randomly generated. It will tend to flatten out to the extreme left and right of the domain, so sometimes you may chase a zero off to the limits of the plot: a wild goose chase."),
    p("Sometimes the numerical methods give a very good hint of how to revise your guess. Sometimes the methods send you astray. In order to help you get a sense for why this happens, we provide you with 'training wheels' in the form of a graph of the function itself. (In real zero-finding and optimization problems, you don't have this: it amounts to the method of exhaustion."),
    p("You can start over with a new, randomly generated function whenever you like. Some will be easy, some hard."),

    actionButton("zfind_restart", "Start again with a new function"),
    checkboxInput("zfind_show_fun", "Use training wheels."),
    radioButtons("zfind_hint_level", "Method:",
                 choices = c(
                   "1) Function value" = 1,
                   "2) Left or right"  = 2,
                   "3) Local linear function" = 3,
                   "4) Local quadratic function" = 4
                 ),
    ),
    plotOutput("zfind_graphics", click=clickOpts("zfind_xstar"))
)
}

