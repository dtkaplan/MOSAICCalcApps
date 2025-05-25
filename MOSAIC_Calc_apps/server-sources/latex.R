LaTeX_answers <-
  c(r"(\sqrt{x})",
    r"(\frac{x^2}{2})",
    r"(\sum_{i=1}^n \frac{x^n}{n!})",
    r"(\frac{d}{dt}x)",
    r"(x^\star = \frac{b}{a} \pm \sqrt{b^2 - 4 a c}}{2a})",
    r"(\sin\left(\strut\\frac{2\pi}{P} t\right)",
    r"(\int_a^b x^2 dx"),
    r"(\partial_t t^2)",
    r"(\int^b_c x^ dx)"
    )


LaTeX_squash_str <- "\\{|\\}| |\t|\n"
LaTeX_compare <- gsub(LaTeX_squash_str, "", LaTeX_answers)
LaTeX_counter <- reactiveVal(value = 1)
LaTeX_hint_on <- reactiveVal(value = FALSE)
observeEvent(input$LaTeX_show_hint, {
  LaTeX_hint_on(!LaTeX_hint_on())
})
observeEvent(input$LaTeX_nextq, {
  LaTeX_hint_on(FALSE)
  newval <- LaTeX_counter() + 1
  if (newval > length(LaTeX_answers)) newval <- 1
  LaTeX_counter(newval)
})
output$LaTeX_rendered <- renderUI({
  withMathJax(paste0("$$", input$LaTeX_latex, "$$"))
})
output$LaTeX_prompt <- renderUI({
  input$LaTeX_nextq
  withMathJax(paste0("Typeset this: $$", LaTeX_answers[LaTeX_counter()],"$$"))
})
output$LaTeX_hint_area <- renderText({
  if (LaTeX_hint_on()) LaTeX_answers[LaTeX_counter()]
  else ""
})
output$LaTeX_feedback <- renderUI({
  if (gsub(LaTeX_squash_str, "", input$LaTeX_latex) %in% LaTeX_compare) "Success!"
  else ""
})


