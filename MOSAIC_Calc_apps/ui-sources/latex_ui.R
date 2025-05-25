latex_ui <- function() {
  basicUI <- list(
    HTML('<div style="font-size: 25px;">
  <div id="LaTeX_prompt" class="shiny-html-output"></div>
</div>
<hr/>
<div style="font-size: 25px;">
  <div class="form-group shiny-input-container">
    <label class="control-label" for="latex">Type latex math here.</label>
    <textarea id="LaTeX_latex" class="form-control" rows="5" cols="50" style="font-size: 25px"></textarea>
  </div>
</div>
<div id="LaTeX_rendered" class="shiny-html-output" style="font-size:25px;"></div>

<div id="LaTeX_feedback" class="shiny-html-output" style="color: green; font-size:30px;"></div>'),
    actionButton("LaTeX_nextq", "Next"), actionButton("LaTeX_show_hint", "Show answer"),
    div(textOutput("LaTeX_hint_area"), style="font-size: 16px; font-family: monospace;")
  )
}
