
sailing_tmp <- rfun(~ t, seed=638)
sailing_vel <- makeFun(1+(10+sailing_tmp(30*(t-0.08)))/4 ~ t)

output$sailing_boat_velocity <- renderPlot({
  dt <- as.numeric(input$sailing_dt)
  nsegs = ceiling(0.2/dt)
  palette <- topo.colors(nsegs)
  v_data <- tibble::tibble(
    tstart = dt*(0:(nsegs-1)),
    tmid = tstart + dt/2,
    tend = tstart + dt,
    v = sailing_vel(tstart),
    seg = palette,
    vlabs = paste0(" vel(t=", tstart, ")")
  )
  P <- slice_plot(sailing_vel(t) ~ t, domain(t = c(0,0.2))) %>%
    gf_labs(y = "Velocity (mph)", x = "Time (hours)") %>%
    gf_lims(y=c(0, NA), x = c(0,0.22)) %>%
    gf_errorbar(0 ~ tstart + tend, data = v_data,
                color = ~ seg, inherit = FALSE, size=2) %>%
    gf_errorbar(v ~ tstart + tend, data = v_data,
                color = ~ seg, inherit=FALSE)
  if (input$sailing_show_area) {
    P <- P %>% gf_rect(0 + v ~ tstart + tend, data = v_data, inherit=FALSE,
                       fill = ~ seg, alpha = 0.5)
  }

  if (nsegs < 25) {
    P <- P %>% gf_text(.3 ~ tmid, data = v_data,
                       label = 1:nsegs - 1, color = ~ seg, inherit=FALSE) %>%
      gf_text(v ~ tend, data = v_data, label = ~vlabs,
              color= ~ seg, hjust="left", inherit=FALSE)
  }

  P  + theme(legend.position = "none")
})
