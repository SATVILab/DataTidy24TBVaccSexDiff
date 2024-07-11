plot_summed_longitudinal <- function(plot_tbl, p_title) {
  # plot
  p <- ggplot(plot_tbl, aes(x = timepoint)) + # nolint
    geom_errorbar( # nolint
      aes(ymin = lb, ymax = ub), # nolint
      width = 0.2, col = "gray25"
    ) +
    geom_line(aes(y = med)) + # nolint
    geom_point(aes(y = med)) + # nolint
    facet_grid( # nolint
      ~vaccine,
      labeller = labeller(vaccine = vacc_lab_tidy) # nolint
    ) +
    labs( # nolint
      x = "Timepoint",
      y = "Summed\nResponse",
      title = p_title
    ) +
    cowplot::theme_cowplot() +
    cowplot::background_grid(major = "xy", minor = "xy") +
    theme( # nolint
      strip.background = element_rect(fill = "white", colour = "black") # nolint
    )
  p |>
    plot_bw_set_white()
}

plot_bw_set_white <- function(p) {
  p +
    theme( # nolint
      plot.background = element_rect(fill = "white"), # nolint
      panel.background = element_rect(fill = "white") # nolint
    )
}
