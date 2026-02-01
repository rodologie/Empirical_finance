line_plot <- function(data, var){
  
  label   <- labels[[var]]
  formula <- formulas[[var]]
  
  y <- data[[var]]
  
  ggplot(data, aes(x = period, y = .data[[var]])) +
    geom_line(color = "blue", linewidth = 0.6) +
    scale_x_continuous(
      breaks = seq(0, 1000, by = 100),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      # breaks = seq(
      #   floor(min(y, na.rm = TRUE)),
      #   ceiling(max(y, na.rm = TRUE))
      # ),
      expand = expansion(mult = c(0.01, 0.01)),
      name = formula
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = paste("Simulated", label, "Process")
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold"),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid = element_blank()
    )
}
