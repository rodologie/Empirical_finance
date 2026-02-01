plot_res <- function(model, var) {
  
  res <- residuals(model)
  
  df_res <- data.frame(
    time = time(res),
    residual = as.numeric(res)
  )
  
  label <- labels[[var]]
  
  ggplot(df_res, aes(x = time, y = residual)) +
    geom_line(colour = "blue") +
    labs(
      x = NULL,
      y = NULL,
      title = paste("Resdiuals:", label)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(color = "black", hjust = 1),
      axis.text.y = element_text(color = "black"),
      plot.title = element_text(face = "bold"),
      axis.line   = element_line(color = "black"),
      axis.ticks  = element_line(color = "black"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
}

