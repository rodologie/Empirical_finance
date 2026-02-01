nice_acf <- function(data, var, name, residuals = FALSE, lag.max = 10, conf = 0.95) {
  
  if (residuals == TRUE){
    x <- data
    x <- x[!is.na(x)]
  } else {
    x <- data[[var]]
    x <- x[!is.na(x)]
  }
  
  acf_obj <- acf(
    x,
    lag.max = lag.max,
    plot = FALSE
  )
  
  T <- length(x)
  z <- qnorm(1 - (1 - conf) / 2)
  ci <- z / sqrt(T)
  
  df_acf <- data.frame(
    lag = as.numeric(acf_obj$lag),
    acf = as.numeric(acf_obj$acf)
  )
  
  df_acf <- df_acf[df_acf$lag != 0, ]
  
  if (var == "lr"){
    label <- labels[[name]]
  } else {
    label <- labels[[var]]
  }
  
  ggplot(df_acf, aes(lag, acf)) +
    geom_col(
      width = 0.6, 
      fill = "blue",
      alpha = 0.4,
      color = "blue",
      linewidth = 0.9
    ) +
    geom_hline(
      yintercept = c(-ci, ci),
      linetype = "dashed",
      colour = "grey40"
    ) +
    scale_x_continuous(
      breaks = 1:lag.max,
      limits = c(0.5, lag.max + 0.5)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.08, 0.08))
    ) + 
    labs(
      title = paste("ACF:", label),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      # -- options for axis text 
      axis.text.x  = element_text(color = "black"),
      axis.text.y  = element_text(color = "black"),
      # -- options for the title
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(color = "black"),
      axis.title.y = element_text(color = "black"),
      # -- option to mark a line in both axis
      axis.line = element_line(color = "black"),
      # -- option to include ticks in both axis
      # axis.ticks.x = element_line(color = "black"),
      # axis.ticks.y = element_line(color = "black"),
      # -- remove vertical grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}