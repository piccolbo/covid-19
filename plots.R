library(ggplot2)

theme_covid = theme(legend.position = "none", text = element_text(size = 20))

plot_timeseries = function(data, type, smoothing, prevalence) {
  ggplot(data = data,
         mapping = aes(
           x = date,
           y = (if (smoothing)
             smoothed.increase
             else
               increase) / (if (prevalence)
                 population / 1E5
                 else
                   1),
           label = region,
           color = region
         )) +
    geom_line() +
    geom_dl(method = "angled.boxes") +
    scale_y_log10(labels = identity) +
    ylab(paste("New daily",
               type,
               (if (prevalence)
                 "per hundred thousands"
                else
                  ""))) +
    theme_covid
}


plot_growthvssize = function(data, type, prevalence) {
  ggplot(
    data = data,
    mapping = aes(
      x = smoothed.cumulative.value / (if (prevalence)
        population / 1E5
        else
          1),
      y = 100 * decimal_trunc(smoothed.increase / smoothed.cumulative.value),
      label = region,
      color = region
    )
  ) +
    geom_line() +
    geom_dl(method = "angled.boxes") +
    scale_x_log10(labels = identity,
                  limits =
                    c(1 / (if (prevalence)
                      max(data$population) / 1E5
                      else
                        1), NA)) +
    scale_y_log10(labels = identity) +
    xlab(paste(type, (if (prevalence)
      "per hundred thousands"
      else
        ""))) +
    ylab("growth rate") +
    theme(legend.position = "none")
    theme_covid
}
