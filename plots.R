library(ggplot2)

theme_covid = theme(legend.position = "none", text = element_text(size = 20))

plot_timeseries = function(data, type, smoothing, prevalence) {
  ggplot(data = data,
         mapping = aes(
           x = date,
           y = (if (smoothing)
             smoothed.increase
             else
               increase),
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
    data = ungroup(data) %>%
      filter(smoothed.cumulative > quantile(smoothed.cumulative, probs = .5) & ratio >=0.01 & ratio < 0.99
    ) %>% arrange(date),
    mapping = aes(
      x =  (
        if (prevalence)
          1e-3 * smoothed.cumulative
        else
          smoothed.cumulative
      ),
      y = 100 * smoothed.increase / smoothed.cumulative,
      label = region,
      color = region
    )
  ) +
    geom_path() +
    geom_point() +
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
