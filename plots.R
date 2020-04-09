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
    # geom_text(mapping = aes(label = date)) +
    geom_abline(intercept = (-30:30),
                slope = -1,
                color = "white") +
    geom_path() +
    geom_point() +
    geom_dl(method = "angled.boxes") +
    geom_point(mapping = aes(
      y = ifelse(
        date == lockdate,
        100 * smoothed.increase / smoothed.cumulative,
        NA
      )
    ), shape = "diamond", size = 4) +
    scale_x_log10(labels = decimal_trunc) +
    scale_y_log10(labels = decimal_trunc) +
      else
        "")),
      y = "Daily new as percentage of cumulative",
      caption = element_text("Horizontal lines represent exponential growth. Diagonal lines represent equal number of new cases per day.\n Each dot is one day. Diamond shapes are start dates of shelter-in-place order (US states only and incomplete).", margin = 0)) +
    theme_covid
}
