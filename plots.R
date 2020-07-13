library(ggplot2)
library(directlabels)
library(dplyr)
theme_covid = theme(legend.position = "none", text = element_text(size = 20))

plot_timeseries = function(data, type, smoothing, prevalence) {
  ggplot(data = data,
         mapping = aes(
           x = date,
           y = (if (smoothing)
             smoothed.new
             else
               new),
           label = region,
           color = region
         )) +
    geom_line() +
    geom_dl(method = "angled.boxes") +
    scale_y_log10(
      labels = identity,
      limits =c(if (prevalence) .01 else 1, NA)) +
    ylab(paste("New daily",
               type,
               (if (prevalence)
                 "per hundred thousands"
                else
                  ""))) +
    theme_covid
}


plot_growthvssize = function(data, type, prevalence) {
  plot = ggplot(
    data = ungroup(data) %>%
      # filter(
      #   active > quantile(active, probs = .1, na.rm = TRUE) &
      #     !is.na(ratio)
      # ) %>%
      arrange(date),
    mapping = aes(
      x =  (if (prevalence)
        1e-3 * active
        else
          active),
      y = 100 * ratio,
      label = region,
      color = region
    )
  ) +
    # geom_text(mapping = aes(label = date)) +
    # geom_abline(intercept = (-30:30),
    #             slope = -1,
    #             color = "white") +
    geom_path() +
    geom_point() +
    geom_dl(method = "angled.boxes") +
    geom_point(mapping = aes(y = ifelse(date == lockdate,
                                        100 * ratio,
                                        NA)),
    shape = "diamond",
    size = 4) +
    scale_x_log10(labels = decimal_trunc) +
    scale_y_log10(labels = decimal_trunc) +
    labs(
      x = paste("New daily", type, (if (prevalence)
        "as percentage of population"
        else
          "")),
      y = "Daily new as percentage of moving average",
      caption = element_text(
        "Horizontal lines represent exponential growth. Each dot is one day.\nDiamond shapes are start dates of shelter-in-place order (US states only and incomplete).",
        margin = 0
      )
    ) +
    theme_covid
  ann_col = "grey"
  # if (prevalence)
  #   plot = plot +
  #     geom_hline(yintercept = 100, color = ann_col) +
  #     geom_vline(xintercept = 0.02, color = ann_col) +
  #     geom_vline(xintercept = 0.003, color = ann_col) +
  #     annotate(
  #       "text",
  #       x = c(1e-5, 1e-5),
  #       y = c(105, 95),
  #       label = c("spread", "control"),
  #       color = ann_col,
  #       size = 5
  #     ) + annotate(
  #       "text",
  #       x = c(0.0025, 0.0035, 0.015, 0.025),
  #       y = 50,
  #       label = c(
  #         "case tracking possible",
  #         "case tracking overwhelmed",
  #         "normal standards of care",
  #         "crisis standards of care"
  #       ),
  #       angle = 90,
  #       color = ann_col,
  #       size = 5
  #     )
  plot
}
