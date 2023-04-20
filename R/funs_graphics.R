clrs <- MetBrewer::met.brewer("Tiepolo")

set_annotation_fonts <- function() {
  ggplot2::update_geom_defaults("label", list(family = "Noto Sans", face = "plain"))
  ggplot2::update_geom_defaults("text", list(family = "Noto Sans", face = "plain"))
}

theme_pandem <- function(base_size = 11, base_family = "Noto Sans", prior = FALSE) {
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1), family = "Noto Sans", face = "bold"),
          plot.subtitle = element_text(size = rel(0.8),
                                       family = "Noto Sans", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Noto Sans", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8),
                                    family = "Noto Sans", face = "bold"),
          axis.title.x = element_text(hjust = 0, margin = margin(t = 10)),
          axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.7), vjust = 0.5,
                                      family = "Noto Sans", face = "plain"),
          legend.key.size = unit(0.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          legend.justification = "left",
          legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Noto Sans", face = "bold"),
          strip.background = element_rect(fill = "white", colour = NA))
  
  if (prior) {
    ret <- ret +
      theme(panel.grid.major = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank())
  } else {
    ret
  }
}
