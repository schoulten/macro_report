
# Code to generate utility objects for data visualizations


# Chart colors
colors_42 <- c(
  "#282f6b", "#b22200", "#eace3f", "#224f20", "#5f487c", "#b35c1e", "#419391",
  "#839c56", "#3b89bc", "#666666", "#C1C1C1"
  )


# Global theme
theme_42 <- ggplot2::theme_light() +
  ggplot2::theme(
    plot.title          = ggtext::element_markdown(size = 20, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle       = ggplot2::element_text(size = 14),
    plot.caption        = ggtext::element_textbox_simple(
      size   = 8,
      margin = ggplot2::margin(5, 0, 0, 0)
      ),
    axis.title          = ggplot2::element_text(size = 14, face = "bold"),
    axis.text           = ggplot2::element_text(size = 12, face = "bold"),
    legend.position     = "top",
    legend.text         = ggplot2::element_text(face = "bold", size = 10)
    )
ggplot2::theme_set(theme_42)


# Format x date labels
format_x_date <- function(x) {
  dplyr::if_else(
    is.na(dplyr::lag(x)) | !lubridate::year(dplyr::lag(x)) == lubridate::year(x),
    paste(lubridate::month(x, label = TRUE), "\n", lubridate::year(x)),
    paste(lubridate::month(x, label = TRUE))
    )
}


# Scale axis date points
scale_dates <- function(
    span = "1 year",
    format = "%Y",
    expands = c(0.05, 0.08),
    axis = "x",
    ...
    ) {
  if (axis == "x") {
    ggplot2::scale_x_date(
      date_breaks = span,
      date_labels = format,
      expand      = ggplot2::expansion(mult = expands),
      ...
      )
  } else if (axis == "y") {
    ggplot2::scale_y_date(
      date_breaks = span,
      date_labels = format,
      expand      = ggplot2::expansion(mult = expands),
      ...
      )
  }
}


# Scale axis number points
scale_numbers <- function(
    breaks = scales::extended_breaks(n = 8),
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    axis = "y",
    ...
    ) {
  if (axis == "y") {
    ggplot2::scale_y_continuous(breaks = breaks, labels = labels, ...)
  } else if (axis == "x") {
    ggplot2::scale_x_continuous(breaks = breaks, labels = labels, ...)
  }
}


# Create text label for given data point
label_txt <- function(df, map, color = colors_42[1], nx = 75, ny = 0.5) {
  ggrepel::geom_label_repel(
    mapping            = map,
    data               = df,
    nudge_x            = nx,
    nudge_y            = ny,
    min.segment.length = 0,
    fill               = color,
    segment.color      = color,
    segment.size       = 0.8,
    label.size         = NA,
    color              = "white",
    size               = 4,
    fontface           = "bold"
  )
}
