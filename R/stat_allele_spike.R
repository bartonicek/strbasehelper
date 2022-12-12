#'
#'@export
StatAlleleSpike <- ggplot2::ggproto("StatAlleleSpike", ggplot2::Stat,
                                    compute_group = function(data, scales, xmin = NULL, xmax = NULL) {
                                      x <- c(xmin, data$x, xmax)
                                      y <- c(0, data$y, 0)

                                      x <- as.numeric(rbind(x - 0.5, x - 0.25, x, x + 0.25, x + 0.5))
                                      y <- as.numeric(rbind(0, y / 4, y, y / 4, 0))

                                      data.frame(x = x, y = y)
                                    },
                                    setup_params = function(data, params) {
                                      if (!is.null(params$xmin) && !is.null(params$xmax))
                                        return(params)

                                      xmin <- min(data$x)
                                      xmax <- max(data$x)
                                      range <- xmax - xmin


                                      params$xmin <- xmin - 0.1 * range
                                      params$xmax <- xmax + 0.1 * range

                                      params
                                    },
                                    required_aes = c("x", "y"))

#'
#'@export
stat_allele_spike <- function(mapping = NULL, data = NULL, geom = "line",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, xmin = NULL, xmax = NULL, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = geom,
    stat = StatAlleleSpike,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmin = xmin, xmax = xmax, na.rm = na.rm, ...)
  )

}
