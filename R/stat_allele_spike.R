#'
#'@export
StatAlleleSpike <- ggplot2::ggproto("StatAlleleSpike", ggplot2::Stat,
                                    compute_group = function(data, scales) {
                                      x <- data$x
                                      y <- data$y

                                      x2 <- as.numeric(rbind(x - 2, x - 1, x, x + 1, x + 2))
                                      y2 <- as.numeric(rbind(0, y / 4, y, y / 4, 0))

                                      data.frame(x = x2, y = y2)
                                    },
                                    required_aes = c("x", "y"))

#'
#'@export
stat_allele_spike <- function(mapping = NULL, data = NULL, geom = "line",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = geom,
    stat = StatAlleleSpike,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}
