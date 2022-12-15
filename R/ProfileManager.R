
#' Profile Manager
#' @description
#' An R6 class that handles fetching STRBase tables and linking it with profile data
#'
#' @export
ProfileManager <- R6::R6Class("ProfileManager", list(
  #' @field multiplex The multiplex to be used
  multiplex = NULL,
  #' @field profile_data Profile data, stored as a long dataframe (locus, colour, allele, count)
  profile_data = NULL,
  #' @field unique_loci Vector of unique loci present in the data
  unique_loci = NULL,
  #' @field invalid_loci Vector of loci to skip during fetching and data cleaning
  invalid_loci = c("AMEL"),
  #' @field tables A list of all STRBase tables, one element per each unique (valid) locus
  tables = list(),

  #' @description
  #' Create a new ProfileManager object
  #' @param profile_data Profile data, in a long format
  #' @param multiplex The multiplex to be used for linking
  initialize = function(profile_data, multiplex = "Powerplex 16") {

    cols <- c("allele", "colour", "locus", "n")
    stopifnot(all.equal(sort(names(profile_data)), cols))

    self$multiplex <- multiplex
    self$profile_data <- profile_data
    self$unique_loci <- unique(profile_data$locus)
  },

  #' @description
  #' Retrives multiple sets of tables with information about gene loci from STRBase
  #' @param loci Loci to be retrived (defaults to all unique loci in the profile data)
  fetch_strbase_tables = function(loci = NULL) {

    if (is.null(loci)) {
      loci <- self$unique_loci
    }

    for (i in seq_along(loci)) {

      if (loci[i] %in% self$invalid_loci) next

      # Print progress
      cat("Fetching ", sprintf('%-12s', paste0(loci[i], "...")),
          '[', i, '/', length(loci), ']', '\n', sep = "")

      url <- paste0("https://strbase.nist.gov/str_",
                    loci[i], ".htm")
      self$tables[[loci[i]]] <- rvest::read_html(url) |>
        rvest::html_table(fill = TRUE, header = TRUE)
    }
  },

  #' @description
  #' Extract basepair information from STRBase tables
  extract_basepairs = function() {
    if (length(self$tables) == 0) {
      stop("Please fetch tables from STRBase first using $fetch_strbase_tables() method")
    }

    loci <- names(self$tables)
    locus_bp_tab <- data.frame()

    for (i in seq_along(loci)) {

      if (loci[i] %in% self$invalid_loci) next

      tab2 <- self$tables[[loci[i]]][[2]]
      tab3 <- self$tables[[loci[i]]][[3]]

      set <- tab2[grep("PowerPlex 16", tab2[, 3, drop = TRUE]), 1, drop = TRUE]
      set_number <- as.numeric(sub('^Set ([1-9])$', '\\1', set))
      set_pattern <- paste0("^Set [0-9,]*", set_number, "[,0-9]*$")

      col_names <- names(tab3)
      ref_col <- grep("^Ref.$", col_names)
      rep_str_col <- grep("^Repeat.+Structure", col_names)
      bp_col <- grep(set_pattern, col_names)

      variant_rows <- grepl("variant", tab3[, ref_col, drop = TRUE])
      empty_rep_rows <- tab3[, rep_str_col] == ""

      bp_tab <- tab3[!variant_rows & !empty_rep_rows, c(1, bp_col)]
      names(bp_tab) <- c("allele", "base_pairs")
      bp_tab$locus <- loci[i]
      bp_tab$allele <- as.numeric(gsub("^(\\d{2}\\.?\\d?).*[[:blank:]].*$", "\\1", bp_tab$allele))
      bp_tab$base_pairs <- as.numeric(gsub(" bp$", "", bp_tab$base_pairs))

      bp_tab <- na.omit(bp_tab)
      bp_tab <- bp_tab[!duplicated(bp_tab), ]

      locus_bp_tab <- rbind(locus_bp_tab, bp_tab)
    }

    if ("base_pairs" %in% names(self$profile_data)) {
      self$profile_data <- subset(self$profile_data, select = -c(base_pairs))
    }
    self$profile_data <- dplyr::left_join(self$profile_data, locus_bp_tab,
                               by = c("locus", "allele"))

  },

  #' @description
  #' Plot base pair information
  profile_plot = function() {

    profile_data <- subset(self$profile_data, !is.na(base_pairs))
    max_count <- max(profile_data$n)

    ggplot2::ggplot(profile_data, ggplot2::aes(x = base_pairs, y = n, col = colour)) +
      ggplot2::geom_line(stat = "allele_spike") +
      ggplot2::geom_label(ggplot2::aes(group = locus, y = max(n) + 0.5, label = locus),
                 stat = "summary", fun = "mean", orientation = "y") +
      ggplot2::geom_text(ggplot2::aes(y = -Inf, label = allele), vjust = 2, size = 3) +
      ggplot2::scale_x_continuous(position = "top") +
      ggplot2::scale_color_manual(values = c("#377eb8", "#4daf4a", "black")) +
      ggplot2::ylim(0, max_count + 1) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::facet_wrap(~ colour, nrow = 3, scales = "free_x") +
      ggplot2::labs(x = "Base pairs", y = NULL) +
      ggplot2::guides(col = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x  = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = '#F7F7F2'),
            panel.border = ggplot2::element_rect(fill = NA, colour = '#999690'),
            panel.spacing = ggplot2::unit(2, "lines"),
            plot.margin = ggplot2::unit(c(1, 1, 2, 1), "lines"),
            plot.background = ggplot2::element_rect(fill = "#DEDED9"))


  }
))
