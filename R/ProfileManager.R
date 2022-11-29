
#' Profile Manager
#' @description
#' An R6 class that handles fetching STRBase tables and linking it with profile data
#'
#'
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
      bp_col_num <- grep(set_pattern, names(tab3))
      variant_rows <- grepl("variant", tab3[, ncol(tab3), drop = TRUE])

      bp_tab <- tab3[!variant_rows, c(1, bp_col_num)]
      names(bp_tab) <- c("allele", "base_pairs")
      bp_tab$locus <- loci[i]
      bp_tab$allele <- as.numeric(gsub("^(\\d{2}).+$", '\\1', bp_tab$allele))
      bp_tab$base_pairs <- as.numeric(gsub(" bp$", "", bp_tab$base_pairs))

      bp_tab <- na.omit(bp_tab)
      bp_tab <- bp_tab[!duplicated(bp_tab), ]

      locus_bp_tab <- rbind(locus_bp_tab, bp_tab)
    }

    self$profile_data <- dplyr::left_join(self$profile_data, locus_bp_tab,
                               by = c("locus", "allele"))

  }
))
