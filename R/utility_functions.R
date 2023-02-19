#' Combine person-level data
#' @description
#' Combine multiple individual-person datasets into a long dataset
#' @export
#'
#' @param person_data A list of person-level datasets with the following columns: locus, allele, colour, n
combine_person_data <- function(person_data) {
  aggregate(do.call(`rbind`, person_data), n ~ ., sum)
}
