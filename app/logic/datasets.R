box::use(readr[read_csv])

#' @export
data <- read_csv("app/logic/optimised_data.csv")

#' @export
continent_location <- read_csv("app/logic/continent_location.csv")