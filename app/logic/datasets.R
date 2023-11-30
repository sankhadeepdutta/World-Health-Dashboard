box::use(readr[read_csv])

#' Read the main data for the World Health Dashboard
#'
#' @export
data <- read_csv("app/logic/optimised_data.csv")

#' Read the continent location data for the World Health Dashboard
#'
#' @export
continent_location <- read_csv("app/logic/continent_location.csv")

#' Introduction text for the World Health Dashboard
#'
#' @export
dashboard_intro <- "In the wake of the global pandemic, COVID-19, the importance of maintaining impeccable hygiene and sanitation practices has never been more apparent. The World Health Dashboard serves as a powerful testament to the profound impact that these simple yet critical practices can have on the overall health and well-being of the world population over time. Join on this journey as we explore the tangible evidence that proper hygiene and sanitary practices are not just the first line of defense against pandemics, but also the foundation upon which a resilient, thriving society is built. The World Health Dashboard is your window into the transformative impact of these practices, demonstrating the path towards a healthier tomorrow."

#' Information about the dataset used in the World Health Dashboard
#'
#' @export
dataset_info <- "The dataset used in this dashboard is a part of the World Development Indicators (WDI), which is the primary World Bank collection of development indicators, compiled from officially-recognized international sources. It presents the most current and accurate global development data available, and includes national, regional and global estimates."

