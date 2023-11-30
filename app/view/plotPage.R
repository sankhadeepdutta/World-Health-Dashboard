box::use(shiny.blueprint[JS, Card, HTMLSelect, Text],
         shiny[NS, div, uiOutput, moduleServer, renderUI, reactiveVal, observeEvent],
         htmltools[HTML, h3, tags],
         imola[gridPanel])

box::use(app / view / plot)

choices = c("Asia",
            "Africa",
            "Europe",
            "Oceania",
            "North America",
            "South America",
            "Antarctica")


#' Initialize the Shiny module UI
#'
#' @param id The Shiny module ID
#'
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    div(
      class = "controls bp4-card bp4-elevation-3",
      div(
        class = "control1",
        h3("Select continent"),
        HTMLSelect(
          class = "bp4-large",
          onChange = setInput(ns("continent"), ".target.value"),
          options = choices
        )
      ),
      div(class = "control2", h3("Select country"),
          uiOutput(ns("input_country")))
    ),
    div(
      class = "level-1-plots",
      
      div(class = "level-1-plot1",
          plot$ui(ns("plot2"))),
      
      div(class = "level-1-plot2",
          plot$ui(ns("plot1")))
    ),
    
    div(class = "level-2-plot", style = "background-color: transparent;",
        plot$ui(ns("plot3")))
  )
}

#' Initialize the Shiny module server
#'
#' @param id The Shiny module ID
#' @param data The input data for the module
#'
#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    parameter <- unique(data$Series.Name)
    
    continent_name <- reactiveVal("Asia")
    continent_filtered <-
      reactiveVal(subset(data, continent == "Asia"))
    country_name <- reactiveVal("Afghanistan")
    country_filtered <-
      reactiveVal(subset(data, continent == "Asia") |> subset(name_long == "Afghanistan"))
    
    output$input_country <- renderUI({
      ns <- session$ns
      if (continent_name() == "Antarctica") {
        Text("No data available")
      }
      else{
        countries <-
          unique(continent_filtered()$name_long)
        
        HTMLSelect(class = "bp4-large", onChange = setInput(ns("country"), ".target.value"),
                   options = countries)
      }
    })
    
    observeEvent(input$continent, {
      continent_name(input$continent)
      continent_filtered(subset(data, continent == continent_name()))
      country_name(unique(continent_filtered()$name_long)[1])
      country_filtered(subset(continent_filtered(), name_long == country_name()))
    })
    
    observeEvent(input$country, {
      country_name(input$country)
      country_filtered(subset(continent_filtered(), name_long == country_name()))
    })
    
    observeEvent(country_filtered(), {
      plot$server(
        "plot1",
        country_filtered(),
        parameter[c(1, 3, 4)],
        "Different mortality rates of population over time",
        2
      )
      plot$server("plot2",
                  country_filtered(),
                  parameter[2],
                  "Population life expectancy over time", 3)
      plot$server(
        "plot3",
        country_filtered(),
        parameter[5:8],
        "Percentage of population practicing hygienes over time",
        2
      )
    })
  })
}

#' Set input value for Shiny module
#'
#' @param inputId The input ID
#' @param accessor The accessor for the input value
#'
#' @return JavaScript code for setting Shiny input value
#'
#' @export
setInput <- function(inputId, accessor = NULL) {
  JS(paste0("x => Shiny.setInputValue('", inputId, "', x", accessor, ")"))
}
