box::use(shiny.blueprint[Navbar,
                         NavbarGroup,
                         NavbarHeading,
                         NavbarDivider,
                         Button, triggerEvent,
                         reactOutput,
                         renderReact,
                         Divider, Dialog,
                         AnchorButton],
         shiny[NS, moduleServer,
               div, tags, HTML,
               reactiveVal, observeEvent,
               observe, req],
         shiny.router[router_ui,
                      router_server,
                      route, route_link],
         imola[flexPanel],
         htmltools[h2, h3],
         waiter[spin_3, useWaiter,
                spin_fading_circles,
                autoWaiter, waiterPreloader])
box::use(app / view / map)
box::use(app / view / plotPage)
box::use(app / logic / datasets[data, continent_location])

#' Initialize the Shiny module UI for the World Health Dashboard
#'
#' @param id The Shiny module ID
#'
#' @export
ui <- function(id) {
  ns <- NS(id)
  flexPanel(
    useWaiter(),
    waiterPreloader(html = spin_fading_circles()),
    autoWaiter(html = spin_3(), color = "rgba(17, 20, 24, 0.005)"),
    template = NULL,
    direction = "column",
    gap = "10px",
    Navbar(
      NavbarGroup(
        NavbarHeading("World Health Dashboard"),
        NavbarDivider(),
        tags$a(
          style = "text-decoration:none",
          Button(
            minimal = TRUE,
            large = TRUE,
            icon = "home",
            text = "Home"
          ),
          href = route_link("home")
        ),
        tags$a(
          style = "text-decoration:none",
          Button(
            minimal = TRUE,
            large = TRUE,
            icon = "grouped-bar-chart",
            text = "Graph"
          ),
          href = route_link("graph")
        )
      )
    ),
    router_ui(route("home", map$ui(ns("geospatial"))),
              route("graph", plotPage$ui(ns("trend_plot"))))
  )
}

#' Initialize the Shiny module server for the World Health Dashboard
#'
#' @param id The Shiny module ID
#'
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("home")
    map$server("geospatial", data, continent_location)
    plotPage$server("trend_plot", data)
  })
}
