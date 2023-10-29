box::use(shiny.blueprint[Navbar, NavbarGroup, NavbarHeading, NavbarDivider, Button, triggerEvent, reactOutput, renderReact, Divider, Dialog, AnchorButton],
         shiny[NS, moduleServer, div, tags, HTML, reactiveVal, observeEvent, observe, req],
         shiny.router[router_ui, router_server, route, route_link],
         imola[flexPanel],
         htmltools[h2, h3],
         waiter[spin_3, useWaiter, spin_fading_circles, autoWaiter, waiterPreloader])

box::use(app / view / map)
box::use(app / view / plotPage)
box::use(app / logic / datasets[data, continent_location])

options(warn = -1)

#' @export
ui <- function(id) {
  ns <- NS(id)
  flexPanel(
    useWaiter(),
    waiterPreloader(html = spin_fading_circles()),
    autoWaiter(html = spin_3(), color = "rgba(17, 20, 24, 0.005)"),
    
    tags$head(tags$style(
      HTML(
        'body {background-color: #F8F8FF;}
        .overlay {display: flex; justify-content: center; align-iems: center;}
        .logo {margin: 15px;}
        .bp4-dialog {width:580px;border-radius: 20px;}
        .dialog-text {font-weight:600;}
        .bp4-card {border-radius: 10px;}
        .bp4-navbar {border-radius: 10px;}
        .exit {margin-left: 82%; margin-top:15px; border-radius:20px; min-height: 40px; padding: 5px 20px;}
        .link {border-radius:20px;}
        .sk-fading-circle {height: 70px; width: 70px;}
        .bp4-navbar-heading {background-color: #3B444B; border-radius: 8px; margin: 0; padding: 8px; color: white;}
        .bp4-navbar {padding-left: 8px}
        .bp4-minimal {outline: #fff}
        .bp4-html-select select {border-radius: 20px; outline: #fff;}
        .bp4-button {outline: #fff}
        .bp4-navbar {box-shadow: 0 0 0 1px rgb(17 20 24 / 10%), 0 1px 1px rgb(17 20 24 / 20%), 0 2px 6px rgb(17 20 24 / 20%);}'
      )
    )),
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
            large = T,
            icon = "home",
            text = "Home"
          ),
          href = route_link("home")
        ),
        tags$a(
          style = "text-decoration:none",
          Button(
            minimal = TRUE,
            large = T,
            icon = "grouped-bar-chart",
            text = "Graph"
          ),
          href = route_link("graph")
        )
      )
    ),
    router_ui(route("home", map$ui(ns(
      "map"
    ))),
    route("graph", plotPage$ui(ns(
      "trendPlot"
    ))))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("home")
    ns <- session$ns
    
    map$server("map", data, continent_location)
    plotPage$server("trendPlot", data)
    
  })
}
