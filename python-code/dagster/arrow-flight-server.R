# https://arrow.apache.org/docs/r/articles/flight.html

library(arrow)
demo_server <- load_flight_server("demo_flight_server")
server <- demo_server$DemoFlightServer(port = 8089)
server$serve()
